### Setup
# Clear memory
  rm(list=ls())

# Locations of folders
  data_folder <<- "Data/"
  output_folder <<- "Output/"

# Import libraries
  library(tidyverse)
  library(modelsummary)

#-------------------------------------------------------------
### Clean Raw Data
# Import raw data
  data <- read.csv(paste0(data_folder, "listings.csv"),
                   sep=",", header = TRUE, stringsAsFactors = FALSE)

# Drop unnecessary variables
  data <- data |>
    select(-c("host_thumbnail_url",
              "host_picture_url",
              "listing_url",
              "picture_url",
              "host_url",
              "last_scraped",
              "description",
              "neighborhood_overview",
              "host_about",
              "host_response_time",
              "name",
              "host_location"))

# Drop broken lines - where id is not a character of numbers
  data <- data |>
    mutate(junk = grepl("[[:alpha:]]", id)) |>
    filter(junk == FALSE) |>
    select(-junk)

# Look at the classes and types of data
  sapply(data, class)
  sapply(data, typeof)

# Remove percentage signs for rating variables
  for (perc in c("host_response_rate","host_acceptance_rate")){
    data[[perc]] <- gsub("%","",as.character(data[[perc]]))
  }

# Remove $ and convert to numeric values for price variable
  data$price <- as.numeric(gsub("\\$|,", "", data$price))
  
# Remove Chinese characters and / character for neighbourhood_cleansed
  data$neighbourhood_cleansed <- str_replace_all(data$neighbourhood_cleansed, "[\u4e00-\u9fff/ ]+", "")

# Format binary variables
  for (binary in c("host_is_superhost",
                   "host_has_profile_pic",
                   "host_identity_verified",
                   "instant_bookable",
                   "has_availability")){
    data[[binary]][data[[binary]]=="f"] <- 0
    data[[binary]][data[[binary]]=="t"] <- 1
  }

# Deal with amenities variables
  # Remove the brackets, quotes, and spaces, and split the remaining string at each comma
  data <- data |>
    mutate(amenities = gsub("\\[|\\]|\"|\\s", "", amenities)) |>
    mutate(amenities = strsplit(amenities, ",") |> as.list())

  # Convert amenities to lowercase
  data$amenities <- lapply(data$amenities, tolower)

  # Define and use function to filter out amenities containing backslashes
  filter_amenities <- function(amenities_list) {
    return(amenities_list[!grepl("\\\\", amenities_list)])
  }
  data$amenities <- lapply(data$amenities, filter_amenities)

  # Generate corresponding dummy variables 
  levs <- levels(factor(unlist(data$amenities)))
  data <- cbind(data,as.data.frame(do.call(rbind, lapply(lapply(data$amenities, factor, levs), table))))
  data <- select(data, -amenities)

# Deal with bathrooms variable: extract the number in the bathrooms_text into bathrooms.
  extract_bathrooms <- function(row) {
    bathroom_text <- row["bathrooms_text"]
    if (is.na(bathroom_text)) {
      return(bathroom_text)
    } else if (bathroom_text %in% c("Half-bath", "Shared half-bath", "Private half-bath")) {
      return(0.5)
    } else {
      tryCatch(as.numeric(strsplit(bathroom_text, " ")[[1]][1]), error = function(e) return(NA))
    }
  }
  data$bathrooms <- apply(data, 1, extract_bathrooms)

# Replace :, +, and - with _ (mainly for amenities dummies)
  names(data) <- gsub("[:+-]", "_", names(data))

#-------------------------------------------------------------
### Data Processing for Analysis
# Create numerical variables (beginning with n_)
  # Add new numeric columns from certain columns
  numericals <- c("accommodates",
                  "bathrooms",
                  "review_scores_rating",
                  "number_of_reviews",
                  "reviews_per_month",
                  "minimum_nights",
                  "beds")
  data <- data |>
    mutate_at(vars(numericals), funs("n"=as.numeric))
  
  # Rename columns so they start with n_ as opposed to end with _n
  nnames <- data |>
    select(ends_with("_n")) |>
    names()
  nnames_i <- match(nnames, colnames(data))
  colnames(data)[nnames_i] <- paste0("n_", numericals)
  
  # Create days since first review
  data <- data |>
    mutate(
      n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                  as.Date(first_review ,format="%Y-%m-%d")))
  
# Create factor (categorical) variables (beginning with f_)
  # Existing variables
  table(data$room_type)
  data <- data |>
    filter(room_type != "Hotel room") # too few observations
  
  table(data$property_type)
  table(data$neighbourhood_cleansed)
  
  data <- data |>
    mutate(f_room_type = factor(room_type)) |>
    mutate(f_property_type = factor(property_type)) |>
    mutate(f_neighbourhood_cleansed = factor(neighbourhood_cleansed))
  
  # New variables
  # Pool accommodations with 0,1,2,10 bathrooms
  data <- data |>
    mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,10), labels=c(0,1,2), right = F))
  
  # Pool number of reviews to 3 categories: none, 1-51 and >51
  data <- data |>
    mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))
  
  # Pool and categorize the number of minimum nights: 1, 2, 3, 3+
  data <- data |>
    mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))
  
# Create amenities dummy variables (use distinguishable new names beginning with d_)
  which(names(data) == "2_5yearsold") # first dummy
  which(names(data) == "xiaomirefrigerator") # last dummy
  dummies <- names(data)[seq(62,320)]
  data <- data |>
    mutate_at(vars(all_of(dummies)), funs("d" = (.)))
  
  # Rename columns so they start with d_ as opposed to end with _d
  dnames <- data |>
    select(ends_with("_d")) |>
    names()
  dnames_i <- match(dnames, colnames(data))
  colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))
  
# Keep columns if contain d_, n_,f_, p_, usd_ and some useful variables
  data <- data |>
    select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id,
           neighbourhood_cleansed, property_type, room_type,
           longitude, latitude)
  
#-------------------------------------------------------------
### Deal with Missing Values
# Change Infinite values with NAs
  for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)
  
# Where do we have missing variables now?
  to_filter <- sapply(data, function(x) sum(is.na(x)))
  to_filter[to_filter > 0]
  
# What to do with missing values?
  # 1. Drop if no target
  # Actually, there is no missing value at price in my data set, but this is the first step generally
  data <- data |>
    drop_na(price)
  
  # 2. Replace missing values in terms of domain knowledge
  data <- data |>
    mutate(
      n_bathrooms =  ifelse(is.na(n_bathrooms), 1, n_bathrooms),
      n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds),
      f_bathroom = ifelse(is.na(f_bathroom),1, f_bathroom), 
      f_minimum_nights = ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
      f_number_of_reviews = ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews)
    )
  
  # 3. Replace missing variables with zeros + add flags
  data <- data |>
    mutate(
      flag_days_since=ifelse(is.na(n_days_since),1, 0),
      n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
      flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
      n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
      flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
      n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month),
      flag_n_number_of_reviews=ifelse(n_number_of_reviews==0,1, 0)
    )

# Where do we have missing variables now?
  to_filter <- sapply(data, function(x) sum(is.na(x)))
  to_filter[to_filter > 0]

#-------------------------------------------------------------
### Clean data skimming and saving
# Look at data
  datasummary_skim(data$id)
  datasummary_skim(data, 'categorical')

# Write new data csv
  write.csv(data, file = paste0(data_folder, "airbnb_shanghai_cleaned.csv"))
  