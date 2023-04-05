### Setup
# Clear memory
rm(list=ls())

# Locations of folders
data_folder <<- "Data/"
output_folder <<- "Output/"

# Import libraries
library(tidyverse)

#-------------------------------------------------------------
### Import clean data
  data <- read.csv(paste0(data_folder, "airbnb_shanghai_cleaned.csv"),
                   sep = ",", header = TRUE, row.names = 1)

#-------------------------------------------------------------
### Data Processing for Visualization
# Create log variables
  data <- data |>
    mutate(
      ln_price = log(price),
      ln_price = ifelse(is.na(ln_price),0, ln_price),
    )
  
# Filter price for a reasonable range
  summary(data$price)
  ggplot(data, aes(x = price)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white", fill = "steelblue") +
    geom_density(alpha = .2, fill = "#FF6666") +
    theme_minimal() +
    labs(x = "Price", y = "Density")
  
  data <- data |>
    filter(price < 1000)

# Histograms of price and ln_price
  ggplot(data, aes(x = price)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white", fill = "steelblue") +
    geom_density(alpha = .2, fill = "#FF6666") +
    theme_minimal() +
    labs(x = "Price", y = "Density")
  ggsave(file.path(output_folder, "histogram_price.png"), width = 6, height = 4, dpi = 300, bg="white")
  
  ggplot(data, aes(x = ln_price)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white", fill = "steelblue") +
    geom_density(alpha = .2, fill = "#FF6666") +
    theme_minimal() +
    labs(x = "ln(Price)", y = "Density")
  ggsave(file.path(output_folder, "histogram_lnprice.png"), width = 6, height = 4, dpi = 300, bg="white")
  
#-------------------------------------------------------------
### Data Visualisation
# Heatmap of correlations between numerical variables
  numeric_data <- data |>
    select(n_accommodates, n_bathrooms, n_review_scores_rating, n_number_of_reviews, n_reviews_per_month, n_minimum_nights, n_beds, n_days_since, price)
  
  cor_matrix <- cor(numeric_data)
  cor_matrix <- cor_matrix |>
    as.data.frame() |>
    rownames_to_column(var = "Var1") |>
    pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")
  
  ggplot(cor_matrix, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  ggsave(file.path(output_folder, "correlation_heatmap.png"), width = 6, height = 4, dpi = 300, bg="white")

# Scatter plot of price against n_review_scores_rating
  ggplot(
    data, 
    aes(x = n_review_scores_rating, y = price, color = f_room_type)
  ) + geom_point() +
    geom_smooth() +
    labs(x = "n_review_scores_rating", y = "price", color = "f_room_type")
  ggsave(file.path(output_folder, "scatterplot_price_reviewrating.png"), width = 6, height = 4, dpi = 300, bg="white")
  
# Scatter plot of price against n_accommodates for each region
  # Calculate average price and n_accommodates for each region
  neighbourhood_avg <- data |>
    group_by(f_neighbourhood_cleansed) |>
    summarise(avg_price = mean(price),
              avg_accommodates = mean(n_accommodates))
  
  # Create scatter plot with different colours for different room types
  ggplot(data, aes(x = n_accommodates, y = price, color = f_room_type)) +
    geom_point() +
    facet_wrap(~f_neighbourhood_cleansed, nrow = 3) +
    geom_hline(aes(yintercept = avg_price), data = neighbourhood_avg, 
               linetype = "dashed", color = "black") +
    geom_vline(aes(xintercept = avg_accommodates), data = neighbourhood_avg, 
               linetype = "dashed", color = "black") +
    labs(x = "n_accommodates", y = "price", color = "f_room_type") +
    theme(legend.position = "bottom", legend.text = element_text(size = 6), 
        strip.text.x = element_text(size = 8, face = "bold")) +
    guides(color = guide_legend(ncol = 3)) +
    scale_color_brewer(palette = "Set1")
  ggsave(file.path(output_folder, "scatterplot_price_accommodates.png"), width = 6, height = 4, dpi = 300, bg="white")
  
# Scatter plot of longitude and latitude colored by price
  ggplot(data, aes(x = longitude, y = latitude, color = price, shape = f_neighbourhood_cleansed)) +
    geom_point(size = 3) +
    scale_color_gradient(low = "blue", high = "red") +
    scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 17, 19, 22, 23, 25)) +
    theme_minimal() +
    guides(shape = guide_legend(title = "Neighbourhood")) +
    theme(legend.key.size = unit(0.3, "cm"))
  ggsave(file.path(output_folder, "price_map.png"), width = 6, height = 4, dpi = 300, bg="white")
  