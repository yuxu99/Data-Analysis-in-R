### Setup
# Clear memory
rm(list=ls())

# Locations of folders
data_folder <<- "Data/"
output_folder <<- "Output/"

# Import libraries
library(rattle)
library(tidyverse)
library(caret)
library(ranger)
library(Hmisc)
library(knitr)
library(kableExtra)
library(xtable)
library(pdp)

#-------------------------------------------------------------
### Import clean data
data <- read.csv(paste0(data_folder, "airbnb_shanghai_cleaned.csv"),
                 sep = ",", header = TRUE, row.names = 1)

#-------------------------------------------------------------
### Data Processing for Prediction
# Focus on normal apartments, n_accommodates < 8
  data <- data |>
    filter(n_accommodates < 8)

# Filter price for a reasonable range
  summary(data$price)
  data <- data |>
    filter(price < 1000)
  
#-------------------------------------------------------------
### Create Train and Holdout Samples
  set.seed(2801)

  train_indices <- as.integer(createDataPartition(data$price, p = 0.7, list = FALSE))
  data_train <- data[train_indices, ]
  data_holdout <- data[-train_indices, ]
    
  dim(data_train)
  dim(data_holdout)

#-------------------------------------------------------------
### Data Prediction - Random Forest
# Predictors defining
  # Basic variables
  basic_vars <- c(
    "n_accommodates", "n_beds", "n_days_since",
    "f_property_type","f_room_type", "f_bathroom",
    "f_neighbourhood_cleansed")
  
  # Reviews
  reviews <- c("n_number_of_reviews", "flag_n_number_of_reviews", 
               "n_review_scores_rating", "flag_review_scores_rating")
  
  # Dummy variables
  amenities <-  grep("^d_.*", names(data), value = TRUE)
  
  # Predictors needed in Random Forest
  predictors <- c(basic_vars, reviews, amenities)
  
# Random Forest Model
  # Do 5-fold CV
  train_control <- trainControl(method = "cv",
                                number = 5,
                                verboseIter = FALSE)
  
  # Set tuning for random forest model
  tune_grid <- expand.grid(
    .mtry = c(8, 10, 12),
    .splitrule = "variance",
    .min.node.size = c(5, 10, 15)
  )
  
  # Random forest model
  set.seed(1234)
  system.time({
    rf_model <- train(
      formula(paste0("price ~", paste0(predictors, collapse = " + "))),
      data = data_train,
      method = "ranger",
      trControl = train_control,
      tuneGrid = tune_grid,
      importance = "impurity"
    )
  })
  rf_model
  
# Evaluate random forest model and save outputs
  rf_tuning_model <- rf_model$results |>
    dplyr::select(mtry, min.node.size, RMSE) |>
    dplyr::rename(nodes = min.node.size) |>
    spread(key = mtry, value = RMSE)
  rf_tuning_model
  
  # Convert the table to kable format
  rf_tuning_model_table <- kable(rf_tuning_model, format = "html", caption = "Random Forest Model Tuning Results") |>
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) |>
    column_spec(1, bold = T)
  save_kable(rf_tuning_model_table, "Output/rf_tuning_model_table.html")
  
#-------------------------------------------------------------
### Model Diagnostics
# Variable importance plots
  # First need a function to calculate grouped varimp
  group.importance <- function(rf.obj, groups) {
    var.imp <- as.matrix(sapply(groups, function(g) {
      sum(importance(rf.obj)[g], na.rm = TRUE)
    }))
    colnames(var.imp) <- "MeanDecreaseGini"
    return(var.imp)
  }

  # Variable importance plots
  rf_model_var_imp <- importance(rf_model$finalModel)/1000
  rf_model_var_imp_df <-
    data.frame(varname = names(rf_model_var_imp),imp = rf_model_var_imp) |>
    mutate(varname = gsub("f_neighbourhood_cleansed", "Borough:", varname) ) |>
    mutate(varname = gsub("f_room_type", "Room type:", varname) ) |>
    arrange(desc(imp)) |>
    mutate(imp_percentage = imp/sum(imp))
  
  # 1) Full variable importance plot, top 10 only
  rf_model_var_imp_plot <- ggplot(rf_model_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
    geom_point(color=colors()[37], size=1) +
    geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=colors()[37], size=0.75) +
    ylab("Importance (Percent)") +
    xlab("Variable Name") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_bw() +
    theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
          axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
  rf_model_var_imp_plot
  ggsave(file.path(output_folder, "variable_importance_plot_top_10.png"), plot = rf_model_var_imp_plot, width = 6, height = 4, dpi = 300)
  
  # 2) Variable importance plot grouped
  varnames <- rf_model$finalModel$xNames
  f_neighbourhood_cleansed_varnames <- grep("f_neighbourhood_cleansed",varnames, value = TRUE)
  f_property_type_varnames <- grep("f_property_type",varnames, value = TRUE)
  f_room_type_varnames <- grep("f_room_type",varnames, value = TRUE)
  
  groups <- list(f_neighbourhood_cleansed=f_neighbourhood_cleansed_varnames,
                 f_property_type = f_property_type_varnames,
                 f_room_type = f_room_type_varnames,
                 f_bathroom = "f_bathroom",
                 n_days_since = "n_days_since",
                 n_accommodates = "n_accommodates",
                 n_beds = "n_beds")
  
  rf_model_var_imp_grouped <- group.importance(rf_model$finalModel, groups)
  rf_model_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_var_imp_grouped),
                                              imp = rf_model_var_imp_grouped[,1])  |>
    mutate(imp_percentage = imp/sum(imp))
  
  rf_model_var_imp_grouped_plot <-
    ggplot(rf_model_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
    geom_point(color=colors()[37], size=1) +
    geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=colors()[37], size=0.7) +
    ylab("Importance (Percent)") +   xlab("Variable Name") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_bw() +
    theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
          axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
  rf_model_var_imp_grouped_plot
  ggsave(file.path(output_folder, "variable_importance_plot_grouped.png"), rf_model_var_imp_grouped_plot, width = 6, height = 4, dpi = 300)
  
# Partial Dependence Plots
  # Number of accommodates
  pdp_n_acc <- pdp::partial(rf_model, pred.var = "n_accommodates", pred.grid = distinct_(data_holdout, "n_accommodates"), train = data_train)
  pdp_n_acc_plot <- pdp_n_acc |>
    autoplot( ) +
    geom_point(color=colors()[37], size=2) +
    geom_line(color=colors()[37], size=1) +
    ylab("Predicted price") +
    xlab("Accommodates (persons)") +
    scale_x_continuous(limit=c(1,7), breaks=seq(1,7,1))+
    theme_bw()
  pdp_n_acc_plot
  ggsave(file.path(output_folder, "partial_dependence_plot_naccommodates.png"), pdp_n_acc_plot, width = 6, height = 4, dpi = 300)
  
  # Room types
  pdp_n_roomtype <- pdp::partial(rf_model, pred.var = "f_room_type", pred.grid = distinct_(data_holdout, "f_room_type"), train = data_train)
  pdp_n_roomtype_plot <- pdp_n_roomtype |>
    autoplot( ) +
    geom_point(color=colors()[37], size=2) +
    ylab("Predicted price") +
    xlab("Room type") +
    scale_y_continuous(limits=c(300,400), breaks=seq(300,400, by=10)) +
    theme_bw()
  pdp_n_roomtype_plot
  ggsave(file.path(output_folder, "partial_dependence_plot_roomtype.png"), pdp_n_roomtype_plot, width = 6, height = 4, dpi = 300)
  