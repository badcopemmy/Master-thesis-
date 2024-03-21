library(readxl)
library(dplyr)
library(glmnet)

#####################  Loading and preparing data  ############################ 
orig_data <- read_excel("Data_excel.xlsx")
  head(orig_data)
  summary(orig_data)
  sum(is.na(orig_data)) #Check if any values are NA
  
#Create a smaller subset to be able to run the code faster...
  subset_size <- 5000
  sample_accounts <- sample(unique(orig_data$cust), subset_size)
  orig_data <- orig_data[orig_data$cust %in% sample_accounts, ]

#Create training and test set
    grouped_orig_data <- orig_data %>%
      group_by(cust)
    train_prop <- 0.7  #proportion of orig_data to assign to the training set
    n_train_observations <- ceiling(train_prop * nrow(orig_data))
    group_assignment <- rep("test", nrow(orig_data)) #vector to store the group assignment
    # Assign each group (customer) to either training or test set
    for (i in unique(grouped_orig_data$cust)) {
      customer_orig_data <- orig_data[orig_data$cust == i, ]
      n_customer_observations <- nrow(customer_orig_data)
      n_customer_train <- ceiling(train_prop * n_customer_observations)
      train_indices <- sample(1:n_customer_observations, n_customer_train)
      group_assignment[which(orig_data$cust == i)[train_indices]] <- "train"
    }
    # Add the group assignment to orig_data
    orig_data$group <- group_assignment
    # Split orig_data into training and test sets
    training_set <- orig_data[orig_data$group == "train", ]
    test_set <- orig_data[orig_data$group == "test", ]
    data <- training_set #rename
    data <- subset(data, select = -group) #delete group column
    head(data)
    

#####################  MV analysis  ################################     

X1 <- c(0.03, 0.01, 0.03, 0.002, 0.006)
X2 <- c(-0.02, 0.051, 0.0222, -0.044, -0.0999)
mv_matrix <- matrix(c(X1, X2), nrow = length(X1), byrow = TRUE)    
mv_matrix

#Outliers
#Plots
#Correlation


    
#####################  Lasso regression  ############################ 
#Prepare data
    # Split the data into subsets containing only one rating category
    rating_count <- rating_counts <- table(data$rating)
    rating_count
    rating_sub <- split(data, data$rating)
    rating_sub[1]
    head(data)
    #Delete all US obs
    data <- data[data$rating != "US", ]
    head(data)
  
# Define a function to perform lasso regression for each dataset
lasso_regression <- function(dataset, mv_matrix) {
  # Extract the "pd-diffs" column from the dataset
  pd_diffs <- dataset[["pd_diff"]]
  cv_model <- cv.glmnet(mv_matrix, pd_diffs, alpha = 1)
  best_lambda <- cv_model$lambda.min
  lasso_model <- glmnet(mv_matrix, pd_diffs, alpha = 1, lambda = best_lambda)
  return(lasso_model)
}
# Apply the function for each dataset in the list pd_diff_list
lasso_models <- lapply(pd_diff_list, function(dataset) {
  lasso_regression(dataset, mv_matrix)
})

#Extract relevant info from the resulting models
extract_values <- function(model) {
  return(list(model = model, coefficients = coef(model))) # Return both the model and its coefficients
  }
result <- lapply(lasso_models, extract_values)
# View the extracted values for the first model in the list
print(result[[2]])



##################################  Predictions  ############################ 
#Predictions for 6 years ahead. X1 and X2 represent expected change in macros.
  X1 <- c(0.01, 0.04, 0.01, 0.001, 0.1, 0.03)
  X2 <- c(-0.01, -0.051, -0.01, 0.4, -0.09, -0.1)
  new_data <- matrix(c(X1, X2), nrow = length(X1), byrow = TRUE) 
  
  predicted_responses <- lapply(lasso_models, function(model) {
    predict(model, newx = new_data, s = model$lambda)
  })
  
  predicted_responses
  
################################################################################
rm(list=ls())
