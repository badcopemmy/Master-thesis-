library(readxl)
library(dplyr)
library(glmnet)

#####################  Loading and preparing data  ############################ 
orig_data <- read_excel("Data_excel.xlsx")
  head(orig_data)
  summary(orig_data)
  orig_data$pd <- as.numeric(orig_data$pd)
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

data$def <- ifelse(data$rating == "C4-2", 1, 0)
    head(data)
    
X1 <- c(0.03, 0.01, 0.03, 0.002, 0.006, -0.3)
X2 <- c(-0.02, 0.051, 0.0222, -0.044, -0.0999, -0.1)
X3 <- c(-0.01, 0.1, -0.1, -0.2, -0.01, 0.3)
#mv_matrix <- matrix(c(X1, X2, X3), nrow = length(X1), byrow = TRUE)    
#Outliers
#Plots
#Correlation


    
#####################  Lasso regression  ############################ 
#Prepare data
    #Delete all US obs
    data <- data[data$rating != "US", ]
   

    #Order dates and create obr_nbr column to match macro entries
    obs_dates <- unique(data$date) #The number of unique observation dates
    rank_values <- rank(obs_dates, ties.method = "min")
    data$obs_nbr <- rank_values[match(data$date, obs_dates)]
    head(data)
    tail(data)
   
    
    #Add macros to matching dates
      #Create function that matches values
      extract_value <- function(obs_number, vector) {
        return(vector[obs_number])
        }
      #Apply to all Xi
      for (i in 1:3) {  # Replace 3 with the total number of vectors you have
        col_name <- paste0("X", i)
        data[[col_name]] <- sapply(data$obs_nbr, function(x) extract_value(x, get(col_name)))
        }
      head(data)
      tail(data)
  
############################ Lasso regression
    #Extract the correct matrices and vectors for lasso input
    respons <- as.matrix(R2[, "def"])
    respons
    mean(respons)
    predictors <- as.matrix(R1[, c("pd", "X1", "X2", "X3")])
    cv_model <- cv.glmnet(predictors, respons, family = "binomial", alpha = 1)
    best_lambda <- cv_model$lambda.min
    lasso_model <- glmnet(predictors, respons, family = "binomial", alpha = 1, lambda = best_lambda)   
    coef(lasso_model)  
#############################
    
#Define a function to perform lasso regression for each dataset
  lasso_regression <- function(df) {
    # Extract the "pd-diffs" column from the dataset
    respons <- as.matrix(df[[, "def"]])
    predictors <- as.matrix(df[[, c("pd", "X1", "X2", "X3")]])
    cv_model <- cv.glmnet(predictors, respons, family = "binomial", alpha = 1)
    best_lambda <- cv_model$lambda.min
    lasso_model <- glmnet(predictors, respons, family = "binomial", alpha = 1, lambda = best_lambda)
    return(lasso_model)
  }
  #Apply to all rating groups
  lasso_results <- lapply(rating_sub, lasso_regression)

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
