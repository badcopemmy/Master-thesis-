library(readxl)
library(dplyr)
library(glmnet)

#####################  Loading and preparing data  ############################ 
orig_data <- read_excel("/Users/Emmy/Desktop/exjobb danskebank/Data/TTC_DATA_2021.xlsx")
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

#add fake ratings
    num_rows <- nrow(data)
    random_ratings <- sample(c("A", "B", "C"), num_rows, replace = TRUE)
    data$rating <- random_ratings
    head(data)

#####################  Preparing for regression analysis  ############################ 

    # Split the data into subsets containing only one rating category
    rating_sub <- split(data, data$rating)
    rating_sub[1]
    # Calculate average PD for each unique date in each dataset in rating_sub. 
    average_pd <- lapply(rating_sub, function(dataset) {
      # Aggregate PD by date
      avg_pd <- aggregate(pd ~ date, data = dataset, FUN = mean, na.rm = TRUE)
      return(avg_pd)
    })
    # print resulting average pd list
    lapply(average_pd, function(avg_pd) {
      print(avg_pd)
    })
    

    # Calculate the difference in the "pd" column between consecutive values for each dataset
    pd_diffs <- lapply(average_pd, function(avg_pd) {
      avg_pd$pd_diff <- c(NA, diff(avg_pd$pd))
      return(avg_pd)
    })
    
    #View the results
    pd_diffs

#Lasso regression
X1 <- c(0.03, 0.01, 0.03, 0.002, 0.006)
X2 <- c(-0.02, 0.051, 0.0222, -0.044, -0.0999)
mv_matrix <- matrix(c(X1, X2), nrow = length(X1), byrow = TRUE)
cv_model <- cv.glmnet(mv_matrix, pd_diffs, alpha = 1)
best_lambda <- cv_model$lambda.min
plot(cv_model) 
lasso_model <- glmnet(mv_matrix, pd_diffs, alpha = 1, lambda = best_lambda)
coef(lasso_model) #If zero coefficient, it would just be a dot.
  #Predictions
  X1 <- c(0.01, 0.04, 0.01, 0.001, 0.0009)
  X2 <- c(-0.01, -0.051, -0.01, 0.4, -0.09)
  new_data <- matrix(c(X1, X2), nrow = length(X1), byrow = TRUE)  # Prepare your new data
  predictions <- predict(lasso_model, newx = new_data, s = best_lambda)
  print(predictions)


  
#####################################  Start over  #########################
rm(list=ls())
