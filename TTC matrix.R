#HEJ

data <- read_excel("/Users/Emmy/Desktop/exjobb danskebank/Data/TTC_DATA_2021.xlsx")
head(data)

#add fake ratings
num_rows <- nrow(data)
random_ratings <- sample(c("A", "B", "C"), num_rows, replace = TRUE)
data$rating <- random_ratings
print(data)

#remove all ratings in between start date and end date
data <- data %>%
  filter(date == min(date) | date == max(date))
head(data)

#Create a smaller subset to be able to run the code faster...
subset_size <- 5000
sample_accounts <- sample(unique(data$cust), subset_size)
data <- data[data$cust %in% sample_accounts, ]

# Get unique accounts
unique_accounts <- unique(data$cust)
# Initialize a matrix to store transition counts
num_states <- n_distinct(data$rating)
count_matrix <- matrix(0, nrow = num_states, ncol = num_states,
                            dimnames = list(LETTERS[1:num_states], LETTERS[1:num_states]))
# Iterate over unique accounts
for (acc in unique_accounts) {
  # Filter data for the current account
  acc_data <- data %>%
    filter(cust == acc)
  # Check if the account has data for both the first and last date
  if (n_distinct(acc_data$date) == 2) {
    # Get rating states for the first and last dates
    first_state <- acc_data$rating[1]
    last_state <- acc_data$rating[nrow(acc_data)]
    # Increment the count in the transition matrix
    count_matrix[first_state, last_state] <- count_matrix[first_state, last_state] + 1
  }
}
print(count_matrix)



#Make a transition matrix by dividing by initial ratings
transition_matrix <- count_matrix
for (i in 1:num_states) {
  # Calculate the number of accounts starting in state i
  num_starting_in_state_i <- sum(data$rating[data$date == min(data$date)] == LETTERS[i])
  # Divide each row by the corresponding number of accounts
  transition_matrix[i, ] <- transition_matrix[i, ] / num_starting_in_state_i
}
# Print the transition matrix
print(transition_matrix)


#####################################  Start over  #########################
rm(list=ls())
