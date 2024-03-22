library(etm)
library(msm)
library(readxl)
library(ggplot2)
library(corrplot)
library(dplyr)
library(tidyverse)

data <- read_excel('TTC_DATA_2021_v2.xlsx')


#Anpassa data till quarterly?  




###Correlation test 
# Data prep: 
pd_diff <- c(0.002, 0.004, -0.06, -0.002, 0.001, 0.02) 
year <- c(2022, 2023, 2024, 2025, 2026, 2027)

#Group all macros in a dataframe
X1 <- c(0.03, 0.01, 0.03, 0.002, 0.006, 0.01)   
X2 <- c(-0.02, -0.051, -0.0222, 0.044, -0.0999, -0.02)
X3 <- c(0.03, -0.04, 0.046, -0.1, 0.001, 0.09)
macro <- data.frame(X1, X2, X3)
macro_names <- c("X1", "X2", "X3")
macro_df <- data.frame(year, X1, X2, X3) #Eventuellt gör om så man ej behöver lägga in alla manuellt

#Bra källa för inspo ang correlation: https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/


#1. Analsyis MV against MV - kolla hur dem förhåller sig till varandra, om några är väldigt korrelerade kanske det är överdrivet att ha med båda i modellen 
corrplot(cor(macro), method = "number", type = "upper")


#2. Univariate analysis of each MV

#Outliers - ta bort om dom finns, kan använda scatterplot eller boxplot? Lite oklart vad vi menar med outliers dock? 

#Add observation number to each - Order dates and create obr_nbr column to match macro entries
obs_dates <- unique(data$date) #The number of unique observation dates
rank_values <- rank(obs_dates, ties.method = "min")
data$obs_nbr <- rank_values[match(data$date, obs_dates)]
head(data)
tail(data)


lags <- 1:3  #Define number of lags 

lag_multiple <- function(x, n_vec){
  map(n_vec, lag, x = x) %>% 
    set_names(paste0("lag", n_vec)) 
}

lagged_macro <- cbind(macro, lag_multiple(macro, lags))
head(lagged_macro)

#Create function that matches values - plockar ut rätt värde från "lagged_macro" 
extract_value <- function(obs_number, matrix) {
  return(matrix[obs_number])
}

#Apply to all Xi
for (i in 1:ncol(lagged_macro)) {  
  col_name <- names(lagged_macro)
  data[[col_name[i]]] <- sapply(data$obs_nbr, function(x) extract_value(x, lagged_macro[[col_name[i]]]))
}

head(data)
tail(data)

#Plocka ut matris som kan skickas in i Lasso
lasso_matrix <- as.matrix(select(data, -date, -cust, -rating, -obs_nbr))








#Calculate correlation for each lag against Y-variable
#cor_vector <- cor(pd_diff, lagged_macro, use = "pairwise.complete.obs") 
#cor_vector #OBS - förstå varför vissa blir lika med 1/-1? 



#Välj den variant av varje variabel med högst korrelation mot Y 
# save_max_cor <- c() 
# 
# for (i in seq_along(macro_names)){
#   matching_column_names <- colnames(cor_vector)[grep(macro_names[i], colnames(cor_vector))]
#   selected_columns <- cor_vector[, matching_column_names]
#   max_cor_col <- matching_column_names[which.max(abs(selected_columns))]
#   save_max_cor[i] <- max_cor_col
# }
# save_max_cor   #The names of the lags with maximum correlation 


#Line diagram 
ggplot(macro_df, aes(x = year)) +
  geom_line(aes(y = pd_diff, color = "PD Diff"), linetype = "dashed") +
  geom_line(aes(y = X1, color = "MV1")) +
  geom_line(aes(y = X2, color = "MV2")) +
  geom_line(aes(y = X3, color = "MV3")) +
  # Add any additional geoms, such as points or smooths, as needed
  # Add titles and labels
  labs(title = "Macroeconomic Variables Over Time",
       x = "Year",
       y = "Change") 


#Hur många observationer är det i varje månad? 
counts <- table(data$date)
print(counts)



#####################################  Start over  #########################
rm(list=ls())

