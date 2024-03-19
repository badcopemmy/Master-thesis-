library(etm)
library(msm)
library(readxl)
library(ggplot2)
library(corrplot)
library(dplyr)

               
data <- read_excel('TTC_DATA_2021.xlsx')

###Correlation test 
# Data prep: 
pd_diff <- c(0.002, 0.004, -0.06, -0.002, 0.001) 
year <- c(2022, 2023, 2024, 2025, 2026)
X1 <- c(0.03, 0.01, 0.03, 0.002, 0.006)
X2 <- c(-0.02, -0.051, -0.0222, 0.044, -0.0999)
X3 <- c(0.03, -0.04, 0.046, -0.1, 0.001)
macro_df <- data.frame(year, X1, X2, X3) #Eventuellt gör om så man ej behöver lägga in alla manuellt
macro_df

#Bra källa för inspo ang correlation: https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/

#1. Univariate analysis of each MV - determine lags 

#Outliers - ta bort om dom finns
#Scatterplot eller boxplot? Oklart vad vi menar med outliers? 

#Lags (1,2,3)
#For each MV, define lags:
lags <- 1:3

# Create lags for each X variable
macro_df <- macro_df %>%
  mutate(
    
   # for (lag_value in lags){
      X1_lag1 <- lag(X1, lag_value)
      X2_lag1 = lag(X2, lag_value)
      X3_lag1 = lag(X3, lag_value)
    

  )

macro_df
  
  

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




#2. Analsyis MV against MV 
corrplot(cor(macro_df), method = "number", type = "upper")





#####################################  Start over  #########################
rm(list=ls())
