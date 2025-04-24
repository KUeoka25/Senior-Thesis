################################################################
# Script Name: Correlation Checks.R
# Author: Kenshin Ueoka
# Date: Mar 30, 2025
# Purpose: This script is to run the initial regression from the change in energy prices

################################################################

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(openxlsx)
library(data.table)
library(tidyverse)
install.packages("fixest")
library(fixest)
install.packages("car")
library(car)
install.packages("lmtest")
library(sandwich)
library(lmtest)
#######
Full_Master_Database |> glimpse()

########
#Running Variance Inflation Factors (VIF). To ensure that your fixed-effects regression model is on the right track, it is essential to check for multicollinearity among the independent variables

# Fit a simplified regression model without fixed effects
simplified_model <- lm(Location_Home ~ post_2022 * log(avg_price_30_days) + avg_max_temp + I(avg_max_temp^2) + avg_humidity + I(avg_humidity^2), data = Full_Master_Database)

# Calculate VIF values
vif_base <- vif(simplified_model)
print(vif_base)

vif_values_predictor <- vif(simplified_model, type="predictor")
print(vif_values_predictor)

write.table(vif_base, "vif_results_initial.csv", sep = ",", row.names = TRUE)

#########
#VIF of scaled variables in lm model

scaled_simple_model <- lm(Location_Home ~ post_2022 * log_30dayMA_kWh_price_scaled + avg_max_temp_scaled + avg_max_temp_sq_scaled + avg_humidity_scaled + avg_humidity_sq_scaled, data = Full_Master_Database)
vif_post <- vif(scaled_simple_model)
print(vif(scaled_simple_model))
print(vif(scaled_simple_model, type="predictor"))

write.table(vif_post, "vif_results_after_scaling.csv", sep = ",", row.names = TRUE)


#CONCLUSION: SCALING FIXED MULTICOLLINEARITY

##########
#HOMOSKEDASTICITY CHECKS: Visual
plot(scaled_simple_model$fitted.values, scaled_simple_model$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lwd = 2)

#CONCLUSION: Heteroskedasticity (bad)

##########
#Log transformation of Location_home

logY_scaled_simple_model <-
  lm(log_Location_Home ~ post_2022 * log_30dayMA_kWh_price_scaled + avg_max_temp_scaled + avg_max_temp_sq_scaled + avg_humidity_scaled + avg_humidity_sq_scaled, data =Full_Master_Database)

plot(logY_scaled_simple_model$fitted.values, logY_scaled_simple_model$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lwd = 2)

#Breusch-Pagan test

bptest(logY_scaled_simple_model)

coeftest(logY_scaled_simple_model, vcov = vcovHC(logY_scaled_simple_model, type = "HC1"))

