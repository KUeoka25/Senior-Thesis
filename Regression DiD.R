################################################################
# Script Name: Regression DiD.R
# Author: Kenshin Ueoka
# Date: Mar 17, 2025
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
install.packages("lmtest")
library(sandwich)
library(lmtest)
install.packages("sjPlot")
library(sjPlot)
install.packages("stargazer")
library(stargazer)
install.packages(("modelsummary"))
library(modelsummary)
library(dplyr)
library(fixest)
library(stringr)
library(knitr)
library(kableExtra)

# Create the treatment indicator (post-2022 period)
Full_Master_Database <- Full_Master_Database |>
  mutate(post_2022 = ifelse(Year >= 2022, 1, 0)) |>
  mutate(Kyushu_Binary = ifelse(Region=="Kyushu", 1, 0))

###########
#SCALING VARIABLES
Full_Master_Database <- Full_Master_Database |>
  group_by(pref_code)|>
  mutate(
    avg_max_temp_scaled = scale(avg_max_temp),  # Scaled avg_max_temp
    avg_humidity_scaled = scale(avg_humidity), # Scaled avg_humidity
    log_30dayMA_kWh_price_scaled = scale(log(avg_price_30_days)), # Scaled log(avg_price_30_days)
    avg_max_temp_sq_scaled = avg_max_temp_scaled^2, # Quadratic term after scaling
    avg_humidity_sq_scaled = avg_humidity_scaled^2  # Quadratic term after scaling
  )

#Adding Log Location_Home
Full_Master_Database <- Full_Master_Database |>
  mutate(log_Location_Home = log(Location_Home + 1),
         log_Total_ER = log(Total_ER + 1))

###########
#VIZUALIZATIONS

#Vizualizing pre-22 vs. post-2022 heat strokes
Full_Master_Database |>
  ggplot(aes(x=avg_price_kWh, y=Location_Home, color= as.factor(post_2022)))+
  geom_point(alpha = 0.2)+
  facet_wrap(facets=vars(Region), scales = "free_y")+
  theme_minimal()+
  labs(x="Average Price per kWh (JPY)",
       y="Home Heatstroke ER Visits",
       title = "Pre 2022 vs. Post 2022 Heat Strokes")

#Vizualizing pre-22 vs. post-2022 energy prices DAILY
Full_Master_Database |>
  ggplot(aes(x= date, y=avg_price_kWh, color= as.factor(post_2022)))+
  geom_point(alpha = 0.1)+
  theme_minimal()+
  facet_wrap(facets=vars(Region), scales = "free_y")+
  labs(x="Date",
       y="Daily Average Price per kWh (JPY)",
       title = "Pre 2022 vs. Post 2022 Daily Energy Prices")

#Vizualizing pre-22 vs. post-2022 energy prices MOVING AVERAGE

Full_Master_Database |>
  ggplot(aes(x= date, y=avg_price_30_days, color= as.factor(post_2022)))+
  geom_point(alpha = 0.1)+
  theme_minimal()+
  facet_wrap(facets=vars(Region), scales = "free_y")+
  labs(x="Date",
       y="30-day Moving Average Price per kWh (JPY)",
       title = "Pre 2022 vs. Post 2022 Energy Prices Moving Average")

#Vizualizing 2022 energy prices DAILY
Full_Master_Database |>
  filter(Year==2022)|>
  ggplot(aes(x= date, y=avg_price_kWh))+
  geom_point(alpha = 1)+
  theme_minimal()+
  facet_wrap(facets=vars(Region), scales = "fixed")+
  labs(x="Date",
       y="Daily Average Price per kWh (JPY)",
       title = "Summer 2022 Daily Energy Prices by Region")


#Vizualizing 2022 energy prices Moving averages
Full_Master_Database |>
  filter(Year==2022)|>
  ggplot(aes(x= date, y=avg_price_30_days))+
  geom_point(alpha = 1)+
  theme_minimal()+
  facet_wrap(facets=vars(Region), scales = "fixed")+
  labs(x="Date",
       y="Moving Average Price per kWh (JPY)",
       title = "Summer 2022 Moving Average Energy Prices by Region")



#Vizualizing Heatstroke and Heat Index
Full_Master_Database |>
  ggplot(aes(x= max_heat_index, y=Location_Home, color= as.factor(post_2022)))+
  geom_point(alpha = 0.1)+
  geom_smooth(method = lm, se = FALSE, fullrange = FALSE)+
  theme_minimal()+
  facet_wrap(facets=vars(Region), scales = "free_y")+
  labs(x="Max Daily Heat Index",
       y="Home Heatstroke ER Visits",
       title = "Heatstroke and Heat Index")

# Vizualizing Heatstroke and Daily Max Temperature
Full_Master_Database |>
  ggplot(aes(x= avg_max_temp, y=Location_Home, color= as.factor(post_2022)))+
  geom_point(alpha = 0.1)+
  geom_smooth(method = lm, se = FALSE, fullrange = FALSE)+
  theme_minimal()+
  facet_wrap(facets=vars(Region), scales = "free_y")+
  labs(x="Max Daily Temperature",
       y="Home Heatstroke ER Visits",
       title = "Heatstroke and Daily Max Temperature")

# Vizualizing Heatstroke and Daily Max Temperature
Full_Master_Database |>
  #filter(year(date)==2022)|>
  ggplot(aes(x= contracted_volume_total, y=Location_Home))+
  geom_point(alpha = 0.1)+
  geom_smooth(method = lm, se = FALSE, fullrange = FALSE)+
  theme_minimal()+
  facet_wrap(facets=vars(Region), scales = "fixed")+
  labs(x="Total Contracted Volume (kWh)",
       y="Home Heatstroke ER Visits",
       title = "Heatstroke and Total Contracted Volume")

#Vizualizing Indoor heatstroke histogram
Full_Master_Database |>
  #filter(year(date)==2022)|>
  ggplot(aes(x= Location_Home))+
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Indoor Heatstrokes",
       x = "Indoor Heatstrokes per day",
       y = "Frequency") +
  theme_minimal()+
  xlim(0, 50)

#Vizualizing Indoor heatstroke histogram
Full_Master_Database |>
  #filter(year(date)==2022)|>
  ggplot(aes(x= Total_ER))+
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of ALL Heatstrokes",
       x = "Heatstrokes per day",
       y = "Frequency") +
  theme_minimal()+
  xlim(0, 50)


###########################################
###########################################
###########################################
###########################################
###########################################
###########################################

########
# 0 Just Home Heatstrokes and Avg_Price, fixed effects prefecture and month
fe_model_just_XY <- feols(Location_Home ~ post_2022 * avg_price_kWh | Pref_Name + Month, data = Full_Master_Database)
summary(fe_model_just_XY)

########
# 1 Run the Differences-in-Differences (DiD) regression, FIXED EFFECTS PANEL MODEL
fe_model_basic <- feols(Location_Home ~ post_2022 * avg_price_kWh + max_heat_index | Pref_Name + Month, data = Full_Master_Database)
summary(fe_model_basic)

########
# 2 Adding moving average of previous 30 days as a lag
fe_model_lag30days <- feols(Location_Home ~ post_2022 * (avg_price_kWh + avg_price_30_days) + max_heat_index | Pref_Name + Month, data = Full_Master_Database)
summary(fe_model_lag30days)

# 3 Only using moving average of previous 30 days as a lag
fe_model_ONLYlag30days <- feols(Location_Home ~ post_2022 * (avg_price_30_days) + max_heat_index | Pref_Name + Month, data = Full_Master_Database)
summary(fe_model_ONLYlag30days)
  

########
# 4 removing Kyushu
fe_model_lag30days_no_kyushu <- Full_Master_Database |> 
  filter(Kyushu_Binary==0) |>
  feols(Location_Home ~ post_2022 * (avg_price_kWh + avg_price_30_days) + max_heat_index | Pref_Name + Month)
summary(fe_model_lag30days_no_kyushu)

########
# 5 Using Temperature, Temperature^2, Humidity, Humidity^2 on their own without heat index
fe_model_lag30days_no_heat_index <- Full_Master_Database |> 
  #filter(Kyushu_Binary==0) |>
  feols(Location_Home ~ post_2022 * (avg_price_kWh + avg_price_30_days) + avg_max_temp + avg_max_temp**2 + avg_humidity + avg_humidity**2| Pref_Name + Month)
summary(fe_model_lag30days_no_heat_index)

########
# 6 Using Temperature, Temperature^2, Humidity, Humidity^2 on their own WITH heat index
# fe_model_lag30days_with_heat_index <- Full_Master_Database |>
#   filter(Kyushu_Binary==0) |>
#   feols(Location_Home ~ post_2022 * (avg_price_kWh + avg_price_30_days) + avg_max_temp + avg_max_temp**2 + avg_humidity + avg_humidity**2 + max_heat_index| Pref_Name + Month)
# summary(fe_model_lag30days_with_heat_index)

########
# 7: 5 but KYUSHU ONLY Using Temperature, Temperature^2, Humidity, Humidity^2 on their own without heat index
fe_model_Kyushu_lag30days_no_heat_index <- Full_Master_Database |> 
  filter(Kyushu_Binary==1) |>
  feols(Location_Home ~ post_2022 * (avg_price_kWh + avg_price_30_days) + avg_max_temp + avg_max_temp**2 + avg_humidity + avg_humidity**2 | Pref_Name + Month)
summary(fe_model_Kyushu_lag30days_no_heat_index)

########
# 8: 5 but TOTAL ER visits with kyushu
# Using Temperature, e^Temperature, Humidity, e^Humidity on their own without heat index 
fe_model_TOTALER_lag30days_no_heat_index <- Full_Master_Database |> 
  #filter(Kyushu_Binary==0) |>
  feols(Total_ER ~ post_2022 * (avg_price_kWh + avg_price_30_days) + avg_max_temp + avg_max_temp**2 + avg_humidity + avg_humidity**2| Pref_Name + Month)
summary(fe_model_TOTALER_lag30days_no_heat_index)

########
# 9: 5 with Linear Reg with prefecture/Month controls instead of feols. No month effects
LinReg_model_lag30days_no_heat_index <- 
  lm(Location_Home ~ post_2022 * (avg_price_kWh + avg_price_30_days) + 
       avg_max_temp + avg_max_temp**2 + avg_humidity + avg_humidity**2 + 
       factor(Pref_Name), data = Full_Master_Database)
summary(LinReg_model_lag30days_no_heat_index)

########
# 9.1: 9 but simpler
LinReg_model_basic <- 
  lm(Location_Home ~ log(avg_price_kWh)+
       avg_max_temp + avg_max_temp**2 + avg_humidity + avg_humidity**2 + 
       factor(Pref_Name), data = Full_Master_Database)
summary(LinReg_model_basic)

########
#10: 3 with 7 - no daily average, using heat index, no kyushu
fe_model_lag30days_no_kyush_No_daily<- Full_Master_Database |> 
  filter(Kyushu_Binary==0) |>
  feols(Location_Home ~ post_2022 *  avg_price_30_days + max_heat_index | Pref_Name + Month)
summary(fe_model_lag30days_no_kyush_No_daily)

#########
#11: 5 but Kanto only
fe_model_Kanto <- Full_Master_Database |> 
  filter(Region=="Tokyo") |>
  feols(Location_Home ~ post_2022 * (avg_price_kWh + avg_price_30_days) + avg_max_temp + avg_max_temp**2 + avg_humidity + avg_humidity**2| Pref_Name + Month)
summary(fe_model_Kanto)

##########
#12 5 but without daily prices
# 5 Using Temperature, Temperature^2, Humidity, Humidity^2 on their own without heat index
fe_model_only_movingaverage <- Full_Master_Database |> 
  #filter(Kyushu_Binary==0) |>
  feols(Location_Home ~ post_2022 * (avg_price_30_days) + avg_max_temp + avg_max_temp**2 + avg_humidity + avg_humidity**2| Pref_Name + Month)
summary(fe_model_only_movingaverage)


##########
#13 Logarithm of average price, only moving average
fe_model_log_EP <- Full_Master_Database |> 
  #filter(Kyushu_Binary==0) |>
  feols(Location_Home ~ post_2022 * (log(avg_price_30_days) + log(avg_price_kWh)) + avg_max_temp + avg_max_temp**2 + avg_humidity + avg_humidity**2| Pref_Name + Month)
summary(fe_model_log_EP)

##########
#14 13 Adding fixed effect for day of week
fe_model_log_EP_dow <- Full_Master_Database |> 
  #filter(Kyushu_Binary==0) |>
  feols(Location_Home ~ post_2022 * (log(avg_price_30_days) + log(avg_price_kWh)) + avg_max_temp + avg_max_temp**2 + avg_humidity + avg_humidity**2| Pref_Name + Month + day_of_week)
summary(fe_model_log_EP_dow)

###########
#15 14 With only the 30 day lag
fe_model_log_EP_dow_onlylag <- Full_Master_Database |> 
  #filter(Kyushu_Binary==0) |>
  feols(Location_Home ~ post_2022 * (log(avg_price_30_days)) + avg_max_temp + avg_max_temp**2 + avg_humidity + avg_humidity**2| Pref_Name + Month + day_of_week)
summary(fe_model_log_EP_dow_onlylag)


############
#16 15 with Scaled variables 
fe_scaled <-
  feols(Location_Home ~ post_2022 * log_30dayMA_kWh_price_scaled + avg_max_temp_scaled + avg_max_temp_sq_scaled + avg_humidity_scaled + avg_humidity_sq_scaled| Pref_Name + Month + day_of_week, data = Full_Master_Database)

summary(fe_scaled)

###########
#17 16 with logY
fe_transformed_Y <-
  feols(log_Location_Home ~ post_2022 *log_30dayMA_kWh_price_scaled + avg_max_temp_scaled + avg_max_temp_sq_scaled + avg_humidity_scaled + avg_humidity_sq_scaled | Pref_Name + Month + day_of_week, data = Full_Master_Database)

coeftest(fe_transformed_Y, vcov = vcovHC(fe_transformed_Y, type = "HC1"))

#VIF test
vif(fe_transformed_Y)

#heteroskedasticity test
plot(fe_transformed_Y$fitted.values, fe_transformed_Y$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lwd = 2)
bptest(fe_transformed_Y)


############
# 18 17 with clustered standard errors
fe_transformed_Y_clustered <- feols(
  log_Location_Home ~ post_2022 * log_30dayMA_kWh_price_scaled +  avg_max_temp_scaled + avg_max_temp_sq_scaled + avg_humidity_scaled + avg_humidity_sq_scaled | Pref_Name + Month + day_of_week,
  data = Full_Master_Database,
  se = "cluster"
)
#Purpose: Adjust for both heteroskedasticity and within-group correlation of residuals. This accounts for the possibility that observations within the same group (e.g., regions, time periods) are not independent.
# •	How It Works: Clustered standard errors allow residuals to be correlated within clusters (e.g., all observations from the same region) but assume independence across clusters. This is useful when data is grouped hierarchically or when there is repeated measurement of units over time.
# •	Key Features:
#   •	Adjust for heteroskedasticity within clusters.
#   •	Allow for correlation within groups (clusters), such as individuals within regions or time periods.
#   •	Assume independence between clusters.

summary(fe_transformed_Y_clustered)
#heteroskedasticity test
plot(fe_transformed_Y_clustered$fitted.values, fe_transformed_Y_clustered$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lwd = 2)

Full_Master_Database[which(abs(fe_transformed_Y_clustered$residuals) > 2), ]
qqnorm(fe_transformed_Y_clustered$residuals)
qqline(fe_transformed_Y_clustered$residuals)

############
#19 18 with temp*humidity interactions
fe_transformed_Y_clustered_temphum_interaction <- feols(
  log_Location_Home ~ post_2022 * log_30dayMA_kWh_price_scaled +  (avg_max_temp_scaled + avg_max_temp_sq_scaled) * (avg_humidity_scaled + avg_humidity_sq_scaled) | Pref_Name + Month + day_of_week,
  data = Full_Master_Database,
  se = "cluster"
)
summary(fe_transformed_Y_clustered_temphum_interaction)
#heteroskedasticity test
plot(fe_transformed_Y_clustered_temphum_interaction$fitted.values, fe_transformed_Y_clustered_temphum_interaction$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lwd = 2)

Full_Master_Database[which(abs(fe_transformed_Y_clustered_temphum_interaction$residuals) > 2), ]
qqnorm(fe_transformed_Y_clustered_temphum_interaction$residuals)
qqline(fe_transformed_Y_clustered_temphum_interaction$residuals)

############
#19.1 19 without post2022 interactions
fe_transformed_Y_no_2022interaction <- feols(
  log_Location_Home ~ log_30dayMA_kWh_price_scaled +  (avg_max_temp_scaled + avg_max_temp_sq_scaled) * (avg_humidity_scaled + avg_humidity_sq_scaled) | Pref_Name + Month + day_of_week,
  data = Full_Master_Database,
  se = "cluster"
)
summary(fe_transformed_Y_no_2022interaction)
#heteroskedasticity test
plot(fe_transformed_Y_no_2022interaction$fitted.values, fe_transformed_Y_no_2022interaction$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lwd = 2)

Full_Master_Database[which(abs(fe_transformed_Y_no_2022interaction$residuals) > 2), ]
qqnorm(fe_transformed_Y_no_2022interaction$residuals)
qqline(fe_transformed_Y_no_2022interaction$residuals)


#EXPORTING REGRESSION OUTPUTS in R
tab_model(fe_model_lag30days_no_heat_index, file = "FE OLS initial before scaling.doc")
tab_model(fe_model_log_EP, show.std = TRUE, show.ci = FALSE, show.obs = TRUE)
tab_model(fe_transformed_Y_clustered, fe_transformed_Y_clustered_temphum_interaction)
tab_model(fe_transformed_Y_clustered, fe_transformed_Y_clustered_temphum_interaction, file = "FE OLS tables after scaling.doc")
tab_model(fe_transformed_Y_no_2022interaction, file = "FE OLS tables without 2022 interaction.doc")
?tab_model

dict <- c(
  "post_2022" = "Post-2022",
  "avg_price_kWh" = "Avg. Price per kWh",
  "avg_price_30_days" = "Avg. Price (30 Days)",
  "avg_max_temp" = "Avg. Max Temp",
  "I(avg_max_temp^2)" = "(Avg. Max Temp)^2",
  "avg_humidity" = "Avg. Humidity",
  "I(avg_humidity^2)" = "(Avg. Humidity)^2",
  "post_2022:avg_price_kWh" = "Post-2022 × Avg. Price per kWh",
  "post_2022:avg_price_30_days" = "Post-2022 × Avg. Price (30 Days)"
)

etable(
  fe_model_lag30days_no_heat_index,
  dict = dict,
  tex = TRUE,
  style.tex = style.tex("aer"),  # "aer" mimics the style in your example[2][3]
  fitstat = ~ n + r2,            # Show N and R², customize as needed
  extraline = list(
    "Prefecture FE" = "Yes",
    "Month FE" = "Yes"
  ),
  title = "Initial OLS Estimation with Fixed Effects for Prefecture"
)

######
#SECOND REGRESSION
dict <- c(
  "post_2022" = "Post-2022",
  "log_30dayMA_kWh_price_scaled" = "Log 30-day MA kWh Price (scaled)",
  "avg_max_temp_scaled" = "Avg. Max Temp (scaled)",
  "avg_max_temp_sq_scaled" = "(Avg. Max Temp)^2 (scaled)",
  "avg_humidity_scaled" = "Avg. Humidity (scaled)",
  "avg_humidity_sq_scaled" = "(Avg. Humidity)^2 (scaled)",
  "post_2022:log_30dayMA_kWh_price_scaled" = "Post-2022 × Log 30-day MA kWh Price",
  "avg_max_temp_scaled:avg_humidity_scaled" = "Avg. Max Temp × Avg. Humidity",
  "avg_max_temp_scaled:avg_humidity_sq_scaled" = "Avg. Max Temp × (Avg. Humidity)^2",
  "avg_max_temp_sq_scaled:avg_humidity_scaled" = "(Avg. Max Temp)^2 × Avg. Humidity",
  "avg_max_temp_sq_scaled:avg_humidity_sq_scaled" = "(Avg. Max Temp)^2 × (Avg. Humidity)^2"
)

etable(
  fe_transformed_Y_clustered,
  fe_transformed_Y_clustered_temphum_interaction,
  dict = dict,
  tex = TRUE,
  style.tex = style.tex("aer"),  # Clean, journal-style table
  fitstat = ~ n + r2,            # Show N and R²
  extraline = list(
    "Prefecture FE" = "Yes",
    "Month FE" = "Yes",
    "Day-of-Week FE" = "Yes",
    "Clustered SE" = "Yes"
  ),
  title = "Table: Regression Results for Home Location (with Fixed Effects and Clustered SEs)",
  file = "regression_table.tex"
)


##########
#20 19 filtered for high-heatstroke days

# fe_transformed_Y_clustered_temphum_interaction_many_heatstrokes <- feols(
#   log_Location_Home ~ post_2022 * log_30dayMA_kWh_price_scaled +  (avg_max_temp_scaled + avg_max_temp_sq_scaled) * (avg_humidity_scaled + avg_humidity_sq_scaled) | Pref_Name + Month + day_of_week,
#   data = Full_Master_Database |> filter(Location_Home>=1),
#   se = "cluster"
# )
# summary(fe_transformed_Y_clustered_temphum_interaction_many_heatstrokes)
# #heteroskedasticity test
# plot(fe_transformed_Y_clustered_temphum_interaction_many_heatstrokes$fitted.values, fe_transformed_Y_clustered_temphum_interaction_many_heatstrokes$residuals,
#      xlab = "Fitted Values", ylab = "Residuals",
#      main = "Residuals vs Fitted Values")
# abline(h = 0, col = "red", lwd = 2)


############
# G1 Generalized Linear Model (GLM) with Quasi-Poisson link (better for count variables)
glm_model <- glm(Location_Home ~ post_2022 * log_30dayMA_kWh_price_scaled + avg_max_temp_scaled + avg_max_temp_sq_scaled + avg_humidity_scaled + avg_humidity_sq_scaled,
                 family = quasipoisson(link = "log"), data = Full_Master_Database)
summary(glm_model)

#############
# G2 GLM with temp * humidity interaction
glm_temp_humidity_interaction <- glm(Location_Home ~ post_2022 * log_30dayMA_kWh_price_scaled + (avg_max_temp_scaled + avg_max_temp_sq_scaled) * (avg_humidity_scaled + avg_humidity_sq_scaled),
                 family = quasipoisson(link = "log"), data = Full_Master_Database)
summary(glm_temp_humidity_interaction)

#plotting residuals
Full_Master_Database_predictions <- Full_Master_Database |>
  mutate(Predictions = predict(glm_temp_humidity_interaction, type = "response"),
         Residuals = Location_Home - Predictions)
plot(Full_Master_Database_predictions$Predictions, Full_Master_Database_predictions$Residuals,
     xlab = "Predicted Values", ylab = "Residuals",
     main = "Residuals vs Predicted Values")
abline(h = 0, col = "red")

##########
# FEGLM1 FIXED EFFECTS GLM for prefecture + month FE, no 2022 interaction, temp and humidity interactions
feglm_model_base <- feglm(Location_Home ~ log_30dayMA_kWh_price_scaled +
                       (avg_max_temp_scaled + avg_max_temp_sq_scaled) * 
                       (avg_humidity_scaled + avg_humidity_sq_scaled) | Pref_Name + Month,
                     family = quasipoisson(link = "log"), 
                     data = Full_Master_Database)
summary(feglm_model_base)

#plotting residuals

plot(feglm_model_base$fitted.values, residuals(feglm_model_base),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

# FEGLM2, without temp-humidity interactions
feglm_model_simpler <- feglm(Location_Home ~ log_30dayMA_kWh_price_scaled +
                            (avg_max_temp_scaled + avg_max_temp_sq_scaled) +
                            (avg_humidity_scaled + avg_humidity_sq_scaled) | Pref_Name + Month,
                          family = quasipoisson(link = "log"), 
                          data = Full_Master_Database)
summary(feglm_model_simpler)

# FEGLM3 FIXED EFFECTS GLM for prefecture + month FE, WITH 2022 interaction, temp and humidity interactions
feglm_model_base_with2022 <- feglm(Location_Home ~ post_2022 * log_30dayMA_kWh_price_scaled +
                            (avg_max_temp_scaled + avg_max_temp_sq_scaled) * 
                            (avg_humidity_scaled + avg_humidity_sq_scaled) | Pref_Name + Month,
                          family = quasipoisson(link = "log"), 
                          data = Full_Master_Database)
summary(feglm_model_base_with2022)


plot(feglm_model_base_with2022$fitted.values, residuals(feglm_model_base_with2022),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

# coeftest(feglm_model_base_with2022, vcov = sandwich)

#Checking GLMs for multicollinearity
install.packages("performance")
library(performance)
check_collinearity(feglm_model_base)
check_collinearity(feglm_model_simpler)
check_collinearity(feglm_model_base_with2022)

#Exporting regression tables
tab_model(feglm_model_base, feglm_model_base_with2022, transform = NULL, file = "FEGLM tables 1.doc")
tab_model(feglm_model_base_with2022, )
?tab_model


dict <- c(
  "post_2022" = "Post-2022",
  "log_30dayMA_kWh_price_scaled" = "Log 30-day MA kWh Price (scaled)",
  "avg_max_temp_scaled" = "Avg. Max Temp (scaled)",
  "avg_max_temp_sq_scaled" = "(Avg. Max Temp)^2 (scaled)",
  "avg_humidity_scaled" = "Avg. Humidity (scaled)",
  "avg_humidity_sq_scaled" = "(Avg. Humidity)^2 (scaled)",
  "post_2022:log_30dayMA_kWh_price_scaled" = "Post-2022 × Log 30-day MA kWh Price",
  "avg_max_temp_scaled:avg_humidity_scaled" = "Avg. Max Temp × Avg. Humidity",
  "avg_max_temp_scaled:avg_humidity_sq_scaled" = "Avg. Max Temp × (Avg. Humidity)^2",
  "avg_max_temp_sq_scaled:avg_humidity_scaled" = "(Avg. Max Temp)^2 × Avg. Humidity",
  "avg_max_temp_sq_scaled:avg_humidity_sq_scaled" = "(Avg. Max Temp)^2 × (Avg. Humidity)^2"
)

etable(
  feglm_model_base,
  feglm_model_base_with2022,
  dict = dict,
  tex = TRUE,
  style.tex = style.tex("aer"),  # Matches the compact, journal style
  fitstat = ~ n + aic,           # Show N and AIC
  extraline = list(
    "Prefecture FE" = "Yes",
    "Month FE" = "Yes"
  ),
  headers = c("Base Model", "With Post-2022 Interaction"),
  title = "Quasi-Poisson Generalized Linear Model Estimations with Prefecture Fixed Effects",
  file = "quasipoisson_table.tex"
)


