################################################################
# Script Name: Connecting_datasets.R
# Author: Kenshin Ueoka
# Date: Mar 15, 2025
# Purpose: This script is to connect weather, price, and heatstroke datasets

################################################################

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
# install.packages("jmastats")
# library(jmastats)
# install.packages("clidatajp")
# library(clidatajp)
# install.packages(c("rvest", "dplyr", "openxlsx"))
# library(rvest)
 library(openxlsx)
 library(data.table)
library(tidyverse)
install.packages("slider")
library(slider)

########################
#Adding prefectures and names to weather data

combined_weather_with_station <- combined_weather_data |>
  left_join(stations_df %>% select(block_no, area, station_name, prec_no, pref_code), by = "block_no", relationship = "many-to-many")

#Calculating prefecture averages
prefecture_daily_averages <- combined_weather_with_station |>
  group_by(pref_code, date) |>
  summarise(
    avg_temp = mean(`temperature_average(℃)`, na.rm = TRUE),
    avg_max_temp = mean(`temperature_max(℃)`, na.rm = TRUE),
    avg_humidity = mean(`humidity_average(%)`, na.rm = TRUE),
    min_humidity = mean(`humidity_min(%)`, na.rm = TRUE),
    avg_wind_speed = mean(`wind_average_speed(m/s)`, na.rm = TRUE)
  ) |>
  ungroup() |>
  # mutate(avg_humidity = ifelse(pref_code == "14" & date == as.Date("2021-06-20"), 
  #                             92.00000, avg_humidity)) |> #For June 20 2021 when relative humidity data was unavailable, I apply the 6/19 value of 92. 
  # mutate(min_humidity = ifelse(pref_code == "14" & date == as.Date("2021-06-20"), 
  #                              75.00000, min_humidity)) |> #For June 20 2021 when relative humidity data was unavailable, I apply the 6/19 value of 75. 
  mutate(avg_heat_index = 
           -8.784969475556+
           1.61139411*avg_temp+
           2.33854883889*avg_humidity-
           0.14611605*avg_temp*avg_humidity-
           0.012308094*(avg_temp**2)-
           0.01642482777*avg_humidity**2+
           2.211732*(10**-3)*(avg_temp**2)*avg_humidity+
           7.2546*(10**-4)*avg_temp*(avg_humidity**2)-
           3.582*(10**-6)*(avg_temp**2)*(avg_humidity**2),
         max_heat_index =
           -8.784969475556+
           1.61139411*avg_max_temp+
           2.33854883889*avg_humidity-
           0.14611605*avg_max_temp*avg_humidity-
           0.012308094*avg_max_temp**2-
           0.01642482777*avg_humidity**2+
           2.211732*(10**-3)*(avg_max_temp**2)*avg_humidity+
           7.2546*(10**-4)*avg_max_temp*(avg_humidity**2)-
           3.582*(10**-6)*(avg_max_temp**2)*(avg_humidity**2)
        ) 
        
########################
#Connecting weather and heatstrokes
prefecture_daily_averages$pref_code <- as.numeric(prefecture_daily_averages$pref_code)
heatstrokes_2017_2024$Prefecture_Code <- as.numeric(heatstrokes_2017_2024$Prefecture_Code)

#Adding prefecture names
Prefecture_Codes <- read_csv("Prefecture Codes.csv") |>
  select(pref_no,Pref_JPN_name,	Pref_Name, Region)

#Creating Master Dataset
master_weather_heat_dataset <- prefecture_daily_averages |>
  left_join(heatstrokes_2017_2024, by = c("pref_code" = "Prefecture_Code", "date" = "Date"))|>
  filter(!(format(date, "%Y-%m") == "2020-05"))|> #removing May 2020 since there is no heatstroke data from COVID
  select(-"...17", -Source_File, -Sheet_Name) |>
  left_join(Prefecture_Codes, by = c("pref_code"="pref_no"))


clean_master_weather_heat_dataset <- master_weather_heat_dataset |> na.omit()  #Remove June 20 2021 when relative humidity data was unavailable

#########
#Preparing energy price dataset
Energy_Prices <- read_csv("jepxSpot Electricy Price in Japan.csv")
energy_price_long <- Energy_Prices |>
  pivot_longer(
    cols = starts_with("Hokkaido"):`Shikoku Yen/kWh`,  # Select region price columns
    names_to = "Region", 
    values_to = "Price_Yen_kWh"
  ) |>
  mutate(
    Region = gsub(" Yen/kWh", "", Region)  # Remove " Yen/kWh" from region names
  ) |>
  select(datetime, Date, Region, Price_Yen_kWh, `Contracted Volume kWh`)

#Compute daily average regional prices 
regional_daily_avg_prices <- energy_price_long |>
  group_by(Date,Region)|>
  summarise(avg_price_kWh = mean(Price_Yen_kWh, na.rm = TRUE),
            contracted_volume_total = mean(`Contracted Volume kWh`)) |>
  group_by(Region) |>
  arrange(Date) |>  # Ensure data is ordered by date within each region
  mutate(
    avg_price_30_days = slide_dbl(avg_price_kWh, 
                                  .f = mean, 
                                  .before = 29,  # Include the previous 29 days (total = 30 days)
                                  .complete = TRUE)
  )

#########
#Connecting Energy Price and Weather/Heat datasets by region-date
Full_Master_Database <- clean_master_weather_heat_dataset |>
  left_join(regional_daily_avg_prices, by = c("Region"= "Region", "date" = "Date")) |>
  filter(pref_code!=47) |> #Removing Okinawa since we do not have energy price data
  mutate(day_of_week = wday(date, label = TRUE, abbr = TRUE))

saveRDS(Full_Master_Database, here::here("Full_Master_Database.Rds"))
write_csv(Full_Master_Database,here::here("Full_Master_Database.csv"))
