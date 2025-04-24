################################################################
# Script Name: Descriptive_Stats.R
# Author: Kenshin Ueoka
# Date: April 7, 2025
# Purpose: This script is to make descriptive stats tables

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
library(gtsummary)
library(gt)
install.packages("psych")


#########

Full_Master_Database |>
  summary()
#Using GT summary to make descriptive stats table
descriptive_stats <- tbl_summary(Full_Master_Database |> 
                                   select(avg_temp,
                                          avg_max_temp,
                                          avg_humidity,
                                          avg_heat_index,
                                          max_heat_index,
                                          Total_ER,
                                          Location_Home,
                                          avg_price_kWh,
                                          avg_price_30_days,
                                          post_2022
                                   ))
descriptive_stats

# Convert to gt and save to Word
descriptive_stats <- Full_Master_Database |> 
  select(avg_temp,
         avg_max_temp,
         avg_humidity,
         avg_heat_index,
         max_heat_index,
         Total_ER,
         Location_Home,
         avg_price_kWh,
         avg_price_30_days,
         post_2022
  ) |>
  summarize()

Full_Master_Database |>
  ungroup()|>
  filter(Location_Home<=10)|>
  summarise(n=n())

Descriptive_Stats <- psych::describe(Full_Master_Database |> 
                                       select(avg_temp,
                                              avg_max_temp,
                                              avg_humidity,
                                              avg_heat_index,
                                              max_heat_index,
                                              Total_ER,
                                              Location_Home,
                                              avg_price_kWh,
                                              avg_price_30_days,
                                              post_2022
                                       )) 
Descriptive_Stats |> select(n, mean, sd, median, min, max)

library(stargazer)


