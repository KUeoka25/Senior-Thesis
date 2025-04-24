################################################################
# Script Name: combining_data.R
# Author: Kenshin Ueoka
# Date: Feb 4, 2025
# Purpose: This script is to merge heatstroke datasets

################################################################

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

################################################################

# Summarizing Monthly
heatstroke_monthly <- Full_Master_Database |>
  # mutate(Year = year(Date),
  #        Month = month(Date)) |> 
  filter(Month >= 5 & Month <= 8) |> 
  filter(Year >= 2017)|>
  group_by(Month, Year) |> 
  summarise(NationalERHome = sum(Location_Home, na.rm = TRUE),
            NationalFatal = sum(Severity_Fatal, na.rm = TRUE), .groups = "drop")

# Summarizing Annually
heatstroke_annually <- Full_Master_Database |>
  # mutate(Year = year(Date),
  #        Month = month(Date)) |> 
  filter(Month >= 5 & Month <= 8) |> 
  #filter(Year >= 2017)|>
  group_by(Year) |> 
  summarise(NationalERHome = sum(Location_Home, na.rm = TRUE),
            NationalFatal = sum(Severity_Fatal, na.rm = TRUE), .groups = "drop")


# Plot ER Visits Monthly
ggplot(heatstroke_monthly, aes(x = Month, y = NationalERHome, color = as.factor(Year))) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Total Indoor ER Visits for Heatstroke (May-August, 2017-2024)",
       x = "Month", y = "Total ER Visits", color = "Year") +
  scale_x_discrete(breaks = 5:8) + 
  scale_y_continuous(limits = c(0,25000))+
  theme_minimal()

# Plot ER Visits Annually
ggplot(heatstroke_annually, aes(x = Year, y = NationalERHome)) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Total Indoor ER Visits for Heatstroke (May-August, 2017-2024)",
       x = "Month", y = "Total ER Visits") +
  theme_minimal()

# Plot Fatalities monthly
ggplot(heatstroke_monthly, aes(x = Month, y = NationalFatal, color = as.factor(Year), group = Year)) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Total Fatalities from Heatstroke (May-August, 2017-2024)",
       x = "Month", y = "Total Fatalities", color = "Year") +
  scale_x_continuous(breaks = 5:8) + 
  theme_minimal()




