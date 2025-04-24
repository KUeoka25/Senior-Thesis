################################################################
# Script Name: JMA_Data.R
# Author: Kenshin Ueoka
# Date: Mar 7, 2025
# Purpose: This script is to try the jmastats and clidatajp packages

################################################################

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

################################################################
install.packages("jmastats")
library(jmastats)
install.packages("clidatajp")
library(clidatajp)
install.packages(c("rvest", "dplyr", "openxlsx"))
library(rvest)
library(dplyr)
library(openxlsx)
library(data.table)

###########
#SET UP
library(jmastats)

ls("package:jmastats")
jmastats::stations
stations_df <- as.data.frame(stations)
head(stations_df)

block_numbers_unique <- stations_df |> 
  filter(station_type %in% c("官")) |>
  filter(is.na(note2) | note2 != "日照・湿度・気圧を除く") |>
  select(block_no) |>
  distinct()
   

block_list <- block_numbers_unique |> pull(block_no)
###########
#Dates 
years <- 2017:2024
months <- 5:8

###########
#Creating Excel Workbook
JMA_Dataset <- createWorkbook()

#weather_data <- jma_collect(item = "daily", block_no = "47401" , year = 2022, month = 5, cache = FALSE, pack = FALSE, quiet = TRUE)

# Loop through all stations, years, and months
for (block in block_list) {
  for (year in years) {
    for (month in months) {
      # Try to collect weather data
      weather_data <- tryCatch(
        jma_collect(item = "daily", block_no = block, year = year, month = month, cache = FALSE, pack = FALSE, quiet = TRUE),
        error = function(e) NULL  # Drawing data from JMA Website
      )
      
      # Skip if no data is retrieved
      if (!is.null(weather_data)) {
        # Create a sheet name 
        sheet_name <- paste0(block, "_", year, "_", month)
        sheet_name <- substr(sheet_name, 1, 31)   
        
        # Add the data to the workbook
        addWorksheet(JMA_Dataset, sheet_name)
        writeData(JMA_Dataset, sheet_name, weather_data)
        
        #Sys.sleep(2)  # Pause to avoid overloading JMA’s servers
      }
    }
  }
}

saveWorkbook(JMA_Dataset, "JMA_Weather_All_Prefectures_2017_2024.xlsx", overwrite = TRUE)

######
#BINDING ROWS
file_path <- here::here("JMA_Weather_All_Prefectures_2017_2024.xlsx")

# Read all sheets and bind them together
# weather_data_list <- lapply(sheet_names, function(sheet) {
#   data <- read.xlsx(file_path, sheet = sheet)
#   
#   block_no <- strsplit(sheet, "_")[[1]][1]   # Extract block_no from the sheet name
#   
#   data$block_no <- block_no # Add block_no column
#   
#   return(data)
# })
sheets <- getSheetNames(file_path)
combined_weather_data <- lapply(sheets, function(sheet) {
  data <- tryCatch(
    read_excel(file_path, sheet = sheet) %>%
      mutate(block_no = strsplit(sheet, "_")[[1]][1],  # Extract block_no from sheet name
             Source_Sheet = sheet),  # Add sheet name as metadata
    error = function(e) {
      message(paste("Error reading sheet:", sheet))
      return(NULL)
    }
  )
  return(data)
}) %>% bind_rows() 

write.csv(combined_weather_data, "Combined_JMA_Weather_2017_2022.csv", row.names = FALSE)
