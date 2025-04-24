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

################################################################
# MERGING DATA
# Define file names based on the naming pattern
h_files <- sprintf("heatstroke003_data_h%d.xlsx", 20:30)
r_files <- sprintf("heatstroke003_data_r%d.xlsx", 1:6)

# Combine all filenames into a list
all_files <- c(h_files, r_files)

# Read and combine all data files, including all sheets
combined_data <- lapply(all_files, function(file) {
  if (file.exists(file)) {
    sheets <- excel_sheets(file)
    sheet_data <- lapply(sheets, function(sheet) {
      read_excel(file, sheet = sheet) %>% mutate(Source_File = file, Sheet_Name = sheet)
    })
    bind_rows(sheet_data)
  } else {
    message(paste("File not found:", file))
    return(NULL)
  }
}) %>% bind_rows()

message("CSV file created successfully: combined_heatstroke_data.csv")

################################################################
# CLEANING DATA

# Replace NA values in "年齢区分：不明" with 0
combined_data <- combined_data %>% mutate(`年齢区分：不明` = ifelse(is.na(`年齢区分：不明`), 0, `年齢区分：不明`))

# Filter rows with any NA values
na_rows <- combined_data %>% filter(if_any(everything(), is.na))

# Save rows with NAs to a separate CSV
write.csv(na_rows, "rows_with_NA.csv", row.names = FALSE)

# Rename columns manually
combined_data <- combined_data %>% rename(
  Date = `日付`,
  Prefecture_Code = `都道府県コード`,
  Total_ER = `搬送人員（計）`,
  Age_Under28d = `年齢区分：新生児`,
  Age_Under7y = `年齢区分：乳幼児`,
  Age_Under18y = `年齢区分：少年`,
  Age_Adult = `年齢区分：成人`,
  Age_Over65y =`年齢区分：高齢者`,
  Age_Unknown = `年齢区分：不明`,
  Severity_Fatal = `傷病程度：死亡`,
  Severity_Heavy = `傷病程度：重症`,
  Severity_Medium = `傷病程度：中等症`,
  Severity_Light = `傷病程度：軽症`,
  Severity_Other = `傷病程度：その他`,
  Location_Home = `発生場所：住居`,
  Location_Work_Industrial = `発生場所：仕事場①`,
  Location_Work_Agricultural = `発生場所：仕事場②`,
  Location_Academic = `発生場所：教育機関`,
  Location_Public_Indoor = `発生場所：公衆(屋内)`,
  Location_Public_Outdoor = `発生場所：公衆(屋外)`,
  Location_Road = `発生場所：道路`,
  Location_Other = `発生場所：その他`
)

# Removing Data from 2008 ~ 2016 with NA values for Location
heatstrokes_2017_2024 <- combined_data |>
  mutate(Year = year(Date),
         Month = month(Date)) |> 
  filter(Year >= 2017)


# Save combined data to CSV
write.csv(combined_data, "combined_heatstroke_data.csv", row.names = FALSE)
write.csv(heatstrokes_2017_2024, "heatstrokes_2017_2024.csv", row.names = FALSE)



