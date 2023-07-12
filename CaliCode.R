rm(list = ls())
setwd("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/California/BayDeltaCyanos/BayDeltaCyanos")

library(readxl);library(dplyr);library(ggplot2);library(httr);library(tidyverse)

#Reading in the Data -----------------------------------------------------------------------------------------------------------------

#Water Quality Data -------------------------------------------------------------------------------------------------------------------- 
# Set the file path and name of the Excel file
file_path <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/California/BayDeltaCyanos/BayDeltaCyanos/waterqualitydata/2022_BayDelta_Compiled_WQ_Data.xlsx"

# Specify the tab name or index
tab_name <- "Collated_Data"   # Replace with the actual tab name or index

# Read the Excel file with specified tab
WQ_data <- read_excel(file_path, sheet = tab_name)

#Aerosol Data -------------------------------------------------------------------------------------------------------------------------
file_path <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/California/BayDeltaCyanos/BayDeltaCyanos/PMdata/aerosol_data_bay_delta.xlsx"

PM_data <- read_excel(file_path)

#Meteorological Data -----------------------------------------------------------------------------------------------------------------
# Set the folder path where the CSV files are located
folder_path <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/California/BayDeltaCyanos/BayDeltaCyanos/metdata"

# Get the list of file names in the folder
file_names <- list.files(path = folder_path, pattern = "\\.csv$", full.names = T)

# Create an empty list to store the data frames
data_frames <- list()

# Loop through each file name and read the CSV file
for (i in seq_along(file_names)) {
  # Read the CSV file
  data <- read.csv(file_names[i])
  
  # Assign a unique name to the data frame
  data_name <- paste0("data_", i)
  
  # Save the data frame in the list
  data_frames[[data_name]] <- data
}

met_data <- do.call(rbind, data_frames)
met_data <- met_data %>% mutate(name = case_when(name == "discovery bay california" ~ "DB", name == "stockton california" ~ "ST",TRUE ~ name)) %>%
  select(name, datetime, temp, humidity, precip, windgust, windspeed, winddir, sealevelpressure, solarradiation) %>% 
  mutate(carddir = case_when(winddir >= 337.5 | winddir < 22.5 ~ "N", winddir >= 22.5 & winddir < 67.5 ~ "NE", winddir >= 67.5 & winddir < 112.5 ~ "E", winddir >= 112.5 & winddir < 157.5 ~ "SE", winddir >= 157.5 & winddir < 202.5 ~ "S", winddir >= 202.5 & winddir < 247.5 ~ "SW", winddir >= 247.5 & winddir < 292.5 ~ "W", winddir >= 292.5 & winddir < 337.5 ~ "NW", TRUE ~ NA_character_))

#PurpleAir Data -----------------------------------------------------------------------------------------------------------------
folder_path <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/California/BayDeltaCyanos/BayDeltaCyanos/purpleairdata"

# Get the list of file names in the folder
file_names <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Create an empty list to store the data frames
data_frames <- list()

# Loop through each file name and read the CSV file
for (i in seq_along(file_names)) {
  # Read the CSV file
  data <- read.csv(file_names[i])
  
  # Convert the 'time_stamp' column from Unix time to POSIXct
  data$time_stamp <- as.POSIXct(data$time_stamp, origin = "1970-01-01 00:00:00")
  
  # Convert the 'time_stamp' column to the desired format
  data$time_stamp <- format(data$time_stamp, "%Y-%m-%d")
  
  # Perform calculations on columns 'pm2.5_cf_1_a' and 'pm2.5_cf_1_b'
  if ("pm2.5_cf_1_a" %in% colnames(data) && "pm2.5_cf_1_b" %in% colnames(data)) {
    data$sensor_dif <- data$pm2.5_cf_1_a - data$pm2.5_cf_1_b
    data$channel_comp <- ((data$pm2.5_cf_1_a - data$pm2.5_cf_1_b) * 2) / (data$pm2.5_cf_1_a + data$pm2.5_cf_1_b)
    
    # Remove rows where sensor_dif > 5 or channel_comp > 2
    data <- data[data$sensor_dif <= 5 & data$sensor_dif >= -5 & data$channel_comp <= 0.61 & data$channel_comp >= -0.61, ]
  }
  
  # Calculate the 'corrected_by_RH' column
  if ("pm2.5_cf_1" %in% colnames(data) && "humidity" %in% colnames(data)) {
    data$corrected_by_RH <- 0.524 * data$pm2.5_cf_1 - 0.0862 * data$humidity + 5.75
  }
  
  # Remove duplicate rows
  data <- data[!duplicated(data), ]
  
  
  # Assign a unique name to the data frame
  data_name <- paste0("purpleair_", i)
  
  # Assign the data frame to a variable with the unique name
  assign(data_name, data)
  
  # Save the data frame name in the list
  data_frames[[i]] <- data_name
}

# Rename data frames with names starting with "purpleair" to "DB"
for (i in 1:3) {
  old_name <- paste0("purpleair_", i)
  new_name <- paste0("DB_", i)
  assign(new_name, get(old_name))
  rm(list = old_name)
}
# Combine data frames starting with "DB"
combined_DB <- bind_rows(mget(ls(pattern = "^DB"))) %>% mutate(site = rep("DB", n()))

# Rename data frames with names starting with "purpleair" to "ST"
for (i in 4:8) {
  old_name <- paste0("purpleair_", i)
  new_name <- paste0("ST_", i - 3)
  assign(new_name, get(old_name))
  rm(list = old_name)
}
# Combine data frames starting with "ST"
combined_ST <- bind_rows(mget(ls(pattern = "^ST"))) %>% select(-starts_with("X")) %>% mutate(site = rep("ST", n()))

# Combine the data frames
combined <- bind_rows(combined_DB, combined_ST)

# Subset the data for June to October 2022
combined_filtered <- combined %>% filter(time_stamp >= as.POSIXct("2022-06-01") & time_stamp <= as.POSIXct("2022-10-31"))

# Plot the time series as a line graph with a horizontal line
ggplot(combined_filtered, aes(x = time_stamp, y = corrected_by_RH, color = site)) +
  geom_point() +
  geom_hline(aes(yintercept = 12.5), linetype = "dashed", color = "black") +
  labs(x = "Time Stamp", y = "Corrected by RH") +
  scale_color_discrete(name = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------------------------------------------------------------------------------------------------------------
# Combining Aerosol and Water data for correlation analyses
water_air_correlation <- WQ_data %>% group_by(date, site) %>% summarize(across(where(is.numeric), list(avg = mean, sd = sd), na.rm = TRUE)) %>%
  ungroup() %>% rename_with(~ ifelse(grepl("MC|ATX|CYN", .), paste0(., "W"), .), .cols = everything()) %>% rename(start_date = date)

# Merge the water and aerosol data
merged_data <- water_air_correlation %>% left_join(PM_data, by = c("site", "start_date"))
merged <- merged_data %>% select(-contains("_sd"))

ggplot(merged, aes(x = Myxo_avg, y = `2MIB_PM2.5`)) +
  geom_point()












