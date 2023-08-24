rm(list = ls())
setwd("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/California/BayDeltaCyanos/BayDeltaCyanos")

library(readxl);library(dplyr);library(ggplot2);library(httr);library(tidyverse);library(zoo);library(purrr);library(broom);library(patchwork); library(openair);library(gplots);library(factoextra); library(corrr);library(corrplot);library(FactoMineR);library(ggcorrplot)

#Reading in the Data ----------------------------------------------------------------------------------------------------------------

#Water Quality Data -------------------------------------------------------------------------------------------------------------------- 
# Set the file path and name of the Excel file
file_path <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/California/BayDeltaCyanos/BayDeltaCyanos/waterqualitydata/2022_BayDelta_Compiled_WQ_Data.xlsx"

# Specify the tab name or index
tab_name1 <- "Collated_Data"   # Replace with the actual tab name or index
tab_name2 <- "Weekly_Averages"

# Read the Excel file with specified tab
WQ_data <- read_excel(file_path, sheet = tab_name1) %>% group_by(date) %>% fill(`2MIB`, GSM, .direction = "downup") %>% ungroup()
weekly_avg_WQ_data <- read_excel(file_path, sheet = tab_name2)

#Aerosol Data -------------------------------------------------------------------------------------------------------------------------
file_path <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/California/BayDeltaCyanos/BayDeltaCyanos/PMdata/aerosol_data_bay_delta.xlsx"

aerosol_data <- read_excel(file_path) %>% rename(date = mid_date)

# Specify the tab name or index
PM_filters <- "PM_filters"
# Read the Excel file with specified tab
avg_aerosol_data <- read_excel(file_path, sheet = PM_filters)

#Meteorological Data -----------------------------------------------------------------------------------------------------------------
# Set the folder path where the CSV files are located
folder_path <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/California/BayDeltaCyanos/BayDeltaCyanos/metdata/original_files"

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
  mutate(carddir = case_when(winddir >= 337.5 | winddir < 22.5 ~ "N", winddir >= 22.5 & winddir < 67.5 ~ "NE", winddir >= 67.5 & winddir < 112.5 ~ "E", winddir >= 112.5 & winddir < 157.5 ~ "SE", winddir >= 157.5 & winddir < 202.5 ~ "S", winddir >= 202.5 & winddir < 247.5 ~ "SW", winddir >= 247.5 & winddir < 292.5 ~ "W", winddir >= 292.5 & winddir < 337.5 ~ "NW", TRUE ~ NA_character_)) %>% rename(date = datetime, site = name) %>% filter(date >= as.POSIXct("2022-06-20") & date <= as.POSIXct("2022-10-01")) %>% mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% mutate(windgust = windgust/3.6, windspeed = windspeed/3.6) #converting wind speed to m s-1 from kph

# Write the filtered dataframe to a CSV file
write.csv(met_data, file = "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/California/BayDeltaCyanos/BayDeltaCyanos/metdata/cleaned_met_data.csv", row.names = FALSE)

# Read the file back in
file_path <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/California/BayDeltaCyanos/BayDeltaCyanos/metdata/cleaned_met_data.xlsx"
# Specify the tab name or index
weekly_avg_met_tab <- "weekly_avg_met"   
# Read the Excel file with specified tab
weekly_avg_met <- read_excel(file_path, sheet = weekly_avg_met_tab)
weekly_avg_met <- weekly_avg_met %>% mutate(windgust = windgust/3.6, windspeed = windspeed/3.6)

#plotting Wind Roses
# Create wind rose plot
windRose(met_data, ws="windspeed", wd="winddir", ws.int = 3, type= "site", cols = "heat", paddle = FALSE, 
         key = list(plot.style = c("ticks", "border"),
                    fit = "all", height = 0.5,
                    space = "bottom"))
windRose(weekly_avg_met, ws="windspeed", wd="winddir", ws.int = 3, type= "site", cols = "heat", paddle = FALSE)

#PurpleAir Data -----------------------------------------------------------------------------------------------------------------
folder_path <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/California/BayDeltaCyanos/BayDeltaCyanos/purpleairdata/original_files"

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
combined_filtered <- combined %>% filter(time_stamp >= as.POSIXct("2022-06-20") & time_stamp <= as.POSIXct("2022-10-01")) %>% select(time_stamp, corrected_by_RH, site) %>% rename(date = time_stamp, `PM2.5` = corrected_by_RH) %>% mutate(date = as.Date(date, format = "%Y-%m-%d"))

PM_data_to_merge <- combined_filtered

# Convert the date column to the correct format "%Y-%m-%d"
PM_data_to_merge <- PM_data_to_merge %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

# Filter PM_data_to_merge for dates between "2022-06-25" and "2022-10-01"
cleaned_purpleair_data <- PM_data_to_merge %>%
  filter(date >= as.Date("2022-06-25") & date <= as.Date("2022-10-01"))

# Write the filtered dataframe to a CSV file
write.csv(cleaned_purpleair_data, file = "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/California/BayDeltaCyanos/BayDeltaCyanos/purpleairdata/cleaned_purpleair_data.csv", row.names = FALSE)

# Read the file back in
file_path <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/California/BayDeltaCyanos/BayDeltaCyanos/purpleairdata/cleaned_purpleair_data.xlsx"
# Specify the tab name or index
pa_weekly_avg_tab <- "pa_weekly_avg"   
# Read the Excel file with specified tab
pa_weekly_avg <- read_excel(file_path, sheet = pa_weekly_avg_tab)

# Plot the time series as a line graph with a horizontal line
ggplot(combined_filtered, aes(x = date, y = `PM2.5`, color = site)) +
  geom_point() +
  geom_hline(aes(yintercept = 12.5), linetype = "dashed", color = "black") +
  labs(x = "Date", y = "Mass Concentration PM2.5") +
  scale_color_discrete(name = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------------------------------------------------------------------------------------------------------------
# Combining Aerosol and Water data for correlation analyses
# 1. ALL DATA IS AVERAGED BY WEEK 
merged_data <- merge(avg_aerosol_data, weekly_avg_WQ_data, by = c("mid_date", "site", "sample_ID"))
merged_data <- merge(merged_data, weekly_avg_met, by = c("mid_date", "site", "sample_ID")) %>%
  mutate_at(vars(starts_with("SPATT")), as.numeric) %>% mutate(`2MIB_PM2.5` = as.integer(`2MIB_PM2.5`))
all_weekly_avg <- merge(merged_data, pa_weekly_avg, by = c("mid_date", "site", "sample_ID"))

# exploring the weekly average data with simple plots 
ggplot(all_weekly_avg, aes(x = PO4, y = SPATT_MCRR, color = site)) +
  geom_point()

# Select columns starting with "SPATT" and the date column
selected_data <- all_weekly_avg %>%
  select('2MIB_PM2.5', starts_with("SPATT"))

# Reshape the data using pivot_longer
reshaped_data <- selected_data %>%
  pivot_longer(cols = starts_with("SPATT"), 
               names_to = "SPATT_column", 
               values_to = "SPATT_value")

# 2. DAILY DATA IS IMPUTED FOR LARGER STATISTICAL ANALYSES --------------------------------------------
cleaned_purpleair_data.1 <- cleaned_purpleair_data %>% group_by(date, site) %>% summarise(PM2.5 = mean(PM2.5, na.rm = TRUE))
aerosol_data.1 <- aerosol_data %>% select(-sample_ID)
met_data.1 <- met_data %>% select(-carddir)

compiled_data_imp <- WQ_data %>% full_join(cleaned_purpleair_data.1, by = c("site", "date")) %>% select(date, site,`PM2.5`, everything()) %>% full_join(met_data.1, by = c("site", "date")) %>% full_join(aerosol_data.1, by = c("site", "date")) 

# Function for imputing missing values using linear interpolation
impute_all_columns <- function(data) {
  # Find the index of the "date" column
  date_col_index <- which(names(data) == "date")
  
  # List to store imputed dataframes for each column
  imputed_data_list <- list()
  
  # Iterate through columns following "date"
  for (col_index in (date_col_index + 1):ncol(data)) {
    col_name <- names(data)[col_index]
    
    # Convert columns into vectors
    values <- data[[col_name]]
    index <- data$date
    
    # Convert the column to a zoo time-series object
    ts_data <- zoo(values, index)
    
    # Perform linear interpolation for imputation
    imputed_data <- na.approx(ts_data)
    
    # Create a dataframe with dates and imputed values
    imputed_df <- data.frame(Date = index(imputed_data), Value = coredata(imputed_data))
    
    # Remove duplicates based on Date
    unique_imputed_df <- imputed_df %>%
      distinct(Date, .keep_all = TRUE)
    
    # Add the unique imputed dataframe to the list
    imputed_data_list[[col_name]] <- unique_imputed_df
  }
  
  # Combine the imputed dataframes into a single dataframe
  combined_imputed_data <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE),
                                  imputed_data_list)
  
  return(combined_imputed_data)
}

#Imputations For Discovery Bay
DB_imputation_df <- compiled_data_imp %>% filter(site == "DB") %>% select(-site, -sample_ID) %>%
  group_by(date) %>% summarise_all(~mean(.))

imputed_results <- impute_all_columns(DB_imputation_df) 
original_colnames <- colnames(DB_imputation_df)
colnames(imputed_results) <- original_colnames
DB_imputed_results <- imputed_results %>% filter(date >= as.Date('2022-06-25') & date <= as.Date('2022-09-26'))
DB_imputed_results <- DB_imputed_results %>% mutate(site = rep("DB", nrow(DB_imputed_results))) %>% select(date, site, everything())

#Imputations For Stockton
ST_imputation_df <- compiled_data_imp %>% filter(site == "ST") %>% select(-site, -sample_ID) %>%
  group_by(date) %>% summarise_all(~mean(.))

imputed_results <- impute_all_columns(ST_imputation_df) 
original_colnames <- colnames(ST_imputation_df)
colnames(imputed_results) <- original_colnames
ST_imputed_results <- imputed_results %>% filter(date >= as.Date('2022-06-25') & date <= as.Date('2022-09-26'))
ST_imputed_results <- ST_imputed_results %>% mutate(site = rep("ST", nrow(ST_imputed_results))) %>% select(date, site, everything())

#combining ST and DB results 
all_imputed_data <- rbind(ST_imputed_results, DB_imputed_results)

# CONDUCTING SINGLE LINEAR REGRESSION ANALYSES TO DETERMINE WHICH VARIABLES ARE ASSOCIATED TO CREATE GROUPINGS FOR PRINCIPAL COMPONENT ANALYSES 

# Function to perform linear regression for all combinations of columns
# removing date to see if the regression function works without Date 
imputed_data <- all_imputed_data %>% select(-date, -site) %>%
  rename_with(~ gsub("PM2.5", "PM2", .), contains("PM2.5")) %>% rename(purpleair_PM = `PM2`, nineteen_but = `19but`, hex_fuco = `19hex-fuco`, cis_neo = `9cis-Neo`, MIB = `2MIB`, BCar = `B-Car`, MIB_PM2 = `2MIB_PM2`) %>%
  select(-where(~ all(. == 0 | is.na(.)))) %>% select(-matches("MC")) #once I have actual MC data get rid of the final command

practice.mod <- glm(MIB_PM2 ~ MIB, data = imputed_data)
summary(practice.mod)

# Define the function
perform_all_regressions <- function(data) {
  colnames_data <- colnames(data)
  combinations <- combn(colnames_data, 2, simplify = FALSE)
  
  results <- map_df(combinations, ~ {
    col1 <- .[[1]]
    col2 <- .[[2]]
    
    model <- glm(data = data, formula = as.formula(paste(col1, "~", col2)))
    summary <- broom::tidy(model)
    
    # Get degrees of freedom, residual deviance, and AIC
    dof <- model$df.residual
    deviance <- model$deviance
    aic <- AIC(model)
    
    # Create a summary dataframe
    summary_df <- data.frame(Column1 = col1, Column2 = col2, dof, deviance, aic, summary)
    
    return(summary_df)
  })
  
  return(results)
}

# Perform linear regression for all combinations of columns
regression_results <- as.data.frame(perform_all_regressions(imputed_data))
filtered_regression_results <- regression_results %>% filter(p.value <= 0.05, term != "(Intercept)") 

# plotting some relationships simply for visualization purposes 
# Define the function
#simple_geom_plot <- function(x_var, y_var) {
 # plot_title <- paste("Scatter Plot of", x_var, "vs", y_var)
 # ggplot(cleaned_imputed_df, aes(x = !!sym(x_var), y = !!sym(y_var))) +
   # geom_point() +
  #  labs(x = x_var, y = y_var, title = plot_title) +
 #  theme_minimal()
#}

#simple_geom_plot("Total_chl_a", "ATXa")


# 3. VISUALIZING THE DATA IN USEFUL WAYS ------------------------------------------------------------------
water_air_correlation <- WQ_data %>% group_by(date, site) %>% summarize(across(where(is.numeric), list(avg = mean), na.rm = TRUE)) %>%
  ungroup() %>% rename_with(~ ifelse(grepl("MC|ATX|CYN", .), paste0(., "W"), .), .cols = everything()) 

# Merge the water and aerosol data
merged_data <- water_air_correlation %>% left_join(aerosol_data, by = c("site", "date"))
merged <- merged_data %>% select(-contains("_sd")) %>% mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Merge PM data with water quality data
merged.1 <- PM_data_to_merge %>% full_join(merged, by = c("site", "date")) %>% full_join(met_data, by = c("site", "date")) %>% mutate(date = as.POSIXct(date, format = "%Y-%m-%d")) %>% mutate(`2MIB_PM2.5` = as.integer(`2MIB_PM2.5`))

#showing relationship between 2MIB and PM2.5 
ggplot(merged.1, aes(x = date, y = `PM2.5`, color = site)) +
  geom_line() +
  geom_hline(aes(yintercept = 12.5), linetype = "dashed", color = "black") +
  geom_point(aes(x=date, y = `2MIB_PM2.5`), color = "red") +
  geom_point(aes(x=date, y = `2MIB_avg`), color = "black") +
  labs(x = "Date", y = "Mass Concentration PM2.5") +
  scale_color_discrete(name = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Phyto-pigment visualization 
pigments <- WQ_data %>% select(date, site, Chl_c1c2, Perid, `19but`, Fuco, `19hex-fuco`, `9cis-Neo`, Viola, Diadino, Anthera, Myxo, Allo, Diato, Monado, Lutein, Zea, gyro, Chl_b, `B-Car`, Total_chl_a, cantha, echino)

# Select columns for plotting
columns_to_plot <- c( "Chl_c1c2","Perid","19but","Fuco","19hex-fuco","9cis-Neo","Viola","Diadino","Anthera","Myxo","Allo","Diato","Monado","Lutein","Zea","gyro","Chl_b","B-Car","Total_chl_a","cantha","echino")

#DISCO BAY
subset_data <- WQ_data %>% filter(site == "DB") %>% select(date, all_of(columns_to_plot))
# Create a new dataframe for normalized data
normalized_data <- subset_data %>%
  mutate(across(.cols = columns_to_plot, ~ . / Total_chl_a)) %>%
  select(date, columns_to_plot) %>% select(-Total_chl_a) #%>% mutate(Total_chl_a = subset_data$Total_chl_a)
# Reshape the data for plotting
data_for_plot <- normalized_data %>%
  pivot_longer(cols = -date, names_to = "Pigment", values_to = "Relative Abundance") %>% mutate(date = as.character(date))

DB.phytos <- ggplot(data_for_plot, aes(x = date, y = `Relative Abundance`, fill = Pigment)) +
  geom_bar(stat = "identity") +
  labs(title = "Phytoplankton in Discovery Bay, 2022",
       x = "Date", y = "Relative Abundance") +
  scale_fill_viridis_d() +  # You can use other color scales as well
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") +
  scale_y_continuous(limits = c(0,2))  
# Cyanobacterial Pigments Grouped (Mxyo and Zea)
cyano_biomass <- normalized_data %>% rowwise() %>% mutate(Cyanobacteria = sum(c_across(c(Myxo, Zea, cantha, echino)), na.rm = TRUE),
                                                          Others = sum(c_across(!c(date, Myxo, Zea, cantha, echino)), na.rm = TRUE)) %>% select(date, Cyanobacteria, Others) %>%
  pivot_longer(cols = -date, names_to = "Pigment", values_to = "Relative Abundance") %>% mutate(date = as.character(date)) %>% filter(Pigment == "Cyanobacteria")
# Define a custom color palette
custom_colors <- c("Cyanobacteria" = "darkgreen", "Others" = "gray80")
DB.cyanos <-ggplot(cyano_biomass , aes(x = date, y = `Relative Abundance`, fill = Pigment)) +
  geom_bar(stat = "identity") +
  labs(title = "Cyanobacterial Biomass in Discovery Bay, 2022",
       x = "Date", y = "Relative Abundance") + scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") +
  scale_y_continuous(limits = c(0,0.5))  # Adjust legend text size

#STOCKTON
subset_data.ST <- WQ_data %>% filter(site == "ST") %>% select(date, all_of(columns_to_plot))
# Create a new dataframe for normalized data
normalized_data.ST <- subset_data.ST %>%
  mutate(across(.cols = columns_to_plot, ~ . / Total_chl_a)) %>%
  select(date, columns_to_plot) %>% select(-Total_chl_a)
# Reshape the data for plotting
data_for_plot.ST <- normalized_data.ST %>%
  pivot_longer(cols = -date, names_to = "Pigment", values_to = "Relative Abundance") %>% mutate(date = as.character(date))

ST.phytos <- ggplot(data_for_plot.ST, aes(x = date, y = `Relative Abundance`, fill = Pigment)) +
  geom_bar(stat = "identity") +
  labs(title = "Phytoplankton in Stockton, 2022",
       x = "Date", y = "Relative Abundance") +
  scale_fill_viridis_d() +  # You can use other color scales as well
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.key.size = unit(0.5, "cm"),  # Adjust legend key size
        legend.key.width = unit(1, "cm"),  # Adjust legend key width
        legend.text = element_text(size = 8)) + # Adjust legend text size
  scale_y_continuous(limits = c(0,2))
# Cyanobacterial Pigments Grouped (Mxyo and Zea)
cyano_biomass.ST <- normalized_data.ST %>% rowwise() %>% mutate(Cyanobacteria = sum(c_across(c(Myxo, Zea, cantha, echino)), na.rm = TRUE),
                                                                Others = sum(c_across(!c(date, Myxo, Zea, cantha, echino)), na.rm = TRUE)) %>% select(date, Cyanobacteria, Others) %>%
  pivot_longer(cols = -date, names_to = "Pigment", values_to = "Relative Abundance") %>% mutate(date = as.character(date)) %>% filter(Pigment == "Cyanobacteria")
# Define a custom color palette
ST.cyanos <- ggplot(cyano_biomass.ST , aes(x = date, y = `Relative Abundance`, fill = Pigment)) +
  geom_bar(stat = "identity") +
  labs(title = "Cyanobacterial Biomass in Stockton, 2022",
       x = "Date", y = "Relative Abundance") + scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.key.size = unit(0.5, "cm"),  # Adjust legend key size
        legend.key.width = unit(1, "cm"),  # Adjust legend key width
        legend.text = element_text(size = 8)) + # Adjust legend text size
  scale_y_continuous(limits = c(0,0.5))

#Visualize Stockton and Discovery Bay together
DB.phytos + ST.phytos
DB.cyanos + ST.cyanos

# PEARSON'S CORRELATION COEFFICIENT 
# Function to calculate Pearson's Correlation Coefficient and plot heatmap
create_correlation_heatmap <- function(data, columns) {
  # Select columns and remove rows with all zeros or NA values
  data_selected <- data %>%
    select(all_of(columns)) %>%
    select(-where(~ all(. == 0 | is.na(.)))) %>% drop_na()
  
  # Calculate correlation matrix
  cor_matrix <- cor(data_selected)
  
  # Create a triangular correlation matrix
  cor_matrix_triangular <- cor_matrix
  cor_matrix_triangular[upper.tri(cor_matrix_triangular, diag = TRUE)] <- NA
  
  # Convert triangular correlation matrix to a data frame
  tidy_cor <- as.data.frame(as.table(cor_matrix_triangular)) 
  
  # Rename the columns
  colnames(tidy_cor) <- c("Variable1", "Variable2", "Correlation")
  
  # Round correlation values to 1 decimal place
  rounded_data <- tidy_cor %>% mutate(Correlation = signif(Correlation, 1))
  
  # Create the heatmap
  ggplot(rounded_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "dodgerblue2", mid = "white", high = "red", midpoint = 0) +
    labs(title = "Correlation Heatmap", x = "Variable", y = "Variable") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = Correlation), color = "black", size = 3)
}

# Heatmaps with grouped comparisons
colnames(all_weekly_avg)

#PHYTOPLANKTON PIGMENTS
create_correlation_heatmap(all_weekly_avg, c("Chlde_a","unknown_pigment",  "Chl_c1c2","Perid" , "19but" , "Fuco" , "19hex-fuco",  "9cis-Neo","Viola" ,"Diadino", "Anthera","Myxo","Allo","Diato","Monado","Lutein", "Zea", "gyro","Chl_b" , "Chl_a", "B-Car","Total_chl_a" , "cantha","echino" )) 
create_correlation_heatmap(all_imputed_data, c("Chlde_a","unknown_pigment",  "Chl_c1c2","Perid" , "19but" , "Fuco" , "19hex-fuco",  "9cis-Neo","Viola" ,"Diadino", "Anthera","Myxo","Allo","Diato","Monado","Lutein", "Zea", "gyro","Chl_b" , "Chl_a", "B-Car","Total_chl_a" , "cantha","echino" )) #checking to see if imputed data and weekly averages are similar in results, they are 

# TOXINS
create_correlation_heatmap(all_weekly_avg, c("particulate_N", 'molar_ratio', "Chl_a", "Myxo", "Zea", "cantha", "echino", "SPATT_MCLR", "SPATT_MCRR", "SPATT_MCLA", "SPATT_MCYR", "SPATT_ATX", "ATXa", "homoATXa", "pa_PM2.5", "solarradiation")) 
create_correlation_heatmap(all_imputed_data, c('molar_ratio', "Chl_a", "Myxo", "Zea", "NOx", "PO4", "NH4",  "ATXa", "homoATXa")) 

# WEATHER AND PM2.5
create_correlation_heatmap(all_weekly_avg, c("temp","humidity","precip","windgust","windspeed", "winddir","sealevelpressure", "solarradiation","pa_PM2.5")) 

# PRINCIPAL COMPONENT ANALYSES 
pca.data <- all_imputed_data %>% select(-date, -site) %>% select(-where(~ all(. == 0 | is.na(.)))) %>% select("PM2.5","Chl_a","Zea"  , "Myxo", "ATXa", "homoATXa", "2MIB_PM2.5", "solarradiation", "humidity", "temp", "2MIB", "GSM") %>% mutate(`2MIB_PM2.5` = ifelse(is.na(`2MIB_PM2.5`), 0, `2MIB_PM2.5`)) %>% drop_na()
normalized.pca.data <- scale(pca.data)
pca.corr.matrix <- cor(normalized.pca.data)
pca.matrix.df <- as.data.frame(pca.corr.matrix)
ggcorrplot(pca.corr.matrix)
data.pca <- princomp(pca.corr.matrix)
summary(data.pca)
data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = T)
fviz_pca_biplot(data.pca, col.var = "cos2", repel = TRUE, labelsize = 5 , pointsize = 2, font.family = "Arial", arrowsize = 1.25)

pca.data.1 <- all_imputed_data %>% select(-date, -site) %>% select(-where(~ all(. == 0 | is.na(.)))) %>% select("PM2.5","Chl_a","Zea", "Myxo", "cantha", "echino", "ATXa", "homoATXa", "2MIB_PM2.5", "windspeed", "humidity","2MIB", "GSM") %>% mutate(`2MIB_PM2.5` = ifelse(is.na(`2MIB_PM2.5`), 0, `2MIB_PM2.5`)) %>% drop_na()
normalized.pca.data <- scale(pca.data.1)
pca.corr.matrix <- cor(normalized.pca.data)
pca.matrix.df <- as.data.frame(pca.corr.matrix)
ggcorrplot(pca.corr.matrix)
data.pca <- princomp(pca.corr.matrix)
summary(data.pca)
data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = T)
fviz_pca_biplot(data.pca, col.var = "cos2", repel = TRUE, labelsize = 5 , pointsize = 2, arrowsize = 1.25)

pca.data.2 <- all_weekly_avg %>% select(-mid_date, -site, -sample_ID) %>% select(-where(~ all(. == 0 | is.na(.)))) %>% select("pa_PM2.5","Chl_a", "Myxo", "ATXa", "2MIB_PM2.5", "SPATT_MCLA", "SPATT_MCLR", "SPATT_ATX") %>% mutate(`2MIB_PM2.5` = ifelse(is.na(`2MIB_PM2.5`), 0, `2MIB_PM2.5`)) %>% drop_na()
normalized.pca.data <- scale(pca.data.2)
pca.corr.matrix <- cor(normalized.pca.data)
pca.matrix.df <- as.data.frame(pca.corr.matrix)
ggcorrplot(pca.corr.matrix)
data.pca <- princomp(pca.corr.matrix)
summary(data.pca)
data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = T)
fviz_pca_biplot(data.pca, col.var = "cos2", repel = TRUE, labelsize = 5 , pointsize = 2, arrowsize = 1.25)

#MAKING A MAP OF THE SITES -------------------------------------------------------------------------------------
install.packages("mapview")
library()
library(leaflet)
# Create a leaflet map centered on Stockton
m <- leaflet() %>% setView(lng = -121.446567, lat = 37.919217, zoom = 10) %>% 
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = -121.587703, lat = 37.905529, radius = 15, color = "black") %>%
  addCircleMarkers(lng = -121.306645, lat = 37.953817, radius = 15, color = "blue") 
m
