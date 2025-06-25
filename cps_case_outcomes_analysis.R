
# ===============================================================
# Analysing Criminal Case Outcomes: A Data Science Perspective on Crown Prosecution Service Dataset
# Created by: Onur Tuncay
# ===============================================================

# ===============================================================
# Load libraries
# ===============================================================
library(tidyverse)   # Core data manipulation and visualization package collection
library(dplyr)       # Data wrangling: select, filter, mutate, summarise operations
library(ggplot2)     # Data visualization with customizable plots
library(reshape2)    # Data reshaping between wide and long formats, e.g., melt, dcast
library(caret)       # Machine learning model training, partitioning, and evaluation like RMSE, confusion matrix
library(broom)       # Tidying and organizing model outputs for reporting
library(GGally)      # Extended visualization tools like ggpairs for pairwise scatterplots
library(ggpubr)      # Combining and enhancing ggplot2 visualizations for publication-ready figures
library(glmnet)      # Regularized regression models: Ridge and Lasso via glmnet, cv.glmnet
library(cluster)     # Clustering algorithms and evaluation metrics like silhouette analysis
library(factoextra)  # Visualization of clustering results like dendrograms and cluster plots
library(dbscan)      # Density-based clustering via DBSCAN and kNN distance plots
library(randomForest) # Random Forest classification and regression models with variable importance
library(xgboost)     # Gradient boosting machine learning models for classification and regression
library(fmsb)        # Creating radar charts for model comparison and multivariate visualization

# ===============================================================
# PART 1: Define folders and list CSV files
# ===============================================================
# Set the path to the dataset folders (for 2014 and 2015)
base_path <- "C:/Users/onurt/OneDrive/Desktop/UoG Msc Data Science/CT7202 - Data Analysis and Visualisation Principles/Assessment/Dataset"
folders <- c("2014", "2015")
full_paths <- file.path(base_path, folders)

# Function to extract the date from the file name 
extract_date_from_filename <- function(file_path) {
  file_name <- tolower(basename(file_path))
  parts <- strsplit(file_name, "_")[[1]]
  month <- parts[length(parts) - 1]
  year <- str_replace(parts[length(parts)], ".csv", "")
  date_str <- paste0("01-", month, "-", year)
  as.Date(date_str, format = "%d-%B-%Y")
}

# ===============================================================
# PART 2: Read and combine data from all files
# ===============================================================
data_list <- list()

for (folder in full_paths) {
  file_list <- list.files(path = folder, pattern = "*.csv", full.names = TRUE)
  for (file in file_list) {
    df <- read_csv(file, show_col_types = FALSE)
    df$Date <- extract_date_from_filename(file)
    data_list[[length(data_list) + 1]] <- df
  }
}

combined_data <- bind_rows(data_list)

#  Check for duplicate rows
duplicate_rows <- combined_data %>%
  duplicated()

# Count number of duplicate rows
num_duplicates <- sum(duplicate_rows)
cat("Number of duplicate rows found:", num_duplicates, "\n")

# ===============================================================
# PART 3: Create a 43-row placeholder for the missing month (November 2015)
# ===============================================================
# Determine the number of rows typically in one monthly dataset
expected_row_count <- 43  # max(table(combined_data$Month)) = 43 in this dataset

# Create a template row with all NAs
template_row <- combined_data[1, ]
template_row[1, ] <- NA

# Replicate this row 43 times
november_placeholder <- template_row[rep(1, expected_row_count), ]
november_placeholder$Date <- as.Date("2015-11-01")

# Bind the placeholder data to the main dataset
combined_data <- bind_rows(combined_data, november_placeholder)

# ===============================================================
# PART 4: Control Data Count by Month
# ===============================================================

# Group by Date and calculate total rows
monthly_counts <- combined_data %>%
  group_by(Date) %>%
  summarise(
    Total_Rows = n()
  ) %>%
  arrange(Date)

# Display the grouped data
print(monthly_counts)

# Plotting the results
ggplot(monthly_counts, aes(x = Date, y = Total_Rows)) +
  geom_col(fill = "steelblue") +
  labs(title = "Total Row Counts by Date",
       x = "Date",
       y = "Number of Rows") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  # writing date for each month
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# ===============================================================
# PART 5: Sanitizing Column Names 
# ===============================================================

# 1. Rename the first unnamed column to "Area"
combined_data <- combined_data %>%
  rename(Area = `...1`)

# 2. Rename all other columns for clarity and consistency
combined_data <- combined_data %>%
  rename(
    homicide_convictions = `Number of Homicide Convictions`,
    homicide_convictions_pct = `Percentage of Homicide Convictions`,
    homicide_unsuccessful = `Number of Homicide Unsuccessful`,
    homicide_unsuccessful_pct = `Percentage of Homicide Unsuccessful`,
    
    offences_person_convictions = `Number of Offences Against The Person Convictions`,
    offences_person_convictions_pct = `Percentage of Offences Against The Person Convictions`,
    offences_person_unsuccessful = `Number of Offences Against The Person Unsuccessful`,
    offences_person_unsuccessful_pct = `Percentage of Offences Against The Person Unsuccessful`,
    
    sexual_offences_convictions = `Number of Sexual Offences Convictions`,
    sexual_offences_convictions_pct = `Percentage of Sexual Offences Convictions`,
    sexual_offences_unsuccessful = `Number of Sexual Offences Unsuccessful`,
    sexual_offences_unsuccessful_pct = `Percentage of Sexual Offences Unsuccessful`,
    
    burglary_convictions = `Number of Burglary Convictions`,
    burglary_convictions_pct = `Percentage of Burglary Convictions`,
    burglary_unsuccessful = `Number of Burglary Unsuccessful`,
    burglary_unsuccessful_pct = `Percentage of Burglary Unsuccessful`,
    
    robbery_convictions = `Number of Robbery Convictions`,
    robbery_convictions_pct = `Percentage of Robbery Convictions`,
    robbery_unsuccessful = `Number of Robbery Unsuccessful`,
    robbery_unsuccessful_pct = `Percentage of Robbery Unsuccessful`,
    
    theft_handling_convictions = `Number of Theft And Handling Convictions`,
    theft_handling_convictions_pct = `Percentage of Theft And Handling Convictions`,
    theft_handling_unsuccessful = `Number of Theft And Handling Unsuccessful`,
    theft_handling_unsuccessful_pct = `Percentage of Theft And Handling Unsuccessful`,
    
    fraud_forgery_convictions = `Number of Fraud And Forgery Convictions`,
    fraud_forgery_convictions_pct = `Percentage of Fraud And Forgery Convictions`,
    fraud_forgery_unsuccessful = `Number of Fraud And Forgery Unsuccessful`,
    fraud_forgery_unsuccessful_pct = `Percentage of Fraud And Forgery Unsuccessful`,
    
    criminal_damage_convictions = `Number of Criminal Damage Convictions`,
    criminal_damage_convictions_pct = `Percentage of Criminal Damage Convictions`,
    criminal_damage_unsuccessful = `Number of Criminal Damage Unsuccessful`,
    criminal_damage_unsuccessful_pct = `Percentage of Criminal Damage Unsuccessful`,
    
    drugs_offences_convictions = `Number of Drugs Offences Convictions`,
    drugs_offences_convictions_pct = `Percentage of Drugs Offences Convictions`,
    drugs_offences_unsuccessful = `Number of Drugs Offences Unsuccessful`,
    drugs_offences_unsuccessful_pct = `Percentage of Drugs Offences Unsuccessful`,
    
    public_order_convictions = `Number of Public Order Offences Convictions`,
    public_order_convictions_pct = `Percentage of Public Order Offences Convictions`,
    public_order_unsuccessful = `Number of Public Order Offences Unsuccessful`,
    public_order_unsuccessful_pct = `Percentage of Public Order Offences Unsuccessful`,
    
    other_offences_convictions = `Number of All Other Offences (excluding Motoring) Convictions`,
    other_offences_convictions_pct = `Percentage of All Other Offences (excluding Motoring) Convictions`,
    other_offences_unsuccessful = `Number of All Other Offences (excluding Motoring) Unsuccessful`,
    other_offences_unsuccessful_pct = `Percentage of All Other Offences (excluding Motoring) Unsuccessful`,
    
    motoring_convictions = `Number of Motoring Offences Convictions`,
    motoring_convictions_pct = `Percentage of Motoring Offences Convictions`,
    motoring_unsuccessful = `Number of Motoring Offences Unsuccessful`,
    motoring_unsuccessful_pct = `Percentage of Motoring Offences Unsuccessful`,
    
    admin_finalised_unsuccessful = `Number of Admin Finalised Unsuccessful`,
    percentage_l_motoring_unsuccessful = `Percentage of L Motoring Offences Unsuccessful`
  )

# 3. Display updated column names
cat("Sanitized Column Names:\n")
print(colnames(combined_data))



# ===============================================================
# PART 6: Further Sanitizing Columns and Scaling Percentage Values
# ===============================================================

# Step 1: Remove any percentage symbols (%) from the relevant columns
# Step 2: Convert percentage columns to numeric 
# Step 3: Scale percentage values by dividing them by 100

# Identify columns that contain either 'pct' or 'percentage' in their names
percentage_columns <- names(combined_data)[str_detect(names(combined_data), "pct|percentage")]


# Remove any '%' characters and scale values
combined_data <- combined_data %>%
  mutate(across(all_of(percentage_columns), ~ {
    # Remove percentage symbol if it exists (just in case)
    cleaned <- str_replace_all(as.character(.x), "%", "")
    # Convert to numeric and divide by 100
    as.numeric(cleaned) / 100
  }))

# check first few rows
head(combined_data)


# ===============================================================
# PART 7: Handling Missing Records - Step 1
# Filling November 2015 Missing Data Based on November 2014
# ===============================================================

# Filter November 2014 and November 2015 data
nov_2014_data <- combined_data %>% filter(Date == as.Date("2014-11-01"))
nov_2015_data <- combined_data %>% filter(Date == as.Date("2015-11-01"))

# Copy values from November 2014 to November 2015 for missing rows
# Assuming the Area structure is the same in both months
nov_2015_data_filled <- nov_2015_data

for (col in names(nov_2015_data_filled)) {
  if (col != "Date") {
    nov_2015_data_filled[[col]][is.na(nov_2015_data_filled[[col]])] <- nov_2014_data[[col]][is.na(nov_2015_data_filled[[col]])]
  }
}

# Replace the original November 2015 data with the filled version
combined_data <- combined_data %>%
  filter(!(Date == as.Date("2015-11-01"))) %>%   # Remove old November 2015 data
  bind_rows(nov_2015_data_filled)                  # Add updated November 2015 data



# ===============================================================
# PART 7: Handling Missing Records - Step 2 
# Identifying and Visualizing Columns with Missing Values Only
# ===============================================================

# Calculate number of missing (NA) values for each column
missing_values_summary <- combined_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Missing_Count") %>%
  arrange(desc(Missing_Count))

# Keep only columns with at least one missing value
missing_values_filtered <- missing_values_summary %>%
  filter(Missing_Count > 0)

# Display the filtered missing values table
print(missing_values_filtered)

# Plot the missing values per column with counts on top
missing_plot <- ggplot(missing_values_filtered, aes(x = reorder(Column, -Missing_Count), y = Missing_Count)) +
  geom_bar(stat = "identity", fill = "tomato") +
  geom_text(aes(label = Missing_Count), vjust = -0.5, size = 3) +  # adding missing counts to plot
  labs(title = "Columns with Missing Values",
       x = "Columns",
       y = "Missing Value Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Show the plot
print(missing_plot)



# ===============================================================
# PART 7: Handling Missing Records - Step 3
# ===============================================================

# Fill homicide-related missing percentages with 0 
# Reason: If there are no convictions/unsuccessful cases, the percentage is logically 0

combined_data <- combined_data %>%
  mutate(
    homicide_convictions_pct = if_else(is.na(homicide_convictions_pct), 0, homicide_convictions_pct),
    homicide_unsuccessful_pct = if_else(is.na(homicide_unsuccessful_pct), 0, homicide_unsuccessful_pct)
  )

# Fill 'percentage_l_motoring_unsuccessful' using Area-based averages
# If Area-based average is not available, fill with overall mean as fallback

# First, calculate Area-level averages
area_avg_motoring_pct <- combined_data %>%
  group_by(Area) %>%
  summarise(avg_motoring_unsuccessful = mean(percentage_l_motoring_unsuccessful, na.rm = TRUE))

# Merge these averages back to the main dataset
combined_data <- combined_data %>%
  left_join(area_avg_motoring_pct, by = "Area") %>%
  mutate(
    percentage_l_motoring_unsuccessful = if_else(
      is.na(percentage_l_motoring_unsuccessful),
      avg_motoring_unsuccessful,
      percentage_l_motoring_unsuccessful
    )
  ) %>%
  select(-avg_motoring_unsuccessful)  # Remove helper column

# Verify that no missing values remain in the targeted columns
missing_check <- combined_data %>%
  summarise(
    homicide_convictions_pct_missing = sum(is.na(homicide_convictions_pct)),
    homicide_unsuccessful_pct_missing = sum(is.na(homicide_unsuccessful_pct)),
    percentage_l_motoring_unsuccessful_missing = sum(is.na(percentage_l_motoring_unsuccessful))
  )

print(missing_check)


# ===============================================================
# PART 8: Feature Reductions - Subsetting Only Convictions Related Columns
# ===============================================================

# Select only the columns related to convictions
# Logic: Keep "Area", "Date", and all columns containing "convictions" in their names

# Identify conviction-related columns
conviction_columns <- names(combined_data)[str_detect(names(combined_data), "convictions")]

# Always keep Area and Date for context
selected_columns <- c("Area", "Date", conviction_columns)

# Create a new dataset with only the selected columns
convictions_data <- combined_data %>% 
  select(all_of(selected_columns))

# Display the structure of the new convictions_data
glimpse(convictions_data)



# ===============================================================
# PART 9: Creating an Alternate Long Format Data Frame
# ===============================================================

# Reshape the dataset: pivot all conviction-related columns into long format
long_data <- combined_data %>%
  pivot_longer(
    cols = -c(Area, Date),        # Keep Area and Date as they are
    names_to = "Category",         # New column name for the original variable names
    values_to = "Value"            # New column name for the corresponding values
  )

# Preview the first few rows
head(long_data)


colnames(combined_data)

# ===============================================================
# PART 10: Creating an Alternate Aggregated Data Frame
# Aggregating All Conviction and Unsuccessful Case Numbers
# ===============================================================

# Select Area, Date, and only the "convictions" and "unsuccessful" number columns (excluding percentages)
convictions_data <- combined_data %>%
  select(
    Area, Date,
    homicide_convictions, homicide_unsuccessful,
    offences_person_convictions, offences_person_unsuccessful,
    sexual_offences_convictions, sexual_offences_unsuccessful,
    burglary_convictions, burglary_unsuccessful,
    robbery_convictions, robbery_unsuccessful,
    theft_handling_convictions, theft_handling_unsuccessful,
    fraud_forgery_convictions, fraud_forgery_unsuccessful,
    criminal_damage_convictions, criminal_damage_unsuccessful,
    drugs_offences_convictions, drugs_offences_unsuccessful,
    public_order_convictions, public_order_unsuccessful,
    other_offences_convictions, other_offences_unsuccessful,
    motoring_convictions, motoring_unsuccessful,
    admin_finalised_unsuccessful
  )

# Aggregate total convictions and unsuccessful cases
aggregated_data <- convictions_data %>%
  mutate(
    total_convictions = rowSums(select(., ends_with("_convictions")), na.rm = TRUE),
    total_unsuccessful = rowSums(select(., ends_with("_unsuccessful")), na.rm = TRUE)
  ) %>%
  select(Area, Date, total_convictions, total_unsuccessful)  # Keep only totals

# Preview the aggregated dataset
head(aggregated_data)



# ===============================================================
# PART 10: Visualization of Aggregated Convictions per Area (Step 2)
# ===============================================================

# Summarize total convictions by Area
area_convictions_summary <- aggregated_data %>%
  group_by(Area) %>%
  summarise(total_convictions = sum(total_convictions, na.rm = TRUE)) %>%
  arrange(desc(total_convictions))  # Sort Areas by total convictions

# Plot bar chart
ggplot(area_convictions_summary, aes(x = reorder(Area, -total_convictions), y = total_convictions)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Total Convictions by Area",
    x = "Area",
    y = "Total Convictions"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# ===============================================================
# PART 11: Outlier Detection and Visualization - Column Based
# ===============================================================

# Select only numeric columns for outlier detection
numeric_cols <- combined_data %>%
  select(where(is.numeric))

# Function to detect outliers using IQR method
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  return(x < lower_bound | x > upper_bound)
}

# Create a dataframe to store outlier counts
outlier_summary <- data.frame(Column = character(), Outlier_Count = integer())

# Loop through each numeric column and calculate number of outliers
for (col_name in names(numeric_cols)) {
  outlier_flags <- detect_outliers(numeric_cols[[col_name]])
  outlier_count <- sum(outlier_flags, na.rm = TRUE)
  
  outlier_summary <- rbind(outlier_summary, data.frame(Column = col_name, Outlier_Count = outlier_count))
}

# View outlier counts
print(outlier_summary)

# Visualize outlier counts using a bar chart
ggplot(outlier_summary, aes(x = reorder(Column, -Outlier_Count), y = Outlier_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Outlier_Count), vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(
    title = "Number of Outliers per Numeric Column (IQR Method)",
    x = "Columns",
    y = "Outlier Count"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# ===============================================================
# PART 12: Adding Regional Classification
# ===============================================================

# Define the Area to Region mapping correctly
area_to_region <- tibble::tibble(
  Area = c(
    "Avon and Somerset", "Bedfordshire", "Cambridgeshire", "Cheshire", "Cleveland",
    "Cumbria", "Derbyshire", "Devon and Cornwall", "Dorset", "Durham",
    "Dyfed Powys", "Essex", "Gloucestershire", "GreaterManchester", "Gwent",
    "Hampshire", "Hertfordshire", "Humberside", "Kent", "Lancashire",
    "Leicestershire", "Lincolnshire", "Merseyside", "Metropolitan and City", "Norfolk",
    "Northamptonshire", "Northumbria", "North Wales", "North Yorkshire", "Nottinghamshire",
    "South Wales", "South Yorkshire", "Staffordshire", "Suffolk", "Surrey",
    "Sussex", "Thames Valley", "Warwickshire", "West Mercia", "West Midlands",
    "West Yorkshire", "Wiltshire"
  ),
  Region = c(
    "South West",        # Avon and Somerset
    "East of England",   # Bedfordshire
    "East of England",   # Cambridgeshire
    "North West",        # Cheshire
    "North East",        # Cleveland
    "North West",        # Cumbria
    "East Midlands",     # Derbyshire
    "South West",        # Devon and Cornwall
    "South West",        # Dorset
    "North East",        # Durham
    "Wales",             # Dyfed Powys
    "East of England",   # Essex
    "South West",        # Gloucestershire
    "North West",        # GreaterManchester
    "Wales",             # Gwent
    "South East",        # Hampshire
    "East of England",   # Hertfordshire
    "Yorkshire and The Humber", # Humberside
    "South East",        # Kent
    "North West",        # Lancashire
    "East Midlands",     # Leicestershire
    "East Midlands",     # Lincolnshire
    "North West",        # Merseyside
    "London",            # Metropolitan and City
    "East of England",   # Norfolk
    "East Midlands",     # Northamptonshire
    "North East",        # Northumbria
    "Wales",             # North Wales
    "Yorkshire and The Humber", # North Yorkshire
    "East Midlands",     # Nottinghamshire
    "Wales",             # South Wales
    "Yorkshire and The Humber", # South Yorkshire
    "West Midlands",     # Staffordshire
    "East of England",   # Suffolk
    "South East",        # Surrey
    "South East",        # Sussex
    "South East",        # Thames Valley
    "West Midlands",     # Warwickshire
    "West Midlands",     # West Mercia
    "West Midlands",     # West Midlands
    "Yorkshire and The Humber", # West Yorkshire
    "South West"         # Wiltshire
  )
)

# Merge Region info into combined_data
combined_data <- combined_data %>%
  left_join(area_to_region, by = "Area") %>%
  mutate(
    Region = if_else(Area == "National", "National", Region)  # Set Region to 'National' where Area is 'National'
  )

# Now combined_data has a new column 'Region'


# ===============================================================
# PART 13: Feature Engineering from Date Column
# ===============================================================

# Extract Year, Month, and Quarter from the Date column
combined_data <- combined_data %>%
  mutate(
    Year = lubridate::year(Date),          # Extract Year as numeric
    Month_Number = lubridate::month(Date),  # Extract Month as numeric (1-12)
    Quarter = lubridate::quarter(Date)      # Extract Quarter as numeric (1-4)
  )



# ===============================================================
# PART 14: Check Data Types of Each Column
# ===============================================================

# Use the glimpse function to view column names and their data types
glimpse(combined_data)

# ===============================================================
# PART 15: Descriptive Analysis of Numeric Variables
# ===============================================================

# Select only numeric columns from the combined dataset
numeric_data <- combined_data %>%
  select(where(is.numeric))

# Calculate descriptive statistics: mean, median, standard deviation, minimum, and maximum for each numeric variable
descriptive_stats <- numeric_data %>%
  summarise_all(list(
    mean = ~ mean(., na.rm = TRUE),
    median = ~ median(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE)
  ))

# Display the descriptive statistics
print(descriptive_stats)



# ===============================================================
# PART 16: Plot mean convictions by offence type using summary_stats
# ===============================================================

# Select only conviction-related mean columns
conviction_means <- descriptive_stats %>%
  select(contains("_convictions_mean")) %>%
  pivot_longer(cols = everything(), names_to = "Offence", values_to = "Mean_Convictions")

# Clean offence names for better readability
conviction_means <- conviction_means %>%
  mutate(Offence = str_replace(Offence, "_convictions_mean", ""),
         Offence = str_replace_all(Offence, "_", " "),
         Offence = str_to_title(Offence))

# Plot
ggplot(conviction_means, aes(x = reorder(Offence, -Mean_Convictions), y = Mean_Convictions)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Convictions by Offence Type",
       x = "Offence Type",
       y = "Mean Number of Convictions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ===============================================================
# PART 17: Plot conviction vs unsuccessful rates (%) by offence type
# ===============================================================

# Select and reshape both conviction and unsuccessful percentages
percentage_df <- descriptive_stats %>%
  select(contains("_convictions_pct_mean"), contains("_unsuccessful_pct_mean")) %>%
  pivot_longer(cols = everything(), names_to = "Metric", values_to = "Percentage") %>%
  mutate(Offence = case_when(
    str_detect(Metric, "homicide") ~ "Homicide",
    str_detect(Metric, "offences_person") ~ "Offences Against the Person",
    str_detect(Metric, "sexual_offences") ~ "Sexual Offences",
    str_detect(Metric, "burglary") ~ "Burglary",
    str_detect(Metric, "robbery") ~ "Robbery",
    str_detect(Metric, "theft_handling") ~ "Theft & Handling",
    str_detect(Metric, "fraud_forgery") ~ "Fraud & Forgery",
    str_detect(Metric, "criminal_damage") ~ "Criminal Damage",
    str_detect(Metric, "drugs_offences") ~ "Drugs Offences",
    str_detect(Metric, "public_order") ~ "Public Order",
    str_detect(Metric, "other_offences") ~ "Other Offences",
    str_detect(Metric, "motoring") ~ "Motoring"
  ),
  Type = ifelse(str_detect(Metric, "convictions"), "Conviction %", "Unsuccessful %"))

# Plot
ggplot(percentage_df, aes(x = reorder(Offence, -Percentage), y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Conviction %" = "forestgreen", "Unsuccessful %" = "firebrick")) +
  labs(title = "Conviction vs Unsuccessful Rates by Offence Type",
       x = "Offence Type",
       y = "Percentage (%)",
       fill = "Outcome Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ===============================================================
# PART 18: Time Series Plot – National Convictions vs Unsuccessful Cases
# ===============================================================

# Filter data to include only "National" records
national_data <- aggregated_data %>%
  filter(Area == "National")

# Reshape the data to long format for plotting
national_long <- national_data %>%
  pivot_longer(cols = c(total_convictions, total_unsuccessful),
               names_to = "Case_Type",
               values_to = "Count")

# Plot time series for both conviction and unsuccessful cases
ggplot(national_long, aes(x = Date, y = Count, color = Case_Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.5) +
  labs(
    title = "National Case Trends Over Time",
    x = "Date",
    y = "Number of Cases",
    color = "Case Type"
  ) +
  scale_color_manual(values = c(
    "total_convictions" = "forestgreen",
    "total_unsuccessful" = "firebrick"
  ),
  labels = c("Total Convictions", "Total Unsuccessful")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ===============================================================
# PART 19: Time Series Plot – Regional Convictions and Unsuccessful Cases Over Time
# ===============================================================

# Add total_convictions and total_unsuccessful per row to combined_data
combined_data <- combined_data %>%
  mutate(
    total_convictions = rowSums(select(., ends_with("_convictions")), na.rm = TRUE),
    total_unsuccessful = rowSums(select(., ends_with("_unsuccessful")), na.rm = TRUE)
  )

# Filter out "National" and group by Date + Region
regional_data <- combined_data %>%
  filter(Region != "National") %>%
  group_by(Date, Region) %>%
  summarise(
    total_convictions = sum(total_convictions, na.rm = TRUE),
    total_unsuccessful = sum(total_unsuccessful, na.rm = TRUE),
    .groups = "drop"
  )

# Reshape data to long format
regional_long <- regional_data %>%
  pivot_longer(cols = c(total_convictions, total_unsuccessful),
               names_to = "Case_Type",
               values_to = "Count")

# Plot regional trends using facet wrap
ggplot(regional_long, aes(x = Date, y = Count, color = Case_Type)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ Region, scales = "free_y") +
  scale_color_manual(values = c(
    "total_convictions" = "forestgreen",
    "total_unsuccessful" = "firebrick"
  ),
  labels = c("Total Convictions", "Total Unsuccessful")) +
  labs(
    title = "Regional Case Outcome Trends Over Time (Excluding National)",
    x = "Date",
    y = "Number of Cases",
    color = "Case Type"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

# ===============================================================
# PART 20: Time Series of Conviction Percentages for London by Offence Type
# ===============================================================

# Filter only London region 
london_data <- combined_data %>% 
  filter(Region == "London")

# Select only date and *_convictions_pct columns
conviction_pct_cols <- names(london_data)[str_detect(names(london_data), "_convictions_pct")]
selected_cols <- c("Date", conviction_pct_cols)

london_pct_data <- london_data %>% 
  select(all_of(selected_cols))

# Reshape to long format
london_pct_long <- london_pct_data %>%
  pivot_longer(
    cols = -Date,
    names_to = "Offence_Type",
    values_to = "Conviction_Pct"
  ) %>%
  mutate(
    Offence_Type = str_replace(Offence_Type, "_convictions_pct", ""),
    Offence_Type = str_replace_all(Offence_Type, "_", " "),
    Offence_Type = str_to_title(Offence_Type)
  )

# Plot time series for London
ggplot(london_pct_long, aes(x = Date, y = Conviction_Pct, color = Offence_Type)) +
  geom_line(size = 1) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Conviction Percentages Over Time by Offence Type in London",
    x = "Date",
    y = "Conviction Percentage",
    color = "Offence Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# ===============================================================
# PART 21: Time Series of Unsuccessful Percentages for London by Offence Type
# ===============================================================

# Filter only London region
london_data <- combined_data %>% 
  filter(Region == "London")

# Select Date and *_unsuccessful_pct columns
unsuccessful_pct_cols <- names(london_data)[str_detect(names(london_data), "_unsuccessful_pct")]
selected_cols <- c("Date", unsuccessful_pct_cols)

london_unsuccessful_data <- london_data %>% 
  select(all_of(selected_cols))

# Reshape to long format
london_unsuccessful_long <- london_unsuccessful_data %>%
  pivot_longer(
    cols = -Date,
    names_to = "Offence_Type",
    values_to = "Unsuccessful_Pct"
  ) %>%
  mutate(
    Offence_Type = str_replace(Offence_Type, "_unsuccessful_pct", ""),
    Offence_Type = str_replace_all(Offence_Type, "_", " "),
    Offence_Type = str_to_title(Offence_Type)
  )

# Plot the time series
ggplot(london_unsuccessful_long, aes(x = Date, y = Unsuccessful_Pct, color = Offence_Type)) +
  geom_line(size = 1) +
  scale_color_viridis_d(option = "magma") +
  labs(
    title = "Unsuccessful Outcomes Over Time by Offence Type in London",
    x = "Date",
    y = "Unsuccessful Percentage",
    color = "Offence Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


str(convictions_data)

# ===============================================================
# PART 22: Heatmap of Total Convictions by Area and Date
# ===============================================================

#aggregated_data$Date <- as.Date(aggregated_data$Date)

# exclude "National"
aggregated_data <- aggregated_data %>% 
  filter(Area != "National")

# Plot Heatmap
ggplot(aggregated_data, aes(x = Date, y = Area, fill = total_convictions)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "darkred") +
  labs(
    title = "Total Convictions by Area and Date",
    x = "Date",
    y = "Area",
    fill = "Total Convictions"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ===============================================================
# PART 23: Correlation Matrix of Numeric Variables
# ===============================================================


# Select only numeric columns from the dataset
numeric_data <- convictions_data %>%
  select(where(is.numeric))

# Compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Melt the correlation matrix into long format for ggplot2
melted_cor_matrix <- melt(cor_matrix)

# Plot the correlation heatmap
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Draw heatmap tiles with white borders
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +  # Use a clean theme
  labs(title = "Correlation Heatmap of Numeric Variables",
       x = "Variable", y = "Variable") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# ===============================================================
# PART 24: Distribution Plots
# ===============================================================

# Pie Chart for Conviction Types Distribution
# Remove "National" entries
filtered_data <- convictions_data %>%
  filter(Area != "National")

# Select conviction columns only
conviction_columns <- filtered_data %>%
  select(ends_with("_convictions"))

# Sum totals per conviction type
conviction_sums <- colSums(conviction_columns, na.rm = TRUE)

# Create data frame
conviction_df <- data.frame(
  Type = names(conviction_sums),
  Total = as.numeric(conviction_sums)
)

# Add percentage and updated labels for legend
conviction_df <- conviction_df %>%
  mutate(
    Percentage = Total / sum(Total) * 100,
    Label = paste0(Type, " (", round(Percentage, 1), "%)")
  )

# Plot pie chart with legend labels including percentages
ggplot(conviction_df, aes(x = "", y = Percentage, fill = Label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Percentage Distribution of Conviction Types",
    fill = "Conviction Type"
  ) +
  theme_void() +
  theme(legend.position = "right")

## Pie Chart End

## Distribution of Total Convictions and Unsuccessful Cases 
# Select only the two homicide-related columns
long_data <- aggregated_data %>%
  select(total_convictions, total_unsuccessful) %>%
  pivot_longer(cols = everything(), names_to = "Convictions_Unsuccessful", values_to = "Count")

# Create histograms for both variables
ggplot(long_data, aes(x = Count, fill = Convictions_Unsuccessful)) +
  geom_histogram(binwidth = 5, color = "blue", alpha = 0.7) +
  facet_wrap(~ Convictions_Unsuccessful, scales = "free") + # Separate plots for each category
  labs(
    title = "Distribution of Total Convictions and Unsuccessful Cases",
    x = "Number of Cases",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Remove legend for cleaner visualization

## Distribution of Total Convictions and Unsuccessful Cases End

# Filter out the 'National' area to avoid skewing the distribution
filtered_data <- convictions_data %>%
  filter(Area != "National")

# Create a boxplot of offences_person_convictions by Area
ggplot(filtered_data, aes(x = reorder(Area, offences_person_convictions, median), 
                          y = offences_person_convictions)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red", outlier.size = 1) +
  coord_flip() +  # Flip coordinates to make area labels readable
  labs(
    title = "Distribution of Offences Against the Person Convictions by Area",
    x = "Area",
    y = "Number of Convictions"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))  # Smaller y-axis text for better readability


# Create a boxplot of sexual_offences_convictions by Area
ggplot(filtered_data, aes(x = reorder(Area, sexual_offences_convictions, median), 
                          y = sexual_offences_convictions)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.size = 1) +
  coord_flip() +  # Flip coordinates to make area labels readable
  labs(
    title = "Distribution of Sexual Offences Convictions by Area",
    x = "Area",
    y = "Number of Convictions"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))  # Adjust y-axis text size for better readability

## DENSTIY PLOTS

# Select only conviction columns (ending with '_convictions') and the row index
convictions_only <- filtered_data %>%
  select(ends_with("_convictions"))

# Add row ID for reshaping
convictions_only$row_id <- 1:nrow(convictions_only)

# Convert from wide to long format for ggplot
long_data <- convictions_only %>%
  pivot_longer(
    cols = -row_id,
    names_to = "Conviction_Type",
    values_to = "Value"
  )

# Plot all densities in one plot
ggplot(long_data, aes(x = Value, color = Conviction_Type, fill = Conviction_Type)) +
  geom_density(alpha = 0.2) +
  scale_x_log10() +  # Apply log10 transformation
  labs(
    title = "Density Plot of All Conviction Types (Log-Scaled)",
    x = "Log10(Number of Convictions)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

## Unsuccessful cases density plot

# Select only columns ending with '_unsuccessful' and the row index
unsuccessful_only <- filtered_data %>%
  select(ends_with("_unsuccessful"))

# Add row ID for reshaping
unsuccessful_only$row_id <- 1:nrow(unsuccessful_only)

# Convert from wide to long format for ggplot
long_data <- unsuccessful_only %>%
  pivot_longer(
    cols = -row_id,
    names_to = "Unsuccessful_Type",
    values_to = "Value"
  )

# Plot all densities in one plot
ggplot(long_data, aes(x = Value, color = Unsuccessful_Type, fill = Unsuccessful_Type)) +
  geom_density(alpha = 0.2) +
  scale_x_log10() +  # Apply log10 transformation
  labs(
    title = "Density Plot of All Unsuccessful Case Types (Log-Scaled)",
    x = "Log10(Number of Unsuccessful Cases)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


## Regional Plots

# Find top 10 areas by total convictions
top_areas <- aggregated_data %>%
  group_by(Area) %>%
  summarise(total = sum(total_convictions)) %>%
  arrange(desc(total)) %>%
  slice_head(n = 10) %>%
  pull(Area)

# Filter data for top areas only
top_filtered <- aggregated_data %>%
  filter(Area %in% top_areas)

# Plot for top 5 areas
ggplot(top_filtered, aes(x = Date, y = total_convictions, color = Area)) +
  geom_line(size = 1) +
  labs(
    title = "Convictions Over Time for Top 10 Areas",
    x = "Date",
    y = "Total Convictions"
  ) +
  theme_minimal()

# Select only the *_convictions columns
conviction_totals <- filtered_data %>%
  select(ends_with("_convictions")) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "crime_category", values_to = "total_convictions") %>%
  arrange(desc(total_convictions)) %>%
  slice_head(n = 10)  # Take top 10 categories

# Clean up category names (optional)
conviction_totals$crime_category <- gsub("_convictions", "", conviction_totals$crime_category)
conviction_totals$crime_category <- gsub("_", " ", conviction_totals$crime_category)

# Plot the top 10 conviction categories
ggplot(conviction_totals, aes(x = reorder(crime_category, total_convictions), y = total_convictions)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +  # Flip for better readability
  labs(
    title = "Top 10 Crime Categories by Convictions",
    x = "Crime Category",
    y = "Total Convictions"
  ) +
  theme_minimal()

#filter Metropolitan and City
metro_data <- convictions_data %>%
  filter(Area == "Metropolitan and City")
# Select only the *_convictions columns
conviction_totals <- metro_data %>%
  select(ends_with("_convictions")) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "crime_category", values_to = "total_convictions") %>%
  arrange(desc(total_convictions)) %>%
  slice_head(n = 10)  # Take top 10 categories

# Clean up category names (optional)
conviction_totals$crime_category <- gsub("_convictions", "", conviction_totals$crime_category)
conviction_totals$crime_category <- gsub("_", " ", conviction_totals$crime_category)

# Plot the top 10 conviction categories
ggplot(conviction_totals, aes(x = reorder(crime_category, total_convictions), y = total_convictions)) +
  geom_col(fill = "darkblue") +
  coord_flip() +  # Flip for better readability
  labs(
    title = "Top 10 Crime Categories by Convictions in Metropolitan and City Area",
    x = "Crime Category",
    y = "Total Convictions"
  ) +
  theme_minimal()


# ===============================================================
# PART 25: Hypotesis Testing Part1: Homicide Across Regions
# ===============================================================

filtered_data <- combined_data %>%
  filter(Area != "National")

# Run ANOVA test
anova_result <- aov(homicide_convictions ~ Region, data = filtered_data)

# Summary of ANOVA
summary(anova_result)

# Post-hoc test (ANOVA is significant)
# Tukey's Honest Significant Difference test to see which groups differ
TukeyHSD(anova_result)

# Boxplot to visualize the distribution
ggplot(filtered_data, aes(x = Region, y = homicide_convictions)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Homicide Convictions Across Regions",
       x = "Region ",
       y = "Homicide Convictions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ===============================================================
# PART 26: Hypotesis Testing Part2: Kruskal-Wallis Test for Theft Handling Convictions Across Months
# ===============================================================

# Ensure 'Month' is a factor 
filtered_data$Month_Number <- as.factor(filtered_data$Month_Number)

# ---------------------------
# Summary statistics
# ---------------------------
# View basic descriptive stats for each quarter
summary_stats <- filtered_data %>%
  group_by(Month_Number) %>%
  summarise(
    median_th = median(theft_handling_convictions, na.rm = TRUE),
    IQR = IQR(theft_handling_convictions, na.rm = TRUE),
    count = n()
  )
print(summary_stats)

# ---------------------------
# Kruskal-Wallis Test
# ---------------------------
# Non-parametric test for comparing medians across groups
kruskal_result <- kruskal.test(theft_handling_convictions ~ Month_Number, data = filtered_data)
print(kruskal_result)

# ---------------------------
# Visualization - Boxplot
# ---------------------------
# Create a boxplot to visually inspect differences across months
ggplot(filtered_data, aes(x = Month_Number, y = robbery_convictions)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.color = "red") +
  labs(
    title = "Distribution of Theft Handling Across Months",
    x = "Month",
    y = "Number of Theft Handling Convictions"
  ) +
  theme_minimal()

# ===============================================================
# PART 27: Hypotesis Testing Part3: Drug Offences Across Regions
# ===============================================================

# Run ANOVA test
anova_result <- aov(drugs_offences_convictions ~ Region, data = filtered_data)

# Summary of ANOVA
summary(anova_result)

# Post-hoc test (ANOVA is significant)
# Tukey's Honest Significant Difference test to see which groups differ
TukeyHSD(anova_result)

# Boxplot to visualize the distribution
ggplot(filtered_data, aes(x = Region, y = drugs_offences_convictions)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Drug Offences Convictions Across Regions",
       x = "Region ",
       y = "Drug Offences Convictions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ===============================================================
# PART 28: OLS Linear Regression
# ===============================================================

# Set seed for reproducibility
set.seed(123)

# Split the data: 80% training, 20% testing
split_index <- createDataPartition(filtered_data$drugs_offences_convictions, p = 0.8, list = FALSE)
train_data <- filtered_data[split_index, ]
test_data <- filtered_data[-split_index, ]

# Train OLS model on training data
ols_model <- lm(drugs_offences_convictions ~ 
                  sexual_offences_convictions + 
                  homicide_convictions + 
                  theft_handling_convictions + 
                  robbery_convictions, 
                data = train_data)

# Predict on test data
ols_predictions <- predict(ols_model, newdata = test_data)

# Calculate R-squared
sst <- sum((test_data$drugs_offences_convictions - mean(test_data$drugs_offences_convictions))^2)
sse <- sum((test_data$drugs_offences_convictions - ols_predictions)^2)
r_squared_ols <- 1 - (sse / sst)

# Calculate RMSE
rmse_ols <- RMSE(ols_predictions, test_data$drugs_offences_convictions)

# Print results
cat("OLS Linear Regression (Test Set):\n")
cat("R-squared:", round(r_squared_ols, 4), "\n")
cat("RMSE:", round(rmse_ols, 4), "\n")



# ===============================================================
# PART 29: OLS Linear Regression Visualization
# ===============================================================


# Create predicted values and residuals
predicted_values <- predict(ols_model, newdata = filtered_data)
residuals <- filtered_data$drugs_offences_convictions - predicted_values

# 1. Actual vs Predicted Plot
p1 <- ggplot(filtered_data, aes(x = predicted_values, y = drugs_offences_convictions)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Values",
       x = "Predicted Convictions",
       y = "Actual Convictions") +
  theme_minimal()

# 2. Residual Plot
p2 <- ggplot(filtered_data, aes(x = predicted_values, y = residuals)) +
  geom_point(color = "darkorange", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray20") +
  labs(title = "Residual Plot",
       x = "Predicted Convictions",
       y = "Residuals") +
  theme_minimal()

# 3. QQ Plot for Residuals
p3 <- ggqqplot(residuals, title = "QQ Plot of Residuals")

# Combine all plots
ggarrange(p1, p2, p3, ncol = 2, nrow = 2)



# ===============================================================
# PART 30: Drug Convictions vs Others: Scatter Plot Matrix
# ===============================================================


# Select relevant columns
scatter_data <- filtered_data[, c("drugs_offences_convictions",
                                  "sexual_offences_convictions",
                                  "homicide_convictions",
                                  "theft_handling_convictions",
                                  "robbery_convictions")]

# Create a scatter plot matrix
ggpairs(scatter_data, 
        title = "Pairwise Scatter Plots: Drugs vs Other Convictions",
        upper = list(continuous = wrap("cor", size = 2)), 
        lower = list(continuous = wrap("points", alpha = 0.6, size = 1)), 
        diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme(axis.text = element_text(size = 8), # axis labels 
        strip.text = element_text(size = 7)) # panel headlines







# ===============================================================
# PART 31: Create summary table comparing multiple linear regression models
# ===============================================================

# Define outcome variables
outcomes <- c("homicide_convictions", "robbery_convictions", 
              "theft_handling_convictions", "sexual_offences_convictions")

# Initialize results dataframe
lm_results <- data.frame(
  analysis_type = character(),
  r_squared = numeric(),
  adjusted_r_squared = numeric(),
  estimated_coefficient = numeric(),
  std_error = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through outcome variables and fit models
for (outcome in outcomes) {
  formula <- as.formula(paste(outcome, "~ drugs_offences_convictions"))
  model <- lm(formula, data = filtered_data)
  summary_model <- summary(model)
  
  r_squared <- summary_model$r.squared
  adj_r_squared <- summary_model$adj.r.squared
  coef_estimate <- summary_model$coefficients[2, 1]
  std_error <- summary_model$coefficients[2, 2]
  p_value <- summary_model$coefficients[2, 4]
  
  lm_results <- rbind(lm_results, data.frame(
    analysis_type = paste0(gsub("_convictions", "", outcome), " vs Drug"),
    r_squared = round(r_squared, 4),
    adjusted_r_squared = round(adj_r_squared, 4),
    estimated_coefficient = round(coef_estimate, 4),
    std_error = round(std_error, 4),
    p_value = format.pval(p_value, digits = 3, eps = .001)
  ))
  
}

# View table
print(lm_results)


# ===============================================================
# PART 32: Ridge Regression
# ===============================================================
# Prepare x and y matrices for glmnet
# Prepare data for Ridge
x <- model.matrix(drugs_offences_convictions ~ 
                    sexual_offences_convictions + 
                    homicide_convictions + 
                    theft_handling_convictions + 
                    robbery_convictions, 
                  data = filtered_data)[, -1]  # Remove intercept column

y <- filtered_data$drugs_offences_convictions

x_train <- as.matrix(train_data[, c("sexual_offences_convictions", 
                                    "homicide_convictions", 
                                    "theft_handling_convictions", 
                                    "robbery_convictions")])

y_train <- train_data$drugs_offences_convictions

x_test <- as.matrix(test_data[, c("sexual_offences_convictions", 
                                  "homicide_convictions", 
                                  "theft_handling_convictions", 
                                  "robbery_convictions")])

y_test <- test_data$drugs_offences_convictions

# Train Ridge model (alpha = 0 for Ridge)
ridge_model <- glmnet(x_train, y_train, alpha = 0)

# Cross-validation to find optimal lambda
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)

# Best lambda value
best_lambda <- cv_ridge$lambda.min

# Predict on test set using best lambda
ridge_predictions <- predict(ridge_model, s = best_lambda, newx = x_test)

# Calculate R-squared
sst_ridge <- sum((y_test - mean(y_test))^2)
sse_ridge <- sum((y_test - ridge_predictions)^2)
r_squared_ridge <- 1 - (sse_ridge / sst_ridge)

# Calculate RMSE
rmse_ridge <- RMSE(ridge_predictions, y_test)

# Print Ridge results
cat("Ridge Regression (Test Set):\n")
cat("Best Lambda:", round(best_lambda, 4), "\n")
cat("R-squared:", round(r_squared_ridge, 4), "\n")
cat("RMSE:", round(rmse_ridge, 4), "\n")

# ===============================================================
# PART 33: Ridge Regression Visualization
# ===============================================================


# Create predicted values and residuals for Ridge
predicted_ridge <- predict(ridge_model, newx = x, s = best_lambda)
residuals_ridge <- y - predicted_ridge
residuals_ridge <- as.vector(residuals_ridge)
# 1. Actual vs Predicted Plot
p1_ridge <- ggplot(data = NULL, aes(x = predicted_ridge, y = y)) +
  geom_point(color = "purple", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkred") +
  labs(title = "Ridge: Actual vs Predicted",
       x = "Predicted Convictions",
       y = "Actual Convictions") +
  theme_minimal()

# 2. Residual Plot
p2_ridge <- ggplot(data = NULL, aes(x = predicted_ridge, y = residuals_ridge)) +
  geom_point(color = "darkgreen", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  labs(title = "Ridge: Residual Plot",
       x = "Predicted Convictions",
       y = "Residuals") +
  theme_minimal()

# 3. QQ Plot for Residuals
p3_ridge <- ggqqplot(residuals_ridge, title = "Ridge: QQ Plot of Residuals")

# Combine all plots
ggarrange(p1_ridge, p2_ridge, p3_ridge, ncol = 2, nrow = 2)


# ===============================================================
# PART 34: Ridge Regression Paremeter Coefficients
# ===============================================================

# Ridge regression coeffs.
ridge_coef <- coef(ridge_model, s = best_lambda)
ridge_se <- apply(predict(ridge_model, x_train, s = best_lambda, type = "response") - y_train, 2, sd)

ridge_results <- data.frame(
  variable = rownames(ridge_coef),
  coefficient = as.vector(ridge_coef)
)

ridge_results <- ridge_results[-1, ]

print(ridge_results)

# ===============================================================
# PART 35: Lasso Regression (Model + Evaluation + Visualization)
# ===============================================================

# Train Lasso model with cross-validation (alpha = 1 for Lasso)
cv_lasso <- cv.glmnet(x, y, alpha = 1)

# Get best lambda value
best_lambda_lasso <- cv_lasso$lambda.min

# Train final Lasso model using best lambda
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda_lasso)

# Make predictions
lasso_pred <- predict(lasso_model, s = best_lambda_lasso, newx = x)
lasso_resid <- y - lasso_pred

# Calculate R-squared
sst_lasso <- sum((y - mean(y))^2)
sse_lasso <- sum((y - lasso_pred)^2)
r_squared_lasso <- 1 - (sse_lasso / sst_lasso)

# Calculate RMSE
rmse_lasso <- RMSE(lasso_pred, y)

# Print evaluation metrics
cat("Lasso Regression:\n")
cat("Best Lambda:", round(best_lambda_lasso, 4), "\n")
cat("R-squared:", round(r_squared_lasso, 4), "\n")
cat("RMSE:", round(rmse_lasso, 4), "\n")


lasso_pred <- as.numeric(lasso_pred)
lasso_resid <- y - lasso_pred

# 1. Actual vs Predicted Plot
p1_lasso <- ggplot(data = NULL, aes(x = lasso_pred, y = y)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Lasso: Actual vs Predicted",
       x = "Predicted Convictions",
       y = "Actual Convictions") +
  theme_minimal()

# 2. Residual Plot
p2_lasso <- ggplot(data = NULL, aes(x = lasso_pred, y = lasso_resid)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "gray40", linetype = "dashed") +
  labs(title = "Lasso: Residual Plot",
       x = "Predicted Convictions",
       y = "Residuals") +
  theme_minimal()

# 3. QQ Plot for Residuals
p3_lasso <- ggqqplot(lasso_resid, title = "Lasso: QQ Plot of Residuals")

# Combine plots
ggarrange(p1_lasso, p2_lasso, p3_lasso, ncol = 2, nrow = 2)



# Extract coefficients from Lasso model using best lambda
lasso_coefficients <- coef(lasso_model)

# Convert to data frame for readability
lasso_coef_df <- as.data.frame(as.matrix(lasso_coefficients))
colnames(lasso_coef_df) <- "Coefficient"
lasso_coef_df$Variable <- rownames(lasso_coef_df)
lasso_coef_df <- lasso_coef_df[, c("Variable", "Coefficient")]

# Print coefficients
print(lasso_coef_df)


# ===============================================================
# PART 36: Regression Models Comparison
# ===============================================================

model_comparison <- data.frame(
  Model = c("OLS", "Ridge", "Lasso", "OLS", "Ridge", "Lasso"),
  Metric = c("R-squared", "R-squared", "R-squared", "RMSE", "RMSE", "RMSE"),
  Value = c(r_squared_ols, r_squared_ridge, r_squared_lasso,
            rmse_ols, rmse_ridge, rmse_lasso)
)


p1 <- ggplot(model_comparison[model_comparison$Metric == "R-squared", ],
             aes(x = Model, y = Value, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Model Comparison: R-squared",
       y = "R-squared Value",
       x = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 1) + # R-squared between 0-1 
  geom_text(aes(label = round(Value, 3)), vjust = -0.5)



p2 <- ggplot(model_comparison[model_comparison$Metric == "RMSE", ],
             aes(x = Model, y = Value, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Model Comparison: RMSE",
       y = "RMSE Value",
       x = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5)

ggarrange(p1, p2, ncol = 2, labels = c("A", "B"))


str(filtered_data)


# ===============================================================
# PART 37: K-Means Clustering on Crime Convictions
# ===============================================================

# Selecting only numeric conviction-related columns
conviction_features <- filtered_data %>%
  select(
    homicide_convictions,
    robbery_convictions,
    burglary_convictions,
    sexual_offences_convictions,
    theft_handling_convictions,
    drugs_offences_convictions,
    public_order_convictions,
    other_offences_convictions,
    fraud_forgery_convictions
  )

# Scale the features (standardization)
scaled_data <- scale(conviction_features)

# Determine the optimal number of clusters using Elbow Method
fviz_nbclust(scaled_data, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters (K-Means)")

# Silhouette Method
fviz_nbclust(scaled_data, kmeans, method = "silhouette") +
  labs(title = "Silhouette Score for Optimal Clusters")

# Apply K-Means with chosen number of clusters 
set.seed(42)  # for reproducibility
kmeans_result <- kmeans(scaled_data, centers = 4, nstart = 25)

# Add cluster labels to the original data
filtered_data$cluster_kmeans <- as.factor(kmeans_result$cluster)

# Visualize clusters using PCA
fviz_cluster(kmeans_result, data = scaled_data,
             ellipse.type = "norm",
             geom = "point",
             palette = "jco",
             main = "K-Means Clustering on Crime Convictions")

# Review number of elements per cluster
table(filtered_data$cluster_kmeans)

# ===============================================================
# PART 38: Visualizing Crime Type Distributions Across Clusters
# ===============================================================

# Add cluster labels to a copy of the original conviction features
visual_data <- conviction_features
visual_data$Cluster <- filtered_data$cluster_kmeans

# Convert data to long format for ggplot facetting
long_visual_data <- visual_data %>%
  pivot_longer(cols = -Cluster, names_to = "Crime_Type", values_to = "Convictions")

# Create a boxplot for each crime type, grouped by cluster
ggplot(long_visual_data, aes(x = Cluster, y = Convictions, fill = Cluster)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  facet_wrap(~ Crime_Type, scales = "free_y") +
  labs(
    title = "Distribution of Crime Convictions by Cluster",
    x = "Cluster",
    y = "Number of Convictions"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))




# ===============================================================
# PART 39: Hierarchical Clustering and Dendrogram Visualization
# ===============================================================

# Compute the distance matrix (Euclidean distance)
dist_matrix <- dist(scaled_data, method = "euclidean")

# Perform hierarchical clustering using Ward's method
hc_model <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc_model, labels = FALSE, main = "Dendrogram of Hierarchical Clustering", xlab = "", sub = "")

sil_scores <- numeric()

# Loop through a range of k values and calculate silhouette scores
for (k in 2:10) {
  hc <- hclust(dist_matrix, method = "ward.D2")
  cluster_assignments <- cutree(hc, k = k)
  sil <- silhouette(cluster_assignments, dist_matrix)
  sil_scores[k] <- mean(sil[, 3])  # store average silhouette width
}

# Plot silhouette scores for each k
plot(2:10, sil_scores[2:10], type = "b",
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Score vs Number of Clusters (Hierarchical Clustering)",
     col = "blue", pch = 19)
grid()

rect.hclust(hc_model, k = 4, border = "red")  # add rectangular

# Cut the tree into desired number of clusters
filtered_data$cluster_hierarchical <- cutree(hc_model, k = 4)

# Review number of elements per cluster
table(filtered_data$cluster_hierarchical)


# ===============================================================
# PART 40: PCA Visualization of Hierarchical Clusters
# ===============================================================

# Perform PCA on the same scaled data used in clustering
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Prepare a data frame for plotting with cluster labels
pca_df <- data.frame(
  PC1 = pca_result$x[, 1],                     # First principal component
  PC2 = pca_result$x[, 2],                     # Second principal component
  Cluster = as.factor(filtered_data$cluster_hierarchical)  # Cluster labels from hierarchical clustering
)

# Plot the PCA result colored by cluster labels
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2.5, alpha = 0.8) +
  labs(
    title = "PCA Visualization of Hierarchical Clusters (k = 4)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_brewer(palette = "Dark2")



# ===============================================================
# PART 41: Visualizing Crime Type Distributions Across Hierarchical Clusters
# ===============================================================

# Add hierarchical cluster labels to a copy of conviction features
visual_data_hier <- conviction_features
visual_data_hier$Cluster_Hier <- as.factor(filtered_data$cluster_hierarchical)

# Convert data to long format for facetting in ggplot
long_visual_data_hier <- visual_data_hier %>%
  pivot_longer(cols = -Cluster_Hier, names_to = "Crime_Type", values_to = "Convictions")

# Create a boxplot for each crime type grouped by hierarchical cluster
ggplot(long_visual_data_hier, aes(x = Cluster_Hier, y = Convictions, fill = Cluster_Hier)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  facet_wrap(~ Crime_Type, scales = "free_y") +
  labs(
    title = "Distribution of Crime Convictions by Hierarchical Cluster",
    x = "Hierarchical Cluster",
    y = "Number of Convictions"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))




# ===============================================================
# PART 42: DBSCAN Clustering Implementation
# ===============================================================

# Use kNN distance plot to estimate epsilon
kNNdistplot(scaled_data, k = 4)  
abline(h = 1.75, col = "red", lty = 2)  # elbow point

# Apply DBSCAN
dbscan_result <- dbscan(scaled_data, eps = 1.75, minPts = 4) # setting eps according to previous plot 

# Add cluster labels to the filtered_data
filtered_data$cluster_dbscan <- as.factor(dbscan_result$cluster)

dbscan_clusters <- filtered_data$cluster_dbscan

# Remove noise points
non_noise_indices <- which(dbscan_clusters != 0)
cluster_labels <- dbscan_clusters[non_noise_indices]
cluster_labels <- as.numeric(as.character(cluster_labels))
filtered_scaled_data <- scaled_data[non_noise_indices, ]

# Compute silhouette scores only for non-noise points
silhouette_dbscan <- silhouette(cluster_labels, dist(filtered_scaled_data))

# Print average silhouette width
avg_silhouette <- mean(silhouette_dbscan[, 3])
cat("Average Silhouette Score (DBSCAN, non-noise):",avg_silhouette, "\n")

# Visualize clusters with PCA reduction
fviz_cluster(dbscan_result, data = scaled_data,
             geom = "point", stand = FALSE,
             ellipse = FALSE, 
             main = "DBSCAN Clustering with PCA Projection") +
  theme_minimal()


# Review number of elements per cluster
table(filtered_data$cluster_dbscan)

# ===============================================================
# PART 43: Visualizing Crime Type Distributions Across DBSCAN Clusters
# ===============================================================

# Add DBSCAN cluster labels to a copy of the original conviction features
visual_data_dbscan <- conviction_features
visual_data_dbscan$Cluster_DBSCAN <- as.factor(filtered_data$cluster_dbscan)

# Convert data to long format for ggplot facetting
long_visual_data_dbscan <- visual_data_dbscan %>%
  pivot_longer(cols = -Cluster_DBSCAN, names_to = "Crime_Type", values_to = "Convictions")

# Create a boxplot for each crime type, grouped by DBSCAN cluster
ggplot(long_visual_data_dbscan, aes(x = Cluster_DBSCAN, y = Convictions, fill = Cluster_DBSCAN)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  facet_wrap(~ Crime_Type, scales = "free_y") +
  labs(
    title = "Distribution of Crime Convictions by DBSCAN Cluster",
    x = "DBSCAN Cluster",
    y = "Number of Convictions"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))



str(filtered_data)


# ===============================================================
# PART 44: Classification - Random Forest Model
# ===============================================================

# Data Preparation for Classification
# Calculate median value for homicide_convictions_pct
median_val <- median(filtered_data$homicide_convictions_pct, na.rm = TRUE)


# create binary target column using median value
filtered_data$homicide_rate_class <- ifelse(filtered_data$homicide_convictions_pct > median_val, 1, 0)


# Get names of columns that end with '_convictions_pct' 
pct_columns <- names(filtered_data)[
  grepl("_convictions_pct$", names(filtered_data)) & 
    !grepl("unsuccessful", names(filtered_data))
]

# Exclude columns that start with 'homicide_'
pct_columns <- pct_columns[!grepl("^homicide_", pct_columns)]

# Define other desired columns
other_columns <- c("total_convictions", "total_unsuccessful", "Region", "Month_Number", "Quarter", "Area", "Year", "homicide_rate_class")

# Combine all selected columns
final_columns <- c(pct_columns, other_columns)

# Subset the data
selected_data <- filtered_data %>% select(all_of(final_columns))

selected_data$Month_Number <- as.integer(selected_data$Month_Number)

# Apply label encoding to categorical columns
selected_data$Region <- as.factor(selected_data$Region)
selected_data$Area <- as.factor(selected_data$Area)
selected_data$homicide_rate_class <- as.factor(selected_data$homicide_rate_class)

# Check result
str(selected_data)

set.seed(42)
trainIndex <- createDataPartition(selected_data$homicide_rate_class, p = .8, list = FALSE)
train <- selected_data[trainIndex, ]
test  <- selected_data[-trainIndex, ]


# Train Random Forest model
rf_model <- randomForest(homicide_rate_class ~ ., data = train, importance = TRUE)

# Predict on test data
rf_preds <- predict(rf_model, newdata = test)
# Generate confusion matrix and evaluation metrics for Random Forest
cm_rf <- confusionMatrix(rf_preds, test$homicide_rate_class)
print(cm_rf)

# Extract accuracy
accuracy_rf <- cm_rf$overall["Accuracy"]
cat("Accuracy (RF):", accuracy_rf, "\n")

# Extract per-class precision and recall
precision_rf <- diag(cm_rf$table) / colSums(cm_rf$table)
recall_rf <- diag(cm_rf$table) / rowSums(cm_rf$table)

# Compute macro Precision and Recall
macro_precision_rf <- mean(precision_rf, na.rm = TRUE)
macro_recall_rf <- mean(recall_rf, na.rm = TRUE)

# Compute F1 score per class
f1_per_class_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)

# Compute macro F1
macro_f1_rf <- mean(f1_per_class_rf, na.rm = TRUE)

# Print all Random Forest metrics
cat("Macro Precision (RF):", macro_precision_rf, "\n")
cat("Macro Recall (RF):", macro_recall_rf, "\n")
cat("Macro F1 Score (RF):", macro_f1_rf, "\n")


# Convert to a data frame for plotting
cm_df <- as.data.frame(cm_rf$table)

# Rename the columns for clarity
colnames(cm_df) <- c("Reference", "Prediction", "Freq")

# Plot the confusion matrix using ggplot
ggplot(data = cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black", size = 6) +
  scale_fill_gradient(low = "#dceefb", high = "#08519c") +
  labs(title = "Confusion Matrix - Random Forest",
       x = "Actual Class",
       y = "Predicted Class") +
  theme_minimal()

# Plot feature importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)
ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "#2c7fb8") +
  coord_flip() +
  labs(title = "Feature Importance (Random Forest)",
       x = "Features", y = "Mean Decrease in Gini") +
  theme_minimal()
install.packages("randomForest")



# ===============================================================
# PART 45: Classification - Logistic Regression
# ===============================================================

# Logistic Regression modeli (glm)
log_model <- glm(homicide_rate_class ~ ., 
                 data = train, 
                 family = binomial)

# get probabilities
log_probs <- predict(log_model, newdata = test, type = "response")

# define threshold
log_preds <- ifelse(log_probs > 0.5, 1, 0)

# Confusion Matrix
cm_log <- confusionMatrix(as.factor(log_preds), test$homicide_rate_class)
print(cm_log)

# Accuracy
accuracy_log <- cm_log$overall["Accuracy"]
cat("Accuracy (Logistic Regression):", accuracy_log, "\n")

# Precision & Recall
precision_log <- diag(cm_log$table) / colSums(cm_log$table)
recall_log <- diag(cm_log$table) / rowSums(cm_log$table)

# Macro Precision & Recall
macro_precision_log <- mean(precision_log, na.rm = TRUE)
macro_recall_log <- mean(recall_log, na.rm = TRUE)

# F1 Score per class
f1_per_class_log <- 2 * (precision_log * recall_log) / (precision_log + recall_log)

# Macro F1
macro_f1_log <- mean(f1_per_class_log, na.rm = TRUE)

# Print all metrics
cat("Macro Precision (Logistic Regression):", macro_precision_log, "\n")
cat("Macro Recall (Logistic Regression):", macro_recall_log, "\n")
cat("Macro F1 Score (Logistic Regression):", macro_f1_log, "\n")

# Confusion Matrix plot
cm_df_log <- as.data.frame(cm_log$table)
colnames(cm_df_log) <- c("Reference", "Prediction", "Freq")

ggplot(data = cm_df_log, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black", size = 6) +
  scale_fill_gradient(low = "#edf8fb", high = "#006d2c") +
  labs(title = "Confusion Matrix - Logistic Regression",
       x = "Actual Class",
       y = "Predicted Class") +
  theme_minimal()


# Histogram
ggplot(data.frame(Prob = log_probs), aes(x = Prob)) +
  geom_histogram(binwidth = 0.05, fill = "#2c7fb8", color = "white") +
  labs(title = "Predicted Probabilities Histogram",
       x = "Predicted Probability",
       y = "Count") +
  theme_minimal()

# Coefficient değerlerini dataframe yap
coefs <- tidy(log_model)

top_coefs <- coefs %>% 
  arrange(desc(abs(estimate))) %>%
  head(20)

ggplot(top_coefs, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "#08519c") +
  coord_flip() +
  labs(title = "Top 20 Logistic Regression Coefficients",
       x = "Features",
       y = "Coefficient Estimate") +
  theme_minimal()


# ===============================================================
# PART 46: Classification - XGBoost
# ===============================================================

# turn data to numeric format
train_matrix <- model.matrix(homicide_rate_class ~ . -1, data = train)
test_matrix  <- model.matrix(homicide_rate_class ~ . -1, data = test)

# 
train_label <- as.numeric(as.character(train$homicide_rate_class))
test_label  <- as.numeric(as.character(test$homicide_rate_class))

# creating DMatrix objects
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest  <- xgb.DMatrix(data = test_matrix, label = test_label)

# model parameters
params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 4,
  eta = 0.1
)

# train model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain),
  verbose = 0
)

# make predictions
xgb_probs <- predict(xgb_model, newdata = dtest)
xgb_preds <- ifelse(xgb_probs > 0.5, 1, 0)
xgb_preds_factor <- as.factor(xgb_preds)

# Confusion Matrix
cm_xgb <- confusionMatrix(xgb_preds_factor, as.factor(test_label))
print(cm_xgb)

# Accuracy
accuracy_xgb <- cm_xgb$overall["Accuracy"]
cat("Accuracy (XGBoost):", accuracy_xgb, "\n")

# calculate Precision, Recall, F1
precision_xgb <- diag(cm_xgb$table) / colSums(cm_xgb$table)
recall_xgb <- diag(cm_xgb$table) / rowSums(cm_xgb$table)
macro_precision_xgb <- mean(precision_xgb, na.rm = TRUE)
macro_recall_xgb <- mean(recall_xgb, na.rm = TRUE)
f1_per_class_xgb <- 2 * (precision_xgb * recall_xgb) / (precision_xgb + recall_xgb)
macro_f1_xgb <- mean(f1_per_class_xgb, na.rm = TRUE)

cat("Macro Precision (XGBoost):", macro_precision_xgb, "\n")
cat("Macro Recall (XGBoost):", macro_recall_xgb, "\n")
cat("Macro F1 Score (XGBoost):", macro_f1_xgb, "\n")


# Confusion Matrix plot
cm_df_xgb <- as.data.frame(cm_xgb$table)
colnames(cm_df_xgb) <- c("Reference", "Prediction", "Freq")

ggplot(data = cm_df_xgb, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black", size = 6) +
  scale_fill_gradient(low = "#edf8fb", high = "darkblue") +
  labs(title = "Confusion Matrix - XGBoost",
       x = "Actual Class",
       y = "Predicted Class") +
  theme_minimal()


# Feature Importance
importance_matrix <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix, top_n = 20, rel_to_first = TRUE, xlab = "Relative Importance")


# ===============================================================
# PART 47: Classification - Model Comparison
# ===============================================================

# creating a table includes all results
model_metrics <- data.frame(
  Model = c("Random Forest", "Logistic Regression", "XGBoost"),
  Accuracy = c(accuracy_rf, accuracy_log, accuracy_xgb),
  Macro_Precision = c(macro_precision_rf, macro_precision_log, macro_precision_xgb),
  Macro_Recall = c(macro_recall_rf, macro_recall_log, macro_recall_xgb),
  Macro_F1 = c(macro_f1_rf, macro_f1_log, macro_f1_xgb)
)

print(model_metrics)

# datafreme preperation for radar chart
radar_data <- rbind(
  rep(1, 4),   # Max value
  rep(0, 4),   # Min value
  model_metrics[1, 2:5],
  model_metrics[2, 2:5],
  model_metrics[3, 2:5]
)

# adding row names
rownames(radar_data) <- c("Max", "Min", "Random Forest", "Logistic Regression", "XGBoost")
print(radar_data)


# plotting radar chart
radarchart(radar_data,
           axistype = 1,
           pcol = c("#3182bd", "#e6550d", "#31a354"),
           pfcol = c("#3182bd80", "#e6550d80", "#31a35480"),
           plwd = 2,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey30",
           caxislabels = seq(0, 1, 0.2),
           cglwd = 0.8,
           vlcex = 0.9)

# adding legends
legend(x = "topright",
       legend = rownames(radar_data[-c(1, 2), ]),
       bty = "n",
       pch = 20,
       col = c("#3182bd", "#e6550d", "#31a354"),
       text.col = "black",
       cex = 0.9,
       pt.cex = 2)



# ===============================================================
# END 
# ===============================================================


