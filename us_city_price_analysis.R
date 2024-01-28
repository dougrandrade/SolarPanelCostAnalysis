setwd("C:/Users/dougr/Data_Science_Projects/R/SolarPanelCostAnalysis")

# Import modules
library(ggplot2) # plotting
library(tidyr) # data cleaning/prep (pivot_longer())
library(dplyr) # data manipluation (mutate())
library(zoo) # data manipulation (as.yearmon())
library(mice) # missing data imputation
library(forecast) # missing data imputation (na.interp())
library(xts) # for transforming monthly time series to quarterly


#############################################################################
#### 1. Visual analysis of avg U.S. city electricity monthly prices 1979-2023
# - load data set from U.S. Bureau of Labor Statistics
#   https://www.bls.gov/regions/midwest/data/averageenergyprices_selectedareas_table.htm
# - impute missing values
# - time series ggplot(s)
################################################################################

# Read File in from the working directory
# 
# 
us_pwr_df <- read.csv("Price_KWH_US_cities.csv")
summary(us_pwr_df)

########################################
month_df_to_yr_ts <- function(df, value, title = 'Initial Time Series Plot') {
  # Reshape Month columns under single Month column
  data_long <- pivot_longer(df, cols = -Year, names_to = "Month", values_to = value)
  
  # Make the months a factor class
  data_long$Month <- factor(data_long$Month, levels = month.abb)
  
  # Convert the 'Year' column to a Date format
  data_long$Year <- as.Date(data_long$Year, format = "%Y")
  data_long$Year <- as.Date(paste(data_long$Year, "-01-01", sep = ""), format = "%Y-%m-%d")

  # Create a time series object
  ts_data <- ts(data_long[, -c(1, 2)], start = c(min(df$Year), 1), end = c(max(df$Year), 12), frequency = 12)
  
  # Check for NA values in the time series
  if (any(is.na(ts_data))) {
    # Perform imputation if needed (e.g., using na.interp)
    ts_data_imputed <- na.interp(ts_data)
  } else {
    ts_data_imputed <- ts_data
  }
  
  # Plot the time series data for visual analysis
  auto_ts_plot <- autoplot(ts_data_imputed, value) +
    ggtitle(paste('Time Series of ', title)) +
    labs(x = "Year",
         y = value,
         color = 'Color Legend') +
    theme(panel.background = element_rect(fill = "lightgray"))
  
  # Return both the imputed time series data and the plot
  return(list(ts_data = ts_data, data_long = data_long, ts_data_imputed = ts_data_imputed, auto_ts_plot = auto_ts_plot))
}

elec_output <- month_df_to_yr_ts(df = us_pwr_df, value = 'Price KWH', title = 'U.S. Electric Prices')
ts_elec_imp <- elec_output$ts_data_imputed
elec_output$auto_ts_plot

################################################################################
#### 2. Compare the lowest avg month and highest avg month
# - time series ggplot of price different
################################################################################

# Convert the time series to a data frame for plotting
elec_plot_data <- data.frame(
  date = as.Date(time(ts_elec_imp)),
  value = as.vector(ts_elec_imp),
  month = factor(month.abb[cycle(ts_elec_imp)], levels = month.abb)
)
summary(elec_plot_data)

table(is.na(elec_plot_data$month))

# Calculate the average price for each month
avg_prices <- aggregate(value ~ month, data = elec_plot_data, FUN = mean)

# Identify the months with max and min average prices
max_month <- avg_prices$month[which.max(avg_prices$value)]
min_month <- avg_prices$month[which.min(avg_prices$value)]

# Create a new variable for color based on conditions
elec_plot_data$color <- case_when(
  elec_plot_data$month == max_month ~ "red",
  elec_plot_data$month == min_month ~ "blue",
  TRUE ~ "gray"
)


# Create a multi-colored line plot with legend
breaks.vec <- seq(floor_date(min(elec_plot_data$date), unit = "5 years"), max(ceiling_date(elec_plot_data$date, unit = "5 years")), by = "5 years")

ggplot(elec_plot_data, aes(x = date, y = value, color = color)) +
  geom_line(size = 0.703) +
  scale_x_date(limits = as.Date(c('1975-01-01','2025-01-01')),
               breaks = breaks.vec,
               date_labels = '%Y') +
  scale_color_manual(values = c("darkblue", "darkgray", "darkred"), 
                     labels = c(paste('min avg month: ', as.character(min_month)),
                                "Other Months",
                                paste('max avg month: ', as.character(max_month)))) +
  scale_linetype_manual(values = rep("solid", length(unique(elec_plot_data$month))),
                        labels = levels(elec_plot_data$month)) +
  labs(title = "Average Monthly Electricity Prices (1979-2023)",
       x = "Year",
       y = "Average Price (KWH)",
       color = 'Color Legend') +
  theme(panel.background = element_rect(fill = "lightgray"))

################################################################################
################################################################################
#### 3. Visual analysis of U.S. annual GDP, CPI, population 1979-2023
# - impute missing values
# - time series ggplot(s)
# - analysis of whether national electric price trends trail, match, or exceed 
#   national economic growth indicators
################################################################################
################################################################################

# CPI - Consumer Price Index is the measure of the avg change over time in the prices 
# paid by urban consumers for a market basket of goods and services.

# Read files in from the working directory
################################################################################
# All items in U.S. city average, all urban consumers, not seasonally adjusted
# https://data.bls.gov/cgi-bin/surveymost?cu
us_cpi_df <- read.csv("us_cpi.csv")
summary(us_cpi_df)

# Drop "HALF1' and 'HALF2' columns
us_cpi_df <- us_cpi_df[, !(names(us_cpi_df) %in% c('HALF1', 'HALF2'))]

# Prep/transform data to CPI TS
us_cpi_output <- month_df_to_yr_ts(df = us_cpi_df, 
                                 value = 'CPI', 
                                 title = 'U.S. Consumer Price Index')
ts_us_cpi <- us_cpi_output$ts_data_imputed
us_cpi_output$auto_ts_plot

################################################################################
# Food and beverages in U.S. city average, all urban consumers, not seasonally adjusted
us_cpi_food_df <- read.csv("us_cpi_food.csv")
summary(us_cpi_food_df)

# Drop "HALF1' and 'HALF2' columns
us_cpi_food_df <- us_cpi_food_df[, !(names(us_cpi_food_df) %in% c('HALF1', 'HALF2'))]

# Prep/transform data to CPI TS
us_fd_cpi_output <- month_df_to_yr_ts(df = us_cpi_food_df, 
                                   value = 'CPI', 
                                   title = 'U.S. Food Consumer Price Index')
ts_us_fd_cpi <- us_fd_cpi_output$ts_data_imputed
us_fd_cpi_output$auto_ts_plot

################################################################################
# Housing in U.S. city average, all urban consumers, not seasonally adjusted
us_cpi_housing_df <- read.csv("us_cpi_housing.csv")
summary(us_cpi_housing_df)

# Prep/transform data to CPI TS
us_h_cpi_output <- month_df_to_yr_ts(df = us_cpi_housing_df, 
                                      value = 'CPI', 
                                      title = 'U.S. Housing Consumer Price Index')
ts_us_h_cpi <- us_h_cpi_output$ts_data_imputed
us_h_cpi_output$auto_ts_plot

################################################################################
# Combine CPI data
cpi_comb <- cbind('Electric_CPI' = ts_elec_imp, 
                  'National_CPI' = ts_us_cpi, 
                  'Food_CPI' = ts_us_fd_cpi, 
                  'Housing_CPI' = ts_us_h_cpi)

# Convert 'cpi_comb' to an xts object
cpi_xts <- as.xts(cpi_comb)

# Create a quarterly aggregate using apply.quarterly function from zoo
cpi_quarterly <- apply.quarterly(cpi_xts, FUN = mean)

# Convert back to a data frame
cpi_quarterly_df <- as.data.frame(cpi_quarterly)

# Add a new column 'Quarter' to store the corresponding quarter
cpi_quarterly_df$Quarter <- as.yearqtr(index(cpi_quarterly))

# Print or further process cpi_quarterly_df
summary(cpi_quarterly_df)
str(cpi_quarterly_df)

# Plot the time series data
matplot(index(cpi_quarterly), coredata(cpi_quarterly), type = "l", lty = 1, col = 1:4, xlab = "Quarter", ylab = "CPI", main = "Quarterly CPI Trends")

# Add legend
legend("topright", legend = colnames(cpi_quarterly), col = 1:4, lty = 1)

# Add a title
title(main = "Quarterly CPI Trends", sub = "1979-2023")


################################################################################
# GDP - Gross Domestic Product is the value of the final goods and services 
# produced in the U.S. Real (or chained) GDP is adjusted for inflation to 
# compare periods GDP is seasonally adjusted to reflect true patterns in 
# economic activity (not weather, holidays, routine schedules).

# Real Gross Domestic Product, Quantity Indexes, 
# [Billions of chained (2017) dollars] Seasonally adjusted at annual rates
# https://www.bea.gov/itable/national-gdp-and-personal-income
# U.S. Bureau of Economic Analysis, "Table 1.1.6. Real Gross Domestic Product, Chained Dollars" (accessed Saturday, January 27, 2024).

# Set the path to your CSV file
file_path <- "real_gdp_us.csv"

# Read the CSV file with proper parameters
data <- read.csv(file_path, skip = 5, header = FALSE, comment.char = "#")

# Identify the correct row index for "Gross domestic product" in data
gdp_row_index <- grep("Gross domestic product", data$V2)

# Filter the data to include only "Gross domestic product" indicator
gdp_data <- t(data[gdp_row_index, c(2, 3:182)])
gdp_data <- (as.matrix(gdp_data[-c(1),]))

gdp_data <- as.numeric(gdp_data[, 1])

# Assuming your data starts in 1979 Quarter 1
start_date <- as.Date("1979-01-01")
end_date <- as.Date("2023-12-31")

# Create a sequence of dates every 3 months
quarters <- seq(from = start_date, to = end_date, by = "3 months")

# Create a data frame combining the dates and GDP data
gdp_df <- data.frame(Date = quarters, GDP = as.numeric(gdp_data))

# Assuming your data frame is already sorted by date
gdp_ts <- ts(gdp_df$GDP, start = c(1979, 1), frequency = 4)

# Convert time series object to a data frame
#gdp_df_ts <- data.frame(Date = time(gdp_ts), GDP = as.numeric(gdp_ts))

# Plot the time series data for visual analysis
autoplot(gdp_ts) +
  ggtitle('Time Series of US GDP 1979-2023') +
  labs(x = 'Year',
       y = 'GDP ($B)',
       color = 'Color Legend') +
  theme(panel.background = element_rect(fill = 'lightgray'))

cpi_gdp_df <- cbind(cpi_quarterly_df, 'US_GDP' = gdp_ts)

ts_cpi_gdp_df <- ts(cpi_gdp_df, start = c(1979, 1), frequency = 4)

# Standardize only the Electric CPI column
ts_cpi_gdp_df_standardized <- ts_cpi_gdp_df
ts_cpi_gdp_df_standardized <- scale(ts_cpi_gdp_df)

autoplot(ts_cpi_gdp_df_standardized)

################################################################################
#### 4. Visual timeline analysis of major U.S. energy initiatives
# - line or bar ggplot of number of activities over 1979-2023 (if able)
################################################################################


























################################################################################
#### 5. Compare Virginia electricity prices and economic factors to national 
####    statistics (of common-recorded years)
# - impute Virginia economic growth indicators
# - time series ggplot(s)
# - analysis of whether Virginia electric price trends trail, match, or exceed 
#   Virginia economic growth indicators
# - review analysis with time of Virginia's major energy initiatives
################################################################################

# Real gross domestic product of Virginia  from 2000 to 2022 
# [in billion U.S. dollars]
# https://www.statista.com/statistics/188142/gdp-of-the-us-federal-state-of-virginia-since-1997/#:~:text=In%202022%2C%20the%20real%20gross,at%20505.35%20billion%20U.S.%20dollars.
# Statista, "Real gross domestic product of Virginia in the United States from 2000 to 2022" (accessed Saturday, January 27, 2024).
us_gpa_va_df <- read.csv("real_gdp_va.csv")
summary(us_gpa_va_df)

























################################################################################
#### 6. Forecast next 10, 20, 30, 40, 50 years based on monthly and annual 
####    seasonality and adjusted of periodicity of U.S. energy initiative events.
################################################################################