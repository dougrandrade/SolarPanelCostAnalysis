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
# - load data
# - impute missing values
# - time series ggplot(s)
################################################################################
# Electricity per KWH in U.S. cities, average price, not seasonally adjusted
# Electricity price per KWH
# https://data.bls.gov/timeseries/APU000072610?amp%253bdata_tool=XGtable&output_view=data&include_graphs=true
# U.S. Bureau of Labor Statistics, "Consumer Price Index Average Price Data, Series Id:	APU000072610" (January 28, 2024 (2:42:56 PM)).

# Read File in from the working directory
e_price_us_tbl <- read.csv("kwh_price_us.csv")
summary(e_price_us_tbl)

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
  ts_data <- ts(data_long[, -c(1, 2)], 
                start = c(min(df$Year), 1), 
                end = c(max(df$Year), 12), 
                frequency = 12)
  
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
  return(list(ts_data = ts_data, 
              data_long = data_long, 
              ts_data_imputed = ts_data_imputed, 
              auto_ts_plot = auto_ts_plot))
}

e_pr_us_output <- month_df_to_yr_ts(df = e_price_us_tbl, 
                                    value = 'Price kWh', 
                                    title = 'U.S. Electric Prices')
ts_e_pr_us_imp <- e_pr_us_output$ts_data_imputed
e_pr_us_output$auto_ts_plot

################################################################################
#### 2. Compare the lowest avg month and highest avg month
# - time series ggplot of price different
################################################################################

# Convert the time series to a data frame for plotting
e_us_plot_df <- data.frame(
  date = as.Date(time(ts_e_pr_us_imp)),
  value = as.vector(ts_e_pr_us_imp),
  month = factor(month.abb[cycle(ts_e_pr_us_imp)], levels = month.abb)
)
summary(e_us_plot_df)

table(is.na(e_us_plot_df$month))

# Calculate the average price for each month
avg_e_us <- aggregate(value ~ month, data = e_us_plot_df, FUN = mean)

# Identify the months with max and min average prices
e_max_month <- avg_e_us$month[which.max(avg_e_us$value)]
e_min_month <- avg_e_us$month[which.min(avg_e_us$value)]

# Create a new variable for color based on conditions
e_us_plot_df$color <- case_when(
  e_us_plot_df$month == e_max_month ~ "red",
  e_us_plot_df$month == e_min_month ~ "blue",
  TRUE ~ "gray"
)


# Create a multi-colored line plot with legend
breaks.vec <- seq(floor_date(min(e_us_plot_df$date), unit = "5 years"), 
                  max(ceiling_date(e_us_plot_df$date, unit = "5 years")), 
                  by = "5 years")

ggplot(e_us_plot_df, aes(x = date, y = value, color = color)) +
  geom_line(size = 0.703) +
  scale_x_date(limits = as.Date(c('1975-01-01','2025-01-01')),
               breaks = breaks.vec,
               date_labels = '%Y') +
  scale_color_manual(values = c("darkblue", "darkgray", "darkred"), 
                     labels = c(paste('min avg month: ', as.character(e_min_month)),
                                "Other Months",
                                paste('max avg month: ', as.character(e_max_month)))) +
  scale_linetype_manual(values = rep("solid", length(unique(e_us_plot_df$month))),
                        labels = levels(e_us_plot_df$month)) +
  labs(title = "Average Monthly Electricity Prices (1979-2023)",
       x = "Year",
       y = "Average Price ($ kWh)",
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
# https://data.bls.gov/timeseries/APU000072610?amp%253bdata_tool=XGtable&output_view=data&include_graphs=true
# U.S. Bureau of Labor Statistics, "Consumer Price Index for All Urban Consumers (CPI-U), Series Id: CUUR0000SA0" (January 28, 2024 (2:42:56 PM)).
cpi_us_tbl <- read.csv("cpi_us.csv")
summary(cpi_us_tbl)

# Drop "HALF1' and 'HALF2' columns
cpi_us_df <- cpi_us_tbl[, !(names(cpi_us_tbl) %in% c('HALF1', 'HALF2'))]

# Prep/transform data to CPI TS
cpi_us_output <- month_df_to_yr_ts(df = cpi_us_tbl, 
                                 value = 'CPI', 
                                 title = 'U.S. Consumer Price Index')
ts_cpi_us <- cpi_us_output$ts_data_imputed
cpi_us_output$auto_ts_plot

################################################################################
# Food and beverages in U.S. city average, all urban consumers, not seasonally adjusted
# https://data.bls.gov/timeseries/APU000072610?amp%253bdata_tool=XGtable&output_view=data&include_graphs=true
# U.S. Bureau of Labor Statistics, "Consumer Price Index for All Urban Consumers (CPI-U), Series Id: CUUR0000SAF" (January 28, 2024 (2:42:56 PM)).
cpi_food_us_tbl <- read.csv("cpi_food_us.csv")
summary(cpi_food_us_tbl)

# Drop "HALF1' and 'HALF2' columns
cpi_food_us_tbl <- cpi_food_us_tbl[, !(names(cpi_food_us_tbl) %in% c('HALF1', 'HALF2'))]

# Prep/transform data to CPI TS
cpi_fd_us_output <- month_df_to_yr_ts(df = cpi_food_us_tbl, 
                                   value = 'CPI', 
                                   title = 'U.S. Food Consumer Price Index')
ts_cpi_fd_us <- cpi_fd_us_output$ts_data_imputed
cpi_fd_us_output$auto_ts_plot

################################################################################
# Housing in U.S. city average, all urban consumers, not seasonally adjusted (base period 1982-84=100)
# https://data.bls.gov/timeseries/APU000072610?amp%253bdata_tool=XGtable&output_view=data&include_graphs=true
# U.S. Bureau of Labor Statistics, "Consumer Price Index for All Urban Consumers (CPI-U), Series Id: CUUR0000SAH" (January 28, 2024 (2:42:56 PM)).
cpi_house_us_tbl <- read.csv("cpi_housing_us.csv")
summary(cpi_house_us_tbl)

# Prep/transform data to CPI TS
cpi_h_us_output <- month_df_to_yr_ts(df = cpi_house_us_tbl, 
                                      value = 'CPI', 
                                      title = 'U.S. Housing Consumer Price Index')
ts_cpi_h_us <- cpi_h_us_output$ts_data_imputed
cpi_h_us_output$auto_ts_plot

################################################################################
# Combine CPI data
cpi_us_cmb <- cbind('Electric_Price_(KwH)_US' = ts_e_pr_us_imp, 
                  'CPI_US' = ts_cpi_us, 
                  'CPI_Food_US' = ts_cpi_fd_us, 
                  'CPI_Housing_US' = ts_cpi_h_us)

# Convert 'cpi_comb' to an xts object
cpi_cmb_xts <- as.xts(cpi_us_cmb)

# Create a quarterly aggregate using apply.quarterly function from zoo
cpi_cmb_qtr <- apply.quarterly(cpi_cmb_xts, FUN = mean)

# Convert back to a data frame
cpi_cmb_qtr_df <- as.data.frame(cpi_cmb_qtr)

# Add a new column 'Quarter' to store the corresponding quarter
cpi_cmb_qtr_df$Quarter <- as.yearqtr(index(cpi_cmb_qtr))

# Print or further process cpi_quarterly_df
summary(cpi_cmb_qtr_df)
str(cpi_cmb_qtr_df)

# Plot the time series data
matplot(index(cpi_cmb_qtr_df), 
        coredata(cpi_cmb_qtr_df), 
        type = "l", lty = 1, col = 1:4, 
        xlab = "Quarter", ylab = "CPI", main = "Quarterly CPI Trends")

# Add legend
legend("topleft", legend = colnames(cpi_cmb_qtr_df), col = 1:4, lty = 4)

# Add a title
title(main = "Non-Standardized Quarterly CPI Trends Plot", sub = "1979-2023")

################################################################################
# GDP - Gross Domestic Product is the value of the final goods and services 
# produced in the U.S. Real (or chained) GDP is adjusted for inflation to 
# compare periods GDP is seasonally adjusted to reflect true patterns in 
# economic activity (not weather, holidays, routine schedules).

# Real Gross Domestic Product, Quantity Indexes, 
# [Billions of chained (2017) dollars] Seasonally adjusted at annual rates
# https://www.bea.gov/itable/national-gdp-and-personal-income
# U.S. Bureau of Economic Analysis, "Table 1.1.6. Real Gross Domestic Product, Chained Dollars" (accessed Saturday, January 27, 2024).

# Read the CSV file with proper parameters
gdp_us_og_tbl <- read.csv('gdp_real_us.csv', skip = 5, header = FALSE, comment.char = "#")

# Identify the correct row index for "Gross domestic product" in data
gdp_row_index <- grep("Gross domestic product", gdp_us_og_tbl$V2)

# Transpose and format the data to numeric "Gross domestic product"
gdp_us_tbl <- t(gdp_us_og_tbl[gdp_row_index, c(2, 3:182)])
gdp_us_tbl <- (as.matrix(gdp_us_tbl[-c(1),]))
gdp_us_tbl <- as.numeric(gdp_us_tbl[, 1])

# Create a sequence of dates every 3 months
qtr <- seq(from = as.Date("1979-01-01"), to = as.Date("2023-12-31"), by = "3 months")

# Create a data frame combining the dates and GDP data
gdp_us_df <- data.frame(Date = qtr, GDP = as.numeric(gdp_us_tbl))

# Create time-series verion
ts_gdp_us <- ts(gdp_us_df$GDP, start = c(1979, 1), frequency = 4)

# Convert time series object to a data frame
#gdp_df_ts <- data.frame(Date = time(gdp_ts), GDP = as.numeric(gdp_ts))

# Plot the time series data for visual analysis
autoplot(ts_gdp_us) +
  ggtitle('Time Series of US GDP 1979-2023') +
  labs(x = 'Year',
       y = 'GDP ($B)',
       color = 'Color Legend') +
  theme(panel.background = element_rect(fill = 'lightgray'))

cpi_gdp_us_df <- cbind(cpi_cmb_qtr_df, 
                       'GDP_US' = ts_gdp_us)

# Convert the data frame to a time-series
ts_cpi_gdp_us <- ts(cpi_gdp_us_df, 
                    start = c(1979, 1), 
                    frequency = 4)

# Standardize the time-series data
ts_cpi_gdp_std <- ts_cpi_gdp_us
ts_cpi_gdp_std <- scale(ts_cpi_gdp_std)
summary(ts_cpi_gdp_std)

autoplot(ts_cpi_gdp_std) +
  ggtitle('Standardized National CPI (all goods, food, housing) and GDP') +
  labs(x = 'Year',
       y = 'Standardized Data',
       color = 'Color Legend') +
  theme(panel.background = element_rect(fill = 'lightgray'))

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

# Electricity per KWH in Washington-Arlington-Alexandria, DC-VA-MD-WV, average price, not seasonally adjusted
# Electricity price per KWH
# https://data.bls.gov/timeseries/APUS35A72610?amp%253bdata_tool=XGtable&output_view=data&include_graphs=true
# U.S. Bureau of Labor Statistics, "Consumer Price Index Average Price Data" (January 28, 2024 (2:40:32 PM)).
e_price_va_tbl <- read.csv("kwh_price_va.csv")
summary(e_price_va_tbl)
str(e_price_va_tbl)

e_price_va_tbl <- subset(e_price_va_tbl, Year > 2009)

e_pr_va_output <- month_df_to_yr_ts(df = e_price_va_tbl, 
                                    value = 'Price (kwH)', 
                                    title = 'Virginia Electric Prices')

ts_e_pr_va_imp <- e_pr_va_output$ts_data_imputed
e_pr_va_output$auto_ts_plot

################################################################################

# Convert the time series to a data frame for plotting
e_va_plot_df <- data.frame(
  date = as.Date(time(ts_e_pr_va_imp)),
  value = as.vector(ts_e_pr_va_imp),
  month = factor(month.abb[cycle(ts_e_pr_va_imp)], levels = month.abb)
)
summary(e_va_plot_df)

table(is.na(e_va_plot_df$month))

# Calculate the average price for each month
avg_e_va <- aggregate(value ~ month, data = e_va_plot_df, FUN = mean)

# Identify the months with max and min average prices
e_max_month <- avg_e_va$month[which.max(avg_e_va$value)]
e_min_month <- avg_e_va$month[which.min(avg_e_va$value)]

# Create a new variable for color based on conditions
e_va_plot_df$color <- case_when(
  e_va_plot_df$month == e_max_month ~ "red",
  e_va_plot_df$month == e_min_month ~ "blue",
  TRUE ~ "gray"
)


# Create a multicolored line plot with legend
breaks.vec <- seq(floor_date(min(e_va_plot_df$date), unit = "5 years"), 
                  max(ceiling_date(e_va_plot_df$date, unit = "5 years")), 
                  by = "5 years")

ggplot(e_va_plot_df, aes(x = date, y = value, color = color)) +
  geom_line(size = 0.703) +
  scale_x_date(limits = as.Date(c(floor_date(min(e_va_plot_df$date), unit = "5 years"),
                                  max(ceiling_date(e_va_plot_df$date, unit = "5 years")))),
               breaks = breaks.vec,
               date_labels = '%Y') +
  scale_color_manual(values = c("darkblue", "darkgray", "darkred"), 
                     labels = c(paste('min avg month: ', as.character(e_min_month)),
                                "Other Months",
                                paste('max avg month: ', as.character(e_max_month)))) +
  scale_linetype_manual(values = rep("solid", length(unique(e_va_plot_df$month))),
                        labels = levels(e_va_plot_df$month)) +
  labs(title = "Average Monthly Electricity Prices (2010-2022)",
       x = "Year",
       y = "Average Price ($ kWh)",
       color = 'Color Legend') +
  theme(panel.background = element_rect(fill = "lightgray"))

################################################################################
#### 6. Forecast next 10, 20, 30, 40, 50 years based on monthly and annual 
####    seasonality and adjusted of periodicity of U.S. energy initiative events.
################################################################################

# Combine national quarterly data with Virginia (adjusted for quarterly)
# Goal is to use national CPI

