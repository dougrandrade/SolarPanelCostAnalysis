# Import modules
library(ggplot2) # plotting
library(tidyr) # data cleaning/prep (pivot_longer())
library(dplyr) # data manipluation (mutate())
library(zoo) # data manipulation (as.yearmon())
library(mice) # missing data imputation
library(forecast) # missing data imputation (na.interp())

# Read File in from the working directory
setwd("C:/Users/dougr/Data_Science_Projects/R/SolarPanelCostAnalysis")
us_pwr_df <- read.csv("Price_KWH_US_cities.csv")
summary(us_pwr_df)

################################################################################
#### 1. Visual analysis of avg U.S. city electricity monthly prices 1979-2023
# - impute missing values
# - time series ggplots
################################################################################

us_pwr_df_long_a <- pivot_longer(us_pwr_df, cols = -Year, names_to = "Month", values_to = "Price")
summary(us_pwr_df_long_a)

us_pwr_df_long <- us_pwr_df_long_a

# Assuming us_pwr_df_long is available, replace it with your actual data frame
us_pwr_df_long$Month <- factor(us_pwr_df_long$Month, levels = month.abb)

# Convert the 'Year' column to a Date format
us_pwr_df_long$Year <- as.Date(us_pwr_df_long$Year, format="%Y")
us_pwr_df_long$Year <- as.Date(paste(us_pwr_df_long$Year, "-01-01", sep = ""), format = "%Y-%m-%d")
summary(us_pwr_df_long)

# Create a time series object
time_series_data <- ts(us_pwr_df_long[, -c(1, 2)], start = c(1979, 1), end = c(2023, 12), frequency = 12)
summary(time_series_data)

# Perform imputation if needed (e.g., using na.interp)
time_series_data_imputed <- na.interp(time_series_data)
summary(time_series_data_imputed)

# Check the structure of the imputed time series data
str(time_series_data_imputed)

# Plot the time series data for visual analysis
autoplot(time_series_data_imputed) +
  ggtitle('Initial Time Series of U.S. Electricity Prices (1979-2023') +
  theme(panel.background = element_rect(fill = "lightgray"))

# Convert the time series to a data frame for plotting
plot_data <- data.frame(
  date = as.Date(time(time_series_data_imputed)),
  value = as.vector(time_series_data_imputed),
  month = factor(month.abb[cycle(time_series_data_imputed)], levels = month.abb)
)
summary(plot_data)

table(is.na(plot_data$month))

# Calculate the average price for each month
avg_prices <- aggregate(value ~ month, data = plot_data, FUN = mean)

# Identify the months with max and min average prices
max_month <- avg_prices$month[which.max(avg_prices$value)]
min_month <- avg_prices$month[which.min(avg_prices$value)]

# Create a new variable for color based on conditions
plot_data$color <- case_when(
  plot_data$month == max_month ~ "red",
  plot_data$month == min_month ~ "blue",
  TRUE ~ "gray"
)

################################################################################
#### 2. Compare the lowest avg month and highest avg month
# - time series ggplot of price different
################################################################################

# Create a multi-colored line plot with legend

breaks.vec <- seq(floor_date(min(plot_data$date), unit = "5 years"), max(ceiling_date(plot_data$date, unit = "5 years")), by = "5 years")

ggplot(plot_data, aes(x = date, y = value, color = color)) +
  geom_line(size = 0.703) +
  scale_x_date(limits = as.Date(c('1975-01-01','2025-01-01')),
               breaks = breaks.vec,
               date_labels = '%Y') +
  scale_color_manual(values = c("darkblue", "darkgray", "darkred"), 
                     labels = c(paste('min avg month: ', as.character(min_month)),
                                "Other Months",
                                paste('max avg month: ', as.character(max_month)))) +
  scale_linetype_manual(values = rep("solid", length(unique(plot_data$month))),
                        labels = levels(plot_data$month)) +
  labs(title = "Average Monthly Electricity Prices (1979-2023)",
       x = "Year",
       y = "Average Price (KWH)",
       color = 'Color Legend') +
  theme(panel.background = element_rect(fill = "lightgray"))





################################################################################
#### 3. Visual analysis of U.S. annual GDP, inflation, population 1979-2023
# - impute missing values
# - time series ggplot(s)
# - analysis of whether national electric price trends trail, match, or exceed 
#   national economic growth indicators
################################################################################


















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



























################################################################################
#### 6. Forecast next 10, 20, 30, 40, 50 years based on monthly and annual 
####    seasonality and adjusted of periodicity of U.S. energy initiative events.
################################################################################