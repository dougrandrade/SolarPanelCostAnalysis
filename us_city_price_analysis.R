library(ggplot2)
library(mice)
library(tidyr)

# Read File in from the working directory
setwd("C:/Users/dougr/Data_Science_Projects/R/SolarPanelCostAnalysis")
us_pwr_df <- read.csv("Price_KWH_US_cities.csv")

# Convert the 'Year' column to a Date format
us_pwr_df$Year <- as.Date(us_pwr_df$Year, format="%Y")

# Impute missing values
us_pwr_df_imp <- mice(us_pwr_df[-1], defaultMethod = 'pmm', seed = 5, remove_collinear = FALSE, printFlag = FALSE)

# Combine imputed data with 'Year'
us_pwr_df_imp <- cbind(Year = us_pwr_df$Year, complete(us_pwr_df_imp))

# Reshape the data from wide to long format for ggplot
us_pwr_df_long <- pivot_longer(us_pwr_df_imp, cols = -Year, names_to = "Month", values_to = "Price")

# Create a ggplot time series plot for all the months
ggplot(us_pwr_df_long, aes(x = Year, y = Price, color = Month)) +
  geom_line(na.rm = TRUE) +
  labs(title = "Average Electricity Prices In U.S. Cities",
       x = "Year",
       y = "Price/KW",
       color = "Month") +
  theme_minimal()

# Extract avg min & max average priced months
avg_month_vals <- colMeans(us_pwr_df[, -1], na.rm = TRUE)
min_avg_month <- names(avg_month_vals)[which.min(avg_month_vals)]
max_avg_month <- names(avg_month_vals)[which.max(avg_month_vals)]

min_max_month_long <- subset(us_pwr_df_long, Month %in% c(min_avg_month, max_avg_month))
#min_max_month_long$Date <- as.Date(paste(min_max_month_long$Year, min_max_month_long$Month, "01"), format="%Y %b %d")

# ...

# Create a time series plot for the months with the minimum and maximum average
ggplot(min_max_month_long, aes(x = Year, y = Price, color = Month)) +
  geom_line(na.rm = TRUE) +
  labs(title = "Time Series Plot for Min and Max Avg Months",
       x = "Year",
       y = "Electricity Price",
       color = "Month")

