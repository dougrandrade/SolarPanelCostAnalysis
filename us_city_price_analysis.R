library(ggplot2)
library(mice)         # for mice(), complete()
library(tidyr) # for pivot_longer()

#Read File in from your working directory
setwd("C:/Users/dougr/Data_Science_Projects/R")
us_pwr_df = read.csv("Price_KWH_US_cities.csv")  # read csv file

# Convert the 'Year' column to a Date format
us_pwr_df$Year <- as.Date(us_pwr_df$Year,
                          format="%Y")
# summary
summary(us_pwr_df[, -1])

us_pwr_df_imp <- mice(us_pwr_df[-1],
                      defaultMethod = 'pmm',
                      seed = 5,
                      remove_collinear = FALSE,
                      printFlag = FALSE)
summary(us_pwr_df_imp)

us_pwr_df_imp <- cbind(Year = us_pwr_df$Year, complete(us_pwr_df_imp))
summary(us_pwr_df_imp[, -1])

# Reshape the data from wide to long format for ggplot
us_pwr_df_long <- pivot_longer(us_pwr_df_imp,
                               cols = -Year,
                               names_to = "Month",
                               values_to = "Price")

# Create a ggplot time series plot for all the months
ggplot(us_pwr_df_long, aes(x = Year,
                           y = Price,
                           color = Month)) +
  geom_line() +
  labs(title = "Average Electricity Prices In U.S. Cities",
       x = "Year",
       y = "Price/KW",
       color = "Month")

# Create a ggplot time series plot of the highest average month and lowest avg month
avg_month_vals <- colMeans(us_pwr_df[, -1], na.rm = T)
min_avg_month <- names(avg_month_vals)[which.min(avg_month_vals)]
max_avg_month <- names(avg_month_vals)[which.max(avg_month_vals)]

min_max_month_long <- subset(us_pwr_df_long, Month %in% c(min_avg_month, max_avg_month))

# Assuming you have a Date column in your data
min_max_month_long$Date <- as.Date(paste(min_max_month_long$Year, min_max_month_long$Month, "01"), format="%Y %b %d")

# Create a time series plot for the months with the minimum and maximum average
ggplot(min_max_month_long, aes(x = Date, y = Price, color = Month)) +
  geom_line() +
  labs(title = "Time Series Plot for Min and Max Avg Months",
       x = "Date",
       y = "Electricity Price",
       color = "Month") +
  scale_x_date(breaks = seq(as.Date("1979-06-01"), as.Date("2023-07-15"), by="14 days"),
               labels = c("Jun 01", "Jun 15", "Jul 01", "Jul 15"),
               date_labels = "%b %d")
