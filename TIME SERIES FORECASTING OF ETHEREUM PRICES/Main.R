# Load necessary libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(ggplot2)
library(gridExtra)
library(zoo)
library(xts)
library(keras)
library(tensorflow)

# Load the dataset
ethereum_data <- ETH_day



# typeof(ethereum_data$Date)
class(ethereum_data$Date)



# Create the line plot
ggplot(ethereum_data, aes(x = Date, y = Close)) +
  geom_line() +
  labs(title = "Ethereum Closing Prices", x = "Date", y = "Closing Price") +
  theme_minimal()  

# Convert 'Close' prices to a time series object
ethereum_ts <- ts(ethereum_data$Close,start = c(2016, 129), frequency = 365)

# Box-Cox Transformation to find the best lambda
lambda <- BoxCox.lambda(ethereum_ts)
print(paste("Optimal Lambda for Box-Cox Transformation:", lambda))

# Determine if the model is additive or multiplicative
if (lambda > -0.5 & lambda < 0.5) {
  model_type <- "additive"
  print("Using Additive Model")
} else {
  model_type <- "multiplicative"
  print("Using Multiplicative Model")
}

# Decompose the time series
decomposed_ts <- decompose(ethereum_ts, type = model_type)

# Plot the decomposed components
plot(decomposed_ts)

#check Is there Null values in data set
na_count_ori <- sum(is.na(ethereum_data$Close))
cat("Number of values NA in ori dataset:", na_count_ori, "\n")




# Outlier detection using IQR method
Q1 <- quantile(ethereum_data$Close, 0.25)
Q3 <- quantile(ethereum_data$Close, 0.75)
IQR <- Q3 - Q1

# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- ethereum_data %>%
  filter(Close < lower_bound | Close > upper_bound)

# Plot the data with a shaded area for the normal range
ggplot(ethereum_data, aes(x = Date, y = Close)) +
  geom_line() +
  geom_hline(yintercept = lower_bound, linetype = "dashed", color = "red") +
  geom_hline(yintercept = upper_bound, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "lightblue", alpha = 0.5) +
  labs(title = 'Ethereum Closing Prices with Normal Range (IQR Method)',
       x = 'Date', y = 'Closing Price') +
  theme_minimal()

# Boxplot to visualize outliers
ggplot(ethereum_data, aes(x = "", y = Close)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) +
  labs(title = 'Boxplot of Ethereum Closing Prices', x = '', y = 'Closing Price')

# Summary statistics to verify the bounds
summary(ethereum_data$Close)
cat("Lower bound for outliers:", lower_bound, "\n")
cat("Upper bound for outliers:", upper_bound, "\n")
cat("Number of outliers detected:", nrow(outliers), "\n")

# Identify outliers and replace them with NA
ethereum_data$Close_adj <- ifelse(ethereum_data$Close < lower_bound | ethereum_data$Close > upper_bound, NA, ethereum_data$Close)
# Count the number of NA values in the Close_adj column
na_count <- sum(is.na(ethereum_data$Close_adj))
cat("Number of values replaced with NA:", na_count, "\n")

# Create a new column to highlight the replaced values
ethereum_data$highlight <- ifelse(is.na(ethereum_data$Close_adj), "Replaced", "Original")
# Print the count of each category in the 'highlight' column
highlight_counts <- table(ethereum_data$highlight)
print(highlight_counts)

# Interpolate the missing (outlier) values
ethereum_data$Close_adj <- na.approx(ethereum_data$Close_adj, na.rm = FALSE)
# Check if there are any NA values left in the Close_adj column
na_count_after_interpolation <- sum(is.na(ethereum_data$Close_adj))
cat("Number of NA values after interpolation:", na_count_after_interpolation, "\n")

# Plot the data with a shaded area for the normal range
ggplot(ethereum_data, aes(x = Date, y = Close_adj)) +
  geom_line(data = subset(ethereum_data, highlight == "Original"), aes(color = highlight)) +  # Plot original data as a line
  geom_point(data = subset(ethereum_data, highlight == "Replaced"), aes(color = highlight), shape = 16, size = 2) +  # Plot replaced data as points
  geom_hline(yintercept = lower_bound, linetype = "dashed", color = "red") +
  geom_hline(yintercept = upper_bound, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "lightblue", alpha = 0.5) +
  labs(title = 'Ethereum Closing Prices with Outlier Interpolation',
       x = 'Date', y = 'Adjusted Closing Price') +
  scale_color_manual(values = c("Original" = "black", "Replaced" = "blue")) +
  theme_minimal()



# Boxplot to visualize the data after replacing outliers
ggplot(ethereum_data, aes(x = "", y = Close_adj)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) +
  labs(title = 'Boxplot of Adjusted Ethereum Closing Prices', x = '', y = 'Adjusted Closing Price')

# Summary statistics to verify the bounds and changes
summary(ethereum_data$Close_adj)
cat("Lower bound for outliers:", lower_bound, "\n")
cat("Upper bound for outliers:", upper_bound, "\n")
cat("Number of outliers detected:", nrow(outliers), "\n")
cat("Number of values replaced:", sum(ethereum_data$highlight == "Replaced"), "\n")


# ACF and PACF plots using Close_adj
acf_plot <- ggAcf(ethereum_data$Close_adj, main="ACF of Adjusted Ethereum Closing Prices")
pacf_plot <- ggPacf(ethereum_data$Close_adj, main="PACF of Adjusted Ethereum Closing Prices")
grid.arrange(acf_plot, pacf_plot, ncol=2)

# Check for stationarity using Close_adj
adf_test <- adf.test(ethereum_data$Close_adj)
print(adf_test)
# Check stationarity
if (adf_test$p.value < 0.05) {
  cat("The Original Time series  is stationary.\n")
} else {
  cat("The Original Time series Close_adj is not stationary.\n")
}

# Differencing to make the series stationary using Close_adj
diff_ethereum_data <- diff(ethereum_data$Close_adj, differences=1)
adf_test_diff <- adf.test(diff_ethereum_data)
print(adf_test_diff)

# Check stationarity
if (adf_test_diff$p.value < 0.05) {
  cat("The differenced series is stationary.\n")
} else {
  cat("The differenced series is not stationary.\n")
}

# Preparing data frame for plotting the differenced series
ethereum_diff_df <- data.frame(Date = ethereum_data$Date[-1], Close_adj = diff_ethereum_data)

# Plotting the differenced series
ggplot(ethereum_diff_df, aes(x = Date, y = Close_adj)) + 
  geom_line() + 
  labs(title = 'Ethereum Differencing Adjusted Closing Prices', x = 'Date', y = 'Differenced Adjusted Close Price')

# ACF and PACF plots for the differenced series using Close_adj
acf_plot2 <- ggAcf(diff_ethereum_data, main="ACF of Differenced Adjusted Ethereum Closing Prices")
pacf_plot2 <- ggPacf(diff_ethereum_data, main="PACF of Differenced Adjusted Ethereum Closing Prices")

# Arranging ACF and PACF plots side by side
grid.arrange(acf_plot2, pacf_plot2, ncol=2)

# Perform second differencing on the already differenced series
diff_ethereum_data_2nd <- diff(diff_ethereum_data, differences = 1)

# Check stationarity of the second differenced series
adf_test_diff_2nd <- adf.test(diff_ethereum_data_2nd)
print(adf_test_diff_2nd)

# Check stationarity
if (adf_test_diff_2nd$p.value < 0.05) {
  cat("The second differenced series is stationary.\n")
} else {
  cat("The second differenced series is not stationary.\n")
}

# Preparing data frame for plotting the second differenced series
ethereum_diff_df_2nd <- data.frame(Date = ethereum_data$Date[-c(1:2)], Close_adj = diff_ethereum_data_2nd)

# Plotting the second differenced series
ggplot(ethereum_diff_df_2nd, aes(x = Date, y = Close_adj)) + 
  geom_line() + 
  labs(title = 'Ethereum Second Differencing Adjusted Closing Prices', x = 'Date', y = 'Second Differenced Adjusted Close Price')

# ACF and PACF plots for the second differenced series
acf_plot3 <- ggAcf(diff_ethereum_data_2nd, main="ACF of Second Differenced Adjusted Ethereum Closing Prices")
pacf_plot3 <- ggPacf(diff_ethereum_data_2nd, main="PACF of Second Differenced Adjusted Ethereum Closing Prices")

# Arranging ACF and PACF plots side by side
grid.arrange(acf_plot3, pacf_plot3, ncol=2)




ethereum_data_sorted <- ethereum_data[order(ethereum_data$Date), ]
print(ethereum_data_sorted)
# Convert the data into a time series with a correct frequency, starting on May 9, 2016
ethereum_adj_ts <- ts(c(ethereum_data_sorted$Close_adj), start=c(2016, 129), frequency=365)


# Calculate the total number of observations
total_length <- length(ethereum_adj_ts)

# Determine the split point for 80% training data
split_point <- round(total_length * 0.8)

# Create training and testing datasets using the ts window function
# Since the series starts at a specific day, calculate the precise end day for training set
years_passed <- (split_point + 129 - 1) %/% 365  # Adjust for the starting day of the year
end_day_of_year <- (split_point + 129 - 1) %% 365

train_ts <- window(ethereum_adj_ts, end=c(2016 + years_passed, end_day_of_year))
test_ts <- window(ethereum_adj_ts, start=c(2016 + years_passed, end_day_of_year + 1))



# Fit the Holt-Winters model to the training data
hw_model_train <- HoltWinters(train_ts, seasonal="additive")


# Forecast using the trained model
hw_forecast_test <- forecast(hw_model_train, h=length(test_ts))

# Plot the forecast against actual values
plot(hw_forecast_test, main="Holt-Winters Forecast vs Actual")
lines(test_ts, col='red')
legend("topleft", legend=c("Forecast", "Actual"), col=c("blue", "red"), lty=1:1)


# Calculate accuracy metrics using the testing data
accuracy_metrics_test <- accuracy(hw_forecast_test$mean, test_ts)
print(accuracy_metrics_test)




# Calculate residuals
residuals_hw <- residuals(hw_model_train)

# Plot residuals
plot(residuals_hw, type = "l", main = "Residuals from Holt-Winters Model", xlab = "Time", ylab = "Residuals")
abline(h = 0, col = "red")

# Check normality of residuals
qqnorm(residuals_hw)
qqline(residuals_hw, col = "red")

# Calculate and plot autocorrelation of residuals
acf(residuals_hw, main="ACF of Residuals")


















