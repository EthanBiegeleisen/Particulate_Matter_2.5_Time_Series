# Ethan Biegeleisen
# April 11, 2020
# MATG 635 Final Project

# Using Manhattan 2018 PM 2.5 Data

M2018 <- read.csv("Manhattan 2018.csv")
# Brings the data from the CSV file to R

layout(1:1)
plot(M2018[1:2])
# Creates a plot of the 2018 data with the date on the x-axis and the PM 2.5 levels on the y-axis

M2018.ts <- ts(M2018[2], st = c(2018, 1), fr = 365)
# Converts the PM 2.5 levels into a time series
# The frequency is set to 365 because 2018 is not a leap year

plot(M2018.ts, xlab = 'Date', ylab = 'PM 2.5 AQI Value', main = 'Daily PM 2.5 AQI Values')
# Plots the time series


# Using Manhattan 2013-2018 PM 2.5 Data

M2013_2018 <- read.csv("Manhattan 2013-2018.csv")
# Brings the data from the CSV file to R

plot(M2013_2018[1:2])
# Creates a plot of the 2013-2018 data with the date on the x-axis and the PM 2.5 levels on the y-axis

M2013_2018.ts <- ts(M2013_2018[2], st = c(2013, 1), fr = 365.25)
# Converts the PM 2.5 levels into a time series
# The frequency is set to 365.25 because 2016 is a leap year

plot(M2013_2018.ts, xlab = 'Date', ylab = 'PM 2.5 AQI Value', main = 'Daily PM 2.5 AQI Values')
# Plots the time series

plot(decompose(M2013_2018.ts))
# Plots the decomposition of the time series to show its trend, seasonal variation, and error


# Using Manhattan 2013-2019 PM 2.5 Data

M2013_2019 <- read.csv("Manhattan 2013-2019.csv")
# Brings the data from the CSV file to R

plot(M2013_2019[1:2])
# Creates a plot of the 2013-2019 data with the date on the x-axis and the PM 2.5 levels on the y-axis

M2013_2019.ts <- ts(M2013_2019[2], st = c(2013, 1), fr = 365.25)
# Converts the PM 2.5 levels into a time series
# The frequency is set to 365.25 because 2016 is a leap year

plot(M2013_2019.ts, xlab = 'Date', ylab = 'PM 2.5 AQI Value', main = 'Daily PM 2.5 AQI Values')
# Plots the time series

plot(decompose(M2013_2019.ts))
# Plots the decomposition of the time series to show its trend, seasonal variation, and error


# Using Manhattan 2013-2018 PM 2.5 Data with the Data from February 29, 2016 Removed

newM2013_2018 <- read.csv("Manhattan 2013-2018 (No Feb 29).csv")
# Brings the data from the CSV file to R

newM2013_2018.ts <- ts(newM2013_2018[2], st = c(2013, 1), fr = 365) 
# Converts the PM 2.5 levels into a time series
# The frequency is set to 365 when the February 29 date in 2016 is removed from the data set

newM2013_2018.hw <- HoltWinters (newM2013_2018.ts, seasonal = c("additive"), start.periods = 6)
# Uses the Holt-Winters method on the time series
# When this function was used on the 2013-2018 data with February 29, 2016 included and the time series frequency set to 365.25,
# the function had a decomposition error saying the time series had no or less than 2 periods.
# However, the function runs correctly when the 2013-2018 time series without the February 29, 2016 date is used instead and the
# time series frequency is set to 365.

newM2013_2018.hw$alpha
newM2013_2018.hw$beta
newM2013_2018.hw$gamma
# Displays the values chosen for alpha, beta, and gamma by the HoltWinters function

newM2013_2018.hw$SSE
# Displays the sum of one-step ahead prediction errors

plot(newM2013_2018.hw$fitted, main = 'Holt-Winters Fit')
# Plots the Holt-Winters fitted values, level, slope (labeled trend), and seasonal variation

plot(newM2013_2018.hw)
# Plots the Holt-Winters fit against the original time series
# The time series is the black line and the fitted values are the red line

newM2013_2018.predict1 <- predict(newM2013_2018.hw, n.ahead = 1 * 365)
# Predicts Manhattan's 2019 PM 2.5 levels based on the Holt-Winters fit for Manhattan's 2013-2018 PM 2.5 levels

layout(1:2)
ts.plot(newM2013_2018.ts, newM2013_2018.predict1, lty = 1:2, ylab = 'PM 2.5 AQI Value', main = 'Observed 2013-2018 PM 2.5 Levels + 2019 Prediction')
# Plots Manhattan's observed 2013-2018 PM 2.5 levels and the prediction for Manhattan's 2019 PM 2.5 levels

plot(M2013_2019.ts, ylab = 'PM 2.5 AQI Value', main = 'Observed 2013-2019 PM 2.5 Levels')
# Plots Manhattan's observed 2013-2019 PM 2.5 levels

newM2013_2018.predict2 <- predict(newM2013_2018.hw, n.ahead = 4 * 365)
# Predicts Manhattan's 2019-2022 PM 2.5 levels based on the Holt-Winters fit for Manhattan's 2013-2018 PM 2.5 levels

layout(1:1)
plot(decompose(newM2013_2018.predict2))
# Plots the decomposition of Manhattan's estimated 2019-2022 PM 2.5 levels

ts.plot(newM2013_2018.ts, newM2013_2018.predict2, lty = 1:2, ylab = 'PM 2.5 AQI Value', main = 'Observed 2013-2018 PM 2.5 Levels + 2019-2022 Prediction')
# Plots Manhattan's observed 2013-2018 PM 2.5 levels and the prediction for Manhattan's 2019-2022 PM 2.5 levels


# Using Manhattan 2019 PM 2.5 Data

M2019 <- read.csv("Manhattan 2019.csv")
# Brings the data from the CSV file to R

M2019.ts <- ts(M2019[2], st = c(2019, 1), fr = 365)
# Converts the PM 2.5 levels into a time series
# The frequency is set to 365 because 2019 is not a leap year

plot(M2019.ts, col = 'red', xlab = 'Date', ylab = 'PM 2.5 AQI Value', main = 'Daily PM 2.5 AQI Values')
# Plots the time series

lines(newM2013_2018.predict1, col = 'blue')
# Adds the 2019 prediction to the plot

legend(2019.7, 80, legend = c('Observed', 'Predicted'), fill = c('red', 'blue'))
# Adds a legend to the plot


M2019Difference <- newM2013_2018.predict1 - M2019.ts
# Creates an array containing the difference between the 2019 prediction and 2019 observation on each date

min(M2019Difference) # Minumum
max(M2019Difference) # Maximum
mean(M2019Difference) # Mean
sd(M2019Difference) # standard deviation

# The 2019 prediction ranges between 38.94 points below and 33.76 points above the observed 2019 PM 2.5 levels
# However, on average the 2019 prediction is 1.21 points higher than the observed 2019 PM 2.5 levels with a standard deviation of 14.67

M2019DifferenceStandardError <- sd(M2019Difference)/sqrt(length(M2019Difference)) 
# Calculates the standard error (standard deviation/sqrt(n) where n is the number of entries)

c(mean(M2019Difference) - 2.576 * M2019DifferenceStandardError, mean(M2019Difference) + 2.576 * M2019DifferenceStandardError)
# Calculates the 99% confidence interval of the true mean of the difference between the 2019 prediction ranges and the observed 2019 PM 2.5 levels
# The confidence interval is calculated by: xbar - Z * standard error, xbar + Z * standard error
# Z is 2.576 for the 99% confidence interval

# The resulting 99% confidence interval is (-0.77, 3.19)

# Same process as above for the absolute value of M2019Difference

min(abs(M2019Difference)) # Minumum
max(abs(M2019Difference)) # Maximum
mean(abs(M2019Difference)) # Mean
sd(abs(M2019Difference)) # standard deviation

# The smallest difference between a daily observation and a daily prediction is 0.02, while the largest difference between a daily observation and a saily prediction is 38.94
# The mean of the absolute value of the difference between the 2019 observations and predictions is 11.99 while the standard deviation is 8.51

M2019AbsDifferenceStandardError <- sd(abs(M2019Difference))/sqrt(length(abs(M2019Difference))) 
# Calculates the standard error (standard deviation/sqrt(n) where n is the number of entries)

c(mean(abs(M2019Difference)) - 2.576 * M2019AbsDifferenceStandardError, mean(abs(M2019Difference)) + 2.576 * M2019AbsDifferenceStandardError)
# Calculates the 99% confidence interval of the true mean of the absolute value of the difference between the 2019 prediction ranges and the observed 2019 PM 2.5 levels
# The confidence interval is calculated by: xbar - Z * standard error, xbar + Z * standard error
# Z is 2.576 for the 99% confidence interval

# The resulting 99% confidence interval is (10.85, 13.14)


# Checking the average of all the nonnegative entries in M2019Difference and the average of all the negative entries in M2019Difference

nonnegativetotal <- 0 # Initializes the sum of nonnegative entries as zero
nonnegativecount <- 0 # Initializes the number of nonnegative entries as zero
negativetotal <- 0 # Initializes the sum of negative entries as zero
negativecount <- 0 # Initializes the number of negative entries as zero

for (i in 1:365) 
  {if (M2019Difference[i] >= 0)
    {nonnegativetotal <- nonnegativetotal + M2019Difference[i]
     nonnegativecount <- nonnegativecount + 1
     end}
  end}
  # For all values of i between 1 and 365:
  # If the value of the ith entry of M2019Difference is greater than or equal to zero:
  # Then add the value of the ith entry to nonnegativetotal and add 1 to nonnegativecount

for (i in 1:365) 
  {if (M2019Difference[i] < 0)
    {negativetotal <- negativetotal + M2019Difference[i]
    negativecount <- negativecount + 1
    end}
  end}
  # For all values of i between 1 and 365:
  # If the value of the ith entry of M2019Difference is less than zero:
  # Then add the value of the ith entry to negativetotal and add 1 to negativecount

print(nonnegativetotal/nonnegativecount) # Mean of all nonnegative entries in M2019Difference
print(negativetotal/negativecount) # Mean of all negative entries in M2019Difference

# The mean of all nonnegative entries in M2019Difference is 11.81
# The mean of all negative entries in M2019Difference is -12.22
# These are consistent with the 99% confidence interval of the true mean of the absolute value of the difference between the 2019 prediction ranges and the observed PM 2.5 levels
# That 99% confidence interval was found to be (10.85, 13.14)


# Plotting the trend and seasonal effects of the 2013-2019 time series with the 2019 forecast

M2013_2019Decom <- decompose(M2013_2019.ts) # Decomposes the 2013-2019 time series
M2013_2019Trend <- M2013_2019Decom$trend # Exctracts the observed trend from the time series
M2013_2019Seasonal <- M2013_2019Decom$seasonal # Extracts the seasonal effect from the time series

length(M2013_2019.ts) 
# The time series has 2556 entries

plot(M2013_2019Trend + M2013_2019Seasonal, col = 'blue', xlab = 'Date', ylab = 'Daily PM 2.5 AQI Values', main = 'Trend and Seasonal Effect of PM 2.5 AQI Values')
# Plots the trend + seasonal effect for the 2013-2019 time series

lines(newM2013_2018.predict1, col = 'turquoise')
# Plots the 2019 prediction

legend(2013.0, 70, legend = c('Observed', 'Predicted'), fill = c('blue', 'turquoise'))
# Adds a legend to the plot