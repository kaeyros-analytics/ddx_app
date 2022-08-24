# preamble setting the directories and loading packages
rm(list = ls())
#setwd("C:/Users/Tejendra/Desktop/FoldersOnDesktop/UdemyCourse/Section7")
require(tidyverse)
require(tidymodels)
require(data.table)
require(tidyposterior)
require(tsibble)  #tsibble for time series based on tidy principles
require(fable)  #for forecasting based on tidy principles
require(ggfortify)  #for plotting timeseries
require(forecast)  #for forecast function
require(tseries)
require(chron)
require(lubridate)
require(directlabels)
require(zoo)
require(lmtest)
require(TTR)  #for smoothing the time series
require(MTS)
require(vars)
require(fUnitRoots)
require(lattice)
require(grid)

### Multivariate Time Series Datasets
# Generating a random dataframe
set.seed(40)
x = rnorm(100, 1)
y = rnorm(100, 30)
z = rnorm(100, 500)

xyz = data.frame(x, y, z)

class(xyz)

# Converting a data.frame into mts = Multivariate Time Series
mymts = ts(xyz,
           frequency = 12,
           start = c(1940, 4))
head(mymts)

# Standard exploratory tools
plot(mymts)

theme_set(theme_bw())
autoplot(mymts) +
  ggtitle("Time Series Plot of the `mymts' Time-Series") +
  theme(plot.title = element_text(hjust = 0.5)) #for centering the text


# Our further exercise dataset
data(EuStockMarkets)
str(EuStockMarkets)

class(EuStockMarkets)

head(EuStockMarkets)

plot(EuStockMarkets)

autoplot(EuStockMarkets) +
  ggtitle("Time Series Plot of the `EuStockMarkets' Time-Series") +
  theme(plot.title = element_text(hjust = 0.5))

# Main packages - problem: both have different functions VAR
## Testing for stationarity
### tseries - standard test adt.test
apply(EuStockMarkets, 2, adf.test)


# Alternative: lib fUnitRoots, function
apply(EuStockMarkets, 2, adfTest, 
      lags=0, #maximum number of lags used for error term correction
      type="c", #type of unit root regression
      title = "ADF Test for EuStockMarkets Data") #title of the project

## Warning in FUN(newX[, i], ...): p-value greater than printed p-value

## Warning in FUN(newX[, i], ...): p-value greater than printed p-value

## Warning in FUN(newX[, i], ...): p-value greater than printed p-value

# Differencing the whole mts
stnry = diffM(EuStockMarkets) #difference operation on a vector of time series. Default order of differencing is 1.
head(stnry)
str(stnry)

# Retest
apply(stnry, 2, adf.test)

## Warning in FUN(newX[, i], ...): p-value smaller than printed p-value

## Warning in FUN(newX[, i], ...): p-value smaller than printed p-value

## Warning in FUN(newX[, i], ...): p-value smaller than printed p-value

## VAR modeling
plot.ts(stnry)

autoplot(ts(stnry,
            start = c(1990,130),
            frequency = 260)) +
  ggtitle("Time Series Plot of the stationary `EuStockMarkets' Time-Series")


# autoplot(ts(stnry,
#             start = c(1990,1),
#             frequency = 12)) +
#   ggtitle("Time Series Plotff of the stationary `EuStockMarkets' Time-Series")

# Lag order identification
#We will use two different functions, from two different packages to identify the lag order for the VAR model. Both functions are quite similar to each other but differ in the output they produce. vars::VAR is a more powerful and convinient function to identify the correct lag order. 
VARselect(stnry, 
          type = "none", #type of deterministic regressors to include. We use none becasue the time series was made stationary using differencing above. 
          lag.max = 10) #highest lag order

# Creating a VAR model with vars
var.a <- vars::VAR(stnry,
                   lag.max = 10, #highest lag order for lag length selection according to the choosen ic
                   ic = "AIC", #information criterion
                   type = "none") #type of deterministic regressors to include
summary(var.a)

# Residual diagnostics
#serial.test function takes the VAR model as the input.  
serial.test(var.a)

#selecting the variables
# Granger test for causality
#for causality function to give reliable results we need all the variables of the multivariate time series to be stationary. 
causality(var.a, #VAR model
          cause = c("DAX")) #cause variable. If not specified then first column of x is used. Multiple variables can be used. 

## Forecasting VAR models
fcast = predict(var.a, n.ahead = 25) # we forecast over a short horizon because beyond short horizon prediction becomes unreliable or uniform
par(mar = c(2.5,2.5,2.5,2.5))
plot(fcast)


# Forecasting the DAX index
DAX = fcast$fcst[1]; DAX # type list

# Extracting the forecast column
x = DAX$DAX[,1]; x

# Inverting the differencing
#To get the data to the original scale we invert the time series
#since the values are just difference from the previous value, to get the values on the original scale we add the last value from the DAX time series to the predicted values.
#the plot of the predicted values will also show that over longer horizon the predicted values are not reliable
x = cumsum(x) + 5473.72
par(mar = c(2.5,2.5,1,2.5)) #bottom, left, top, and right
plot.ts(x)

# Adding data and forecast to one time series
DAXinv =ts(c(EuStockMarkets[,1], x),
           start = c(1991,130), 
           frequency = 260)
plot(DAXinv)

#DAXinv_datframe <- as.data.frame(DAXinv[1786:1885])
DAXinv_datframe <- as.data.frame(DAXinv) 
colnames(DAXinv_datframe) <- c("x")
head(DAXinv_datframe)

ggplot() + 
  geom_line(data = as.data.frame(DAXinv_datframe[1:75,]), aes(y = get("DAXinv_datframe[1:75, ]"), x = seq(1, 75)), color = "green") +
  geom_line(data = as.data.frame(DAXinv_datframe[76:100,]), aes(y = get("DAXinv_datframe[76:100, ]"), x = seq(76, 100)), color = "red") +
  ggtitle("Plot of forecast of the VAR model on `EuStockMarkets''s DAX time series") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("Value")

## Creating an advanced plot with visual separation
# Converting to object zoo
x = zoo(DAXinv[1786:1885])

# Advanced xyplot from lattice
xyplot(x, grid=TRUE, panel = function(x, y, ...){
  panel.xyplot(x, y, col="red", ...)
  grid.clip(x = unit(76, "native"), just=c("right"))
  panel.xyplot(x, y, col="green", ...) })

#we repeat the plots from above using the ggplot2 package 
# Inverting the differencing
x_dat_frame <- as.data.frame(x)
ggplot(x_dat_frame, aes(y = x, x = seq(1, length(x_dat_frame$x)))) + 
  geom_line() + 
  ggtitle("Plot of forecast of the VAR model on `EuStockMarkets''s DAX time series") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("Value")

# Adding data and forecast to one time series
autoplot(DAXinv) +
  ggtitle("Time Series Plot of the `DAXinv' Time-Series") +
  theme(plot.title = element_text(hjust = 0.5))

