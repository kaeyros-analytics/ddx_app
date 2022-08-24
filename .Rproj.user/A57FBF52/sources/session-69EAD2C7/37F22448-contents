
# 0. Setting and loading packages  ####
#_______________________________

rm(list = ls(all.names = TRUE)); # activate the step before execution !!!!!!
cat("\f");

# setting system local encoding
Sys.setlocale("LC_ALL", "English_United States.932") # this works perfectly ---> für japanese characters

# setting some options
options(stringsAsFactors = FALSE)
options(Encoding = "latin1")

# Loading required libraries
library(rJava)
library(DBI)
library(RJDBC)
library(pool)
library(shiny)
library(sf)
library(shinyWidgets)
library(stringr)
library(leaflet)
library(plotly)
library(dplyr)
library(DT)
library(shinycssloaders)
library(DBI)
library(RSQLite)
library(pool)
library(lubridate)
library(viridisLite)
library(igraph)
library(visNetwork)
library(zoo)
library(rintrojs)
library(readxl)
library(shinyjs)
library(openxlsx)
library(bcrypt)
library(glue)
library(plotly)
library(dplyr)
library(ggplot2)
library(magrittr)
library(sass)
library(shinydashboard)
library(shinythemes)
library(shiny.react)
library(shiny.router)
library(fpp2)
# require(tidyverse)
# require(tidymodels)
# require(data.table)
# require(tidyposterior)
# require(tsibble)  #tsibble for time series based on tidy principles
# require(fable)  #for forecasting based on tidy principles
# require(ggfortify)  #for plotting timeseries
# require(forecast)  #for forecast function
# require(tseries)
# require(chron)
# require(lubridate)
# require(directlabels)
# require(zoo)
# require(lmtest)
# require(TTR)  #for smoothing the time series
# require(MTS)
# require(vars)
# require(fUnitRoots)
# require(lattice)
# require(grid)



# 1. loading required data   ####
#__________________________


# Constructing path of relevant directories
root <- getwd()
path_data <- paste(root, "/", "input", sep="")
#path_data <- paste(root, "/", "data", sep="")
path_helpers <- paste(root, "/R", sep="")
#path_helpers <- paste(root, "/codes/helpers", sep="")
path_meta <- paste(root, "/", "meta", sep="")


file_path <- paste(path_data, "/testdata.rds", sep="")
system.time(df_testdata <-readRDS(file = file_path))
str(df_testdata)



# use parse_time in order to create standard ambiguous date format.
df_testdata <- df_testdata %>%
  mutate(date = as.Date(date , format = "%Y-%m-%d"))

str(df_testdata)

# subset the data frame 
df_filter <- df_testdata %>%
  filter( client_id == "client_0",
          machine_id == "M_001") %>%
  dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price) %>% 
  relocate(dynamic_price, .before = lot_health_index)

class(df_filter)

# 2. Explorating the time series   ####
#_____________________________


# Converting a data.frame into mts = Multivariate Time Series
mts_df_filter <- stats::ts(df_filter,
                           frequency = 12,
                           start = c(2001, 1),
                           end = c(2021, 12))




class(mts_df_filter)
# [1] "mts"    "ts"     "matrix"

head(mts_df_filter)

str(mts_df_filter)

colnames(mts_df_filter)
# [1] "dynamic_price"            "lot_health_index"         "avg_market_premium_price"

fig1 <- autoplot(mts_df_filter[,"lot_health_index"], colour = "#7879FF") +
  ggtitle("") +
  xlab("Year") +
  ylab("Iot health index") #+
#theme_bw()

plotly::ggplotly(fig1)

fig2 <- autoplot(mts_df_filter[,"dynamic_price"], colour = "#B03F3C") +
  ggtitle("") +
  xlab("Year") +
  ylab("Dynamic price")

plotly::ggplotly(fig2)

fig3 <- autoplot(mts_df_filter[,"avg_market_premium_price"]) +
  ggtitle("") +
  xlab("Year") +
  ylab("Avg market premium price")

plotly::ggplotly(fig3)


# Seasonal Plot
forecast::ggseasonplot(mts_df_filter[,"lot_health_index"], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Iot health index") +
  ggtitle("Seasonal plot: Iot health index")

# Seasonal Plot
forecast::ggseasonplot(mts_df_filter[,"dynamic_price"], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Dynamic price") +
  ggtitle("Seasonal plot: Dynamic price")

# Seasonal Plot
forecast::ggseasonplot(mts_df_filter[,"avg_market_premium_price"], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Avg market premium pric") +
  ggtitle("Seasonal plot: Avg market premium pric")


# Scatterplot matrices
autoplot(mts_df_filter, facets=TRUE) +
  ylab("")

GGally::ggpairs(as.data.frame(mts_df_filter))

# lag plots
forecast::gglagplot(mts_df_filter[,"lot_health_index"])

# Autocorrelation
forecast::ggAcf(mts_df_filter[,"lot_health_index"])


forecast::gglagplot(mts_df_filter[,"dynamic_price"])

# Autocorrelation
forecast::ggAcf(mts_df_filter[,"dynamic_price"])


forecast::gglagplot(mts_df_filter[,"avg_market_premium_price"])

# Autocorrelation
forecast::ggAcf(mts_df_filter[,"avg_market_premium_price"])

# 3. Forecaster Toolbox  ####
#________________________

# Set training data from 1992 to 2007

# df_filter2 <- df_filter %>%
#   dplyr::select(lot_health_index)  %>%
#   head(240)



str(df_filter2)

# Converting a data.frame into mts = Multivariate Time Series
ausbeer <- stats::ts(df_filter,
              frequency = 12,
              start = c(2001, 1))

ausbeer <- stats::ts(df_filter,
              frequency = 12,
              start = c(2001, 1),
              end = c(2020, 12))

class(ausbeer)
str(ausbeer)


beer2 <- stats::window(ausbeer,start=2003,end=c(2020,12))
class(beer2)
autoplot(beer2)

str(beer2)


# Plot some forecasts (h = forecast horizon) for a seasonal time series
forecast::autoplot(beer2[,"dynamic_price"]) +
  forecast::autolayer(meanf(beer2[,"dynamic_price"], h=12),
                      series="Mean", PI=FALSE) +
  autolayer(naive(beer2[,"dynamic_price"], h=12),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(beer2[,"dynamic_price"], h=12),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for monthly Dynamic Price") +
  xlab("Year") + ylab("Dynamic Price") +
  theme(plot.title = element_text(hjust = 0.5)) +     # Center ggplot title
  guides(colour=guide_legend(title="Forecast"))

#plotly::ggplotly(fig4)


# Plot some forecasts (h = forecast horizon) for a seasonal time series
forecast::autoplot(beer2[,"lot_health_index"]) +
  forecast::autolayer(meanf(beer2[,"lot_health_index"], h=12),
                      series="Mean", PI=FALSE) +
  autolayer(naive(beer2[,"lot_health_index"], h=12),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(beer2[,"lot_health_index"], h=12),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for monthly Iot health index") +
  xlab("Year") + ylab("Iot health index") +
  theme(plot.title = element_text(hjust = 0.5)) +     # Center ggplot title
  guides(colour=guide_legend(title="Forecast"))

#plotly::ggplotly(fig4)


# Plot some forecasts (h = forecast horizon) for a seasonal time series
forecast::autoplot(beer2[,"avg_market_premium_price"]) +
  forecast::autolayer(meanf(beer2[,"lot_health_index"], h=12),
                      series="Mean", PI=FALSE) +
  autolayer(naive(beer2[,"avg_market_premium_price"], h=12),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(beer2[,"avg_market_premium_price"], h=12),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for monthly Avg market premium price") +
  xlab("Year") + ylab("Avg market premium price") +
  theme(plot.title = element_text(hjust = 0.5)) +     # Center ggplot title
  guides(colour=guide_legend(title="Forecast"))

#plotly::ggplotly(fig4)

# Plot some forecasts (h = forecast horizon) for a non-seasonal time series

forecast::autoplot(beer2[,"dynamic_price"], size = 0.8) +
  autolayer(meanf(beer2[,"dynamic_price"], h=24),
            series="Mean", PI=FALSE) +
  autolayer(rwf(beer2[,"dynamic_price"], h=24),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(beer2[,"dynamic_price"], drift=TRUE, h=24),
            series="Drift", PI=FALSE) +
  ggtitle("Forecasts for monthly Dynamic price") +
  xlab("Year") + ylab("Dynamic price") +
  theme(plot.title = element_text(hjust = 0.5)) +     # Center ggplot title
  guides(colour=guide_legend(title="Forecast"))

autoplot(beer2[,"dynamic_price"]) +
  autolayer(rwf(beer2[,"dynamic_price"], drift=TRUE, h=12),
            series="Drift", PI=TRUE) +
  ggtitle("Forecasts for monthly Dynamic price") +
  xlab("Year") + ylab("Dynamic price") +
  theme(plot.title = element_text(hjust = 0.5)) +     # Center ggplot title
  guides(colour=guide_legend(title="Forecast"))

# Plot some forecasts (h = forecast horizon) for a non-seasonal time series

forecast::autoplot(beer2[,"lot_health_index"], size = 0.8) +
  autolayer(meanf(beer2[,"lot_health_index"], h=12),
            series="Mean", PI=FALSE) +
  autolayer(rwf(beer2[,"lot_health_index"], h=12),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(beer2[,"lot_health_index"], drift=TRUE, h=12),
            series="Drift", PI=FALSE) +
  ggtitle("Forecasts for monthly iot health index") +
  xlab("Year") + ylab("Iot health index") +
  theme(plot.title = element_text(hjust = 0.5)) +     # Center ggplot title
  guides(colour=guide_legend(title="Forecast"))

autoplot(beer2[,"lot_health_index"]) +
  autolayer(rwf(beer2[,"lot_health_index"], drift=TRUE, h=12),
            series="Drift", PI=TRUE) +
  ggtitle("Forecasts for monthly Iot health index") +
  xlab("Year") + ylab("Iot health index") +
  theme(plot.title = element_text(hjust = 0.5)) +     # Center ggplot title
  guides(colour=guide_legend(title="Forecast"))

# Plot some forecasts (h = forecast horizon) for a non-seasonal time series

forecast::autoplot(beer2[,"avg_market_premium_price"], size = 0.8) +
  autolayer(meanf(beer2[,"avg_market_premium_price"], h=12),
            series="Mean", PI=FALSE) +
  autolayer(rwf(beer2[,"avg_market_premium_price"], h=12),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(beer2[,"avg_market_premium_price"], drift=TRUE, h=12),
            series="Drift", PI=FALSE) +
  ggtitle("Forecasts for monthly avg market premium price") +
  xlab("Year") + ylab("Avg market premium price") +
  theme(plot.title = element_text(hjust = 0.5)) +     # Center ggplot title
  guides(colour=guide_legend(title="Forecast"))

autoplot(beer2[,"avg_market_premium_price"]) +
  autolayer(rwf(beer2[,"avg_market_premium_price"], drift=TRUE, h=12),
            series="Drift", PI=TRUE) +
  ggtitle("Forecasts for monthly avg market premium price") +
  xlab("Year") + ylab("Avg market premium price") +
  theme(plot.title = element_text(hjust = 0.5)) +     # Center ggplot title
  guides(colour=guide_legend(title="Forecast"))

# calendar adjustments

autoplot(mts_df_filter, facet=TRUE) +
  xlab("Years") + ylab("") +
  ggtitle("Milk production per cow")

# Box-cox Transformation
(lambda <- BoxCox.lambda(beer2[,"dynamic_price"])) # find optimal lambda
#> [1] 1.999924
autoplot(BoxCox(beer2[,"dynamic_price"],lambda))

# Box-cox Transformation
(lambda <- BoxCox.lambda(beer2[,"avg_market_premium_price"])) # find optimal lambda
#> [1] 1.999924
autoplot(BoxCox(beer2[,"avg_market_premium_price"],lambda))


# Features of power transformations
# Choose a simple value of  λ. It makes explanations easier.
# The forecasting results are relatively insensitive to the value of  λ.
# Often no transformation is needed.
# Transformations sometimes make little difference to the forecasts but have a 
# large effect on prediction intervals.


fc <- rwf(beer2[,"dynamic_price"], drift=TRUE, lambda=0, h=12, level=80)
fc2 <- rwf(beer2[,"dynamic_price"], drift=TRUE, lambda=0, h=12, level=80,
           biasadj=TRUE)

autoplot(beer2[,"dynamic_price"]) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))

fc <- rwf(beer2[,"avg_market_premium_price"], drift=TRUE, lambda=0, h=12, level=80)
fc2 <- rwf(beer2[,"avg_market_premium_price"], drift=TRUE, lambda=0, h=12, level=80,
           biasadj=TRUE)

autoplot(beer2[,"lot_health_index"]) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))

# A good forecasting method will yield residuals with the following properties:
#   
# 1- The residuals are uncorrelated. If there are correlations between residuals,
# then there is information left in the residuals which should be used in 
# computing forecasts.
# 
# 2- The residuals have zero mean. If the residuals have a mean other than zero, 
# then the forecasts are biased.

# In addition to these essential properties, it is useful (but not necessary) for 
# the residuals to also have the following two properties.
# 
# 3- The residuals have constant variance.
# 4- The residuals are normally distributed.

mts_df_filter[,"dynamic_price"]

autoplot(mts_df_filter[,"dynamic_price"]) +
  xlab("Years") + ylab("Dynamic price") +
  ggtitle("")


res <- residuals(naive(mts_df_filter[,"dynamic_price"]))
autoplot(res) + xlab("Years") + ylab("") +
  ggtitle("")

gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of residuals")

# Portmanteau Tests of autocorrelation
# lag=h and fitdf=K
Box.test(res, lag=6, fitdf=0)

# Box-Pierce test
# 
# data:  res
# X-squared = 9.4544, df = 6, p-value = 0.1496

Box.test(res,lag=6, fitdf=0, type="Lj")

# Box-Ljung test
# 
# data:  res
# X-squared = 9.656, df = 6, p-value = 0.1399
 
#> # R-function checkresiduals
checkresiduals(naive(mts_df_filter[,"dynamic_price"]))

##

#mts_df_filter[,"dynamic_price"]

autoplot(mts_df_filter[,"lot_health_index"]) +
  xlab("Years") + ylab("Iot health index") +
  ggtitle("")


res <- residuals(naive(mts_df_filter[,"lot_health_index"]))
autoplot(res) + xlab("Years") + ylab("") +
  ggtitle("")

gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of residuals")

# Portmanteau Tests of autocorrelation
# lag=h and fitdf=K
Box.test(res, lag=6, fitdf=0)

# Box-Pierce test
# 
# data:  res
# X-squared = 9.4544, df = 6, p-value = 0.1496

Box.test(res,lag=6, fitdf=0, type="Lj")

# Box-Ljung test
# 
# data:  res
# X-squared = 9.656, df = 6, p-value = 0.1399

#> # R-function checkresiduals
checkresiduals(naive(mts_df_filter[,"lot_health_index"]))




# Standard exploratory tools
plot(mts_df_filter, main = "")




apply(mts_df_filter, 2, adf.test)
# Warning in FUN(newX[, i], ...): p-value smaller than printed p-value

# Alternative: lib fUnitRoots, function
apply(mts_df_filter, 2, adfTest, 
      lags=0, #maximum number of lags used for error term correction
      type="c", #type of unit root regression
      title = "ADF Test for mts_df_filter Data") #title of the project

# Warning in FUN(newX[, i], ...): p-value smaller than printed p-value

# Differencing the whole mts
stnry1 <- diffM(mts_df_filter) #difference operation on a vector of time series. Default order of differencing is 1.

#stnry2 <- log1p(mts_df_filter) #difference operation on a vector of time series. Default order of differencing is 1.

#log.test <- log1p(stnry1)

# Retest
apply(stnry1, 2, adf.test)

## VAR modeling
plot.ts(stnry1)

# Lag order identification
#We will use two different functions, from two different packages to identify the lag order for the VAR model. Both functions are quite similar to each other but differ in the output they produce. vars::VAR is a more powerful and convinient function to identify the correct lag order. 
VARselect(stnry1, 
          type = "none", #type of deterministic regressors to include. We use none becasue the time series was made stationary using differencing above. 
          lag.max = 10) #highest lag order

# $selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 1      1      1      9 
# 
# $criteria
#                   1             2            3             4             5             6             7             8             9            10
# AIC(n) -4.397895e+01           NaN          NaN -4.381314e+01           NaN           NaN           NaN           NaN           NaN           NaN
# HQ(n)  -4.392468e+01           NaN          NaN -4.359607e+01           NaN           NaN           NaN           NaN           NaN           NaN
# SC(n)  -4.384442e+01           NaN          NaN -4.327501e+01           NaN           NaN           NaN           NaN           NaN           NaN
# FPE(n)  7.946690e-20 -1.306664e-19 -1.09841e-19  9.382509e-20 -1.496559e-19 -1.587668e-19 -2.122166e-20 -1.111979e-19 -1.604727e-19 -4.902965e-20

# Lag order identification
#We will use two different functions, from two different packages to identify the lag order for the VAR model. Both functions are quite similar to each other but differ in the output they produce. vars::VAR is a more powerful and convinient function to identify the correct lag order. 
# VARselect(log1p(stnry1), 
#           type = "none", #type of deterministic regressors to include. We use none becasue the time series was made stationary using differencing above. 
#           lag.max = 10) #highest lag order

# Lag order identification
#We will use two different functions, from two different packages to identify the lag order for the VAR model. Both functions are quite similar to each other but differ in the output they produce. vars::VAR is a more powerful and convinient function to identify the correct lag order.
VARselect(log1p(mts_df_filter),
                  type = "none", #type of deterministic regressors to include. We use none becasue the time series was made stationary using differencing above.
                  lag.max = 10) #highest lag order


# Creating a VAR model with vars
var.a.mts_df_filter <- vars::VAR(stnry1,
                                 lag.max = 10, #highest lag order for lag length selection according to the choosen ic
                                 ic = "AIC", #information criterion
                                 type = "none") #type of deterministic regressors to include


# Creating a VAR model with vars
var.a.mts_df_filter <- vars::VAR(log1p(mts_df_filter),
                   lag.max = 1, #highest lag order for lag length selection according to the choosen ic
                   ic = "AIC", #information criterion
                   type = "none") #type of deterministic regressors to include

# var.a.mts_df_filter <- vars::VAR(mts_df_filter,
#                                  lag.max = 8, #highest lag order for lag length selection according to the choosen ic
#                                  ic = "AIC", #information criterion
#                                  type = "none") #type of deterministic regressors to include

#rapply( var.a.mts_df_filter, f=function(x) ifelse(is.nan(x),0,x), how="replace" )

summary(var.a.mts_df_filter) 

serial.test(var.a.mts_df_filter)

var.a.mts_df_filter$datamat


#selecting the variables ot_health_index, dynamic_price,avg_market_premium_price
# Granger test for causality
#for causality function to give reliable results we need all the variables of the multivariate time series to be stationary. 
causality(var.a.mts_df_filter, #VAR model
          cause = c("dynamic_price")) #cause variable. If not specified then first column of x is used. Multiple variables can be used. 

# we forecast over a short horizon because beyond short horizon prediction becomes unreliable or uniform
fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = 100)
#par(mfrow = c(0,0))
par(mar = c(2.5,2.5,2.5,2.5))
#par(mfrow = c(2, 2))
plot(fcast1)


# Forecasting the DAX index
dynamic_price = fcast1$fcst[1]; dynamic_price # type list

# Extracting the forecast column
xx = dynamic_price$dynamic_price[,1]
xx
var.a.mts_df_filter$datamat[, "dynamic_price"]

# Inverting the differencing
#To get the data to the original scale we invert the time series
#since the values are just difference from the previous value, to get the values on the original scale we add the last value from the DAX time series to the predicted values.
#the plot of the predicted values will also show that over longer horizon the predicted values are not reliable
tail(mts_df_filter)
xx = cumsum(xx) + 214.8883 
par(mar = c(2.5,2.5,1,2.5)) #bottom, left, top, and right
plot.ts(xx)


# Adding data and forecast to one time series
dynamic_price_inv =ts(c(df_filter[,1], xx),
           start = c(2001,1),
           end = c(2021,12),
           frequency = 12)
plot(dynamic_price_inv)

#[181:252] #[1:180]

dynamic_price_inv_dataframe <- as.data.frame(dynamic_price_inv) 
colnames(dynamic_price_inv_dataframe) <- c("x")
head(dynamic_price_inv_dataframe)

ggplot() + 
  geom_line(data = as.data.frame(dynamic_price_inv_dataframe[1:180,]), aes(y = get("dynamic_price_inv_dataframe[1:180, ]"), x = seq(1, 180)), color = "green") +
  geom_line(data = as.data.frame(dynamic_price_inv_dataframe[181:252,]), aes(y = get("dynamic_price_inv_dataframe[181:252, ]"), x = seq(181, 252)), color = "red") +
  ggtitle("Plot of forecast of the VAR model on  time series") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("Value")


## Creating an advanced plot with visual separation
# Converting to object zoo
x = zoo(dynamic_price_inv)

# Advanced xyplot from lattice
xyplot(xx, grid=TRUE, panel = function(x, y, ...){
  panel.xyplot(x, y, col="red", ...)
  grid.clip(x = unit(181, "native"), just=c("right"))
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
autoplot(dynamic_price_inv) +
  ggtitle("Time Series Plot of the dynamic price Time-Series") +
  theme(plot.title = element_text(hjust = 0.5))




tsibble_train_df_testdata <- tsibble::as_tsibble(train_df_testdata, index = date)

tsibble_test_df_testdata <- tsibble::as_tsibble(test_df_testdata, index = date)

tamp1 <- tsibble_train_df_testdata[, c(4, 5)]
#tamp1 <- tsibble::as_tsibble(tsibble_train_df_testdata[,  7])

tamp2 <- tsibble_test_df_testdata[, c(4, 5)]
#tamp2 <- tsibble_test_df_testdata[,  7]

hm_naive <- snaive(tamp1, h = 50)
autoplot(hm_naive) +
  autolayer(fitted(hm_naive)) +
  autolayer(tamp2, color = rgb(0, 0, 0, 0.6)) +
  scale_x_continuous(breaks = 2001:2021) +
  labs(x = "year", y = "Dynamic Prices")

# check the residus

forecast::checkresiduals(hm_naive)




