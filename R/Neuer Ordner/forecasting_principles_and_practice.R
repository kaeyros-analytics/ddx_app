# Forecasting:Principles and Practice

# 0. Loading required packages  ####
#______________________________

library("RSQLite")
library(tidyverse)
library(tidymodels)
library(data.table)
library(tidyposterior)
library(tsibble)       # tibble for time series based on tidy principles
library(fable)         # for forecasting based on tidy principles
library(ggfortify)     # for plotting time series
library(forecast)      # for forecast function
library(tseries)
library(chron)
library(lubridate)
library(directlabels)
library(zoo)
library(lmtest)
library(TTR)  #for smoothing the time series
library(MTS)
library(vars)
library(fUnitRoots)
library(lattice)
library(grid)
library(fpp2)


# 1. Getting started  ####
#___________________


# 2. Time series graphics  ####
#________________________

# generate sample data
y <- stats::ts(c(123,39,78,52,110), start=2012)
class(y) # "ts"

y <- ts(c(123,39,78,52,110), start=2003, frequency=12)


# times series plots

# get some data
path.data <- file.path(getwd(), "data")
load(file=paste0(path.data, "/melsyd.rda"))
class(melsyd)
# [1] "mts" "ts" 
head(melsyd)

 fig <- autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")

 plotly::ggplotly(fig)
 
 
 
 load(file=paste0(path.data, "/a10.rda"))
 class(a10)
 a10_df <- data.frame(date=as.Date(index(a10)), Y = melt(a10)$value)
 # [1] "mts" "ts" 
 head(a10)
 
 fig <- autoplot(a10) +
   ggtitle("Antidiabetic drug sales") +
   ylab("$ million") +
   xlab("Year")
 
 plotly::ggplotly(fig)
 
 
 # Seasonal Plot
 ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
   ylab("$ million") +
   ggtitle("Seasonal plot: antidiabetic drug sales")
 
 # Polar Seasonal Plot
 ggseasonplot(a10, polar=TRUE) +
   ylab("$ million") +
   ggtitle("Polar seasonal plot: antidiabetic drug sales")

 #Seasonal subseries plots
 ggsubseriesplot(a10) +
   ylab("$ million") +
   ggtitle("Seasonal subseries plot: antidiabetic drug sales")
 
 
 # Scatterplots
 load(file=paste0(path.data, "/elecdemand.rda"))
 class(elecdemand)
 autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) +
   xlab("Year: 2014") + ylab("") +
   ggtitle("Half-hourly electricity demand: Victoria, Australia")
 
 qplot(Temperature, Demand, data=as.data.frame(elecdemand)) +
   ylab("Demand (GW)") + xlab("Temperature (Celsius)")


 # Scatterplot matrices
 load(file=paste0(path.data, "/visnights.rda"))
 class(visnights)
 autoplot(visnights[,1:5], facets=TRUE) +
   ylab("Number of visitor nights each quarter (millions)")
 
 GGally::ggpairs(as.data.frame(visnights[,1:5]))
 

 # lag plots
 load(file=paste0(path.data, "/ausbeer.rda"))
 class(ausbeer)
 beer2 <- window(ausbeer, start=1992)
 gglagplot(beer2)
 
 # Autocorrelation
 ggAcf(beer2)
 
 # # Trend and seasonality in ACF plots
 # load(file=paste0(path.data, "/elec.rda"))
 # class(elec)
 # aelec <- window(elec, start=1980)
 # autoplot(aelec) + xlab("Year") + ylab("GWh")
 
 # White noise
 set.seed(30)
 y <- ts(rnorm(50))
 autoplot(y) + ggtitle("White noise")
 
 
 ggAcf(y)
 
 # Exercise: Analyse Autocorrelation
 
 # read data 
 tute1 <- read.csv2(paste0(path.data,"/tute1.csv"), header=TRUE)
 View(tute1)
 colnames(tute1) # [1] "Datum"    "Sales"    "AdBudget" "GDP"   
 
 # convert to times series
 mytimeseries <- ts(tute1[,-1], start=1981, frequency=4)
 mytimeseries
 
 # construct plot of each of the time series (all tseries togther in a single plot)
 autoplot(mytimeseries, facets=TRUE) # each time series in her plot 
 autoplot(mytimeseries, facets=FALSE) # all times series in a single plot
 

 # single plots ()
 autoplot(mytimeseries[,"Sales"])
 
 # read retail data
retaildata <- readxl::read_excel(paste0(path.data,"/retail.xlsx"), skip=1)


# create suitable ttime series object
myts <- ts(retaildata[,"A3349873A"],
           frequency=12, start=c(1982,4))
myts


autoplot(myts) # plotting the times series
ggseasonplot(myts) # visualizing seasonal effects
ggsubseriesplot(myts)
gglagplot(myts)
ggAcf(myts)


# 3. Forecaster Toolbox  ####
#________________________

# loading required data
load(file=paste0(path.data, "/ausbeer.rda"))
class(ausbeer)

# Set training data from 1992 to 2007
beer2 <- window(ausbeer,start=1956,end=c(2007,4))
autoplot(beer2)

# Plot some forecasts (h = forecast horizon) for a seasonal time series
autoplot(beer2) +
  autolayer(meanf(beer2, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(beer2, h=11),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(beer2, h=11),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))


# Plot some forecasts (h = forecast horizon) for a non-seasonal time series

# loading required data
load(file=paste0(path.data, "/goog200.rda"))
class(goog200)
goog200


autoplot(goog200) +
  autolayer(meanf(goog200, h=40),
            series="Mean", PI=FALSE) +
  autolayer(rwf(goog200, h=40),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(goog200, drift=TRUE, h=40),
            series="Drift", PI=FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))


autoplot(goog200) +
  autolayer(rwf(goog200, drift=TRUE, h=40),
            series="Drift", PI=TRUE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))


# calendar adjustments
dframe <- cbind(Monthly = milk,
                DailyAverage = milk/monthdays(milk))

autoplot(dframe, facet=TRUE) +
  xlab("Years") + ylab("Pounds") +
  ggtitle("Milk production per cow")


# Box-cox Transformation
(lambda <- BoxCox.lambda(elec)) # find optimal lambda
#> [1] 0.2654
autoplot(BoxCox(elec,lambda))

# Features of power transformations
# Choose a simple value of  λ. It makes explanations easier.
# The forecasting results are relatively insensitive to the value of  λ.
# Often no transformation is needed.
# Transformations sometimes make little difference to the forecasts but have a 
# large effect on prediction intervals.


fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80,
           biasadj=TRUE)

autoplot(eggs) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))

(lambda <- BoxCox.lambda(eggs)) # find optimal lambda
#> [1] 0.3956183
autoplot(BoxCox(eggs,lambda))

# Example: Forecasting the Google daily closing stock price

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

autoplot(goog200) +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")


res <- residuals(naive(goog200))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of residuals")


# Portmanteau Tests
# lag=h and fitdf=K
Box.test(res, lag=10, fitdf=0)
#> 
#>  Box-Pierce test
#> 
#> data:  res
#> X-squared = 11, df = 10, p-value = 0.4

Box.test(res,lag=10, fitdf=0, type="Lj")
#> 
#>  Box-Ljung test
#> 
#> data:  res
#> X-squared = 11, df = 10, p-value = 0.4
#> 
#> # R-function checkresiduals
checkresiduals(naive(goog200))


# Evaluating Forecast accuracy
data(ausbeer)
beer2 <- window(ausbeer,start=1992,end=c(2007,4))
beerfit1 <- meanf(beer2,h=10) # naive forecast
beerfit2 <- rwf(beer2,h=10) # random-walk forecast
beerfit3 <- snaive(beer2,h=10) # seasonal naive
beerfit4 <- rwf(beer2,h=10, drift = TRUE) # random-walk forecast + drift

autoplot(window(ausbeer, start=1992)) +
  autolayer(beerfit1, series="Mean", PI=FALSE) +
  autolayer(beerfit2, series="Naïve", PI=FALSE) +
  autolayer(beerfit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecasts for quarterly beer production") +
  guides(colour=guide_legend(title="Forecast"))

# with confidence intervals
autoplot(window(ausbeer, start=1992)) +
  #autolayer(beerfit1, series="Mean", PI=TRUE) +
  #autolayer(beerfit2, series="Naïve", PI=TRUE) +
  #autolayer(beerfit3, series="Seasonal naïve", PI=TRUE) +
  autolayer(beerfit4, series="Naïve", PI=TRUE) +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecasts for quarterly beer production") +
  guides(colour=guide_legend(title="Forecast"))

beer3 <- window(ausbeer, start=2008)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)


googfc1 <- meanf(goog200, h=40)
googfc2 <- rwf(goog200, h=40)
googfc3 <- rwf(goog200, drift=TRUE, h=40)
autoplot(subset(goog, end = 240)) +
  autolayer(googfc1, PI=FALSE, series="Mean") +
  autolayer(googfc2, PI=FALSE, series="Naïve") +
  autolayer(googfc3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google stock price (daily ending 6 Dec 13)") +
  guides(colour=guide_legend(title="Forecast"))

googtest <- window(goog, start=201, end=240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)


# Time Serioes cross Validation
e <- tsCV(goog200, rwf, drift=TRUE, h=1)
sqrt(mean(e^2, na.rm=TRUE))
#> [1] 6.233
sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm=TRUE))
#> [1] 6.169

# Example: using tsCV()

e <- tsCV(goog200, forecastfunction=naive, h=8)
# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()


e <- tsCV(goog200, forecastfunction=rwf, drift = TRUE,  h=8)
# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()


# prediction intervals
naive(goog200, 20)
rwf(goog200, h=10, drift = TRUE)
naive(goog200, 20)

autoplot(rwf(goog200, h=10, drift = TRUE))


# non-normally distributed resiuals --> bootstrap
naive(goog200, bootstrap=TRUE)
naive(goog200)


forecast(ausbeer, h=4)
#>         Point Forecast Lo 80 Hi 80 Lo 95 Hi 95
#> 2010 Q3          404.6 385.9 423.3 376.0 433.3
#> 2010 Q4          480.4 457.5 503.3 445.4 515.4
#> 2011 Q1          417.0 396.5 437.6 385.6 448.4
#> 2011 Q2          383.1 363.5 402.7 353.1 413.1


# 4. Judgmental Forecast  ####
#________________________


# 5. Time series regression models  ####
#___________________________________


autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")

uschange %>%
  as.data.frame() %>%
  ggplot(aes(x=Income, y=Consumption)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
#> `geom_smooth()` using formula 'y ~ x'


tslm(Consumption ~ Income, data=uschange)
#> 
#> Call:
#> tslm(formula = Consumption ~ Income, data = uschange)
#> 
#> Coefficients:
#> (Intercept)       Income  
#> 
#> 
#> 
#>       0.545        0.281
#>       

# Multiple linear regression
uschange %>%
  as.data.frame() %>%
  GGally::ggpairs()

autoplot(uschange, facets = TRUE)

# fitting the model
fit.consMR <- tslm(
  Consumption ~ Income + Production + Unemployment + Savings,
  data=uschange)
summary(fit.consMR)

autoplot(uschange[,'Consumption'], series="Data") +
  autolayer(fitted(fit.consMR), series="Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=" "))

cbind(Data = uschange[,"Consumption"],
      Fitted = fitted(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("Percent change in US consumption expenditure") +
  geom_abline(intercept=0, slope=1)

# check residuals
checkresiduals(fit.consMR)


# some useful predictors
fit.beer <- tslm(beer2 ~ trend + season)
summary(fit.beer)
#> 
#> Call:
#> tslm(formula = beer2 ~ trend + season)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -42.90  -7.60  -0.46   7.99  21.79 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 441.8004     3.7335  118.33  < 2e-16 ***
#> trend        -0.3403     0.0666   -5.11  2.7e-06 ***
#> season2     -34.6597     3.9683   -8.73  9.1e-13 ***
#> season3     -17.8216     4.0225   -4.43  3.4e-05 ***
#> season4      72.7964     4.0230   18.09  < 2e-16 ***
#> ---
#> Signif. codes:  
#> 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 12.2 on 69 degrees of freedom
#> Multiple R-squared:  0.924,  Adjusted R-squared:  0.92 
#> F-statistic:  211 on 4 and 69 DF,  p-value: <2e-16
#> 
#> 
#> 
#> # 
autoplot(beer2, series="Data") +
autolayer(fitted(fit.beer), series="Fitted") +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Quarterly Beer Production")


cbind(Data=beer2, Fitted=fitted(fit.beer)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted,
             colour = as.factor(cycle(beer2)))) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Quarterly beer production") +
  scale_colour_brewer(palette="Dark2", name="Quarter") +
  geom_abline(intercept=0, slope=1)



# Fourier series
fourier.beer <- tslm(beer2 ~ trend + fourier(beer2, K=2))
summary(fourier.beer)
#> 
#> Call:
#> tslm(formula = beer2 ~ trend + fourier(beer2, K = 2))
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -42.90  -7.60  -0.46   7.99  21.79 
#> 
#> Coefficients:
#>                           Estimate Std. Error t value
#> (Intercept)               446.8792     2.8732  155.53
#> trend                      -0.3403     0.0666   -5.11
#> fourier(beer2, K = 2)S1-4   8.9108     2.0112    4.43
#> fourier(beer2, K = 2)C1-4  53.7281     2.0112   26.71
#> fourier(beer2, K = 2)C2-4  13.9896     1.4226    9.83
#>                           Pr(>|t|)    
#> (Intercept)                < 2e-16 ***
#> trend                      2.7e-06 ***
#> fourier(beer2, K = 2)S1-4  3.4e-05 ***
#> fourier(beer2, K = 2)C1-4  < 2e-16 ***
#> fourier(beer2, K = 2)C2-4  9.3e-15 ***
#> ---
#> Signif. codes:  
#> 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 12.2 on 69 degrees of freedom
#> Multiple R-squared:  0.924,  Adjusted R-squared:  0.92 
#> F-statistic:  211 on 4 and 69 DF,  p-value: <2e-16
#> 
#> 
#> 
#> 

# forecasting with regression
beer2 <- window(ausbeer, start=1992)
fit.beer <- tslm(beer2 ~ trend + season)
fcast <- forecast(fit.beer)
autoplot(fcast) +
  ggtitle("Forecasts of beer production using regression") +
  xlab("Year") + ylab("megalitres")



fit.consBest <- tslm(
  Consumption ~ Income + Savings + Unemployment,
  data = uschange)
h <- 4
newdata <- data.frame(
  Income = c(1, 1, 1, 1),
  Savings = c(0.5, 0.5, 0.5, 0.5),
  Unemployment = c(0, 0, 0, 0))
fcast.up <- forecast(fit.consBest, newdata = newdata)
newdata <- data.frame(
  Income = rep(-1, h),
  Savings = rep(-0.5, h),
  Unemployment = rep(0, h))
fcast.down <- forecast(fit.consBest, newdata = newdata)

autoplot(uschange[, 1]) +
  ylab("% change in US consumption") +
  autolayer(fcast.up, PI = TRUE, series = "increase") +
  autolayer(fcast.down, PI = TRUE, series = "decrease") +
  guides(colour = guide_legend(title = "Scenario"))


# building predictive regressive models
fit.cons <- tslm(Consumption ~ Income, data = uschange)
h <- 4
fcast.ave <- forecast(fit.cons,
                      newdata = data.frame(
                        Income = rep(mean(uschange[,"Income"]), h)))
fcast.up <- forecast(fit.cons,
                     newdata = data.frame(Income = rep(5, h)))
autoplot(uschange[, "Consumption"]) +
  ylab("% change in US consumption") +
  autolayer(fcast.ave, series = "Average increase",
            PI = TRUE) +
  autolayer(fcast.up, series = "Extreme increase",
            PI = TRUE) +
  guides(colour = guide_legend(title = "Scenario"))

# Forecasting with a nonlinear trend

boston_men <- window(marathon, start=1924)
h <- 10
fit.lin <- tslm(boston_men ~ trend)
fcasts.lin <- forecast(fit.lin, h = h)
fit.exp <- tslm(boston_men ~ trend, lambda = 0)
fcasts.exp <- forecast(fit.exp, h = h)

t <- time(boston_men)
t.break1 <- 1950
t.break2 <- 1980
tb1 <- ts(pmax(0, t - t.break1), start = 1924)
tb2 <- ts(pmax(0, t - t.break2), start = 1924)

fit.pw <- tslm(boston_men ~ t + tb1 + tb2)
t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] + seq(h)
tb2.new <- tb2[length(tb2)] + seq(h)

newdata <- cbind(t=t.new, tb1=tb1.new, tb2=tb2.new) %>%
  as.data.frame()
fcasts.pw <- forecast(fit.pw, newdata = newdata)

fit.spline <- tslm(boston_men ~ t + I(t^2) + I(t^3) +
                     I(tb1^3) + I(tb2^3))
fcasts.spl <- forecast(fit.spline, newdata = newdata)

autoplot(boston_men) +
  autolayer(fitted(fit.lin), series = "Linear") +
  autolayer(fitted(fit.exp), series = "Exponential") +
  autolayer(fitted(fit.pw), series = "Piecewise") +
  autolayer(fitted(fit.spline), series = "Cubic Spline") +
  autolayer(fcasts.pw, series="Piecewise") +
  autolayer(fcasts.lin, series="Linear", PI=FALSE) +
  autolayer(fcasts.exp, series="Exponential", PI=FALSE) +
  autolayer(fcasts.spl, series="Cubic Spline", PI=FALSE) +
  xlab("Year") + ylab("Winning times in minutes") +
  ggtitle("Boston Marathon") +
  guides(colour = guide_legend(title = " "))

# Alternative formulation of cubic splines
boston_men %>%
  splinef(lambda=0) %>%
  autoplot()

# check residuals
boston_men %>%
  splinef(lambda=0) %>%
  checkresiduals()


# 6. Time series decomposition  ####
#_____________________________


# 7. Exponential Smoothing  ####
#________________________

oildata <- window(oil, start=1996)
autoplot(oildata) +
  ylab("Oil (millions of tonnes)") + xlab("Year")


oildata <- window(oil, start=1996)
# Estimate parameters
fc <- ses(oildata, h=5)
# Accuracy of one-step-ahead training errors
round(accuracy(fc),2)
#>               ME  RMSE   MAE MPE MAPE MASE  ACF1
#> Training set 6.4 28.12 22.26 1.1 4.61 0.93 -0.03
#> 


fitted(fc)
autoplot(fc) +
autolayer(fitted(fc), series="Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year")

# hoplt and damped holt - trend method
air <- window(ausair, start=1990)
fc <- holt(air, h=5)


fc <- holt(air, h=15)
fc2 <- holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(air) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))


autoplot(air) +
  #autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=TRUE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))

# example: sheep in Asia
autoplot(livestock) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, holt, damped=TRUE, h=1)
# Compare MSE:
mean(e1^2, na.rm=TRUE)
#> [1] 178.3
mean(e2^2, na.rm=TRUE)
#> [1] 173.4
mean(e3^2, na.rm=TRUE)
#> [1] 162.6
# Compare MAE:
mean(abs(e1), na.rm=TRUE)
#> [1] 8.532
mean(abs(e2), na.rm=TRUE)
#> [1] 8.803
mean(abs(e3), na.rm=TRUE)
#> [1] 8.024
#> 


# damped holt method

fc <- holt(livestock, damped=TRUE)
# Estimated parameters:
fc[["model"]]
#> Damped Holt's method 
#> 
#> Call:
#>  holt(y = livestock, damped = TRUE) 
#> 
#>   Smoothing parameters:
#>     alpha = 0.9999 
#>     beta  = 3e-04 
#>     phi   = 0.9798 
#> 
#>   Initial states:
#>     l = 223.35 
#>     b = 6.9046 
#> 
#>   sigma:  12.84
#> 
#>   AIC  AICc   BIC 
#> 427.6 429.7 438.7


# 8. ARIMA Model  ####
#________________

cbind("Sales ($million)" = a10,
      "Monthly log sales" = log(a10),
      "Annual change in log sales" = diff(log(a10),12)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Antidiabetic drug sales")


cbind("Billion kWh" = usmelec,
      "Logs" = log(usmelec),
      "Seasonally\n differenced logs" =
        diff(log(usmelec),12),
      "Doubly\n differenced logs" =
        diff(diff(log(usmelec),12),1)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Monthly US net electricity generation")


# uRoot Tests -- autoorrelation
library(urca)
goog %>% ur.kpss() %>% summary()

# ARIMA Modeling
elecequip %>% stl(s.window='periodic') %>% seasadj() -> eeadj
autoplot(eeadj)

eeadj %>% diff() %>% ggtsdisplay(main="")


# # find optimalö p. d , q

# p = order of the autoregressive part;
# d = degree of first differencing involved;
# q = order of the moving average part.

# order(p,d,q) means, that you have an ARIMA(p, d, q) 
# model: ϕ(B)(1−B)dXt=θ(B)Zt, where B is a lag operator and ϕ(B)=1−ϕ1B−⋯−ϕpBp 
# also θ(B)=1+θ1B+⋯+θqBq.
# 
# The best way to find p, d, q values in R is to use auto.arima function 
# from library(forecast). For example, auto.arima(x, ic = "aic"). For more 
# information look up ?auto.arima.

auto.arima(elecequip , ic = "aic")

(fit <- Arima(eeadj, order=c(3,1,1)))

checkresiduals(fit)

autoplot(forecast(fit))
