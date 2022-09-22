# 0. Setting and loading packages  ####
#_______________________________

# book link
# https://bookdown.org/singh_pratap_tejendra/intro_time_series_r/multivariate-ts-analysis.html

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



# Constructing path of relevant directories
root <- getwd()
#path_data <- paste(root, "/", "input", sep="")
path_input <- file.path(root, "input")
path_helpers <- paste(root, "/R", sep="")
#path_helpers <- paste(root, "/codes/helpers", sep="")
path_meta <- paste(root, "/", "meta", sep="")

path_sqlite <- file.path(path_input, "dyn_pricing_db.sqlite3")

# connect to the sqlite database
con <- DBI::dbConnect(drv=RSQLite::SQLite(), dbname = path_sqlite)
## list all tables
tables <- DBI::dbListTables(con)
## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]
tables
# load customer data into R-environment
df_testdata <- DBI::dbGetQuery(conn = con,
                               statement=paste("SELECT * FROM input_data_final", sep=""))

customers_data_init <- DBI::dbGetQuery(conn = con,
                                       statement=paste("SELECT * FROM input_data_final_scaled", sep=""))

dbDisconnect(con)


# use parse_time in order to create standard ambiguous date format.
df_testdata <- df_testdata %>%
  mutate(date = as.Date(date , format = "%Y-%m-%d"),
         year_ = lubridate::year(date)) 

# rescaling the variable lot_health_index 

# changing the scale initial vector
rescaled_lot_health_index <- scales::rescale(df_testdata$lot_health_index, to = c(20, 100))

# replace the variable lot_health_index through rescaled variable rescaled_lot_health_index
df_testdata$lot_health_index <- rescaled_lot_health_index


str(df_testdata)
# gpr, industry_demand_forecast, production_index, iot_health_index, tempertature, inflation_rate
# industry_demand_forecast (DEU_dmd), production_index(DEU_prd_index), duration_customer_relation
# not good company_size
# subset the data frame 
df_filter2 <- df_testdata %>%
  filter( client_id == "client_0",
          machine_id == "M_001") %>%
  dplyr::select(dynamic_pricing, lot_health_index, GPR, avg_temperature_month, inflation_rate, DEU_dmd, date, year_)
class(df_filter2)



# rescaling the variable lot_health_index

# changing the scale initial vector
# rescaled_lot_health_index <- scales::rescale(df_filter2$lot_health_index, to = c(20, 100))
# 
# # replace the variable lot_health_index through rescaled variable rescaled_lot_health_index
# df_filter2$lot_health_index <- rescaled_lot_health_index

class(df_filter2)

mts_df_filter2 <- stats::ts(df_filter2[- c(7,8)],
                            frequency = 12,
                            start = c(min(df_filter2$year_), 1),
                            end = c(max(df_filter2$year_), 12))


class(mts_df_filter2)
# [1] "mts"    "ts"     "matrix"

head(mts_df_filter2)
tail(mts_df_filter2)
# min(df_testdata$year_)
# max(df_testdata$year_)
str(mts_df_filter2)


# Standard exploratory tools

#par(mar = c(2.5,2.5,2.5,2.5))
plot(mts_df_filter2, main = "")

# visualization with autoplot
ggplot2::theme_set(theme_bw()) 
autoplot(mts_df_filter2) +
  ggtitle("Time Series Plot of the Data Frame' Time-Series") +
  theme(plot.title = element_text(hjust = 0.5)) #for centering the text


# Main packages - problem: both have different functions VAR
## Testing for stationarity
### tseries - standard test adt.test
apply(mts_df_filter2, 2, tseries::adf.test)
#p-value = 0.06213  > 0.05 we conclude that the time series is nonstationary

# Alternative: lib fUnitRoots, function
apply(mts_df_filter2, 2, fUnitRoots::adfTest, 
      lags=0, #maximum number of lags used for error term correction
      type="c", #type of unit root regression
      title = "ADF Test for mts_df_filter2 Data") #title of the project

# p-value = 0.06213 > 0.05 we conclude that the time series is nonstationary

# Differencing the whole mts
stnry1 <- MTS::diffM(mts_df_filter2) #difference operation on a vector of time series. Default order of differencing is 1.

# Error in vars::VAR(stnry1, lag.max = 10, ic = "AIC", type = "none") : 
#   The matrix 'y' should contain at least two variables. For univariate analysis consider ar() and arima() in package stats.

# Retest
apply(stnry1, 2, tseries::adf.test)

## VAR modeling
stats::plot.ts(stnry1)

stnry2 <- log1p(mts_df_filter2)

#log.test <- log1p(stnry1)

# Retest
apply(stnry2, 2, tseries::adf.test)

## VAR modeling
stats::plot.ts(stnry2)



# Lag order identification
#We will use two different functions, from two different packages to identify the lag order for the VAR model. Both functions are quite similar to each other but differ in the output they produce. vars::VAR is a more powerful and convinient function to identify the correct lag order.
# vars::VARselect(log1p(mts_df_filter2),
#                 type = "none", #type of deterministic regressors to include. We use none becasue the time series was made stationary using differencing above.
#                 lag.max = 10) #highest lag order

vars::VARselect(stnry1,
                type = "none", #type of deterministic regressors to include. We use none becasue the time series was made stationary using differencing above.
                lag.max = 10) #highest lag order
# 
# $selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 9      2      1      9 
# 
# $criteria
# 1            2            3            4            5            6            7            8            9           10
# AIC(n) 3.652188e+01 3.602029e+01 3.589391e+01 3.561569e+01 3.532087e+01 3.516112e+01 3.501011e+01 3.498132e+01 3.470757e+01 3.476001e+01
# HQ(n)  3.679245e+01 3.656143e+01 3.670562e+01 3.669797e+01 3.667372e+01 3.678454e+01 3.690410e+01 3.714587e+01 3.714269e+01 3.746571e+01
# SC(n)  3.718861e+01 3.735374e+01 3.789408e+01 3.828259e+01 3.865449e+01 3.916147e+01 3.967718e+01 4.031511e+01 4.070809e+01 4.142725e+01
# FPE(n) 7.266571e+15 4.405919e+15 3.896192e+15 2.969854e+15 2.236487e+15 1.938853e+15 1.707647e+15 1.714121e+15 1.360583e+15 1.514564e+15



# Creating a VAR model with vars
var.a.mts_df_filter2 <- vars::VAR(stnry1,
                                  lag.max = 9, #highest lag order for lag length selection according to the choosen ic
                                  ic = "AIC", #information criterion
                                  type = "none") #type of deterministic regressors to include


summary(var.a.mts_df_filter2)

# Residual diagnostics
#serial.test function takes the VAR model as the input
vars::serial.test(var.a.mts_df_filter2)

#var.a.mts_df_filter2$datamat


#selecting the variables ot_health_index, dynamic_price,avg_market_premium_price
# Granger test for causality
#for causality function to give reliable results we need all the variables of the multivariate time series to be stationary. 
vars::causality(var.a.mts_df_filter2, #VAR model
                cause = c("lot_health_index")) #cause variable. If not specified then first column of x is used. Multiple variables can be used. 

# we forecast over a short horizon because beyond short horizon prediction becomes unreliable or uniform
fcast1 = stats::predict(var.a.mts_df_filter2, n.ahead = 12,level=c(80,95))
par(mar = c(2.5,2.5,2.5,2.5))
plot(fcast1)

# Forecasting the lot_health_index
lot_health = fcast1$fcst[2] # type list

# Extracting the forecast column
x_predict. = lot_health$lot_health_index[,1]

# Inverting the differencing
#To get the data to the original scale we invert the time series
#since the values are just difference from the previous value, to get the values on the original scale we add the last value from the DAX time series to the predicted values.
#the plot of the predicted values will also show that over longer horizon the predicted values are not reliable
# test <- tail(mts_df_filter2[, "dynamic_price"])[6]
# x2 = cumsum(x) + test 
# par(mar = c(2.5,2.5,1,2.5)) #bottom, left, top, and right
# plot.ts(x2)
# x2 = exp(x) - 1

# Inverting the differencing
#To get the data to the original scale we invert the time series
#since the values are just difference from the previous value, to get the values on the original scale we add the last value from the DAX time series to the predicted values.
#the plot of the predicted values will also show that over longer horizon the predicted values are not reliable

# Extract the last values from the variable dynamic_pricing
yy <- tail(mts_df_filter2)[,2][6]
#y <- test[,1][6]
x2_predict. = cumsum(x_predict.) + yy
par(mar = c(2.5,2.5,1,2.5)) #bottom, left, top, and right
plot.ts(x2_predict.)

# Adding data and forecast to one time series
lot_health_inv <- ts(c(df_filter2[,2], x2_predict.),
                   start = c(min(df_filter2$year_),1),
                   end = c(max(df_filter2$year_),12),
                   frequency = 12)
#plot(dynamic_price_inv22)

#convert lot_health_inv to data frame and set a name from the column 
lot_health_inv_dataframe <- as.data.frame(lot_health_inv) 
colnames(lot_health_inv_dataframe) <- c("x")
#head(dynamic_price_inv_dataframe)

# extract the date from the time series lot_health_inv
date_ts <- as.Date(time(lot_health_inv))


#choose a part of data frame for each forecast period
nbr_row1 <- nrow(lot_health_inv_dataframe)
n_period1 <- 12

n_11 <- nbr_row1 - n_period1
n_22 <- nbr_row1 - n_period1 + 1

# Create confidence interval
date_ci1 <- date_ts[n_22:nbr_row1]
#lower1 <- round((x2_predict. - 1.95), 2)
#upper1 <- round((x2_predict. + 1.95), 2)

lower1 <- round((lot_health$lot_health_index[,2] + yy), 2)
upper1 <- round((lot_health$lot_health_index[,3] + yy), 2)

plot_lot_health_index <- plot_ly() 
plot_lot_health_index <- plot_lot_health_index %>% add_trace(data = as.data.frame(lot_health_inv_dataframe[1:n_11,]),  
                                                             x = ~date_ts[1: n_11], y = ~get("lot_health_inv_dataframe[1:n_11, ]"),
                                                             mode = 'lines', line = list(color = "#6495ED"),
                                                             hovertext = paste('Date: ',"<b>",date_ts[1: n_11], "</b>",
                                                                               '<br>Iot health index:',"<b>",
                                                                               round(lot_health_inv_dataframe[1:n_11,],2), "</b>"),
                                                             hoverinfo = 'text') 
plot_lot_health_index <- plot_lot_health_index %>% add_trace(data = as.data.frame(lot_health_inv_dataframe[n_22:nbr_row1,]),  
                                                             x = ~date_ts[n_22:nbr_row1], y = ~get("lot_health_inv_dataframe[n_22:nbr_row1, ]"),
                                                             mode = 'lines', line = list(color = "#ff0000"),
                                                             hovertext = paste('Date: ',"<b>",date_ts[n_22:nbr_row1], "</b>",
                                                                               '<br>Predicted iot health index:',"<b>",
                                                                               round(lot_health_inv_dataframe[n_22:nbr_row1,],2), "</b>"),
                                                             hoverinfo = 'text', showlegend = F)
# Confidence interval with the function add_ribbons
plot_lot_health_index <- plot_lot_health_index %>% add_ribbons(x = date_ci1,
                                                               ymin = lower1,
                                                               ymax = upper1,
                                                               color = I("#87A2FB"),
                                                               showlegend = F,
                                                               hoverinfo="text",
                                                               hovertext = paste('Date:',"<b>",date_ci1, "</b> \n",
                                                                                 '95%_Upper:',"<b>",upper1, "</b> \n",
                                                                                 '95%_Lower:',"<b>",lower1, "</b>"))
plot_lot_health_index <- plot_lot_health_index %>% layout(title = "",
                                                          xaxis = list(title = "Date"),
                                                          yaxis = list (title = ""))
plot_lot_health_index <- plot_lot_health_index %>%
  config(displayModeBar = T, displaylogo = FALSE, modeBarButtonsToRemove = list(
    'sendDataToCloud',
    #'toImage',
    #'autoScale2d',
    'toggleSpikelines',
    'resetScale2d',
    'lasso2d',
    'zoom2d',
    'pan2d',
    'select2d'#,
    #'hoverClosestCartesian'#,
    #'hoverCompareCartesian'
  ),
  scrollZoom = T)

plot_lot_health_index
