
rm(list = ls(all.names = TRUE)); # activate the step before execution !!!!!!
cat("\f");


# ---------------------------
# (02) SETTING USEFUL OPTIONS ####
# ---------------------------

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



#library(DT) # uses javascript data tables package to produce powerful and attractive data tables.



# Constructing path of relevant directories
root <- getwd()
path_data <- paste(root, "/", "input", sep="")
#path_data <- paste(root, "/", "data", sep="")
path_helpers <- paste(root, "/R", sep="")
#path_helpers <- paste(root, "/codes/helpers", sep="")
path_meta <- paste(root, "/", "meta", sep="")

# defining parameters
#lib_location <- .libPaths()[1]

# defining "package" parameters
# lib_location <- .libPaths()[1] 
# repo_location <-  "https://cloud.r-project.org"


# -----------------------------
# (02) LOADING REQUIRED PACKAGES ####0
# ------------------------------

# Loading required functions helper functions
source(paste(path_helpers, "/", "install_packages.R", sep=""))
source(paste(path_helpers, "/", "load_packages.R", sep=""))
#source(paste(path_helpers, "/", "get_db_credentials.R", sep=""))
# 
# # Launch function to install packages if necessary
# install_packages(root, lib_location, repo_location)
# 
# # Launch function to load required packages
# load_packages(path_meta, lib_location)
# 
# # Launch function to load required packages
# load_packages(path_meta, lib_location)
# 
# # Launch function to load required packages
# db_credentials <- get_db_credentials(path_meta)
# 
# 
# # --------------------------------------------------
# # (03) RETRIEVING REQUIRED CREDENTIALS FOR DB ACCESS ####
# # --------------------------------------------------
# 
# # loading file containing db credentials
# db_credentials <- get_db_credentials(path_meta)
# db_name_mpay <-  "mpay_db"
# 
# 
# # getting credentials to access the required database (mpay_db)
# db_type <- (db_credentials %>%
#                 filter(toupper(db_name) == toupper(db_name_mpay)) %>%
#                 dplyr::select(db_type) %>%
#                 collect())$db_type
# 
# db_host <- (db_credentials %>%
#                 filter(toupper(db_name) == toupper(db_name_mpay)) %>%
#                 dplyr:: select(db_host) %>%
#                 collect())$db_host
# 
# db_user <- (db_credentials %>%
#                 dplyr::filter(toupper(db_name) == toupper(db_name_mpay)) %>%
#                 dplyr::select(db_user) %>%
#                 collect())$db_user
# 
# db_pwd <- (db_credentials %>%
#                filter(toupper(db_name) == toupper(db_name_mpay)) %>%
#                dplyr::select(db_pwd) %>%
#                collect())$db_pwd
# 
# 
# 
# --------------------------------------
# (04) CONNECTING R TO THE MPAY DATABASE ####
# --------------------------------------

# # Getting the data file directory
# file_path <- paste(path_data, "/", "generated_db.sqlite3", sep = "")
# # # Here we only use the function dbConnect without the parameter dbname, host, username,
# # # password because we have already saved the SQL data into RStudio
# # # creating the connection to the database
# conn <- DBI::dbConnect(RSQLite::SQLite(), file_path)
# 
# # # getting list of tables available
# available_tables <- DBI::dbListTables(conn)

#=========================================
# (1) LOADING AND EXPLORING AVAILABLE DATA ####
#=========================================


# --------------------------
# (10) LOADING REQUIRED DATA ####
# --------------------------
# 
# tic()
# for(k in 1:length(available_tables)){
# 
#     form <- parse(text = paste(available_tables[k]," <- dbReadTable(con, '", available_tables[k], "')", sep=""))
#     eval(form)
# 
# }
# for(k in 1:length(available_tables)){
# 
# read the data frame from the sql database
# k <- 2
# form <- parse(text = paste(available_tables[k]," <- DBI::dbReadTable(conn, '", available_tables[k], "')", sep=""))
# eval(form)
# #k <- 2
# form <- parse(text = paste("saveRDS(", available_tables[k], ", file = '" , path_data, "/", available_tables[k], ".rds')", sep=""))
# eval(form)
# 
# }
# toc()  # 0.22 sec elapsed

# (04) LOADING REQUIRED FACTS DATA
# --------------------------------

file_path <- paste(path_data, "/testdata.rds", sep="")
system.time(df_testdata <-readRDS(file = file_path))
str(df_testdata)


# check of missing values
sapply(df_testdata, function(k) sum(is.na(k)))

# getting number of unique client_id
length(unique(df_testdata$client_id))
# [1] 100 client

# getting number of unique machine_id
length(unique(df_testdata$machine_id ))
# [1] 3 machines

# getting number of unique user_location
length(unique(df_testdata$localization_lat))
# [1] 300 locations

# use parse_time in order to create standard unambigous date format.
df_testdata <- df_testdata %>%
  mutate(date = as.Date(date , format = "%Y-%m-%d"))

#df_testdata$lot_health_index1 <- 100*df_testdata$lot_health_index

str(df_testdata)
# subset the data frame to training data and test

df_filter <- df_testdata %>%
  filter( client_id == "client_0",
          machine_id == "M_001") %>%
  dplyr::select(date, lot_health_index, dynamic_price,avg_market_premium_price)

class(df_filter)

# train_df_testdata <- df_filter %>%
#   filter(date >= "2001-01-31" & date <= "2015-12-31") #[1:180]
# 
# class(train_df_testdata)
# 
# test_df_testdata <- df_filter %>%
#   filter(date > "2015-12-31") # [181:252] #[1:180]
# 
# class(test_df_testdata)

df_filter <- df_filter %>%
  dplyr::select(lot_health_index, dynamic_price, avg_market_premium_price)

# Relocate before a specific column
df_filter <- df_filter %>% relocate(dynamic_price, .before = lot_health_index)
  
  
# train_df_testdata <- train_df_testdata %>%
#   dplyr::select(dynamic_price, lot_health_index, avg_market_premium_price)
# 
# test_df_testdata <- test_df_testdata %>%
#   dplyr::select( dynamic_price, lot_health_index, avg_market_premium_price)

# Converting a data.frame into mts = Multivariate Time Series

# df_filter <- df_filter %>%
#   dplyr::select(lot_health_index)

mts_df_filter <- stats::ts(df_filter,
                           frequency = 12,
                           start = c(2001, 1),
                           end = c(2021, 12))


class(mts_df_filter)
# [1] "mts"    "ts"     "matrix"

head(mts_df_filter)

str(mts_df_filter)

# mts_train_df_testdata <- stats::ts(train_df_testdata,
#                            frequency = 12,
#                            start = c(2001, 1),
#                            end = c(2015, 12))
# 
# class(mts_train_df_testdata)
# # [1] "mts"    "ts"     "matrix"
# 
# head(mts_train_df_testdata)
# 
# mts_test_df_testdata <- stats::ts(test_df_testdata,
#                                    frequency = 12,
#                                    start = c(2015, 1),
#                                    end = c(2021, 12))
# 
# class(mts_test_df_testdata)
# # [1] "mts"    "ts"     "matrix"
# head(mts_test_df_testdata)


# Standard exploratory tools
#par(mfrow = c(1,1))
par(mar = c(2.5,2.5,2.5,2.5))
plot(mts_df_filter, main = "")
# par(mar = c(0,0,0,0))
# plot(mts_train_df_testdata, main = "Lot Health Index")
# 
# plot(mts_test_df_testdata)

theme_set(theme_bw()) 
autoplot(mts_df_filter) +
  ggtitle("Time Series Plot of the Data Frame' Time-Series") +
  theme(plot.title = element_text(hjust = 0.5)) #for centering the text

# theme_set(theme_bw())
# autoplot(mts_train_df_testdata) +
#   ggtitle("Time Series Plot of the Data Frame Time-Series") +
#   theme(plot.title = element_text(hjust = 0.5)) #for centering the text
# 
# theme_set(theme_bw())
# autoplot(mts_test_df_testdata) +
#   ggtitle("Time Series Plot of the Data Frame Time-Series") +
#   theme(plot.title = element_text(hjust = 0.5)) #for centering the text

# Main packages - problem: both have different functions VAR
## Testing for stationarity
### tseries - standard test adt.test
apply(mts_df_filter, 2, tseries::adf.test)
# Warning in FUN(newX[, i], ...): p-value smaller than printed p-value

# Alternative: lib fUnitRoots, function
apply(mts_df_filter, 2, fUnitRoots::adfTest, 
      lags=0, #maximum number of lags used for error term correction
      type="c", #type of unit root regression
      title = "ADF Test for mts_df_filter Data") #title of the project

# Warning in FUN(newX[, i], ...): p-value smaller than printed p-value

# Differencing the whole mts
stnry1 <- MTS::diffM(mts_df_filter) #difference operation on a vector of time series. Default order of differencing is 1.

# Error in vars::VAR(stnry1, lag.max = 10, ic = "AIC", type = "none") : 
#   The matrix 'y' should contain at least two variables. For univariate analysis consider ar() and arima() in package stats.

#stnry2 <- log1p(mts_df_filter) #difference operation on a vector of time series. Default order of differencing is 1.

#log.test <- log1p(stnry1)

# Retest
apply(stnry1, 2, tseries::adf.test)

## VAR modeling
plot.ts(stnry1)

# Lag order identification
#We will use two different functions, from two different packages to identify the lag order for the VAR model. Both functions are quite similar to each other but differ in the output they produce. vars::VAR is a more powerful and convinient function to identify the correct lag order. 
vars::VARselect(stnry1, 
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
vars::VARselect(log1p(mts_df_filter),
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
plot(fcast1, xlim = c(2001, 2020))


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




