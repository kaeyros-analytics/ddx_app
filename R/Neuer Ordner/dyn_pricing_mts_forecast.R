# The current  R-Script contains the steps required to analyse and forecast
# 
# Change History
# Date			    Editor				    Action
# 17.08.2022   Thierry Monthe    Initial Creation

#' Main function, with a Step-By-Step approach for the Multivariate Time series
#' Analysis and subsequently forecasts
#'
#' Following activities will be performed
#'
#'  1- load required data from sqlite-database
#'  
#'  2- create appropriate times series
#'  
#'  3- create VAR (Vector Autoregression time series model
#'  
#'  
#' der VDKx. Diese werden als Zwischenergebnisse für nachfolgende Sequenzen
#' ins Austausch-Verzeichnis geschrieben.
#'
#' @param path_exchange \code{String} mit dem Pfad des Austausch-Verzeichnis,
#' unter dem die erzeugten Zwischenergebnisse abgelegt werden und unter dem das
#' Environment vdkx abgelegt ist.
#' @param path_input \code{String} mit dem Pfad des Input-Verzeichnisses, in dem
#' die Zulieferungstabellen abgelegt sind.
#' @param vdkx_input_filename \code{String} mit den Dateinamen des
#' Environments vdkx, welches zu Beginn dieser Sequenz geladen werden soll.
#' @param vdkx_output_filename \code{String} mit den Dateinamen des
#' Environments vdkx, welches am Ende dieser Sequenz weggeschrieben werden soll.
#' @param sgmt_id \code{Integer} mit der Segmentierungs_ID, die bei der
#' Bestimmung der Segmente aus der Segmentierungsdatenbank verwendet werden
#' soll.
#' @param loglevel \code{String} der angibt, welche Meldungen in die Log-Datei
#' geschrieben werden sollen. Folgende Ausprägungen sind möglich:
#' 'DEBUG', 'INFO', 'WARNING', 'ERROR'
#' @param state_error \code{String} der das Status-Level bei einem Fehler
#' angibt.
#' @param logfile_path \code{String} mit dem Pfad, unter dem das Logfile
#' gespeichert werden soll.
#' @param logfile_prefix \code{String} mit dem Präfix, mit dem der Name des
#' Logfiles versehen wird.
#' @return Status der Ausführung
#' @family vdkx_segmentierung
#' @export


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


# 1. Loading required data  ####
#______________________________

# set path to input data
path_input <- file.path(getwd(), "data")
path_sqlite <- file.path(path_input, "generated_db.sqlite3")  

# connect to the sqlite database
con <- dbConnect(drv=RSQLite::SQLite(), dbname = path_sqlite)

## list all tables
tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

lDataFrames <- vector("list", length=length(tables))

# ## create a data.frame for each table
# for (i in seq(along=tables)) {
#   lDataFrames[[i]] <- dbGetQuery(conn = con, 
#                                  statement=paste("SELECT * FROM '", 
#                                                  tables[[i]], "'", sep=""))
# }

# load testdata into R-environment
testdata <- dbGetQuery(conn = con, statement=paste("SELECT * FROM testdata", 
                                                   sep=""))
# check structure of the loaded data 
str(testdata$client_id)
View(testdata)

# select a client and a machine
data <- testdata %>%
  dplyr::filter(client_id == "client_0" &  machine_id == "M_001") %>%
  dplyr::select(-c("id", "client_id", "machine_id", ))


# 2. creating appropriate time series  ####
#____________________________________

# subsetting the data frame and keeping only times series to be analyzed
data_ts <- data %>%
  dplyr::select(-c("date", "fixed_price", "localization_lat", 
                   "localization_lon"))

View(data_ts)

#transform variables to log in order to avoid 
data_ts$lot_health_index <- log1p(data_ts$lot_health_index)
data_ts$dynamic_price <- log1p(data_ts$dynamic_price)
data_ts$avg_market_premium_price <- log1p(data_ts$avg_market_premium_price)


# converting the data frame into mts-format
mymts = ts(data_ts,
           frequency = 12,
           start = c(2001, 1),
           end = c(2021, 12))
mymts



# 3. exploration of the time series top be analyzed  ####
#__________________________________________________

# Standard plot
plot(mymts)

# plotting all times series together
theme_set(theme_bw())
autoplot(mymts) +
  ggtitle("Time Series Plot of the `mymts' Time-Series") +
  theme(plot.title = element_text(hjust = 0.5)) #for centering the text

# visualizing head of the times series
head(mymts)


head(mymts)

# 4. Assessing the stationary of the mts  ####
#________________________________________

# using the augmented Dickey Fuller Test
apply(mymts, 2, adf.test)

# using the augmented lib fUnitRoots
apply(mymts, 2, adfTest, 
      lags=0, #maximum number of lags used for error term correction
      type="c", #type of unit root regression
      title = "ADF Test for Dynamic Pricing Data") #title of the project



# summary:
#
# 2 Times series (iot_health_index and dynamic_price) are non-stationary
# Null hypothesis narrowly rejected 




# 5. making the non stationary data stationary  ####
#_____________________________________________

# Differencing the whole mts
stnry = diffM(mymts) #difference operation on a vector of time series. Default 
# order of differencing is 1.
head(stnry)

# Retest
apply(stnry, 2, adf.test)

## Plotting the transformed data
plot.ts(stnry)

autoplot(ts(stnry,
            start = c(2001, 1),
            end = c(2021, 12),
            frequency = 12)) +
  ggtitle("Time Series Plot of the stationary dynamic pricing Time-Series")

# Lag order identification
#We will use two different functions, from two different packages to identify the lag order for the VAR model. Both functions are quite similar to each other but differ in the output they produce. vars::VAR is a more powerful and convinient function to identify the correct lag order. 
VARselect(stnry, 
          type = "none", #type of deterministic regressors to include. We use none becasue the time series was made stationary using differencing above. 
          lag.max = 100) #highest lag order


# Creating a VAR model with vars
var.a <- vars::VAR(stnry,
                   lag.max = 10, #highest lag order for lag length selection according to the choosen ic
                   ic = "AIC", #information criterion
                   type = "none") #type of deterministic regressors to include
summary(var.a)


# Residual diagnostics
#serial.test function takes the VAR model as the input.  
serial.test(var.a)
