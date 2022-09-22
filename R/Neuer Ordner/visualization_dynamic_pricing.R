
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

# df_testdata1 <- customers_data_init %>%
#   mutate(date = as.Date(date , format = "%Y-%m-%d"),
#          year_ = lubridate::year(date)) 

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
df_filter1 <- df_testdata %>%
  filter( client_id == "client_0",
          machine_id == "M_001") %>%
  dplyr::select(dynamic_pricing, lot_health_index, GPR, avg_temperature_month, inflation_rate, DEU_dmd, date, year_)
class(df_filter1)

mts_df_filter1 <- stats::ts(df_filter1[- c(7,8)],
                           frequency = 12,
                           start = c(min(df_filter1$year_), 1),
                           end = c(max(df_filter1$year_), 12))


class(mts_df_filter1)
# [1] "mts"    "ts"     "matrix"

head(mts_df_filter1)
tail(mts_df_filter1)
# min(df_testdata$year_)
# max(df_testdata$year_)
str(mts_df_filter1)


# Standard exploratory tools

#par(mar = c(2.5,2.5,2.5,2.5))
plot(mts_df_filter1, main = "")

# visualization with autoplot
ggplot2::theme_set(theme_bw()) 
autoplot(mts_df_filter1) +
  ggtitle("Time Series Plot of the Data Frame' Time-Series") +
  theme(plot.title = element_text(hjust = 0.5)) #for centering the text


# Main packages - problem: both have different functions VAR
## Testing for stationarity
### tseries - standard test adt.test
apply(mts_df_filter1, 2, tseries::adf.test)
#p-value = 0.06213  > 0.05 we conclude that the time series is nonstationary

# Alternative: lib fUnitRoots, function
apply(mts_df_filter1, 2, fUnitRoots::adfTest, 
      lags=0, #maximum number of lags used for error term correction
      type="c", #type of unit root regression
      title = "ADF Test for mts_df_filter1 Data") #title of the project

# p-value = 0.06213 > 0.05 we conclude that the time series is nonstationary

# Differencing the whole mts
stnry1 <- MTS::diffM(mts_df_filter1) #difference operation on a vector of time series. Default order of differencing is 1.

# Error in vars::VAR(stnry1, lag.max = 10, ic = "AIC", type = "none") : 
#   The matrix 'y' should contain at least two variables. For univariate analysis consider ar() and arima() in package stats.

# Retest
apply(stnry1, 2, tseries::adf.test)

## VAR modeling
stats::plot.ts(stnry1)

stnry2 <- log1p(mts_df_filter1)

#log.test <- log1p(stnry1)

# Retest
apply(stnry2, 2, tseries::adf.test)

## VAR modeling
stats::plot.ts(stnry2)



# Lag order identification
#We will use two different functions, from two different packages to identify the lag order for the VAR model. Both functions are quite similar to each other but differ in the output they produce. vars::VAR is a more powerful and convinient function to identify the correct lag order.
# vars::VARselect(log1p(mts_df_filter1),
#                 type = "none", #type of deterministic regressors to include. We use none becasue the time series was made stationary using differencing above.
#                 lag.max = 10) #highest lag order

vars::VARselect(stnry1,
                type = "both", #type of deterministic regressors to include. We use none becasue the time series was made stationary using differencing above.
                lag.max = 10) #highest lag order

# $selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 10      6      1     10 
# 
# $criteria
# 1            2            3            4            5            6           7           8           9          10
# AIC(n)     12.29229     12.29025     12.19700     11.97155     11.74252     11.55946    11.44521    11.37940    11.38344    11.35790
# HQ(n)      12.41255     12.53076     12.55776     12.45256     12.34379     12.28097    12.28698    12.34142    12.46571    12.56043
# SC(n)      12.58861     12.88290     13.08596     13.15684     13.22413     13.33739    13.51946    13.74998    14.05033    14.32112
# FPE(n) 218016.95645 217626.60532 198382.97729 158549.88146 126371.59047 105576.07749 94612.27257 89134.70316 90213.69942 88831.77094



# Creating a VAR model with vars
var.a.mts_df_filter1 <- vars::VAR(stnry1,
                                 lag.max = 9, #highest lag order for lag length selection according to the choosen ic
                                 ic = "AIC", #information criterion
                                 type = "both") #type of deterministic regressors to include


summary(var.a.mts_df_filter1)

# Residual diagnostics
#serial.test function takes the VAR model as the input
vars::serial.test(var.a.mts_df_filter1)

#var.a.mts_df_filter1$datamat


#selecting the variables ot_health_index, dynamic_price,avg_market_premium_price
# Granger test for causality
#for causality function to give reliable results we need all the variables of the multivariate time series to be stationary. 
vars::causality(var.a.mts_df_filter1, #VAR model
                cause = c("dynamic_pricing")) #cause variable. If not specified then first column of x is used. Multiple variables can be used. 

# we forecast over a short horizon because beyond short horizon prediction becomes unreliable or uniform
fcast1 = stats::predict(var.a.mts_df_filter1, n.ahead = 12)
par(mar = c(2.5,2.5,2.5,2.5))
plot(fcast1)

#fcast1$endog
# Forecasting the dynamic_price
dynamic_price <- fcast1$fcst[1]

dynamic_price # type list

class(dynamic_price)
# [1] "list"


# Extracting the forecast column
x_predict = dynamic_price$dynamic_pricing[,1]
#xx
#var.a.mts_df_filter1$datamat[, "dynamic_price"]

# Inverting the differencing
#To get the data to the original scale we invert the time series
#since the values are just difference from the previous value, to get the values on the original scale we add the last value from the DAX time series to the predicted values.
#the plot of the predicted values will also show that over longer horizon the predicted values are not reliable
# test <- tail(mts_df_filter1[, "dynamic_price"])[6]
# x2 = cumsum(x) + test 
# par(mar = c(2.5,2.5,1,2.5)) #bottom, left, top, and right
# plot.ts(x2)
# x2 = exp(x) - 1

# Inverting the differencing
#To get the data to the original scale we invert the time series
#since the values are just difference from the previous value, to get the values on the original scale we add the last value from the DAX time series to the predicted values.
#the plot of the predicted values will also show that over longer horizon the predicted values are not reliable

# Extract the last values from the variable dynamic_pricing
y <- tail(mts_df_filter1)[,1][6]
#y <- test[,1][6]
x2_predict = cumsum(x_predict) + y
par(mar = c(2.5,2.5,1,2.5)) #bottom, left, top, and right
plot.ts(x2_predict)

# Adding data and forecast to one time series
dynamic_price_inv <- ts(c(df_filter1[,1], x2_predict),
                        start = c(min(df_filter1$year_),1),
                        end = c(max(df_filter1$year_),12),
                        frequency = 12)
plot(dynamic_price_inv)

dynamic_price_inv_dataframe <- as.data.frame(dynamic_price_inv) 
colnames(dynamic_price_inv_dataframe) <- c("x")

# extract the date from the time series dynamic_price_inv
date <- as.Date(time(dynamic_price_inv))

#choose a part of data frame for each forecast period
nbr_row <- nrow(dynamic_price_inv_dataframe)

n_period <- 12

n_1 <- nbr_row - n_period
n_2 <- nbr_row - n_period + 1 

date_ci <- date[n_2:nbr_row]
lower <- round(x2_predict - 1.95, 2)
upper <- round(x2_predict + 1.95, 2)
df_ci <- cbind.data.frame(date_ci, lower, upper)
colnames(df_ci) <- c("date_ci","lower", "upper")

x_lower <- dynamic_price$dynamic_pricing[,2] + y
x_upper <- dynamic_price$dynamic_pricing[,3] + y

plot_dynamic_price <- plot_ly() 
plot_dynamic_price <- plot_dynamic_price %>% add_trace(data = as.data.frame(dynamic_price_inv_dataframe[1:n_1,]), x = ~date[1:n_1], 
                                                       y = ~get("dynamic_price_inv_dataframe[1:n_1, ]"),
                                                       mode = 'lines', line = list(color = "#17B169"),
                                                       hovertext = paste('Date: ',"<b>",date[1:n_1], "</b>",
                                                                         '<br>Dynamic price:',"<b>",
                                                                         round(dynamic_price_inv_dataframe[1:n_1, ],2), " </b>"),
                                                       hoverinfo = 'text') 
plot_dynamic_price <- plot_dynamic_price %>% add_trace(data = as.data.frame(dynamic_price_inv_dataframe[n_2:nbr_row,]), 
                                                       x = ~date[n_2:nbr_row], y = ~get("dynamic_price_inv_dataframe[n_2:nbr_row, ]"),
                                                       mode = 'lines', line = list(color = "#ff0000"),
                                                       hovertext = paste('Date: ',"<b>",date[n_2:nbr_row], "</b>",
                                                                         '<br>Predicted dynamic price:',"<b>",
                                                                         round(dynamic_price_inv_dataframe[n_2:nbr_row, ],2), " </b>"),
                                                       hoverinfo = 'text',
                                                       
                                                       showlegend = F) 
plot_dynamic_price <- plot_dynamic_price %>% add_ribbons(#data = df_ci,
                                                         x = ~date_ci,
                                                         ymin = ~lower,
                                                         ymax = ~upper,
                                                         color = I("#87A2FB"),
                                                         showlegend = F,
                                                         hoverinfo="text",
                                                         hovertext = paste('Date:',"<b>",date_ci, "</b> \n",
                                                                           'Upper:',"<b>",lower, "</b> \n",
                                                                           'Lower:',"<b>",upper, "</b>"))
                                                         # line = list(color = 'rgba(7, 164, 181, 0.05)'),
                                                         # fillcolor = 'rgba(7, 164, 181, 0.2)')

plot_dynamic_price <- plot_dynamic_price %>% layout(title = "",
                                                    xaxis = list(title = "Date"),
                                                    yaxis = list (title = ""))
plot_dynamic_price <- plot_dynamic_price %>%
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

plot_dynamic_price
