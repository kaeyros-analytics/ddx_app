
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



# 1. loading required data   ####
#__________________________


# Constructing path of relevant directories
root <- getwd()
path_data <- paste(root, "/", "input/archives", sep="")
#path_data <- paste(root, "/", "data", sep="")
path_helpers <- paste(root, "/R", sep="")
#path_helpers <- paste(root, "/codes/helpers", sep="")
path_meta <- paste(root, "/", "meta", sep="")


file_path <- paste(path_data, "/testdata.rds", sep="")
system.time(df_test <-readRDS(file = file_path))
str(df_test)



# use parse_time in order to create standard ambiguous date format.
# df_test <- df_test %>%
#   mutate(date = as.Date(date , format = "%Y-%m-%d"))

df_test <- df_test %>%
  mutate(date = as.Date(date , format = "%Y-%m-%d"),
         year_ = lubridate::year(date)) 


# rescaling the variable lot_health_index

# changing the scale initial vector
rescaled_lot_health_index <- scales::rescale(df_test$lot_health_index, to = c(20, 100))

# replace the variable lot_health_index through rescaled variable rescaled_lot_health_index
df_test$lot_health_index <- rescaled_lot_health_index

str(df_test)

# subset the data frame 
df_filter <- df_test %>%
  filter( client_id == "client_0",
          machine_id == "M_001") %>%
  dplyr::select(lot_health_index, dynamic_price, avg_market_premium_price, date, fixed_price, year_, localization_lon, localization_lat) %>% 
  relocate(dynamic_price, .before = lot_health_index)

# rescaling the variable lot_health_index

# changing the scale initial vector
# rescaled_lot_health_index <- scales::rescale(df_filter$lot_health_index, to = c(20, 100))
# 
# # replace the variable lot_health_index through rescaled variable rescaled_lot_health_index
# df_filter$lot_health_index <- rescaled_lot_health_index

class(df_filter)

mts_df_filter <- stats::ts(df_filter[- c(4,5,6,7,8)],
                           frequency = 12,
                           start = c(min(df_filter$year_), 1),
                           end = c(max(df_filter$year_), 12))


class(mts_df_filter)
# [1] "mts"    "ts"     "matrix"

head(mts_df_filter)
# min(df_test$year_)
# max(df_test$year_)
str(mts_df_filter)


# Standard exploratory tools

#par(mar = c(2.5,2.5,2.5,2.5))
plot(mts_df_filter, main = "")

# visualization with autoplot
ggplot2::theme_set(theme_bw()) 
autoplot(mts_df_filter) +
  ggtitle("Time Series Plot of the Data Frame' Time-Series") +
  theme(plot.title = element_text(hjust = 0.5)) #for centering the text


# Main packages - problem: both have different functions VAR
## Testing for stationarity
### tseries - standard test adt.test
apply(mts_df_filter, 2, tseries::adf.test)
#p-value = 0.06213  > 0.05 we conclude that the time series is nonstationary

# Alternative: lib fUnitRoots, function
apply(mts_df_filter, 2, fUnitRoots::adfTest, 
      lags=0, #maximum number of lags used for error term correction
      type="c", #type of unit root regression
      title = "ADF Test for mts_df_filter Data") #title of the project

# p-value = 0.06213 > 0.05 we conclude that the time series is nonstationary

# Differencing the whole mts
stnry1 <- MTS::diffM(mts_df_filter) #difference operation on a vector of time series. Default order of differencing is 1.

# Error in vars::VAR(stnry1, lag.max = 10, ic = "AIC", type = "none") : 
#   The matrix 'y' should contain at least two variables. For univariate analysis consider ar() and arima() in package stats.

# Retest
apply(stnry1, 2, tseries::adf.test)

## VAR modeling
stats::plot.ts(stnry1)

stnry2 <- log1p(mts_df_filter)

#log.test <- log1p(stnry1)

# Retest
apply(stnry2, 2, tseries::adf.test)

## VAR modeling
stats::plot.ts(stnry2)



# Lag order identification
#We will use two different functions, from two different packages to identify the lag order for the VAR model. Both functions are quite similar to each other but differ in the output they produce. vars::VAR is a more powerful and convinient function to identify the correct lag order.
vars::VARselect(log1p(mts_df_filter),
                type = "none", #type of deterministic regressors to include. We use none becasue the time series was made stationary using differencing above.
                lag.max = 10) #highest lag order

# $selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 1      1      1      1 
# 
# $criteria
# 1             2             3             4             5             6             7             8             9
# AIC(n) -3.253561e+01 -3.248689e+01 -3.246344e+01 -3.241225e+01 -3.243685e+01 -3.242534e+01 -3.237562e+01 -3.231863e+01 -3.227970e+01
# HQ(n)  -3.248334e+01 -3.238235e+01 -3.230664e+01 -3.220317e+01 -3.217550e+01 -3.211173e+01 -3.200973e+01 -3.190047e+01 -3.180927e+01
# SC(n)  -3.240586e+01 -3.222738e+01 -3.207418e+01 -3.189324e+01 -3.178808e+01 -3.164682e+01 -3.146734e+01 -3.128060e+01 -3.111191e+01
# FPE(n)  7.412502e-15  7.782787e-15  7.968007e-15  8.387725e-15  8.185831e-15  8.283442e-15  8.709990e-15  9.226780e-15  9.601088e-15
# 10
# AIC(n) -3.225451e+01
# HQ(n)  -3.173181e+01
# SC(n)  -3.095697e+01
# FPE(n)  9.856314e-15


# Creating a VAR model with vars
var.a.mts_df_filter <- vars::VAR(log1p(mts_df_filter),
                                 lag.max = 1, #highest lag order for lag length selection according to the choosen ic
                                 ic = "AIC", #information criterion
                                 type = "none") #type of deterministic regressors to include


summary(var.a.mts_df_filter)

# Residual diagnostics
#serial.test function takes the VAR model as the input
vars::serial.test(var.a.mts_df_filter)

#var.a.mts_df_filter$datamat


#selecting the variables ot_health_index, dynamic_price,avg_market_premium_price
# Granger test for causality
#for causality function to give reliable results we need all the variables of the multivariate time series to be stationary. 
vars::causality(var.a.mts_df_filter, #VAR model
                cause = c("dynamic_price")) #cause variable. If not specified then first column of x is used. Multiple variables can be used. 

# we forecast over a short horizon because beyond short horizon prediction becomes unreliable or uniform
fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = 12)
par(mar = c(2.5,2.5,2.5,2.5))
plot(fcast1)

#fcast1$endog
# Forecasting the dynamic_price
dynamic_price = fcast1$fcst[1]

dynamic_price # type list

class(dynamic_price)
# [1] "list"


# Extracting the forecast column
x = dynamic_price$dynamic_price[,1]
#xx
#var.a.mts_df_filter$datamat[, "dynamic_price"]

# Inverting the differencing
#To get the data to the original scale we invert the time series
#since the values are just difference from the previous value, to get the values on the original scale we add the last value from the DAX time series to the predicted values.
#the plot of the predicted values will also show that over longer horizon the predicted values are not reliable
# test <- tail(mts_df_filter[, "dynamic_price"])[6]
# x2 = cumsum(x) + test 
# par(mar = c(2.5,2.5,1,2.5)) #bottom, left, top, and right
# plot.ts(x2)

x2 = exp(x) - 1

#df <- dynamic_price_inv_dataframe[1:240,]

# Adding data and forecast to one time series
dynamic_price_inv <- ts(c(df_filter[,1], x2),
                        start = c(min(df_filter$year_),1),
                        end = c(max(df_filter$year_),12),
                        frequency = 12)
plot(dynamic_price_inv)

#[181:252] #[1:180]

dynamic_price_inv_dataframe <- as.data.frame(dynamic_price_inv) 
colnames(dynamic_price_inv_dataframe) <- c("x")

#date <- dat.df$time 

# extract the date from the time series dynamic_price_inv
date <- as.Date(time(dynamic_price_inv))

#choose a part of data frame for each forecast period
nbr_row <- nrow(dynamic_price_inv_dataframe)

n_period <- 12

n_1 <- nbr_row - n_period
n_2 <- nbr_row - n_period + 1 

################# from module dynamic price #################################

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
                                                                         '<br>Dynamic price:',"<b>",
                                                                         round(dynamic_price_inv_dataframe[n_2:nbr_row, ],2), " </b>"),
                                                       hoverinfo = 'text',
                                                       
                                                       showlegend = F) 

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
################# end from module dynamic price #############################

library(ggpp)
dat.df <- ggpp::try_data_frame(dynamic_price_inv)
str(dat.df)
head(dynamic_price_inv_dataframe)

theme_set(theme_bw()) 
p <- ggplot() + 
  geom_line(data = as.data.frame(dynamic_price_inv_dataframe[1:240,]), mapping = aes(y = get("dynamic_price_inv_dataframe[1:240, ]"),
                                                                                     x = date [1:240]
  ), color = "green") +
  geom_line(data = as.data.frame(dynamic_price_inv_dataframe[241:252,]), aes(y = get("dynamic_price_inv_dataframe[241:252, ]"), 
                                                                             x = date [241:252]),
            color = "red") +
  ggtitle("Plot of forecast of the VAR model on  time series") +
  #scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("")

plotly::ggplotly(p)

plotly::ggplotly(p, tooltip = "text") 


## Creating an advanced plot with visual separation
# Converting to object zoo
x = zoo(dynamic_price_inv)

# extract the date from the time series lot_health_inv
date <- as.Date(time(lot_health_inv))
date[1: n_11]
date[n_22:nbr_row1]

# Advanced xyplot from lattice
zoo::xyplot(x, grid=TRUE, panel = function(x, y, ...){
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
  theme(plot.title = element_text(hjust = 0.5)
        

        

plot_dynamic_price <- plot_ly() 
plot_dynamic_price <- plot_dynamic_price %>% add_trace(data = as.data.frame(dynamic_price_inv_dataframe[1:240,]), x = ~seq(1, 240), y = ~get("dynamic_price_inv_dataframe[1:240, ]"),
                                                       mode = 'lines', line = list(color = "#2B7A0B"),
                                                       hovertext = paste('Time: ',"<b>",seq(1, 240), "</b>",
                                                                         '<br>Dynamic price:',"<b>",round(dynamic_price_inv_dataframe[1:240, ],2), " € </b>"),
                                                       hoverinfo = 'text') 
plot_dynamic_price <- plot_dynamic_price %>% add_trace(data = as.data.frame(dynamic_price_inv_dataframe[241:252,]), x = ~seq(241, 252), y = ~get("dynamic_price_inv_dataframe[241:252, ]"),
                                                       mode = 'lines', line = list(color = "#ff0000"),
                                                       hovertext = paste('Time: ',"<b>",seq(241, 252), "</b>",
                                                                         '<br>Dynamic price:',"<b>",round(dynamic_price_inv_dataframe[241:252, ],2), " € </b>"),
                                                       hoverinfo = 'text',
                                                       
                                                       showlegend = F) 

plot_dynamic_price <- plot_dynamic_price %>% layout(title = "",
                                                    xaxis = list(title = "Time"),
                                                    yaxis = list (title = "Dynamic price in €"))#%>%
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



plot_dynamic_price <- plot_dynamic_price %>% add_lines(data =df_filter, y = df_filter$fixed_price, x = seq(1, nrow(df_filter)), 
                                                       line = list(color = "#E8AA42"), hoverinfo = 'text',
                                                       inherit = FALSE, showlegend = FALSE)
plot_dynamic_price <- plot_dynamic_price %>% add_annotations(x = 50, y = df_filter$fixed_price[1] + 0.01, 
                                                             text = paste("fixed premium = ", round(df_filter$fixed_price[1], 2), " €", sep = ""),
                                                             showarrow  = F)

plot_dynamic_price

##########################

# subset the data frame
df_filter <- df_test %>%
  filter(client_id == "client_0",
         machine_id == "M_001") %>%
  #date >= input$daterange[1] & date <= input$daterange[2]) %>%
  dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price, date, year_) %>%
  relocate(dynamic_price, .before = lot_health_index)


mts_df_filter <- stats::ts(df_filter[, -c(4,5)],
                           frequency = 12,
                           start = c(min(df_filter$year_), 1),
                           end = c(max(df_filter$year_), 12))



# Creating a VAR model with vars
var.a.mts_df_filter <- vars::VAR(log1p(mts_df_filter),
                                 lag.max = 1, #highest lag order for lag length selection according to the choose AIC
                                 ic = "AIC", #information criterion
                                 type = "none") #type of deterministic regressors to include


#fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = input$selected_forecast_period) 
fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = 12)

#browser() 

# Forecasting the dynamic_price
lot_health = fcast1$fcst[2] # type list

# Extracting the forecast column
x_predict. = lot_health$lot_health_index[,1]

# Inverting the log1p
#To get the data to the original scale we invert the time series, to get the values on the original scale 
#we add the last value from the variable dynamic_price time series to the predicted values.
x2_predict. = exp(x_predict.) - 1

# Adding data and forecast to one time series
lot_health_inv =ts(c(df_filter[,2], x2_predict.),
                   start = c(min(df_filter$year_), 1),
                   end = c(max(df_filter$year_), 12),
                   frequency = 12)
#plot(dynamic_price_inv22)

#convert lot_health_inv to data frame and set a name from the column 
lot_health_inv_dataframe <- as.data.frame(lot_health_inv) 
colnames(lot_health_inv_dataframe) <- c("x")
#head(dynamic_price_inv_dataframe)

#choose a part of data frame for each forecast period
nbr_row1 <- nrow(lot_health_inv_dataframe)
n_period1 <- as.numeric(12)

n_11 <- nbr_row1 - n_period1
n_22 <- nbr_row1 - n_period1 + 1 
#length(df_filter$fixed_price)

ggplot2::theme_set(theme_bw()) 
plot_dynamic_price <- ggplot() + 
  geom_line(data = as.data.frame(lot_health_inv_dataframe[1:n_11,]), aes(y = get("lot_health_inv_dataframe[1:n_11, ]"), 
                                                                         x = seq(1, n_11)), color = "#7879FF", size=0.71) +
  geom_line(data = as.data.frame(lot_health_inv_dataframe[n_22:nbr_row1,]), aes(y = get("lot_health_inv_dataframe[n_22:nbr_row1, ]"), 
                                                                                x = seq(n_22, nbr_row1)), color = "red", size=0.71) +
  ggtitle("") + # Plot of forecast of the VAR model on  time series
  #scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("")

plot_dynamic_price


plot_lot_health_index <- plot_ly() 
plot_lot_health_index <- plot_lot_health_index %>% add_trace(data = as.data.frame(lot_health_inv_dataframe[1:n_11,]),  
                                                             x = ~seq(1, n_11), y = ~get("lot_health_inv_dataframe[1:n_11, ]"),
                                                             mode = 'lines', line = list(color = "#2B7A0B"),
                                                             hovertext = paste('Time: ',"<b>",seq(1, n_11), "</b>",
                                                                               '<br>Iot health index:',"<b>",
                                                                               round(lot_health_inv_dataframe[1:n_11,],2), "</b>"),
                                                             hoverinfo = 'text') 
plot_lot_health_index <- plot_lot_health_index %>% add_trace(data = as.data.frame(lot_health_inv_dataframe[n_22:nbr_row1,]),  
                                                             x = ~seq(n_22, nbr_row1), y = ~get("lot_health_inv_dataframe[n_22:nbr_row1, ]"),
                                                             mode = 'lines', line = list(color = "#ff0000"),
                                                             hovertext = paste('Time: ',"<b>",seq(n_22, nbr_row1), "</b>",
                                                                               '<br>Iot health index:',"<b>",
                                                                               round(lot_health_inv_dataframe[n_22:nbr_row1,],2), "</b>"),
                                                             hoverinfo = 'text', showlegend = F)
plot_lot_health_index <- plot_lot_health_index %>% layout(title = "",
                                                          xaxis = list(title = "Time"),
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
