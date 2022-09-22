
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
library(base)
library(MTS)
library(vars)
# library(ggfortify)

# 1. Loading required data   ####
#__________________________

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

#str(df_testdata)

# subset the data frame to one client_id and one machine_id

# df_filter <- df_testdata %>%
#   filter( client_id == "client_0",
#           machine_id == "M_001") %>%
#   dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price) %>% 
#   relocate(dynamic_price, .before = lot_health_index)

# 2. UI Modules  ####
#_______________

# filter modules 
eval(parse('./R/modules/filter_client_id.R', encoding="UTF-8"))
eval(parse('./R/modules/filter_machine_id.R', encoding="UTF-8"))
eval(parse('./R/modules/filter_daterange.R', encoding="UTF-8"))
eval(parse('./R/modules/filter_forecast_period.R', encoding="UTF-8"))
eval(parse('./R/modules/filter_forecast_method.R', encoding="UTF-8"))
eval(parse('./R/modules/filter_average.R', encoding="UTF-8"))
eval(parse('./R/modules/calculate_button.R', encoding="UTF-8"))

# 3. SERVER Modules  ####
#__________________

# plot modules
eval(parse('./R/modules/dynamic_price_forecast.R', encoding="UTF-8"))
eval(parse('./R/modules/lot_health_forecast.R', encoding="UTF-8"))
eval(parse('./R/modules/map_graph.R', encoding="UTF-8"))
#eval(parse('./R/modules/map_graph.R', encoding="UTF-8"))


