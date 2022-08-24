# ============================================================================================ #
#                                                                                              #
#  Following activities will be performed in the following script:                             #
#                                                                                              #
#    (1) Loading the   data required for the dashbaord-implementation                          #
#                                                                                              #
#                                                                                              #
#                                                                                              #
#     Input Parameters:                                                                        #
#     ----------------                                                                         #
#                                                                                              #
#                                                                                              #
#       (1) db_name          : name of the database containing Ease Customer Data              #
#                                                                                              #
#       (2) db_credentials   : data frame containing required credentials for db access        #
#                              of all available databases (db.sqlite3)                         #
#                                                                                              #                                                                                              #
#      Output Parameters:                                                                      #
#      -----------------                                                                       #
#                                                                                              #
#      ** RData for each single table will be produced and saved into the ./data-directory     #
#                                                                                              #
#                                                                                              #
# ============================================================================================ #




#======================
# (0) PREPARATION STEPS ####
#======================


# --------------------------------------
# (01) CLEARING ENVIROMNMENT AND CONSOLE ####
# --------------------------------------

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
# # --------------------------------------
# # (04) CONNECTING R TO THE MPAY DATABASE ####
# # --------------------------------------
# 
# # Getting the data file directory
# file_path <- paste(path_data, "/", "db_new.sqlite3", sep = "")
# # # Here we only use the function dbConnect without the parameter dbname, host, username,
# # # password because we have already saved the SQL data into RStudio
# # # creating the connection to the database
# conn <- DBI::dbConnect(RSQLite::SQLite(), file_path)
# 
# # # getting list of tables available
# available_tables <- DBI::dbListTables(conn)
# 
# # retrieving database drivers
# drv <- dbDriver(db_type)
# 
# # creating the connection to the database
# con <- dbConnect(drv, dbname= "mpay", host= db_host, username= db_user, password= db_pwd) # agnes chnage the dbname according to the schema(dbname) u have in ur sql-DB
# 
# 
# # checking the number of opened db connextions
# nbr_connections <- dbListConnections(MySQL())
# length(nbr_connections)
# 
# # getting list of tables available
# available_tables <- dbListTables(con)
# 

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
#   # read the data frame from the sql database
#   k <- 12
#   form <- parse(text = paste(available_tables[k]," <- DBI::dbReadTable(conn, '", available_tables[k], "')", sep=""))
#   eval(form)
# 
#   str(test_project_trackerdata)
#   # Convert the variables id, user_tracking_id, service_id, offer_id and purchase_y_n to character
#   test_project_trackerdata$id <- as.character(test_project_trackerdata$id)
#   test_project_trackerdata$user_tracking_id <- as.character(test_project_trackerdata$user_tracking_id)
#   test_project_trackerdata$service_id <- as.character(test_project_trackerdata$service_id)
#   test_project_trackerdata$offer_id <- as.character(test_project_trackerdata$offer_id)
#   test_project_trackerdata$purchase_y_n <- as.character(test_project_trackerdata$purchase_y_n)
#   test_project_trackerdata$favorie_y_n  <- as.character(test_project_trackerdata$favorie_y_n)
#   test_project_trackerdata$add_in_basket_y_n  <- as.character(test_project_trackerdata$add_in_basket_y_n)
#   test_project_trackerdata$remove_in_basket_y_n  <- as.character(test_project_trackerdata$remove_in_basket_y_n)
#   ## find index for purchase_y_n == "0" and set price_offer == 0 for the found index
# 
#   # index_purchased_no <- which(test_project_trackerdata$purchase_y_n == "0")
#   # test_project_trackerdata$price_offer[index_purchased_no] = 0
#   # convert the variables timestamp_start and timestamp_end to date
#   test_project_trackerdata$timestamp_start <- lubridate::ymd_hms(test_project_trackerdata$timestamp_start)
#   test_project_trackerdata$timestamp_end <- lubridate::ymd_hms(test_project_trackerdata$timestamp_end)
#   str(test_project_trackerdata)
#   # save the loaded data frame in order data
#   #k <- 13
#   form <- parse(text = paste("saveRDS(", available_tables[k], ", file = '" , path_data, "/", available_tables[k], ".rds')", sep=""))
#   eval(form)
# 
# }
# toc()  # 0.22 sec elapsed

# (04) LOADING REQUIRED FACTS DATA
# --------------------------------

file_path <- paste(path_data, "/test_project_trackerdata.rds", sep="")
system.time(ease_data_simulation <-readRDS(file = file_path))
str(ease_data_simulation)


# check of missing values
sapply(ease_data_simulation, function(k) sum(is.na(k)))

# getting number of unique user_tracking_id
length(unique(ease_data_simulation$user_tracking_id))
# [1] 100 user

# getting number of unique ip
length(unique(ease_data_simulation$ip))
# [1] 100 user

# getting number of unique user_location
length(unique(ease_data_simulation$user_location))
# [1] 8 locations

# use parse_time in order to create standard unambigous date format.
ease_data_for_visualization <- ease_data_simulation %>%
  mutate(only_start_date = as.Date(timestamp_start , format = "%Y-%m-%d"),
         only_start_end = as.Date(timestamp_end, format = "%Y-%m-%d"),
         #years_ = as.Date(timestamp_end, format = "%Y"),
         years_ = lubridate::year(timestamp_end),
         quarter_ = lubridate::quarter(timestamp_end),
         month_ = lubridate::month(timestamp_end),
         week_ = lubridate::week(timestamp_end),
         day_ = lubridate::day(timestamp_end),
         year_month = zoo::as.yearmon(only_start_date, "%Y-%m"),
         time_h_m_s = hms::as_hms(difftime(timestamp_end, timestamp_start)),
         #month_year = zoo::as.yearmon(only_start_end))
         month_year = format(only_start_end, "%m-%Y"))

str(ease_data_for_visualization)


# ease_data_for_visualization$test <- year(ease_data_for_visualization$only_start_end)*100 + 
#   month(ease_data_for_visualization$only_start_end)


#------------------------------
# KPI: determine user location
#------------------------------


data_location_2019 <- ease_data_for_visualization %>%
  filter(years_ == 2019) %>%
  group_by(user_location) %>%
  summarise(nbr_user = length(unique(user_tracking_id))) %>%
  arrange(desc(nbr_user))

user_table <- data_location_2019  %>% 
  kableExtra::kbl(align = "c") %>% 
  kableExtra::kable_styling(
    bootstrap_options = c("striped","hover","condensed"),
    full_width = F,
    font_size = 16,
    fixed_thead = T
  ) %>% 
  kableExtra::scroll_box(
    height = "500px")

user_table

######################### visualization with type = "bar" ############################

# calculate the total purchased  ánd number of user per location, per month 
data_location_sale_2019_ <- ease_data_for_visualization %>%
  filter(years_ == 2019,
         purchase_y_n == "1") %>%
  group_by(user_location, month_year) %>%
  summarise(nbr_user = length(unique(user_tracking_id)),
            total_sale = sum(price_offer)) %>%
  arrange(desc(total_sale))

user_table1 <- data_location_sale_2019_  %>% 
  kableExtra::kbl(align = "c") %>% 
  kableExtra::kable_styling(
    bootstrap_options = c("striped","hover","condensed"),
    full_width = F,
    font_size = 16,
    fixed_thead = T
  ) %>% 
  kableExtra::scroll_box(
    height = "500px")

user_table1

# the top 10 purchased
#top10_purchased <- head(data_location_sale_2019_, 10)
#
# the top 10 user location
#top_user_location <- top10_purchased$user_location 
#
# search only the information from the top 10 agents
#data_top_user_location_ <- which(data_location_sale_2019_$user_location %in% top_user_location)

#data_top_user_location_ <- data_location_sale_2019_[data_top_user_location_, ]



plot_top10_user_location_stack_ <- plotly::plot_ly(data_location_sale_2019_, x = ~month_year,
                                                   type = "bar", 
                                                   y = ~total_sale, color = ~user_location,
                                                   colors = c("slateblue", "#008080", "#BDB76B", "#B8860B"),
                                                   #text = ~most_visited_service, textposition = 'outside',
                                                   hovertext = paste("Date :",data_location_sale_2019_$month_year,
                                                                     "<br>User location :", data_location_sale_2019_$user_location,
                                                                     "<br>Nber of user :", data_location_sale_2019_$nbr_user,
                                                                     "<br>Purchased:",round(data_location_sale_2019_$total_sale/1000000,
                                                                                            digits = 1), "million CFA"),
                                                   hoverinfo = 'text') %>%
  layout(title = "Total purchased of each location per month",  barmode="stack", bargap=0.3,
         legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>User location</b>')),
         uniformtext=list(minsize=15, mode='show'),
         xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                      #categoryorder = "total descending",
                      # change x-axix size
                      tickfont = list(size = 14), 
                      # change x-title size
                      titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                      tickangle= -45, tickvals = data_location_sale_2019_$month_year),
         yaxis = list(title = "<b> Purchased in CFA </b>",
                      titlefont = list(size = 16),
                      #categoryorder = "total descending",
                      # change x-axix size
                      tickfont = list(size = 14))
         #tickvals = df_most_visited_service_month_year$most_visited_service)
         #hoverlabel=list(bgcolor="gainsboro")
         #width = 500, autosize=F,
         #bargap = 0.1, bargroupgap = 0.1,
  ) %>%
  config(displayModeBar = F, 
         scrollZoom = T)


plot_top10_user_location_stack_


####################### end visualization with type = "bar"  stacked user location #################

#########################     ########################################################

plot_top10_user_location_bar_ <- plotly::plot_ly(data_location_sale_2019_, x = ~month_year,
                                                   type = "bar", 
                                                   y = ~total_sale, color = ~user_location,
                                                   colors = c("slateblue", "#008080", "#BDB76B", "#B8860B"),
                                                   #text = ~most_visited_service, textposition = 'outside',
                                                   hovertext = paste("Date :",data_location_sale_2019_$month_year,
                                                                     "<br>User location :", data_location_sale_2019_$user_location,
                                                                     "<br>Nber of user :", data_location_sale_2019_$nbr_user,
                                                                     "<br>Purchased:",round(data_location_sale_2019_$total_sale/1000000,
                                                                                            digits = 1), "million CFA"),
                                                   hoverinfo = 'text') %>%
  layout(title = "Total purchased of each location per month", bargap=0.3,
         legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>User location</b>')),
         uniformtext=list(minsize=15, mode='show'),
         xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                      #categoryorder = "total descending",
                      # change x-axix size
                      tickfont = list(size = 14), 
                      # change x-title size
                      titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                      tickangle= -45, tickvals = data_location_sale_2019_$month_year),
         yaxis = list(title = "<b> Purchased in CFA </b>",
                      titlefont = list(size = 16),
                      #categoryorder = "total descending",
                      # change x-axix size
                      tickfont = list(size = 14))
         #tickvals = df_most_visited_service_month_year$most_visited_service)
         #hoverlabel=list(bgcolor="gainsboro")
         #width = 500, autosize=F,
         #bargap = 0.1, bargroupgap = 0.1,
  ) %>%
  config(displayModeBar = F, 
         scrollZoom = T)


plot_top10_user_location_bar_

######################## end #########################################################

####################### start visualization with type = "line"  user location ##############

colline <- c("slateblue", "#008080", "#BDB76B", "#B8860B", "#FFF8DC", "#A9A9A9", "#FF7F50", "#5F9EA0")

#ltys <- c(
#  Austin = "dashdot",
#  `Collin County` = "longdash",
#  Dallas = "dash",
#  Houston = "solid",
#  `San Antonio` = "dot"
#)
fig_sale_plot <- plotly::plot_ly(data_location_sale_2019_) %>%
  add_lines(x = ~month_year, y = ~total_sale, linetype = ~user_location) %>%
  layout(title = "Total purchased of each location per month",
         hovermode = "x unified",
         uniformtext=list(minsize=15, mode='show'),
         legend = list(x = 100, y = 0.90, title=list(color= "blue", text='<b> User location </b>')),
         xaxis = list(title = "<b> Date </b>", tickangle= -45, #type="date", tickformat="%m-%Y",
                      tickvals = data_location_sale_2019_$month_year,
                      tickfont = list(size = 14), # size from x axis values
                      titlefont = list(size = 15)),
         yaxis = list(title = "<b> Purchasing in CFA </b>", titlefont = list( size = 16))) %>%
  config(displayModeBar = T, scrollZoom = T)

fig_sale_plot
####################### end visualization with type = "line"  user location ##############


# calculate the most purchased service, number of user per location 
data_location_sale_2019 <- ease_data_for_visualization %>%
  filter(years_ == 2019,
         purchase_y_n == "1") %>%
  group_by(user_location, service_name, month_year) %>%
  summarise(nbr_user = length(unique(user_tracking_id)),
            total_sale = sum(price_offer)) %>%
  arrange(desc(total_sale))

# the top 10 purchased
top10_purchased <- head(data_location_sale_2019, 10)

# the top 10 user location
top_user_location <- top10_purchased$user_location 

# search only the information from the top 10 agents
top_10_user_location <- which(data_location_sale_2019$user_location %in% top_user_location)

data_top_user_location <- data_location_sale_2019[top_10_user_location, ]

plot_user_location_stack_ <- plotly::plot_ly(data_location_sale_2019, x = ~month_year,
                                                   type = "bar", 
                                                   y = ~total_sale, color = ~user_location,
                                                   colors = c("slateblue", "#008080", "#6B8E23", "#BDB76B"),
                                                   #text = ~nbr_user, textposition = 'outside',
                                                   hovertext = paste("Date :",data_location_sale_2019$month_year,
                                                                     "<br>User location :", data_location_sale_2019$user_location,
                                                                     "<br>Service name :", data_location_sale_2019$service_name,
                                                                     "<br>Nber of user :", data_location_sale_2019$nbr_user,
                                                                     "<br>Purchased:",round(data_location_sale_2019$total_sale/1000000,
                                                                                            digits = 1), "million CFA"),
                                                   hoverinfo = 'text') %>%
  layout(title = "Total purchased of each location per month",  barmode="stack", bargap=0.3,
         legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>User location</b>')),
         uniformtext=list(minsize=15, mode='show'),
         xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                      #categoryorder = "total descending",
                      # change x-axix size
                      tickfont = list(size = 14), 
                      # change x-title size
                      titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                      tickangle= -45, tickvals = data_location_sale_2019$month_year),
         yaxis = list(title = "<b> Purchased in CFA </b>",
                      titlefont = list(size = 16),
                      #categoryorder = "total descending",
                      # change x-axix size
                      tickfont = list(size = 14))
         #tickvals = df_most_visited_service_month_year$most_visited_service)
         #hoverlabel=list(bgcolor="gainsboro")
         #width = 500, autosize=F,
         #bargap = 0.1, bargroupgap = 0.1,
  ) %>%
  config(displayModeBar = F, 
         scrollZoom = T)


plot_user_location_stack_



###################################################################################################################################
data_location_2020 <- ease_data_for_visualization %>%
  filter(years_ == 2020) %>%
  group_by(user_location) %>%
  summarise(nbr_user = length(unique(user_tracking_id))) %>%
  arrange(desc(nbr_user))

# calculate the most purchased service, number of user per location 
data_location_sale_2020 <- ease_data_for_visualization %>%
  filter(years_ == 2020,
         purchase_y_n == "1") %>%
  group_by(user_location, service_name, month_year) %>%
  summarise(nbr_user = length(unique(user_tracking_id)),
            total_sale = sum(price_offer)) %>%
arrange(desc(total_sale))

data_location_2021 <- ease_data_for_visualization %>%
  filter(years_ == 2021) %>%
  group_by(user_location) %>%
  summarise(nbr_user = length(unique(user_tracking_id))) %>%
  arrange(desc(nbr_user))

sum(data_location_2021$nbr_user)

data_location <- ease_data_for_visualization %>%
  #filter(years_ == 2021) %>%
  group_by(user_location, years_) %>%
  summarise(nbr_user = length(unique(user_tracking_id))) %>%
  arrange(desc(nbr_user))



# what Service has most visited per year?

df_most_visited_service_month_year <- ease_data_for_visualization %>%
  filter(#purchase_y_n == "1",
         years_ == 2021) %>%
  group_by(service_name, month_year) %>%
  summarise(most_visited_service = n(),
            service_payed = sum(price_offer)) #%>%
 # arrange(most_visited_service) 

################################

user_table2 <- df_most_visited_service_month_year  %>% 
  kableExtra::kbl(align = "c") %>% 
  kableExtra::kable_styling(
    bootstrap_options = c("striped","hover","condensed"),
    full_width = F,
    font_size = 16,
    fixed_thead = T
  ) %>% 
  kableExtra::scroll_box(
    height = "500px")

user_table2
################################

str(df_most_visited_service_month_year)

fig_service_year <- plotly::plot_ly(df_most_visited_service_month_year, x = ~month_year,
                                    type = "bar",
                                    y = ~most_visited_service, color = ~service_name,
                                    text = ~most_visited_service, textposition = 'outside',
                                    hovertext = paste("Date :",df_most_visited_service_month_year$month_year,
                                                      "<br>Service name :", df_most_visited_service_month_year$service_name,
                                                      "<br>Nber of Service :", df_most_visited_service_month_year$most_visited_service,
                                                      "<br>Sold:",round(df_most_visited_service_month_year$service_payed/1000000,
                                                                        digits = 1), "million CFA"),
                                    hoverinfo = 'text') %>%
  layout(title = "",
         legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Service name</b>')),
         uniformtext=list(minsize=15, mode='show'),
         xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                      # change x-axix size
                      tickfont = list(size = 14),
                      # change x-title size
                      titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                      tickangle= -45, tickvals = df_most_visited_service_month_year$month_year),
         yaxis = list(title = "<b> Number of service </b>",
                      titlefont = list(size = 16),
                      # change x-axix size
                      tickfont = list(size = 14))
         #tickvals = df_most_visited_service_month_year$most_visited_service)
         #hoverlabel=list(bgcolor="gainsboro")
         #width = 500, autosize=F,
         #bargap = 0.1, bargroupgap = 0.1,
  ) %>%
  config(displayModeBar = F, 
         scrollZoom = T)
fig_service_year

####################################### sold Service bar representation #################################

df_most_visited_service_month_year <- ease_data_for_visualization %>%
  filter(purchase_y_n == "1",
         years_ == 2021) %>%
  group_by(service_name, month_year) %>%
  summarise(most_visited_service = n(),
            service_payed = sum(price_offer)) #%>%
# arrange(most_visited_service) 


fig_service_year2 <- plotly::plot_ly(df_most_visited_service_month_year, x = ~month_year,
                                    type = "bar",
                                    y = ~service_payed, color = ~service_name,
                                    colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                                    text = ~most_visited_service, textposition = 'outside',
                                    hovertext = paste("Date :",df_most_visited_service_month_year$month_year,
                                                      "<br>Service name :", df_most_visited_service_month_year$service_name,
                                                      "<br>Nber of times ", df_most_visited_service_month_year$service_name, " purchased :", df_most_visited_service_month_year$most_visited_service,
                                                      "<br>Purchased:",round(df_most_visited_service_month_year$service_payed/1000000,
                                                                        digits = 1), "million CFA"),
                                    hoverinfo = 'text') %>%
  layout(title = "Total purchsed by each service per month",
         legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Service name</b>')),
         uniformtext=list(minsize=15, mode='show'),
         xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                      # change x-axix size
                      tickfont = list(size = 14), 
                      # change x-title size
                      titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                      tickangle= -45, tickvals = df_most_visited_service_month_year$month_year),
         yaxis = list(title = "<b> Purchased service in CFA </b>",
                      titlefont = list(size = 16),
                      # change x-axix size
                      tickfont = list(size = 14))
         #tickvals = df_most_visited_service_month_year$most_visited_service)
         #hoverlabel=list(bgcolor="gainsboro")
         #width = 500, autosize=F,
         #bargap = 0.1, bargroupgap = 0.1,
  ) %>%
  config(displayModeBar = F, 
         scrollZoom = T)

fig_service_year2


##########################################    Service stacked ####################################################
#colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen")
fig_service_year2_stack <- plotly::plot_ly(df_most_visited_service_month_year, x = ~month_year,
                                     type = "bar",
                                     y = ~service_payed, color = ~service_name,
                                     #colors = c("#D2B48C", "#4682B4", "#008080", "darkgreen"),
                                     #colors = c("#D2B48C", "#4169E1", "#DB7093", "#2E8B57"),
                                     colors = c("#DB7093", "#9370DB", "#87CEEB", "#66CDAA"),
                                     #text = ~most_visited_service, textposition = 'outside',
                                     hovertext = paste("Date :",df_most_visited_service_month_year$month_year,
                                                       "<br>Service name :", df_most_visited_service_month_year$service_name,
                                                       "<br>Nber of Service :", df_most_visited_service_month_year$most_visited_service,
                                                       "<br>Purchased:",round(df_most_visited_service_month_year$service_payed/1000000,
                                                                              digits = 1), "million CFA"),
                                     hoverinfo = 'text') %>%
  layout(title = "Total purchsed by each service per month",  barmode="stack", bargap=0.3,
         legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Service name</b>')),
         uniformtext=list(minsize=15, mode='show'),
         xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                      # change x-axix size
                      tickfont = list(size = 14), 
                      # change x-title size
                      titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                      tickangle= -45, tickvals = df_most_visited_service_month_year$month_year),
         yaxis = list(title = "<b> Purchased service in CFA </b>",
                      #categoryorder = "total descending",
                      titlefont = list(size = 16),
                      # change x-axix size
                      tickfont = list(size = 14))
         #tickvals = df_most_visited_service_month_year$most_visited_service)
         #hoverlabel=list(bgcolor="gainsboro")
         #width = 500, autosize=F,
         #bargap = 0.1, bargroupgap = 0.1,
  ) %>%
  config(displayModeBar = F, 
         scrollZoom = T)

fig_service_year2_stack


####################################### end sold Service  add_lines #############################


fig_plot_service <- plotly::plot_ly(df_most_visited_service_month_year, color = ~service_name, 
                                    colors = c("#DAA520", "#556B2F", "#FF8C00", "#7B68EE")) %>%
  add_lines(x = ~month_year, y = ~service_payed, linetype = ~service_name, line = list( width = 3),
            hovertemplate = paste("<b>",formatC(df_most_visited_service_month_year$service_payed, 
                                                format = "d", big.mark = "."), "CFA", "<b>")) %>%
  layout(hovermode = "x unified",
         title = "Total purchsed by each service per month",
         uniformtext=list(minsize=15, mode='show'),
         legend = list(x = 100, y = 0.90, title=list(color= "blue", text='<b> Service name </b>')),
         xaxis = list(title = "<b> Date </b>", tickangle= -45, #type="date", tickformat="%m-%Y",
                      tickvals = df_most_visited_service_month_year$month_year,
                      tickfont = list(size = 14), # size from x axis values
                      titlefont = list(size = 15)),
         yaxis = list(title = "<b> Purchasing in CFA </b>", titlefont = list( size = 16))) %>%
  config(displayModeBar = F, scrollZoom = T)
  
  
fig_plot_service



## Display pie chart service
library(formattable)

df_pie_chart_service_month_year <- ease_data_for_visualization %>%
  filter(purchase_y_n == "1",
         years_ == 2021) %>%
  group_by(service_name) %>%
  summarise(most_visited_service = n(),
            service_payed = sum(price_offer)) %>%
  mutate(#freq = round(service_payed / sum(service_payed), 3),
         pop = formattable::percent(service_payed / sum(service_payed)))
# arrange(most_visited_service) 

fig <- plot_ly(df_pie_chart_service_month_year, type='pie', labels = ~service_name,
               textinfo = 'label+percent',
               #texttemplate = "%{percent:.1%}",
               #hoverinfo = 'text',
               hovertemplate = " Service name: %{label} <br> Percentage: %{percent:.1%} <br><extra></extra>",
               # <extra></extra> to remove trace 0
               values = ~pop, textposition = 'outside')
fig <- fig %>% layout(uniformtext=list(minsize=12, mode='hide'))
fig

################ start donuts chart ########################################
fig_donut <- plotly::plot_ly(df_pie_chart_service_month_year, labels = ~service_name, values = ~service_payed,
                             textposition = 'outside', textinfo = 'label+percent',
                             marker = list(colors = c("#CD5C5C", "#8FBC8F", "#DAA520", "#7B68EE")))
                             #marker = list(colors = c("#FDC086",  "#7FC97F", "#BEAED4", "#FFFF99")))

                            # color = ~service_payed, colors = c("#DAA520", "#556B2F", "#FF8C00", "#7B68EE"))
fig_donut <- fig_donut %>% add_pie(hole = 0.6)
fig_donut <- fig_donut %>% layout(title = "Donut charts using Plotly",  showlegend = T,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig_donut
################ end donuts chart ########################################


#################### year_ == 2022 service add_lines #################

df_most_visited_service_month_year <- ease_data_for_visualization %>%
  filter(purchase_y_n == "1",
         years_ == 2021) %>%
  group_by(service_name, month_year) %>%
  summarise(most_visited_service = n(),
            service_payed = sum(price_offer)) #%>%
# arrange(most_visited_service) 

fig_plot_service <- plotly::plot_ly(df_most_visited_service_month_year) %>%
  add_lines(x = ~month_year, y = ~service_payed, linetype = ~service_name, line = list( width = 3),
            hovertemplate = paste("<b>",formatC(df_most_visited_service_month_year$service_payed, 
                                                format = "d", big.mark = "."), "CFA", "<b>")) %>%
  layout(hovermode = "x unified",
         title = "Total purchsed by each service per month",
         uniformtext=list(minsize=15, mode='show'),
         legend = list(x = 100, y = 0.90, title=list(color= "blue", text='<b> Service name </b>')),
         xaxis = list(title = "<b> Date </b>", tickangle= -45, #type="date", tickformat="%m-%Y",
                      tickvals = df_most_visited_service_month_year$month_year,
                      tickfont = list(size = 14), # size from x axis values
                      titlefont = list(size = 15)),
         yaxis = list(title = "<b> Purchasing in CFA </b>", titlefont = list( size = 16))) %>%
  config(displayModeBar = T, scrollZoom = T)


fig_plot_service

#################### end year_ == 2022 service add_lines #################

#-------------------------------------
# KPI: What  offer is most purchased ?
#-------------------------------------

# calculate all offer purchased
df_most_offer <- ease_data_for_visualization %>%
  filter(years_ == "2019",
         purchase_y_n == "1") %>%
  group_by(offer_name, month_year) %>%
  summarise(most_visited_offer = n(),
            offer_purchased = sum(price_offer))


#unique(df_most_offer$offer_name)

################# start display bar chart ###############################################################
fig_offer_year <- plotly::plot_ly(df_most_offer, x = ~month_year,
                                     type = "bar",
                                     y = ~offer_purchased, color = ~offer_name,
                                     colors = "Paired",
                                     #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                                     text = ~most_visited_offer, textposition = 'outside',
                                     hovertext = paste("Date :",df_most_offer$month_year,
                                                       "<br>Offer name :", df_most_offer$offer_name,
                                                       "<br>Nber of offer :", df_most_offer$most_visited_offer,
                                                       "<br>Purchased:",round(df_most_offer$offer_purchased/1000000,
                                                                              digits = 1), "million CFA"),
                                     hoverinfo = 'text') %>%
  layout(title = "Total purchsed by each offer per month",
         legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Offer name</b>')),
         uniformtext=list(minsize=15, mode='show'),
         xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                      # change x-axix size
                      tickfont = list(size = 14), 
                      # change x-title size
                      titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                      tickangle= -45, tickvals = df_most_offer$month_year),
         yaxis = list(title = "<b> Purchased offer in CFA </b>",
                      titlefont = list(size = 16),
                      # change x-axix size
                      tickfont = list(size = 14))
         #tickvals = df_most_visited_service_month_year$most_visited_service)
         #hoverlabel=list(bgcolor="gainsboro")
         #width = 500, autosize=F,
         #bargap = 0.1, bargroupgap = 0.1,
  ) %>%
  config(displayModeBar = F, 
         scrollZoom = T)

fig_offer_year

################# end display bar chart ###############################################################


################ start display bar stacked #############################################################

fig_offer_year_stack <- plotly::plot_ly(df_most_offer, x = ~month_year,
                                  type = "bar",
                                  y = ~offer_purchased, color = ~offer_name,
                                  colors = "Paired",
                                  #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                                  #text = ~most_visited_offer, textposition = 'outside',
                                  hovertext = paste("Date :",df_most_offer$month_year,
                                                    "<br>Offer name :", df_most_offer$offer_name,
                                                    "<br>Nber of offer :", df_most_offer$most_visited_offer,
                                                    "<br>Purchased:",round(df_most_offer$offer_purchased/1000000,
                                                                           digits = 1), "million CFA"),
                                  hoverinfo = 'text') %>%
  layout(title = "Total purchsed by each offer per month",  barmode="stack", bargap=0.3,
         legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Offer name</b>')),
         uniformtext=list(minsize=15, mode='show'),
         xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                      # change x-axix size
                      tickfont = list(size = 14), 
                      # change x-title size
                      titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                      tickangle= -45, tickvals = df_most_offer$month_year),
         yaxis = list(title = "<b> Purchased offer in CFA </b>",
                      titlefont = list(size = 16),
                      # change x-axix size
                      tickfont = list(size = 14))
         #tickvals = df_most_visited_service_month_year$most_visited_service)
         #hoverlabel=list(bgcolor="gainsboro")
         #width = 500, autosize=F,
         #bargap = 0.1, bargroupgap = 0.1,
  ) %>%
  config(displayModeBar = F, 
         scrollZoom = T)

fig_offer_year_stack
################ end display bar stacked #############################################################


############ start display add_lines #####################################################
# calculate all offer purchased
df_most_offer <- ease_data_for_visualization %>%
  filter(years_ == "2019",
         purchase_y_n == "1") %>%
  group_by(offer_name, month_year) %>%
  summarise(most_visited_offer = n(),
            offer_purchased = sum(price_offer))


fig_plot_offer <- plotly::plot_ly(df_most_offer, color = ~offer_name, 
                                  colors = "Paired") %>%
  add_lines(x = ~month_year, y = ~offer_purchased, linetype = ~offer_name, line = list( width = 3),
            hovertemplate = paste("<b>",formatC(df_most_offer$offer_purchased, 
                                                format = "d", big.mark = "."), "CFA", "<b>")) %>%
  layout(hovermode = "x unified",
         title = "Total purchsed by each offer per month",
         uniformtext=list(minsize=15, mode='show'),
         legend = list(x = 100, y = 0.90, title=list(color= "blue", text='<b> Service name </b>')),
         xaxis = list(title = "<b> Date </b>", tickangle= -45, #type="date", tickformat="%m-%Y",
                      tickvals = df_most_offer$month_year,
                      tickfont = list(size = 14), # size from x axis values
                      titlefont = list(size = 15)),
         yaxis = list(title = "<b> Purchasing in CFA </b>", titlefont = list( size = 16))) %>%
  config(displayModeBar = T, scrollZoom = T)


fig_plot_offer

############ end display add_lines #####################################################

# calculate all offer purchased filter by service name

#unique(ease_data_for_visualization$service_name)
# "Travel packages" "Apartements"     "Hotels"    "Car rentals"

df_most_offer_filter_service <- ease_data_for_visualization %>%
  filter(years_ == "2019",
         purchase_y_n == "1",
         service_name == "Car rentals") %>%
  group_by(service_name, offer_name, month_year) %>%
  summarise( most_visited_offer = n(),
            offer_purchased = sum(price_offer))


# unique(df_most_offer_filter_service$offer_name)
# [1] "Location car bafoussam" "Location car douala"    "Location car limbe"     "Location car yaounde" 

################# start display bar chart ###############################################################
fig_offer_year_filter_service <- plotly::plot_ly(df_most_offer_filter_service, x = ~month_year,
                                  type = "bar",
                                  y = ~offer_purchased, color = ~offer_name,
                                  colors = "Paired",
                                  #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                                  text = ~most_visited_offer, textposition = 'outside',
                                  hovertext = paste("Date :",df_most_offer_filter_service$month_year,
                                                    "<br>Service name :", df_most_offer_filter_service$service_name,
                                                    "<br>Offer name :", df_most_offer_filter_service$offer_name,
                                                    "<br>Nber of times ", df_most_offer_filter_service$offer_name, 
                                                    " purchased :", df_most_offer_filter_service$most_visited_offer,
                                                    "<br>Purchased:",round(df_most_offer_filter_service$offer_purchased/1000000,
                                                                           digits = 1), "million CFA"),
                                  hoverinfo = 'text') %>%
  layout(title = "Total purchsed by each offer per month",
         legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Offer name</b>')),
         uniformtext=list(minsize=15, mode='show'),
         xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                      # change x-axix size
                      tickfont = list(size = 14), 
                      # change x-title size
                      titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                      tickangle= -45, tickvals = df_most_offer_filter_service$month_year),
         yaxis = list(title = "<b> Purchased offer in CFA </b>",
                      titlefont = list(size = 16),
                      # change x-axix size
                      tickfont = list(size = 14))
         #tickvals = df_most_visited_service_month_year$most_visited_service)
         #hoverlabel=list(bgcolor="gainsboro")
         #width = 500, autosize=F,
         #bargap = 0.1, bargroupgap = 0.1,
  ) %>%
  config(displayModeBar = F, 
         scrollZoom = T)

fig_offer_year_filter_service

################# end display bar chart ###############################################################

################# start display bar chart stack ###############################################################

fig_offer_year_filter_service_stack <- plotly::plot_ly(df_most_offer_filter_service, x = ~month_year,
                                                 type = "bar",
                                                 y = ~offer_purchased, color = ~offer_name,
                                                 colors = "Paired",
                                                 #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                                                 #text = ~most_visited_offer, textposition = 'outside',
                                                 hovertext = paste("Date :",df_most_offer_filter_service$month_year,
                                                                   "<br>Service name :", df_most_offer_filter_service$service_name,
                                                                   "<br>Offer name :", df_most_offer_filter_service$offer_name,
                                                                   "<br>Nber of offer :", df_most_offer_filter_service$most_visited_offer,
                                                                   "<br>Purchased:",round(df_most_offer_filter_service$offer_purchased/1000000,
                                                                                          digits = 1), "million CFA"),
                                                 hoverinfo = 'text') %>%
  layout(title = "Total purchsed by each offer per month",  barmode="stack", bargap=0.6,
         legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Offer name</b>')),
         uniformtext=list(minsize=15, mode='show'),
         xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                      # change x-axix size
                      tickfont = list(size = 14), 
                      # change x-title size
                      titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                      tickangle= -45, tickvals = df_most_offer_filter_service$month_year),
         yaxis = list(title = "<b> Purchased offer in CFA </b>",
                      titlefont = list(size = 16),
                      # change x-axix size
                      tickfont = list(size = 14))
         #tickvals = df_most_visited_service_month_year$most_visited_service)
         #hoverlabel=list(bgcolor="gainsboro")
         #width = 500, autosize=F,
         #bargap = 0.1, bargroupgap = 0.1,
  ) %>%
  config(displayModeBar = F, 
         scrollZoom = T)

fig_offer_year_filter_service_stack

################# end display bar chart stack ###############################################################


# calculate all offer purchased filter by service name

#unique(ease_data_for_visualization$service_name)
# "Travel packages" "Apartements"     "Hotels"          "Car rentals"

df_most_offer_filter_service <- ease_data_for_visualization %>%
  filter(years_ == "2019",
         purchase_y_n == "1",
         service_name == "Car rentals") %>%
  group_by(service_name, offer_name, month_year) %>%
  summarise( most_visited_offer = n(),
             offer_purchased = sum(price_offer))


# unique(df_most_offer_filter_service$offer_name)
# [1] "Location car bafoussam" "Location car douala"    "Location car limbe"     "Location car yaounde" 

############ end display add_lines #####################################################

fig_plot_offer_filter_service <- plotly::plot_ly(df_most_offer_filter_service, color = ~offer_name, 
                                  colors = "Paired") %>%
  add_lines(x = ~month_year, y = ~offer_purchased, linetype = ~offer_name, line = list( width = 3),
            hovertemplate = paste("<b>",formatC(df_most_offer_filter_service$offer_purchased, 
                                                format = "d", big.mark = "."), "CFA", "<b>",
                                  "<br>Service name :", df_most_offer_filter_service$service_name)) %>%
  layout(hovermode = "x unified",
         title = "Total purchsed by each offer per month",
         uniformtext=list(minsize=15, mode='show'),
         legend = list(x = 100, y = 0.90, title=list(color= "blue", text='<b> Offer name </b>')),
         xaxis = list(title = "<b> Date </b>", tickangle= -45, #type="date", tickformat="%m-%Y",
                      tickvals = df_most_offer_filter_service$month_year,
                      tickfont = list(size = 14), # size from x axis values
                      titlefont = list(size = 15)),
         yaxis = list(title = "<b> Purchasing in CFA </b>", titlefont = list( size = 16))) %>%
  config(displayModeBar = T, scrollZoom = T)


fig_plot_offer_filter_service

############ end display add_lines #####################################################

#----------------------------------------------------------------
# KPI: At what time of day there is more traffic on the website ?
# ----------------------------------------------------------------

ease_data_for_visualization$time_h_m_s

str(ease_data_for_visualization)

df_traffic <- ease_data_for_visualization %>%
  filter(years_ == "2019") %>%
  group_by(month_year) %>%
    summarise(nbr_user = length((user_tracking_id)),
              min_traffic = sum(time_h_m_s)) %>%
  mutate(duration_text = seconds_to_period(min_traffic))

  
fig_offer_year_traffic <- plotly::plot_ly(df_traffic, x = ~month_year,
                                                 type = "bar",
                                                 y = ~min_traffic,
                                                 colors = "Paired",
                                                 #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                                                 text = paste(df_traffic$duration_text, sep = ""), textposition = 'outside',
                                                 hovertext = paste("Date :",df_traffic$month_year,
                                                                   "<br>User :", df_traffic$nbr_user,
                                                                   "<br>Duration :", df_traffic$duration_text),
                                                 hoverinfo = 'text') %>%
  layout(title = "Total connection time per month",
         legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Offer name</b>')),
         uniformtext=list(minsize=15, mode='show'),
         xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                      # change x-axix size
                      tickfont = list(size = 14), 
                      # change x-title size
                      titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                      tickangle= -45, tickvals = df_traffic$month_year),
         yaxis = list(title = "<b> Connection time in seconds </b>",
                      titlefont = list(size = 16),
                      # change x-axix size
                      tickfont = list(size = 14))
         #tickvals = df_most_visited_service_month_year$most_visited_service)
         #hoverlabel=list(bgcolor="gainsboro")
         #width = 500, autosize=F,
         #bargap = 0.1, bargroupgap = 0.1,
  ) %>%
  config(displayModeBar = F, 
         scrollZoom = T)

fig_offer_year_traffic


################# 05.08.2022 traffic overview ##################

df_traffic_2019 <- ease_data_for_visualization %>%
   filter(years_ == "2019") #%>%
  # group_by(month_year) %>%
  # summarise(nbr_user = length((user_tracking_id)),
  #           min_traffic = sum(time_h_m_s)) %>%
  # mutate(duration_text = seconds_to_period(min_traffic))



df_traffic <- ease_data_for_visualization %>%
  filter(years_ == "2019",
         month_ == "August") %>%
  #filter(only_start_end >= min(df_traffic_2019$only_start_end) & only_start_end  <= as.Date("2019-08-31")) %>%
  group_by(only_start_end) %>%
  summarise(nbr_user = length((user_tracking_id)),
                      min_traffic = sum(time_h_m_s)) %>%
             mutate(duration_text = seconds_to_period(min_traffic),
                    real_hours = round(as.numeric(min_traffic) / 3600),
                    Hours = hour(seconds_to_period(min_traffic)))


fig_traffic_overview_per_month <- plotly::plot_ly(df_traffic, x = ~only_start_end,
                                          type = "bar",
                                          y = ~real_hours,
                                          colors = "Paired",
                                          #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                                          #text = paste(df_traffic$duration_text, sep = ""), textposition = 'outside',
                                          hovertext = paste("Date :",df_traffic$only_start_end,
                                                            "<br>Nber of user :", df_traffic$nbr_user,
                                                            "<br>Connection time :", df_traffic$duration_text),
                                          hoverinfo = 'text') %>%
  layout(title = "Total connection time per month",
         legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Offer name</b>')),
         uniformtext=list(minsize=15, mode='show'),
         xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                      # change x-axix size
                      tickfont = list(size = 14), 
                      # change x-title size
                      titlefont = list(size = 16), type="date", tickformat="%d-%b",  #tickformat = "%b-%Y",
                      tickangle= -45, range = df_traffic$only_start_end),
         yaxis = list(title = "<b> Connection time in seconds </b>",
                      titlefont = list(size = 16),
                      # change x-axix size
                      tickfont = list(size = 14))
         #tickvals = df_most_visited_service_month_year$most_visited_service)
         #hoverlabel=list(bgcolor="gainsboro")
         #width = 500, autosize=F,
         #bargap = 0.1, bargroupgap = 0.1,
  ) %>%
  config(displayModeBar = F, 
         scrollZoom = T)

return(fig_traffic_overview_per_month)

fig_offer_year_traffic

Hours = hour(seconds_to_period(df_traffic_2019_month$min_traffic))

timestamp <- df_traffic_2019$time_h_m_s[1] + df_traffic_2019$time_h_m_s[2]

Hours = hour(seconds_to_period(timestamp))

lubridate::

timestamp / 3600

data ="2021/05/25 12:34:25"

# get time from date using format in the 
# form of hours
print(paste(
  "Hours : ", format(as.POSIXct(data), format = "%H")))

test <- df_traffic_2019_month$duration_text[[5]]
lubridate::hour(data)

str(as.data.frame(df_traffic_2019_month))

max(df_traffic_2019$only_start_end)
str(min(df_traffic_2019$only_start_end))

################# end 05.08.2022 traffic overview ##################

################## 04.08.2022 ################################

## calculate total purchased service for each year box

df_total_purchased_service <- ease_data_for_visualization %>%
  filter(years_ == "2020",
         purchase_y_n == "1") %>%
  #group_by(service_name) %>%
  summarise(total_purchased = sum(price_offer))

#total_sales_service = sum(df_total_purchased_service$total_purchased_service)

str_length(as.character(df_total_purchased_service$total_purchased))

#substr(as.character(total_sales_service),1,3)


#substr(as.character(total_sales_service),1,nchar(as.character(total_sales_service))-4)

total_service_sales <- function(){
  if(str_length(as.character(df_total_purchased_service$total_purchased)) == 9) {
    x <- substr(as.character(df_total_purchased_service$total_purchased),1,3)
  } else{
    x <- substr(as.character(df_total_purchased_service$total_purchased),1,4)
  }
  print(x)
}

paste(total_service_sales(), " Mio. CFA", sep = "")

df_total_purchased_service1 <- ease_data_for_visualization %>%
  filter(years_ == "2019",
         purchase_y_n == "1") %>%
  #group_by(service_name) %>%
  summarise(total_purchased_service = sum(price_offer))

df_total_purchased_offer <- ease_data_for_visualization %>%
  filter(years_ == "2019",
         purchase_y_n == "1") %>%
  group_by(offer_name) %>%
  summarise(total_purchased_offer = sum(price_offer))

total_sales_offer = sum(df_total_purchased_offer$total_purchased_offer)


dfdata2019 <- ease_data_for_visualization %>%
  filter(years_ == "2019") %>%
  select(service_name)

sort(unique(dfdata2019$service_name), decreasing = T)
length(unique(dfdata2019$service_name))

dfdata2020 <- ease_data_for_visualization %>%
  filter(years_ == "2020") %>%
  select(service_name)

sort(unique(dfdata2020$service_name), decreasing = T)
unique(dfdata2020$service_name)
length(unique(dfdata2020$service_name))

dfdata2021 <- ease_data_for_visualization %>%
  filter(years_ == "2021") %>%
  select(service_name)

sort(unique(dfdata2021$service_name), decreasing = T)
unique(dfdata2021$service_name)
length(unique(dfdata2021$service_name))

dfdata2022 <- ease_data_for_visualization %>%
  filter(years_ == "2022") %>%
  select(service_name)

sort(unique(dfdata2022$service_name), decreasing = T)
unique(dfdata2022$service_name)
length(unique(dfdata2022$service_name))


# count number of user per year
dfuser2019 <- ease_data_for_visualization %>%
  filter(years_ == "2019") %>%
  summarise(countuser = length(unique(user_tracking_id)))
dfuser2019

dfuser2020 <- ease_data_for_visualization %>%
  filter(years_ == "2020") %>%
  summarise(countuser = length(unique(user_tracking_id)))
dfuser2020

dfuser2021 <- ease_data_for_visualization %>%
  filter(years_ == "2021") %>%
  summarise(countuser = length(unique(user_tracking_id)))
dfuser2021

dfuser2022 <- ease_data_for_visualization %>%
  filter(years_ == "2022") %>%
  summarise(countuser = length(unique(user_tracking_id)))
dfuser2022

# number of time the time that the website have been visited per year

dftime_2019 <- ease_data_for_visualization %>%
  filter(years_ == "2019") %>%
  summarise(ease_number_of_visite = length(user_tracking_id))
dftime_2019

df_active_user <- ease_data_for_visualization %>%
  filter(years_ == "2019",
         purchase_y_n == "1") %>%
  summarise(ease_number_of_visite = length(user_tracking_id))
dftime_2019_active

dftime2020 <- ease_data_for_visualization %>%
  filter(years_ == "2020") %>%
  summarise(ease_number_of_visite = length(user_tracking_id))
dftime2020

dftime2021 <- ease_data_for_visualization %>%
  filter(years_ == "2021") %>%
  summarise(ease_number_of_visite = length(user_tracking_id))
dftime2021

dftime2022 <- ease_data_for_visualization %>%
  filter(years_ == "2022") %>%
  summarise(ease_number_of_visite = length(user_tracking_id))
dftime2022

# the location with the mostbpurchase per years

df_location_purchase <- ease_data_for_visualization %>%
  filter(years_ == "2019", purchase_y_n == "1") %>%
  group_by(user_location) %>%
  summarise(location_most_purchase = sum(price_offer)) %>%
  arrange(desc(location_most_purchase))

most_purchase <- df_location_purchase$location_most_purchase[1]

best_location <- df_location_purchase$user_location[1]


dflocationpurchase2020 <- ease_data_for_visualization %>%
  filter(years_ == "2020", purchase_y_n == "1") %>%
  group_by(user_location) %>%
  summarise(location_most_purchase = sum(price_offer)) %>%
  arrange(desc(location_most_purchase))

dflocationpurchase2021 <- ease_data_for_visualization %>%
  filter(years_ == "2021", purchase_y_n == "1") %>%
  group_by(user_location) %>%
  summarise(location_most_purchase = sum(price_offer)) %>%
  arrange(desc(location_most_purchase))

dflocationpurchase2022 <- ease_data_for_visualization %>%
  filter(years_ == "2022", purchase_y_n == "1") %>%
  group_by(user_location) %>%
  summarise(location_most_purchase = sum(price_offer)) %>%
  arrange(desc(location_most_purchase))



