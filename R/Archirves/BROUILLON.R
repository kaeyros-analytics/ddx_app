# 
# data_2019 <- ease_data_for_visualization %>%
#   filter(years_ == 2019)
# 
# table(data_2019$month_year, useNA = "always")
# 
# data_2020 <- ease_data_for_visualization %>%
#   filter(years_ == 2020)
# 
# table(data_2020$month_year, useNA = "always")
# 
# data_2021 <- ease_data_for_visualization %>%
#   filter(years_ == 2021)
# 
# table(data_2021$month_year, useNA = "always")
# 
# 
# fig_service_year2_stack <- plotly::plot_ly(df_most_visited_service_month_year, x = ~month_year,
#                                            type = "bar", 
#                                            y = ~service_payed, color = ~service_name,
#                                            colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
#                                            text = ~most_visited_service, textposition = 'outside',
#                                            hovertext = paste("Date :",df_most_visited_service_month_year$month_year,
#                                                              "<br>Service name :", df_most_visited_service_month_year$service_name,
#                                                              "<br>Nber of Service :", df_most_visited_service_month_year$most_visited_service,
#                                                              "<br>Purchased:",round(df_most_visited_service_month_year$service_payed/1000000,
#                                                                                     digits = 1), "million CFA"),
#                                            hoverinfo = 'text') %>%
#   layout(title = "",  barmode="stack", bargap=0.3,
#          legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Service name</b>')),
#          uniformtext=list(minsize=15, mode='show'),
#          xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
#                       #categoryorder = "total descending",
#                       # change x-axix size
#                       tickfont = list(size = 14), 
#                       # change x-title size
#                       titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
#                       tickangle= -45, tickvals = df_most_visited_service_month_year$month_year),
#          yaxis = list(title = "<b> Purchased service in CFA </b>",
#                       titlefont = list(size = 16),
#                       categoryorder = "total descending",
#                       # change x-axix size
#                       tickfont = list(size = 14))
#          #tickvals = df_most_visited_service_month_year$most_visited_service)
#          #hoverlabel=list(bgcolor="gainsboro")
#          #width = 500, autosize=F,
#          #bargap = 0.1, bargroupgap = 0.1,
#   ) %>%
#   config(displayModeBar = F, 
#          scrollZoom = T)
# 
# fig_service_year2_stack
# 
# 
# 
# ########################## 26.07.2022 #############################################
# 
# #unique(ease_data_for_visualization$service_name)
# # "Travel packages" "Apartements"     "Hotels"          "Car rentals"
# 
# df_most_offer_filter_service <- ease_data_for_visualization %>%
#   filter(years_ == "2019",
#          purchase_y_n == "1",
#          service_name == "Hotels") %>%
#   group_by(service_name, offer_name, month_year) %>%
#   summarise( most_visited_offer = n(),
#              offer_purchased = sum(price_offer))
# 
# plot_title <- paste("Total purchsed per ",df_most_offer_filter_service$service_name[1], " by each offer per month" ,sep = "")
# 
# fig_offer_year_filter_service <- plotly::plot_ly(df_most_offer_filter_service, x = ~month_year,
#                                                  type = "bar",
#                                                  y = ~offer_purchased, color = ~offer_name,
#                                                  #colors = c("#3182bd", "#db4052", "#37536d", "#32ab60"),
#                                                  colors = c("#7c40db", "#cccccc", "#409fdb", "#32ab60"),
#                                                  #colors = "Paired",
#                                                  #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
#                                                  text = ~most_visited_offer, textposition = 'outside',
#                                                  hovertext = paste("Date :",df_most_offer_filter_service$month_year,
#                                                                    "<br>Service name :", df_most_offer_filter_service$service_name,
#                                                                    "<br>Offer name :", df_most_offer_filter_service$offer_name,
#                                                                    "<br>Nber of times purchased :", df_most_offer_filter_service$most_visited_offer,
#                                                                    "<br>Purchased:",round(df_most_offer_filter_service$offer_purchased/1000000,
#                                                                                           digits = 1), "million CFA"),
#                                                  hoverinfo = 'text') %>%
#   layout(title = plot_title,
#          barmode = 'group', bargap = 0.5, bargroupgap = 0.07,
#          #showlegend = FALSE,
#          legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Offer name</b>')),
#          uniformtext=list(minsize=15, mode='show'),
#          xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
#                       # change x-axix size
#                       tickfont = list(size = 14), 
#                       # change x-title size
#                       titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
#                       tickangle= -45, tickvals = df_most_offer_filter_service$month_year),
#          yaxis = list(title = "<b> Purchased offer in CFA </b>",
#                       titlefont = list(size = 16),
#                       # change x-axix size
#                       tickfont = list(size = 14))
#          #tickvals = df_most_visited_service_month_year$most_visited_service)
#          #hoverlabel=list(bgcolor="gainsboro")
#          #width = 500, autosize=F,
#          #bargap = 0.1, bargroupgap = 0.1,
#   ) %>%
#   config(displayModeBar = F, 
#          scrollZoom = T)
# 
# fig_offer_year_filter_service
# 
# 
# 
# #########################en d26.07.2022 ############################################


# if (interactive()) {
#   
#   library(shiny)
#   library(shinyWidgets)
#   
#   ui <- fluidPage(
#     pickerInput(
#       inputId = "groups",
#       label = "Select one from each group below:",
#       choices = list(
#         Group1 = c("1", "2", "3", "4"),
#         Group2 = c("A", "B", "C", "D")
#       ),
#       multiple = TRUE,
#       options =  list("max-options-group" = 1)
#     ),
#     verbatimTextOutput(outputId = "res_grp"),
#     pickerInput(
#       inputId = "groups_2",
#       label = "Select two from each group below:",
#       choices = list(
#         Group1 = c("1", "2", "3", "4"),
#         Group2 = c("A", "B", "C", "D")
#       ),
#       multiple = TRUE,
#       options =  list("max-options-group" = 2)
#     ),
#     verbatimTextOutput(outputId = "res_grp_2"),
#     pickerInput(
#       inputId = "classic",
#       label = "Select max two option below:",
#       choices = c("A", "B", "C", "D"),
#       multiple = TRUE,
#       options =  list(
#         "max-options" = 2,
#         "max-options-text" = "No more!"
#       )
#     ),
#     verbatimTextOutput(outputId = "res_classic")
#   )
#   
#   server <- function(input, output) {
#     
#     output$res_grp <- renderPrint(input$groups)
#     output$res_grp_2 <- renderPrint(input$groups_2)
#     output$res_classic <- renderPrint(input$classic)
#     
#   }
#   
#   shinyApp(ui, server)
#   
# }
###############################################

# 
# InvoiceDate <- ymd_hm(ease_data_simulation$timestamp_end)
# 
# Max_week_sale$InvoiceDate=mdy_hm(Max_week_sale$InvoiceDate)
# 
# date <- as.Date(ease_data_simulation$timestamp_end)
# str(date)
# 
# length(unique(date))
# 
# system.time(Date1 <-lubridate::as_date(ease_data_simulation$timestamp_end))
# 
# only_start_date <- as.Date(ease_data_simulation$timestamp_start , format = "%Y-%m-%d")
# str(only_start_date)
# 
# only_start_date <- as.Date(only_start_date)
# 
# str(ease_data_simulation$timestamp_start)
# 
# month_year <-format(only_start_date,'%Y-%m')
# 
# Month_Yr = format_ISO8601(only_start_date, precision = "ym")
# 
# ym <- zoo::as.yearmon(only_start_date, "%Y-%m")
# str(ym)
# 
# 
# n_days <- as.numeric(ease_data_simulation$timestamp_end - ease_data_simulation$timestamp_start)
# 
# n_days_1 <- ease_data_simulation$timestamp_end - ease_data_simulation$timestamp_start
# 
#  #calculate time difference in minutes
#   n_days_2 <- difftime(ease_data_simulation$timestamp_end , ease_data_simulation$timestamp_start, units="secs")
# 
# n_days_2[1:6]
# ease_data_simulation$timestamp_start[1]
# ease_data_simulation$timestamp_end[1]
# 
# library(hms)
# n_days_3 <- hms::as_hms(difftime(ease_data_simulation$timestamp_end, ease_data_simulation$timestamp_start))
# 
# n_days_3[1:6]
# 
# str(n_days_3)
# hms::as_hms(n_days_3[1] + n_days_3[2])
# 
# library(scales)
# library(kableExtra)
# 
# data_location_sale_2019  %>% 
#   kableExtra::kbl(align = "c") %>% 
#   kableExtra::kable_styling(
#     bootstrap_options = c("striped","hover","condensed"),
#     full_width = F,
#     font_size = 16,
#     fixed_thead = T
#   ) %>% 
#   kableExtra::scroll_box(
#     height = "500px"
#   )
# 
# data_location_sale_2019 <- ease_data_for_visualization %>%
#   filter(years_ == 2019,
#          purchase_y_n == "1") %>%
#   group_by(user_location, service_name, month_year) %>%
#   summarise(nbr_user = length(unique(user_tracking_id)),
#             total_sale = sum(price_offer)) %>%
#   arrange(desc(total_sale))
# 
# # the top 10 purchased
# top10_purchased <- head(data_location_sale_2019, 10)
# 
# # the top 10 user location
# top_user_location <- top10_purchased$user_location 
# 
# # search only the information from the top 10 agents
# top_10_user_location <- which(data_location_sale_2019$user_location %in% top_user_location)
# 
# data_top_user_location <- data_location_sale_2019[top_10_user_location, ]
# 
# plot_top10_user_location <- plot_ly(data_top_user_location) %>%
#   #filter(only_date >= input$period_time4[1] & only_date <= input$period_time4[2]) %>%
#   add_lines(x = ~month_year, y = ~total_sale, linetype = ~user_location, line = list( width = 3),
#             # change transaction volume appears here strong
#             hovertemplate = paste("<b>",formatC(data_top_user_location$total_sale, format = "d",big.mark = "."), "CFA", "<b>")) %>%
#   config(displayModeBar = F, scrollZoom = T) %>%
#   layout(hovermode = "x unified", #scroll vertical linie over the date
#          legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b> Service type </b>')),
#          uniformtext=list(minsize=15, mode='show'),
#          xaxis = list(title = "<b> Date </b>", tickangle= -45,
#                       #title = "<b> Date </b>",type="date", dtick = "M1", tickangle= -45,
#                       #tickvals = data_top_service_type$only_date,
#                       tickfont = list(size = 14), # size from x axis values
#                       titlefont = list(size = 15)),
#          yaxis = list(title = "<b> Transaction volume in CFA </b>", titlefont = list( size = 16)))
# 
# plot_top10_user_location
# 
# ######################### visualization with type = "bar" ############################
# 
# data_location_sale_2019_ <- ease_data_for_visualization %>%
#   filter(years_ == 2019,
#          purchase_y_n == "1") %>%
#   group_by(user_location, month_year) %>%
#   summarise(nbr_user = length(unique(user_tracking_id)),
#             total_sale = sum(price_offer)) %>%
#   arrange(desc(total_sale))
# 
# # the top 10 purchased
# top10_purchased <- head(data_location_sale_2019_, 10)
# 
# # the top 10 user location
# top_user_location <- top10_purchased$user_location 
# 
# # search only the information from the top 10 agents
# data_top_user_location_ <- which(data_location_sale_2019_$user_location %in% top_user_location)
# 
# data_top_user_location_ <- data_location_sale_2019_[data_top_user_location_, ]
# 
#  
# 
# plot_top10_user_location_stack_ <- plotly::plot_ly(data_location_sale_2019_, x = ~month_year,
#                                            type = "bar", 
#                                            y = ~total_sale, color = ~user_location,
#                                            colors = c("slateblue", "#008080", "#BDB76B", "#B8860B"),
#                                            #text = ~most_visited_service, textposition = 'outside',
#                                            hovertext = paste("Date :",data_location_sale_2019_$month_year,
#                                                              "<br>User location :", data_location_sale_2019_$user_location,
#                                                              "<br>Nber of user :", data_location_sale_2019_$nbr_user,
#                                                              "<br>Purchased:",round(data_location_sale_2019_$total_sale/1000000,
#                                                                                     digits = 1), "million CFA"),
#                                            hoverinfo = 'text') %>%
#   layout(title = "Total purchased of each location per month",  barmode="stack", bargap=0.3,
#          legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>User location</b>')),
#          uniformtext=list(minsize=15, mode='show'),
#          xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
#                       #categoryorder = "total descending",
#                       # change x-axix size
#                       tickfont = list(size = 14), 
#                       # change x-title size
#                       titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
#                       tickangle= -45, tickvals = data_location_sale_2019_$month_year),
#          yaxis = list(title = "<b> Purchased in CFA </b>",
#                       titlefont = list(size = 16),
#                       #categoryorder = "total descending",
#                       # change x-axix size
#                       tickfont = list(size = 14))
#          #tickvals = df_most_visited_service_month_year$most_visited_service)
#          #hoverlabel=list(bgcolor="gainsboro")
#          #width = 500, autosize=F,
#          #bargap = 0.1, bargroupgap = 0.1,
#   ) %>%
#   config(displayModeBar = F, 
#          scrollZoom = T)
# 
# 
# plot_top10_user_location_stack_
# 
# 
# ####################### end visualization with type = "bar"  user location #################
# 
# ####################### start visualization with type = "line"  user location ##############
# 
# colline <- c("slateblue", "#008080", "#BDB76B", "#B8860B", "#FFF8DC", "#A9A9A9", "#FF7F50", "#5F9EA0")
# 
# ltys <- c(
#   Austin = "dashdot",
#   `Collin County` = "longdash",
#   Dallas = "dash",
#   Houston = "solid",
#   `San Antonio` = "dot"
# )
# fig_test_plot <- plotly::plot_ly(data_location_sale_2019_) %>%
#   add_lines(x = ~month_year, y = ~total_sale, linetype = ~user_location
#             #line = list(colors = c("slateblue", "#008080", "#BDB76B", "#B8860B", "#FFF8DC", "#A9A9A9", "#FF7F50", "#5F9EA0"))
#              ) %>%
#   layout(title = "Total purchased of each location per month",
#     hovermode = "x unified",
#          uniformtext=list(minsize=15, mode='show'),
#          legend = list(x = 100, y = 0.90, title=list(color= "blue", text='<b> User location </b>')),
#          xaxis = list(title = "<b> Date </b>", tickangle= -45, #type="date", tickformat="%m-%Y",
#                       tickvals = data_location_sale_2019_$month_year,
#                       tickfont = list(size = 14), # size from x axis values
#                       titlefont = list(size = 15)),
#          yaxis = list(title = "<b> Purchasing in CFA </b>", titlefont = list( size = 16))) %>%
#   config(displayModeBar = T, scrollZoom = T)
# 
# fig_test_plot
# ####################### end visualization with type = "line"  user location ##############
# 
# 
# 
# df_most_visited_service_month_year <- ease_data_for_visualization %>%
#   filter(purchase_y_n == "1",
#          years_ == 2021) %>%
#   group_by(service_name, month_year) %>%
#   summarise(most_visited_service = n(),
#             service_payed = sum(price_offer)) #%>%
# # arrange(most_visited_service) 
# 
# data_location_sale_2019 <- ease_data_for_visualization %>%
#   filter(years_ == 2021, purchase_y_n == "1") %>%
#   group_by(service_name, month_year) %>%
#   summarise(total_sale = sum(price_offer))
#             
# test_data <- data_location_sale_2019 %>%
#   filter(user_location == "Abidjan",
#          service_name == "Travel packages")
# 
# test_data <- as.data.frame(test_data)
# 
# str(test_data)
# 
# plot(test_data$month_year, test_data$total_sale)
# 
# #################################### other #######################
# fig <- plot_ly(data_top_user_location, x = ~month_year, y = ~total_sale, color = ~service_name, 
#                mode = 'lines') 
# fig <- fig %>% add_lines()
# 
# fig
# 
# fig_test2 <- plotly::plot_ly(data_top_user_location) %>%
#   add_lines(x = ~month_year, y = ~total_sale, linetype = ~user_location) %>%
#   layout(hovermode = "x unified",
#          uniformtext=list(minsize=15, mode='show'),
#          legend = list(x = 100, y = 0.90, title=list(color= "blue", text='<b> Service name </b>')),
#          xaxis = list(title = "<b> Date </b>", tickangle= -45, #type="date", tickformat="%m-%Y",
#                       tickvals = data_top_user_location$month_year,
#                       tickfont = list(size = 14), # size from x axis values
#                       titlefont = list(size = 15)),
#          yaxis = list(title = "<b> Purchasing in CFA </b>", titlefont = list( size = 16))) %>%
#   config(displayModeBar = T, scrollZoom = T)
# 
# fig_test2
# 
# #############################################################################################
# 
# 
# fig_test <- plotly::plot_ly(data_location_sale_2019) %>%
#   add_lines(x = ~month_year, y = ~total_sale, linetype = ~service_name) %>%
#   layout(hovermode = "x unified",
#          uniformtext=list(minsize=15, mode='show'),
#          legend = list(x = 100, y = 0.90, title=list(color= "blue", text='<b> Service name </b>')),
#          xaxis = list(title = "<b> Date </b>", tickangle= -45, #type="date", tickformat="%m-%Y",
#                       tickvals = data_location_sale_2019$month_year,
#                       tickfont = list(size = 14), # size from x axis values
#                       titlefont = list(size = 15)),
#          yaxis = list(title = "<b> Purchasing in CFA </b>", titlefont = list( size = 16))) %>%
#   config(displayModeBar = T, scrollZoom = T)
# 
# fig_test
# 
######################################################################## 
#
# # ============================================================================================ #
# #                                                                                              #
# #  Following activities will be performed in the following script:                             #
# #                                                                                              #
# #    (1) Loading the   data required for the dashbaord-implementation                          #
# #                                                                                              #
# #                                                                                              #
# #                                                                                              #
# #     Input Parameters:                                                                        #
# #     ----------------                                                                         #
# #                                                                                              #
# #                                                                                              #
# #       (1) db_name          : name of the database containing Ease Customer Data              #
# #                                                                                              #
# #       (2) db_credentials   : data frame containing required credentials for db access        #
# #                              of all available databases (db.sqlite3)                         #
# #                                                                                              #                                                                                              #
# #      Output Parameters:                                                                      #
# #      -----------------                                                                       #
# #                                                                                              #
# #      ** RData for each single table will be produced and saved into the ./data-directory     #
# #                                                                                              #
# #                                                                                              #
# # ============================================================================================ #
# 
# 
# 
# 
# #======================
# # (0) PREPARATION STEPS ####
# #======================
# 
# 
# # --------------------------------------
# # (01) CLEARING ENVIROMNMENT AND CONSOLE ####
# # --------------------------------------
# 
# rm(list = ls(all.names = TRUE)); # activate the step before execution !!!!!!
# cat("\f");
# 
# 
# # ---------------------------
# # (02) SETTING USEFUL OPTIONS ####
# # ---------------------------
# 
# # setting system local encoding
# Sys.setlocale("LC_ALL", "English_United States.932") # this works perfectly ---> für japanese characters
# 
# # setting some options
# options(stringsAsFactors = FALSE)
# options(Encoding = "latin1")
# 
# # Loading required libraries
# library(rJava)
# library(DBI)
# library(RJDBC)
# library(pool)
# library(shiny)
# library(sf)
# library(shinyWidgets)
# library(stringr)
# library(leaflet)
# library(plotly)
# library(dplyr)
# library(DT)
# library(shinycssloaders)
# library(DBI)
# library(RSQLite)
# library(pool)
# library(lubridate)
# library(viridisLite)
# library(igraph)
# library(visNetwork)
# library(zoo)
# library(rintrojs)
# library(readxl)
# library(shinyjs)
# library(openxlsx)
# library(bcrypt)
# library(glue)
# library(plotly)
# library(dplyr)
# library(ggplot2)
# library(magrittr)
# library(sass)
# library(shinydashboard)
# library(shinythemes)
# library(shiny.react)
# library(shiny.router)
# 
# 
# 
# #library(DT) # uses javascript data tables package to produce powerful and attractive data tables.
# 
# 
# 
# # Constructing path of relevant directories
# root <- getwd()
# path_data <- paste(root, "/", "input", sep="")
# #path_data <- paste(root, "/", "data", sep="")
# path_helpers <- paste(root, "/R", sep="")
# #path_helpers <- paste(root, "/codes/helpers", sep="")
# path_meta <- paste(root, "/", "meta", sep="")
# 
# # defining parameters
# #lib_location <- .libPaths()[1]
# 
# # defining "package" parameters
# # lib_location <- .libPaths()[1] 
# # repo_location <-  "https://cloud.r-project.org"
# 
# 
# # -----------------------------
# # (02) LOADING REQUIRED PACKAGES ####0
# # ------------------------------
# 
# # Loading required functions helper functions
# source(paste(path_helpers, "/", "install_packages.R", sep=""))
# source(paste(path_helpers, "/", "load_packages.R", sep=""))
# #source(paste(path_helpers, "/", "get_db_credentials.R", sep=""))
# # 
# # # Launch function to install packages if necessary
# # install_packages(root, lib_location, repo_location)
# # 
# # # Launch function to load required packages
# # load_packages(path_meta, lib_location)
# # 
# # # Launch function to load required packages
# # load_packages(path_meta, lib_location)
# # 
# # # Launch function to load required packages
# # db_credentials <- get_db_credentials(path_meta)
# # 
# # 
# # # --------------------------------------------------
# # # (03) RETRIEVING REQUIRED CREDENTIALS FOR DB ACCESS ####
# # # --------------------------------------------------
# # 
# # # loading file containing db credentials
# # db_credentials <- get_db_credentials(path_meta)
# # db_name_mpay <-  "mpay_db"
# # 
# # 
# # # getting credentials to access the required database (mpay_db)
# # db_type <- (db_credentials %>%
# #                 filter(toupper(db_name) == toupper(db_name_mpay)) %>%
# #                 dplyr::select(db_type) %>%
# #                 collect())$db_type
# # 
# # db_host <- (db_credentials %>%
# #                 filter(toupper(db_name) == toupper(db_name_mpay)) %>%
# #                 dplyr:: select(db_host) %>%
# #                 collect())$db_host
# # 
# # db_user <- (db_credentials %>%
# #                 dplyr::filter(toupper(db_name) == toupper(db_name_mpay)) %>%
# #                 dplyr::select(db_user) %>%
# #                 collect())$db_user
# # 
# # db_pwd <- (db_credentials %>%
# #                filter(toupper(db_name) == toupper(db_name_mpay)) %>%
# #                dplyr::select(db_pwd) %>%
# #                collect())$db_pwd
# # 
# # 
# # 
# # # --------------------------------------
# # # (04) CONNECTING R TO THE MPAY DATABASE ####
# # # --------------------------------------
# # 
# # # Getting the data file directory
# file_path <- paste(path_data, "/", "db.sqlite3", sep = "")
# # # Here we only use the function dbConnect without the parameter dbname, host, username,
# # # password because we have already saved the SQL data into RStudio
# # # creating the connection to the database
# conn <- DBI::dbConnect(RSQLite::SQLite(), file_path)
# 
# # # getting list of tables available
# available_tables <- DBI::dbListTables(conn)
# 
# # # retrieving database drivers
# # drv <- dbDriver(db_type)
# # 
# # # creating the connection to the database
# # con <- dbConnect(drv, dbname= "mpay", host= db_host, username= db_user, password= db_pwd) # agnes chnage the dbname according to the schema(dbname) u have in ur sql-DB
# # 
# # 
# # # checking the number of opened db connextions
# # nbr_connections <- dbListConnections(MySQL())
# # length(nbr_connections)
# # 
# # # getting list of tables available
# # available_tables <- dbListTables(con)
# # 
# 
# #=========================================
# # (1) LOADING AND EXPLORING AVAILABLE DATA ####
# #=========================================
# 
# 
# # --------------------------
# # (10) LOADING REQUIRED DATA ####
# # --------------------------
# # 
# # tic()
# # for(k in 1:length(available_tables)){
# #
# #     form <- parse(text = paste(available_tables[k]," <- dbReadTable(con, '", available_tables[k], "')", sep=""))
# #     eval(form)
# #
# # }
# for(k in 1:length(available_tables)){
#   
#   # read the data frame from the sql database
#   k <- 13
#   form <- parse(text = paste(available_tables[k]," <- DBI::dbReadTable(conn, '", available_tables[13], "')", sep=""))
#   eval(form)
#   
#   str(test_project_trackerdata)
#   # Convert the variables id, user_tracking_id, service_id, offer_id and purchase_y_n to character
#   test_project_trackerdata$id <- as.character(test_project_trackerdata$id)
#   test_project_trackerdata$user_tracking_id <- as.character(test_project_trackerdata$user_tracking_id)
#   test_project_trackerdata$service_id <- as.character(test_project_trackerdata$service_id)
#   test_project_trackerdata$offer_id <- as.character(test_project_trackerdata$offer_id)
#   test_project_trackerdata$purchase_y_n <- as.character(test_project_trackerdata$purchase_y_n)
#   
#   ## find index for purchase_y_n == "0" and set price_offer == 0 for the found index
#   
#   index_purchased_no <- which(test_project_trackerdata$purchase_y_n == "0")
#   test_project_trackerdata$price_offer[index_purchased_no] = 0
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
# # toc()  # 0.22 sec elapsed
# 
# # (04) LOADING REQUIRED FACTS DATA
# # --------------------------------
# 
# file_path <- paste(path_data, "/test_project_trackerdata.rds", sep="")
# system.time(ease_data_simulation <-readRDS(file = file_path))
# str(ease_data_simulation)
# 
# 
# 
# mpay_payment_temp_df <- ease_data_simulation %>%
#   mutate(only_start_date = as.Date(timestamp_start , format = "%Y-%m-%d"),
#          only_start_end = as.Date(timestamp_end, format = "%Y-%m-%d"),
#          years_ = as.Date(timestamp_end, format = "%Y"),
#          year_ = lubridate::year(timestamp_end),
#          quarter_ = lubridate::quarter(timestamp_end),
#          month_ = lubridate::month(timestamp_end),
#          week_ = lubridate::week(timestamp_end),
#          day_ = lubridate::day(timestamp_end),
#          year_month = format(as.Date(mpay_payment_temp_df$only_start_end), "%Y-%m"),
#          month_year = format(timestamp_end, "%B-%Y"))
# 
# 
# 
# # # what Service has most visited ?
# 
# df_most_visited_service_year <- mpay_payment_temp_df %>%
#   #filter(purchase_y_n == "0") %>%
#   group_by(service_name, year_) %>%
#   summarise(most_visited_service = n(),
#             service_payed = sum(price_offer)) %>%
#   arrange(desc(most_visited_service))
# 
# df_most_visited_service_year <- as.data.frame(df_most_visited_service_year)
# 
# fig_service_year <- plotly::plot_ly(df_most_visited_service_year, x = ~year_, type = "bar",
#                        y = ~most_visited_service, color = ~service_name,
#                        text = ~most_visited_service, textposition = 'auto',
#                        hovertext = paste("Year :",df_most_visited_service_year$year_,
#                                          "<br>Service name :", df_most_visited_service_year$service_name,
#                                          "<br>Nber of Service :", df_most_visited_service_year$most_visited_service,
#                                          "<br>Sold:",round(df_most_visited_service_year$service_payed/1000000,
#                                                            digits = 1), "million CFA"),
#                        hoverinfo = 'text') %>%
#   layout(title = "",
#          legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Service name</b>')),
#          uniformtext=list(minsize=15, mode='show'),
#          xaxis = list(title = "<b> Year </b>", #font = list(size = 0),
#                       # change x-axix size
#                       tickfont = list(size = 14),
#                       # change x-title size
#                       titlefont = list(size = 16),
#                       type="date", tickformat="%Y", tickangle= -45, tickvals = df_most_visited_service_year$year_),
#          yaxis = list(title = "<b> Number of service </b>",
#                       titlefont = list(size = 16),
#                       # change x-axix size
#                       tickfont = list(size = 14))
#          #tickvals = data_summation$mav_number_transactions
#          #hoverlabel=list(bgcolor="gainsboro")
#          #width = 500, autosize=F,
#          #bargap = 0.1, bargroupgap = 0.1,
#   ) %>%
#   config(displayModeBar = F, 
#          scrollZoom = T)
# fig_service_year
# 
# df_most_visited_service_date <- mpay_payment_temp_df %>%
#   #filter(purchase_y_n == "0") %>%
#   group_by(service_name, quarter_) %>%
#   summarise(most_visited_service = n(),
#             service_payed = sum(price_offer)) %>%
#   arrange(desc(most_visited_service))
# 
# df_most_visited_service_year_month <- mpay_payment_temp_df %>%
#   #filter(purchase_y_n == "0") %>%
#   group_by(service_name, month_year) %>%
#   summarise(most_visited_service = n(),
#             service_payed = sum(price_offer)) %>%
#   arrange(desc(most_visited_service))
# 
# #length(unique(mpay_payment_temp_df$only_start_end))
# 
# 
# fig_service_quarter <- plotly::plot_ly(df_most_visited_service_date, x = ~quarter_, type = "bar",
#                                     y = ~most_visited_service, color = ~service_name,
#                                     text = ~most_visited_service, textposition = 'auto',
#                                     hovertext = paste("Year :",df_most_visited_service_date$quarter_,
#                                                       "<br>Service name :", df_most_visited_service_date$service_name,
#                                                       "<br>Nber of Service :", df_most_visited_service_date$most_visited_service,
#                                                       "<br>Sold:",round(df_most_visited_service_date$service_payed/1000000,
#                                                                         digits = 1), "million CFA"),
#                                     hoverinfo = 'text') %>%
#   layout(title = "",
#          legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Service name</b>')),
#          uniformtext=list(minsize=15, mode='show'),
#          xaxis = list(title = "<b> Year </b>", #font = list(size = 0),
#                       # change x-axix size
#                       tickfont = list(size = 14),
#                       # change x-title size
#                       titlefont = list(size = 16),
#                       type="date", tickformat="%Y", tickangle= -45, tickvals = df_most_visited_service_date$quarter_),
#          yaxis = list(title = "<b> Number of service </b>",
#                       titlefont = list(size = 16),
#                       # change x-axix size
#                       tickfont = list(size = 14))
#          #tickvals = data_summation$mav_number_transactions
#          #hoverlabel=list(bgcolor="gainsboro")
#          #width = 500, autosize=F,
#          #bargap = 0.1, bargroupgap = 0.1,
#   ) %>%
#   config(displayModeBar = F, 
#          scrollZoom = T)
# fig_service_quarter
# 
# df_most_service_test <- mpay_payment_temp_df %>%
#   #filter(purchase_y_n == "0") %>%
#   group_by(service_name, year_) %>%
#   summarise(mav_number_agents = n(),
#             service_payed = sum(price_offer))
# 
# df_most_service_year <- mpay_payment_temp_df %>%
#   filter(purchase_y_n == "1") %>%
#   group_by( service_name, year_) %>%
#   summarise(Nbr_service_purchased = n())
# 
# table(mpay_payment_temp_df$year_, useNA = "always")
# 
# df_most_service_month <- mpay_payment_temp_df %>%
#   filter(purchase_y_n == "1") %>%
#   group_by( service_name, month_) %>%
#   summarise(Nbr_service_purchased = n(),
#             service_payed = sum(price_offer))
# 
# df_most_service_month_year <- mpay_payment_temp_df %>%
#   filter(purchase_y_n == "1") %>%
#   group_by( service_name, month_year) %>%
#   summarise(Nbr_service_purchased = n(),
#             service_payed = sum(price_offer))
# 
# 
# 
# df_most_service_month_year %>%
#   plotly::plot_ly(x = ~month_year, y = ~service_payed) %>%
#   add_lines()
# 
# 
#   plotly::plot_ly(df_most_service_month_year) %>%
#     add_lines(x = ~month_year, y = ~service_payed, linetype = ~service_name, 
#               line = list( width = 3))
#   
# 
# 
# table(mpay_payment_temp_df$service_name)
# 
# ## What  offer is most purchased ?
# 
# unique(mpay_payment_temp_df$service_name)
# 
# df_most_offer_pucharsed_year <- mpay_payment_temp_df %>%
#   filter(purchase_y_n == "1") %>%
#          #service_name == "Travel packages") %>%
#   group_by(service_name, offer_name, year_) %>%
#   summarise(Nber_of_offer = n(),
#             most_offer_pucharsed = sum(price_offer)) %>%
#   arrange(desc(Nber_of_offer))
# 
# df_most_offer_pucharsed_year <- as.data.frame(df_most_offer_pucharsed_year)
# 
# 
# fig_offer_year <- plotly::plot_ly(df_most_offer_pucharsed_year, x = ~year_, type = "bar",
#                                        y = ~Nber_of_offer, color = ~offer_name,
#                                        text = ~Nber_of_offer, textposition = 'auto',
#                                        hovertext = paste("Year :",df_most_offer_pucharsed_year$year_,
#                                                          "<br>Service name:", df_most_offer_pucharsed_year$service_name,
#                                                          "<br>Offer name :", df_most_offer_pucharsed_year$offer_name,
#                                                          "<br>Nber of offer :", df_most_offer_pucharsed_year$Nber_of_offer,
#                                                          "<br>Sold:",round(df_most_offer_pucharsed_year$most_offer_pucharsed/1000000,
#                                                                            digits = 1), "million CFA"),
#                                        hoverinfo = 'text') %>%
#   layout(title = "",
#          legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Service name</b>')),
#          uniformtext=list(minsize=15, mode='show'),
#          xaxis = list(title = "<b> Year </b>", #font = list(size = 0),
#                       # change x-axix size
#                       tickfont = list(size = 14),
#                       # change x-title size
#                       titlefont = list(size = 16),
#                       type="date", tickformat="%Y", tickangle= -45, tickvals = df_most_offer_pucharsed_year$year_),
#          yaxis = list(title = "<b> Number of service </b>",
#                       titlefont = list(size = 16),
#                       # change x-axix size
#                       tickfont = list(size = 14))
#          #tickvals = data_summation$mav_number_transactions
#          #hoverlabel=list(bgcolor="gainsboro")
#          #width = 500, autosize=F,
#          #bargap = 0.1, bargroupgap = 0.1,
#   ) %>%
#   config(displayModeBar = F, 
#          scrollZoom = T)
# fig_offer_year
# 
# 
# 
# 
# df_most_offer_pucharsed_date <- mpay_payment_temp_df %>%
#   filter(purchase_y_n == "1",
#          service_name == "Travel packages") %>%
#   group_by(service_name, offer_name, year_, quarter_) %>%
#   summarise(Nber_of_offer = n(),
#             most_offer_pucharsed = sum(price_offer)) %>%
#   arrange(desc(Nber_of_offer))
# 
# fig_offer_year <- plotly::plot_ly(df_most_offer_pucharsed_date, x = ~year_, type = "bar",
#                                   y = ~Nber_of_offer, color = ~offer_name,
#                                   text = ~Nber_of_offer, textposition = 'auto',
#                                   hovertext = paste("Year :",df_most_offer_pucharsed_date$year_,
#                                                     "<br>Service name:", df_most_offer_pucharsed_date$service_name,
#                                                     "<br>Offer name :", df_most_offer_pucharsed_date$offer_name,
#                                                     "<br>Nber of offer :", df_most_offer_pucharsed_date$Nber_of_offer,
#                                                     "<br>Sold:",round(df_most_offer_pucharsed_date$most_offer_pucharsed/1000000,
#                                                                       digits = 1), "million CFA"),
#                                   hoverinfo = 'text') %>%
#   layout(title = "",
#          legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Offer name</b>')),
#          uniformtext=list(minsize=15, mode='show'),
#          xaxis = list(title = "<b> Year </b>", #font = list(size = 0),
#                       # change x-axix size
#                       tickfont = list(size = 14),
#                       # change x-title size
#                       titlefont = list(size = 16),
#                       type="date", tickformat="%Y", tickangle= -45, tickvals = df_most_offer_pucharsed_date$year_),
#          yaxis = list(title = "<b> Number of Offer </b>",
#                       titlefont = list(size = 16),
#                       # change x-axix size
#                       tickfont = list(size = 14))
#          #tickvals = data_summation$mav_number_transactions
#          #hoverlabel=list(bgcolor="gainsboro")
#          #width = 500, autosize=F,
#          #bargap = 0.1, bargroupgap = 0.1,
#   ) %>%
#   config(displayModeBar = F, 
#          scrollZoom = T)
# fig_offer_year
# 
# df_most_offer_pucharsed_date <- mpay_payment_temp_df %>%
#   filter(purchase_y_n == "1",
#          service_name == "Travel packages") %>%
#   group_by(service_name, offer_name, year_) %>%
#   summarise(Nber_of_offer = n(),
#             most_offer_pucharsed = sum(price_offer)) %>%
#   arrange(desc(Nber_of_offer))
# 
# fig_offer_year <- plotly::plot_ly(df_most_offer_pucharsed_date, x = ~year_, type = "bar",
#                                   y = ~most_offer_pucharsed, color = ~offer_name,
#                                   text = ~most_offer_pucharsed, textposition = 'auto',
#                                   hovertext = paste("Year :",df_most_offer_pucharsed_date$year_,
#                                                     "<br>Service name:", df_most_offer_pucharsed_date$service_name,
#                                                     "<br>Offer name :", df_most_offer_pucharsed_date$offer_name,
#                                                     "<br>Nber of offer :", df_most_offer_pucharsed_date$Nber_of_offer,
#                                                     "<br>Sold:",round(df_most_offer_pucharsed_date$most_offer_pucharsed/1000000,
#                                                                       digits = 1), "million CFA"),
#                                   hoverinfo = 'text') %>%
#   layout(title = "",
#          legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Offer name</b>')),
#          uniformtext=list(minsize=15, mode='show'),
#          xaxis = list(title = "<b> Year </b>", #font = list(size = 0),
#                       # change x-axix size
#                       tickfont = list(size = 14),
#                       # change x-title size
#                       titlefont = list(size = 16),
#                       type="date", tickformat="%Y", tickangle= -45, tickvals = df_most_offer_pucharsed_date$year_),
#          yaxis = list(title = "<b> Number of Offer </b>",
#                       titlefont = list(size = 16),
#                       # change x-axix size
#                       tickfont = list(size = 14))
#          #tickvals = data_summation$mav_number_transactions
#          #hoverlabel=list(bgcolor="gainsboro")
#          #width = 500, autosize=F,
#          #bargap = 0.1, bargroupgap = 0.1,
#   ) %>%
#   config(displayModeBar = F, 
#          scrollZoom = T)
# fig_offer_year
# 
# 
# 
# 
# ###################################################
# 
# 
# df_most_offer_pucharsed_date <- mpay_payment_temp_df %>%
#   filter(purchase_y_n == "1") %>%
#   group_by(user_tracking_id, service_name, only_start_end) %>%
#   summarise(offer_most_purchased = sum(price_offer)) %>%
#   arrange(desc(offer_most_purchased))
# 
# df_most_offer_pucharsed_date<- mpay_payment_temp_df %>%
#   filter(purchase_y_n == "1") %>%
#   group_by(user_tracking_id, service_name, only_start_end) %>%
#   summarise(offer_most_purchased = sum(price_offer)) %>%
#   arrange(desc(offer_most_purchased))
# 
# df_most_offer_pucharsed_date2 <- df_most_offer_pucharsed_date %>%
#   filter(offer_most_purchased > 800000)
# 
# top10_transaction_service <- head(df_most_offer_pucharsed_date, 500)
# 
# plotly::plot_ly(df_most_offer_pucharsed_date2) %>%
#   add_lines(x = ~only_start_end, y = ~offer_most_purchased, linetype = ~service_name, 
#             line = list( width = 3))
#   
# df_most_offer_pucharsed_year <- mpay_payment_temp_df %>%
#   filter(purchase_y_n == "1") %>%
#   group_by(service_name, offer_name, year_) %>%
#   summarise(offer_most_purchased = sum(price_offer)) %>%
#   arrange(desc(offer_most_purchased))
# 
# plotly::plot_ly(df_most_offer_pucharsed_year) %>%
#   add_lines(x = ~year_, y = ~offer_most_purchased, linetype = ~service_name, 
#             line = list( width = 3))
# 
# 
# test2 <- test_project_trackerdata$timestamp_start
# test3 <- test_project_trackerdata$timestamp_end
# test4 <- lubridate::hms(test3 - test2)
# test5 <- test3 - test2
# lubridate::hour(120)
# library(lubridate)
# test6 <- lubridate::seconds_to_period(test5)
# test7 <- sprintf('%02d:%02d:%02d',test6@hour, minute(test6), second(test6))
# 
# HMS = kimisc::seconds.to.hms(test5)
# HMSM <- lubridate::hms(HMS)
# str(HMSM)
# # test <- as.Date(test_project_trackerdata$timestamp_end)
# # timeDate <- as.POSIXct(test_project_trackerdata$timestamp_end)
# # test1 <- lubridate::ymd_hms(test_project_trackerdata$timestamp_end)
############################################################################
#
# 
# 
# 
# library(readr)
# 
# file_path <- paste("C:/Users/Gleyne Monthe/Desktop/E_Commerce_Dashboard/EaseProject/db.sqlite3", seq = "")
# #conn = connectDb()
# conn <- DBI::dbConnect(RSQLite::SQLite(), file_path)
# 
# available_tables <- DBI::dbListTables(conn)
# 
# # --------------------------
# # (10) LOADING REQUIRED DATA ####
# # --------------------------
# # 
# # tic()
# # for(k in 1:length(available_tables)){
# #
# #     form <- parse(text = paste(available_tables[k]," <- dbReadTable(con, '", available_tables[k], "')", sep=""))
# #     eval(form)
# #
# # }
# for(k in 1:length(available_tables)){
# 
#     # read the data frame from the sql database
#     form <- parse(text = paste(available_tables[13]," <- DBI::dbReadTable(conn, '", available_tables[13], "')", sep=""))
#     eval(form)
#     
#     str(test_project_trackerdata)
#     # Convert the variables id, user_tracking_id, service_id, offer_id and purchase_y_n to character
#     test_project_trackerdata$id <- as.character(test_project_trackerdata$id)
#     test_project_trackerdata$user_tracking_id <- as.character(test_project_trackerdata$user_tracking_id)
#     test_project_trackerdata$service_id <- as.character(test_project_trackerdata$service_id)
#     test_project_trackerdata$offer_id <- as.character(test_project_trackerdata$offer_id)
#     test_project_trackerdata$purchase_y_n <- as.character(test_project_trackerdata$purchase_y_n)
#     
#     # convert the variables timestamp_start and timestamp_end to date
#     test_project_trackerdata$timestamp_start <- lubridate::ymd_hms(test_project_trackerdata$timestamp_start)
#     test_project_trackerdata$timestamp_end <- lubridate::ymd_hms(test_project_trackerdata$timestamp_end)
#     
#     test2 <- test_project_trackerdata$timestamp_start
#     test3 <- test_project_trackerdata$timestamp_end
#     test4 <- lubridate::hms(test3 - test2)
#     test5 <- test3 - test2
#     lubridate::hour(120)
#     library(lubridate)
#     test6 <- lubridate::seconds_to_period(test5)
#     test7 <- sprintf('%02d:%02d:%02d',test6@hour, minute(test6), second(test6))
#     
#     HMS = kimisc::seconds.to.hms(test5)
#     HMSM <- lubridate::hms(HMS)
#     str(HMSM)
#     
#     
#     # test <- as.Date(test_project_trackerdata$timestamp_end)
#     # timeDate <- as.POSIXct(test_project_trackerdata$timestamp_end)
#     # test1 <- lubridate::ymd_hms(test_project_trackerdata$timestamp_end)
#                         
#     
#     # save the loaded data frame in order data
#     form <- parse(text = paste("saveRDS(", available_tables[k], ", file = '" , path_data, "/", available_tables[k], ".rds')", sep=""))
#     eval(form)
# 
# }
# # toc()  # 0.22 sec elapsed
# # 
# 
# 
# 
# library(plotly)
# tickvals = c("2016-08-01", "2016-09-01", "2016-10-01", 
#              "2016-11-01", "2016-12-01", "2017-01-01",
#              "2017-02-01", "2017-03-01", "2017-04-01",
#              "2017-05-01", "2017-06-01", "2017-07-01",
#              "2017-08-01", "2017-09-01", "2017-10-01",
#              "2017-11-01", "2017-12-01", "2018-01-01")
# df <- data.frame(x = tickvals, index = 1:18, y = 90000)
# plot_ly(data=df, x=~x, y=~y, type="bar") %>% 
#   layout(barmode="group", xaxis=list(type="date", tickformat="%b %Y", tickangle=-90, tickvals=tickvals))
# 
# 
# 
# library(plotly)
# df <- data.frame(x = c("2016-08-01", "2016-09-01", "2016-10-01", 
#                        "2016-11-01", "2016-12-01", "2017-01-01",
#                        "2017-02-01", "2017-03-01", "2017-04-01",
#                        "2017-05-01", "2017-06-01", "2017-07-01",
#                        "2017-08-01", "2017-09-01", "2017-10-01",
#                        "2017-11-01", "2017-12-01", "2018-01-01"), index = 1:18, y = 90000)
# plot_ly(data=df, x=~x, y=~y, type="bar") %>% 
#   layout(barmode="group", xaxis=list(type="date", tickformat="%b %Y", tickangle=-90))
# 
# 
# 
# plot_ly(d, x = ~time, y = ~y) %>%
#   add_lines() %>%
#   rangeslider(d$time[10], d$time[19]) %>% 
#   layout(xaxis = list(ticktext = paste0(c(rep(2018, 3), 
#                                           rep(2019, 4), 
#                                           rep(2020, 2)),
#                                         " Q",
#                                         c(2:4, 1)),
#                       tickvals = d$time[10:18]
#   ))
# 
# 
# quarters <- paste(quarters(mpay_payment_temp_df), format(Date, "%Y"), sep = "-")
# quarters <- factor(quarters, levels = unique(quarters))
# 
# 
# fig
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# test <- year(mpay_payment_temp_df$only_start_end)*100 + 
#   month(mpay_payment_temp_df$only_start_end)
# 
# test1 <- as.Date(as.character(test), format = "%Y-%m")
# 
# 
# year(date)*10000 + month(data)*100 + day(date)
# 
# test2 <- lubridate::ym
# 
# test1 <- as.yearmon(test)
# 
# mpay_payment_temp_df$year_month <- format(as.Date(mpay_payment_temp_df$only_start_end), "%Y-%m")
# 
# df_most_visited_service_month_year <- format(df_most_visited_service_month_year$month_year, "%B-%Y")
# 
# format(timestamp_end, "%B-%Y")
# 
# x <- "Apr-16"
# x <- as.Date(paste0("1-",x),format="%d-%b-%y")
# x3 <- format(x,"%B %Y")
# 
# # use parse_time in order to create standard unambigous date format.
# ease_data_for_visualization <- ease_data_simulation %>%
#   mutate(only_start_date = as.Date(timestamp_start , format = "%Y-%m-%d"),
#          only_start_end = as.Date(timestamp_end, format = "%Y-%m-%d"),
#          years_ = as.Date(timestamp_end, format = "%Y"),
#          year_ = lubridate::year(timestamp_end),
#          quarter_ = lubridate::quarter(timestamp_end),
#          month_ = lubridate::month(timestamp_end),
#          week_ = lubridate::week(timestamp_end),
#          day_ = lubridate::day(timestamp_end),
#          year_month = as.Date(only_start_end), 
#          #month_year = zoo::as.yearmon(only_start_end))
# month_year = format(as.Date(only_start_end), "%Y-%m"))
# 
# str(ease_data_for_visualization)
# 
# df_most_visited_service_month_year <- ease_data_for_visualization %>%
#   filter(purchase_y_n == "1",
#          year_ == 2021) %>%
#   group_by(service_name, month_year) %>%
#   summarise(most_visited_service = n(),
#             service_payed = sum(price_offer)) %>%
#   arrange(most_visited_service) 
# 
# str(df_most_visited_service_month_year)
# 
# convert_to_date <- as.data.frame(df_most_visited_service_month_year)
# str(convert_to_date)
# 
# str(ease_data_for_visualization)
# 
# x <- as.Date(paste0(as.character(201603)), format='%Y%m')
# 
# library(zoo)
# d <- c("200701", "200702")
# 
# lastday <- as.Date(as.yearmon(d, "%Y%m"), frac = 1)
# 
# as.yearmon()
# test2 <- as.yearmon("mar07", "%b%y")
# 
# test3 <- as.yearmon("2007-12-01")
# 
# test4 <- as.Date(as.yearmon("2007-12"))
# 
# 
# str(test4)
# test5 <- ease_data_for_visualization$years_[1]
# 
# 
# test6 <- as.yearmon(ease_data_for_visualization$years)
# str(test6)
#  lubridate::ym("2021-10-15")
#  
#  dates_with_months <- format(ease_data_for_visualization$only_start_date,"%Y-%m")
#  
# 
# finaldata <- lubridate::ym(ease_data_for_visualization$only_start_date[1])
# 
# finaldata <- dmy(readdata$Month)
# 
# 
# df_most_visited_service_month_year <- ease_data_for_visualization %>%
#   filter(purchase_y_n == "1",
#          year_ == 2021) %>%
#   group_by(service_name, month_year) %>%
#   summarise(most_visited_service = n(),
#             service_payed = sum(price_offer)) #%>%
# # arrange(most_visited_service) 
# 
# 
# fig_service_year2 <- plotly::plot_ly(df_most_visited_service_month_year, x = ~month_year,
#                                      type = "bar",
#                                      y = ~service_payed, color = ~service_name,
#                                      text = ~most_visited_service, textposition = 'outside',
#                                      hovertext = paste("Date :",df_most_visited_service_month_year$month_year,
#                                                        "<br>Service name :", df_most_visited_service_month_year$service_name,
#                                                        "<br>Nber of Service :", df_most_visited_service_month_year$most_visited_service,
#                                                        "<br>Purchased:",round(df_most_visited_service_month_year$service_payed/1000000,
#                                                                               digits = 1), "million CFA"),
#                                      hoverinfo = 'text') %>%
#   layout(title = "",
#          legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Service name</b>')),
#          uniformtext=list(minsize=15, mode='show'),
#          xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
#                       # change x-axix size
#                       tickfont = list(size = 14), categoryorder = "array",
#                       categoryarray = ~df_most_visited_service_month_year$service_payed,
#                       # change x-title size
#                       titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
#                       tickangle= -45, tickvals = df_most_visited_service_month_year$month_year),
#          yaxis = list(title = "<b> Purchased service in CFA </b>",
#                       titlefont = list(size = 16),
#                       # change x-axix size
#                       tickfont = list(size = 14))
#          #tickvals = df_most_visited_service_month_year$most_visited_service)
#          #hoverlabel=list(bgcolor="gainsboro")
#          #width = 500, autosize=F,
#          #bargap = 0.1, bargroupgap = 0.1,
#   ) %>%
#   config(displayModeBar = F, 
#          scrollZoom = T)
# 
# fig_service_year2
# 
# 
# unique(ease_data_for_visualization$offer_name)
##################################################
#
# 
# library(DBI)
# 
# mammals <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/Gleyne Monthe/Desktop/E_Commerce_Dashboard/EaseProject/db.sqlite3")
# DBI::dbListTables(mammals)
# 
# dbWriteTable(mammals, "auth_group", auth_group)
# dbListTables(con)
# 
# src_dbi(mammals)
# 
# mammals <- DBI::dbConnect(RSQLite::SQLite(), "data_raw/portal_mammals.sqlite")
# 
# #############14.07.2022 test area ################
# df_most_visited_service_month_year <- ease_data_for_visualization %>%
#   filter(purchase_y_n == "1",
#          year_ == 2021) %>%
#   group_by(service_name, month_year) %>%
#   summarise(most_visited_service = n(),
#             service_payed = sum(price_offer))# %>%
#  # mutate(service_name_level = factor(service_name, levels = c("Apartements", "Car rentals", "Hotels", "Travel packages") ))
# 
# fig_service_year2 <- plotly::plot_ly(df_most_visited_service_month_year, x = ~month_year,
#                                      type = "bar",
#                                      y = ~service_payed, color = ~service_name,
#                                      #colors = "YlGn",
#                                      colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
#                                      text = ~most_visited_service, textposition = 'outside',
#                                      hovertext = paste("Date :",df_most_visited_service_month_year$month_year,
#                                                        "<br>Service name :", df_most_visited_service_month_year$service_name,
#                                                        "<br>Nber of Service :", df_most_visited_service_month_year$most_visited_service,
#                                                        "<br>Purchased:",round(df_most_visited_service_month_year$service_payed/1000000,
#                                                                               digits = 1), "million CFA"),
#                                hoverinfo = 'text') %>% #end plot_ly
#   #add_trace(x = ~month_year, y = ~most_visited_service, yaxis = "y2") %>%
#   layout(title = "Total purchased and number of each service",
#          legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Service name</b>')),
#          uniformtext=list(minsize=15, mode='show'),
#          xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
#                       # change x-axix size
#                       tickfont = list(size = 14), 
#                       # change x-title size
#                       titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
#                       tickangle= -45, tickvals = df_most_visited_service_month_year$month_year),
#          yaxis = list(title = "<b> Purchased service in CFA </b>",
#                       titlefont = list(size = 16),
#                       # change x-axix size
#                       tickfont = list(size = 14))
#          #tickvals = df_most_visited_service_month_year$most_visited_service)
#          #hoverlabel=list(bgcolor="gainsboro")
#          #width = 500, autosize=F,
#          #bargap = 0.1, bargroupgap = 0.1,
#          # yaxis2 = list(title = "<b> Transaction revenue in CFA </b>",
#          #               tickfont = list(size = 14),
#          #               #titlefont = list(size = 18),
#          #               overlaying = "y",
#          #               side = "right")
#          
#   ) %>%
#   config(displayModeBar = F, 
#          scrollZoom = T)
# 
# fig_service_year2




