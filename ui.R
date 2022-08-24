# Html template used to render UI
ui <- htmlTemplate("www/index2.html",
                   
                   selectclientid = selectInput("selected_client_id", "Client_Id",
                   choices = unique(sort(df_testdata$client_id)),
                   selected = 1,
                   selectize = TRUE),
                   selectmachineid = selectInput("selected_machine_id", "",
                                                choices = unique(sort(df_testdata$machine_id)),
                                                selected = 1,
                                                selectize = TRUE),
                   
                   selectmachineid = selectInput("selected_machine_id", "Machine_Id",
                                                 choices = unique(sort(df_testdata$machine_id)),
                                                 selected = 1,
                                                 selectize = TRUE),
                   
                   selectdate = dateRangeInput("daterange3", "Start_Period",
                                  start  = min(df_testdata$date),
                                  end    = max(df_testdata$date),
                                  min    = min(df_testdata$date),
                                  max    = max(df_testdata$date),
                                  format = "dd-mm-yyyy",
                                  separator = " - "),
                   
                   selectperiod = selectInput("selected_forecast_period", "Forecast_Period",
                                                 choices = sort(c(3, 6, 12)),
                                                 selected = 1,
                                                 selectize = TRUE), # DDX_Visualization
                   
                    button = actionButton("actionFilter", title="", 
                                 label="Calculate" ) 
                   
                   
                   
                   
                   # select_servicename = selectInput(inputId = ("service_name"), label = "",
                   #                                  choices = sort(unique(ease_data_for_visualization$service_name)),
                   #                                  selected = 1, width = "auto"),
                   # selectData = select_year_ui(id = "selectinput"),
                   # user_location_plot = user_location_ui("selectinput"),
                   # purchased_service_plot = purchased_service_ui("selectinput"),
                   # purchased_offer_plot = purchased_offer_ui("selectinput"),
                   # traffic_overview_plot = connection_time_ui("selectinput"),
                   # total_purchased_service  = total_purchased_service_box1_ui("selectinput"),
                   # location_most_purchase = location_most_purchase_box2_ui("selectinput"),
                   # nber_user = nber_user_box3_ui("selectinput"),
                   # active_users = nber_active_users_box4_ui("selectinput")
                   # # plot.countries  = countries_map_ui("maps")
) #  end ui