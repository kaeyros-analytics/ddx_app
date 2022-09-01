# Html template used to render UI
ui <- htmlTemplate("www/index2.html",
                   
                   selectclientid = client_id_ui("forecast"),
                   selectmachineid = machine_id_ui("forecast"),
                   selectdate = daterange_ui("forecast"),
                   selectperiod = forecast_period_ui("forecast"),
                   fixed_price = average_fixed_price_ui("forecast"),
                   calculate_button  = calculate_button_ui("forecast"),
                   dynamic_price = dynamic_price_ui(id = "forecast"),
                   lot_health = lot_health_index_ui("forecast"),
                   map_plot = map_town_ui("forecast")
                   
                  
                   
                   
                  
                   
                   # # plot.countries  = countries_map_ui("maps")
) #  end ui