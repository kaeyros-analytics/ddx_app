
server <- function(input, output, session) {
  
   callModule(dynamic_price_server, id = "forecast")
   callModule(lot_health_index_server, id = "forecast")
   callModule(map_town_server, id = "forecast")
  # callModule(connection_time_server, id = "selectinput")
  # callModule(total_purchased_service_box1_server, id = "selectinput")
  # callModule(location_most_purchase_box2_server, id = "selectinput")
  # callModule(nber_user_box3_server, id = "selectinput")
  # callModule(nber_active_users_box4_server, id = "selectinput")
  # #callModule(nber_active_users_box4_server, id = "selectinput")
  
  
} # end server