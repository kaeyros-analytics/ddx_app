
map_town_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "plot3",
      shinycssloaders::withSpinner(leaflet::leafletOutput(outputId = ns("map")), type = 1, color = 'blue'),
      
    ) # end tags$div
  ) # end tagList
} # end map_town_ui


map_town_server <- function(input, output, session) {
  
  
  filter_map <- shiny::eventReactive(eventExpr = input$actionFilter,
                                        valueExpr = {
                                          # subset the data frame 
                                          df_filter <- df_testdata %>%
                                            filter(client_id == input$selected_client_id,
                                                   machine_id == input$selected_machine_id,
                                                   date >= input$daterange[1] & date <= input$daterange[2]) %>%
                                            dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price,fixed_price, year_, localization_lon, 
                                                          localization_lat, client_id, machine_id)
                                        }, ignoreNULL = FALSE
                                        
  ) # end eventReactive
  
  # observeEvent(input$actionFilter,{
  #   
  # })
  

  output$map <- leaflet::renderLeaflet({
    
    # df_filter <- df_testdata %>%
    #   filter(client_id == input$selected_client_id,
    #          machine_id == input$selected_machine_id,
    #          date >= input$daterange[1] & date <= input$daterange[2]) %>%
    #   dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price, year_, localization_lon, localization_lat) 
      leaflet(filter_map()) %>%
      addTiles() %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      # addProviderTiles(providers$Stamen.TonerLite, 
      #                  options = providerTileOptions(noWrap = TRUE)
      # ) %>%
      #setView(lng = 0, lat = 0, zoom = 3) %>%
      setView(lng = 10.4515, lat = 51.1657, zoom = 5) %>%
      #addPolygons()%>%
      addMarkers(lng = ~ localization_lon, lat = ~ localization_lat,
                 popup=~paste(
                   "<b>Client_id </b>: ", as.character(filter_map()$client_id), "<br/>",
                   "<b> Machine_id </b>: ", as.character(filter_map()$machine_id), "<br/>",
                   "<b> Fixed Premium </b>: ", round(unique(filter_map()$fixed_price), 2)))
    
  }) # end output$map
  
} # map_town_server
