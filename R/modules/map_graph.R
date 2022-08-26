
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
  
  output$map <- leaflet::renderLeaflet({
    
    df_filter <- df_testdata %>%
      filter(client_id == input$selected_client_id,
             machine_id == input$selected_machine_id,
             date >= input$daterange[1] & date <= input$daterange[2]) %>%
      dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price, year_, localization_lon, localization_lat) %>% 
      relocate(dynamic_price, .before = lot_health_index) %>%
      leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addCircles(lng = ~ localization_lon, lat = ~ localization_lat, weight = 1,
                 #radius = ~ lifeExp * 5000,
                 #popup = ~ paste(client_id, lifeExp))
      )
    
  })
  
  # output$map = renderLeaflet({
  #   #mapData <- as.data.frame(mapData)
  #   
  #   mapData %>%
  #     filter(year == input$year[2]) %>%
  #     leaflet() %>%
  #     addTiles() %>%
  #     setView(lng = 0, lat = 0, zoom = 2) %>%
  #     addCircles(lng = ~ lon, lat = ~ lat, weight = 1,
  #                radius = ~ lifeExp * 5000, 
  #                popup = ~ paste(country, lifeExp))
  # })
  
} # map_town_server
