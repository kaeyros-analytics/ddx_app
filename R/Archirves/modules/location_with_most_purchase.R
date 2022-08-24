
location_most_purchase_box2_ui <- function(id) {
  
  ns <- NS(id)
  tagList(
    tags$div(
      class = "purchasebox2",
      valueBoxOutput(ns("location_purchase_box2"), width = 3)
    ) # end tags$div
  ) # end tagList
  
} # end valuebox1_ui

location_most_purchase_box2_server <- function(input, output, session) {
  
  output$location_purchase_box2 <- renderValueBox({
    df_location_purchase <- ease_data_for_visualization %>%
      filter(years_ == input$years,
             purchase_y_n == "1") %>%
      group_by(user_location) %>%
      summarise(location_most_purchase = sum(price_offer)) %>%
      arrange(desc(location_most_purchase))
    
    most_purchase <- df_location_purchase$location_most_purchase[1]
    
    best_location <- df_location_purchase$user_location[1]
    
    valueBox(paste(round(most_purchase/1000000, digits = 1), " Mio. CFA", sep = ""),
      #paste0(formatC(most_purchase, format = "d", big.mark = "."), " CFA"),
            paste("Location: ",best_location,  " - Year: ", input$years , sep = ""),
            #icon = icon("fa-solid fa-coins"),
             #subtitle = paste0("Year: " , input$years)
    ) # olive
  }) # end service_purchased_box1
  
} # end total_purchased_service_box1_server
