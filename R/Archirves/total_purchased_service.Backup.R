
total_purchased_service_box1_ui <- function(id) {
  
  ns <- NS(id)
  tagList(
    tags$div(
      class = "purchasebox1",
      valueBoxOutput(ns("service_purchased_box1"), width = 3)
    ) # end tags$div
  ) # end tagList
  
} # end valuebox1_ui

total_purchased_service_box1_server <- function(input, output, session) {
  
  output$service_purchased_box1 <- renderValueBox({
    df_total_purchased_service1 <- ease_data_for_visualization %>%
      filter(years_ == input$years,
             purchase_y_n == "1") %>%
      #group_by(service_name) %>%
      summarise(total_purchased_service = sum(price_offer))
    
    total_sales_service <- df_total_purchased_service1$total_purchased_service
    
    valueBox(paste0(formatC(total_sales_service, format = "d", big.mark = "."), " CFA"),
              icon = icon("fa-solid fa-coins"),
             subtitle = paste0("Year: " , input$years, sep = ""),
             color = "aqua") # olive
  }) # end service_purchased_box1
  
} # end total_purchased_service_box1_server


