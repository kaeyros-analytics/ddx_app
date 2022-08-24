
nber_user_box3_ui <- function(id) {
  
  ns <- NS(id)
  tagList(
    tags$div(
      class = "purchasebox1",
      valueBoxOutput(ns("nber_user_box3"), width = 3)
    ) # end tags$div
  ) # end tagList
  
} # end nber_user_box3_ui

nber_user_box3_server <- function(input, output, session) {
  
  output$nber_user_box3 <- renderValueBox({
    
    df_user <- ease_data_for_visualization %>%
      filter(years_ == input$years) %>%
      summarise(count_user = length(unique(user_tracking_id)))
  
    
    valueBox(paste0(df_user),
             # icon = icon("euro"),
             subtitle = paste0("Year: " , input$years, sep = "")) # olive
  }) # end nber_user_box3
  
} # end total_purchased_service_box1_server
