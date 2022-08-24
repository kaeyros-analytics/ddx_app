
nber_active_users_box4_ui <- function(id) {
  
  ns <- NS(id)
  tagList(
    tags$div(
      class = "purchasebox4",
      valueBoxOutput(ns("active_users_box4"), width = 3)
    ) # end tags$div
  ) # end tagList
  
} # end nber_active_users_box4_ui

nber_active_users_box4_server <- function(input, output, session) {
  
  output$active_users_box4 <- renderValueBox({
    df_active_user <- ease_data_for_visualization %>%
      filter(years_ == input$years,
             purchase_y_n == "1") %>%
      summarise(ease_number_of_visite = length(user_tracking_id))
    
    valueBox(paste0(df_active_user, sep = ""),
             #value = paste0("Year: " , input$years, sep = ""),
             # icon = icon("euro"),
             subtitle = paste0("Year: " , input$years, sep = ""),
             color = "aqua") # olive
    
  }) # end active_users_box4
  
} # end nber_active_users_box4_server
