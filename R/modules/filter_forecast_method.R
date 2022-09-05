

#---------------------
# CREATE UI FUNCTION
#---------------------
forecast_radio_button_ui <- function(id) {
  
  # NS(): Assign module elements to a unique namespace
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  tagList(
    tags$div(
      class = "radioB1",
      radioButtons(inputId = ns("select_forecast"), label =  "Choose a forecast method",
                   choices = list("VAR", "VARMA", "Naive"), inline = FALSE, selected = "VAR")
    )
    
  ) # end taList
  
} # end forecast_radio_button_ui