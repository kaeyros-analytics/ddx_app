
#---------------------
# CREATE UI FUNCTION
#---------------------
forecast_period_ui <- function(id) {
  
  # NS(): Assign module elements to a unique namespace
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  tagList(
    tags$div(class = "filtre1",
      selectInput(inputId = ns("selected_forecast_period"), "Forecast_Period",
                  choices = c(3, 6, 12),
                  selected = 12) # selectize = TRUE,options = list(placeholder="Please Select at Least One Project")
      
    )

  ) # end taList
  
} # end Forecast_Period_ui