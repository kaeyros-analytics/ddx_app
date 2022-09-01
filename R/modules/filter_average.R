
#---------------------
# CREATE UI FUNCTION
#---------------------
average_fixed_price_ui <- function(id) {
  
  # NS(): Assign module elements to a unique namespace
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  tagList(
    tags$div(
      class = "filtre1",
      checkboxInput(inputId = ns("average_fixed_price"), label = "Average", value = FALSE)
    )

  ) # end taList
  
} # end average_fixed_price_ui