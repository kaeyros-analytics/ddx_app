
#---------------------
# CREATE UI FUNCTION
#---------------------
calculate_button_ui <- function(id) {
  
  # NS(): Assign module elements to a unique namespace
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  tagList(
    actionButton(inputId = ns("actionFilter"), title="", label="Calculate" )
  ) # end taList
  
} # end calculate_button_ui