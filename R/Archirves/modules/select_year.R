
#---------------------
# CREATE UI FUNCTION
#---------------------
select_year_ui <- function(id) {
  
  # NS(): Assign module elements to a unique namespace
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  tagList(
    selectyear = selectInput(inputId = ns("years"), label = "",
                             choices = sort(unique(ease_data_for_visualization$years_), decreasing = FALSE),
                             selected = 1, width = "auto")
  ) # end taList
  
} # end select_year_ui

