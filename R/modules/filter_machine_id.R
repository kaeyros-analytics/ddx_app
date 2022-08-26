#---------------------
# CREATE UI FUNCTION
#---------------------
machine_id_ui <- function(id) {
  
  # NS(): Assign module elements to a unique namespace
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("selected_machine_id"), "Machine_Id",
                choices = unique(sort(df_testdata$machine_id)),
                selected = "M_001",
                selectize = TRUE)
  ) # end taList
  
} # end machine_Id_ui