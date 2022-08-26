#---------------------
# CREATE UI FUNCTION
#---------------------
client_id_ui <- function(id) {
  
  # NS(): Assign module elements to a unique namespace
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("selected_client_id"), "Client_Id",
                choices = unique(sort(df_testdata$client_id)),
                selected = "client_0",
                selectize = TRUE)
  ) # end taList
  
} # end client_Id_ui