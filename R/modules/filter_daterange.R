
#---------------------
# CREATE UI FUNCTION
#---------------------
daterange_ui <- function(id) {
  
  # NS(): Assign module elements to a unique namespace
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  tagList(
    dateRangeInput(inputId = ns("daterange"), "Start_Period",
                   start = as.Date("2005-01-31"),
                   #start  = min(df_testdata$date),
                   #end    = max(df_testdata$date),
                   end = as.Date("2019-12-31"),
                   min    = min(df_testdata$date),
                   max    = max(df_testdata$date),
                    
                   format = "dd-mm-yyyy",
                   separator = " - ")
  ) # end taList
  
} # end daterange_ui