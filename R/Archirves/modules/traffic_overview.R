
connection_time_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "form-group mt-2 ml-2",
      selectInput(inputId = ns("choose_month"), label = "Month",
                  choices = unique(ease_data_for_visualization$month_),
                  selected = 1, width = "auto")
    ),
    tags$div(
      class = "map-plot4",
      shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = ns("connection_time_plot")), type = 1, color = 'blue')
    ) # end tags$div
  ) # end tagList
} # end connection_time_ui

connection_time_server <- function(input, output, session) {
  

  
  observe({responsive = TRUE
  
  x <- input$years
  
  # Extracting updated list of available versions for the selected time period
  month_name <-  ease_data_for_visualization %>%
    filter(years_ == input$years) %>%
    select(month_)

  month_name1 <- sort(unique(month_name$month_))
  
  # Updating select input
  updateSelectInput(session, "choose_month",label = "Month", choices = month_name1,
                    selected = month_name1[1])
  
  }) # end observe
  
  
  
  output$connection_time_plot <- renderPlotly({
    df_traffic <- ease_data_for_visualization %>%
      filter(years_ == input$years,
             month_ == input$choose_month) %>%
      #filter(only_start_end >= min(df_traffic_2019$only_start_end) & only_start_end  <= as.Date("2019-08-31")) %>%
      group_by(only_start_end) %>%
      summarise(nbr_user = length((user_tracking_id)),
                min_traffic = sum(time_h_m_s)) %>%
      mutate(duration_text = seconds_to_period(min_traffic),
             real_hours = round(as.numeric(min_traffic) / 3600),
             Hours = hour(seconds_to_period(min_traffic)))
    
    
    fig_traffic_overview_per_month <- plotly::plot_ly(df_traffic, x = ~only_start_end,
                                                      type = "bar",
                                                      y = ~real_hours,
                                                      color = I("#00aeae"),  #008B8B 1C3879 00aeae #5072A7
                                                      #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                                                      #text = paste(df_traffic$duration_text, sep = ""), textposition = 'outside',
                                                      hovertext = paste("Date :",df_traffic$only_start_end,
                                                                        "<br>Nber of user :", df_traffic$nbr_user,
                                                                        "<br>Connection time :", df_traffic$duration_text),
                                                      hoverinfo = 'text') %>%
      layout(title = "",
             legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Offer name</b>')),
             uniformtext=list(minsize=15, mode='show'),
             xaxis = list(title = "<b> Days </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14), 
                          # change x-title size
                          titlefont = list(size = 16), type="date", tickformat="%d-%b",  #tickformat = "%b-%Y",
                          tickangle= -45, range = df_traffic$only_start_end),
             yaxis = list(title = "<b> Connection time in hours </b>",
                          titlefont = list(size = 16),
                          # change x-axix size
                          tickfont = list(size = 14))
             #tickvals = df_most_visited_service_month_year$most_visited_service)
             #hoverlabel=list(bgcolor="gainsboro")
             #width = 500, autosize=F,
             #bargap = 0.1, bargroupgap = 0.1,
      ) %>%
      config(displayModeBar = T, displaylogo = FALSE, modeBarButtonsToRemove = list(
        'sendDataToCloud',
        #'toImage',
        #'autoScale2d',
        'toggleSpikelines',
        'resetScale2d',
        'lasso2d',
        'zoom2d',
        'pan2d',
        'select2d'#,
        #'hoverClosestCartesian'#,
        #'hoverCompareCartesian'
      ), 
             scrollZoom = T)
    
    return(fig_traffic_overview_per_month)
    
   }) # end fig_offer_year_traffic
  
  
  
} # end connection_time_server