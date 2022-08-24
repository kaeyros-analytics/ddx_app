
connection_time_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "map-plot4",
      shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = ns("connection_time_plot")), type = 8, color = 'grey')
    ) # end tags$div
  ) # end tagList
} # end connection_time_ui

connection_time_server <- function(input, output, session) {
  
  output$connection_time_plot <- renderPlotly({
    
    df_traffic <- ease_data_for_visualization %>%
      filter(years_ == input$years) %>%
      group_by(month_year) %>%
      summarise(nbr_user = length((user_tracking_id)),
                min_traffic = sum(time_h_m_s)) %>%
      mutate(duration_text = seconds_to_period(min_traffic),
             Hours = hour(seconds_to_period(min_traffic)))
    
    
    fig_offer_year_traffic <- plotly::plot_ly(df_traffic, x = ~month_year,
                                              type = "bar",
                                              y = ~Hours,
                                              colors = "Paired",
                                              #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                                              #text = paste(df_traffic$duration_text, sep = ""), textposition = 'outside',
                                              hovertext = paste("Date :",df_traffic$month_year,
                                                                "<br>Nber of user :", df_traffic$nbr_user,
                                                                "<br>Connection time :", df_traffic$duration_text),
                                              hoverinfo = 'text') %>%
      layout(title = "Total connection time per month",
             legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Offer name</b>')),
             uniformtext=list(minsize=15, mode='show'),
             xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14), 
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= -45, tickvals = df_traffic$month_year),
             yaxis = list(title = "<b> Connection time in seconds </b>",
                          titlefont = list(size = 16),
                          # change x-axix size
                          tickfont = list(size = 14))
             #tickvals = df_most_visited_service_month_year$most_visited_service)
             #hoverlabel=list(bgcolor="gainsboro")
             #width = 500, autosize=F,
             #bargap = 0.1, bargroupgap = 0.1,
      ) %>%
      config(displayModeBar = F, 
             scrollZoom = T)
    
    return(fig_offer_year_traffic)
    
  }) # end fig_offer_year_traffic
  
} # end connection_time_server