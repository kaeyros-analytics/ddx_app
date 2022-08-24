
purchased_service_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "map-plot2",
      shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = ns("purchased_service_plot")), type = 8, color = 'grey')
    ) # end tags$div
  ) # end tagList
} # end purchased_service_ui

purchased_service_server <- function(input, output, session) {
  
  output$purchased_service_plot <- renderPlotly({
    # what Service has most visited per year?
    
    df_purchased_service <- ease_data_for_visualization %>%
      filter(purchase_y_n == "1",
             years_ == input$years) %>%
      group_by(service_name, month_year) %>%
      summarise(most_visited_service = n(),
                service_payed = sum(price_offer)) #%>%
    # arrange(most_visited_service) 
    
    plot_purchased_service_bar <- plotly::plot_ly(df_purchased_service, x = ~month_year,
                                         type = "bar",
                                         y = ~service_payed, color = ~service_name,
                                         colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                                         text = ~most_visited_service, textposition = 'outside',
                                         hovertext = paste("Date :",df_purchased_service$month_year,
                                                           "<br>Service name :", df_purchased_service$service_name,
                                                           "<br>Nber of times ", df_purchased_service$service_name,
                                                           " purchased :", df_purchased_service$most_visited_service,
                                                           "<br>Purchased:",round(df_purchased_service$service_payed/1000000,
                                                                                  digits = 1), "million CFA"),
                                         hoverinfo = 'text') %>%
      layout(title = "Total purchsed by each service per month", showlegend = T,
             legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Service name</b>')),
             uniformtext=list(minsize=15, mode='show'),
             xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14), 
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= -45, tickvals = df_purchased_service$month_year),
             yaxis = list(title = "<b> Purchased service in CFA </b>",
                          titlefont = list(size = 16),
                          # change x-axix size
                          tickfont = list(size = 14))
             #tickvals = df_purchased_service$most_visited_service)
             #hoverlabel=list(bgcolor="gainsboro")
             #width = 500, autosize=F,
             #bargap = 0.1, bargroupgap = 0.1,
      ) %>%
      config(displayModeBar = F, 
             scrollZoom = T)
    
    return(plot_purchased_service_bar)
    
    # fig_service_year2
    # 
    # ###############################
    # 
    # plot_purchased_service_bar <- plotly::plot_ly(df_purchased_service, x = ~month_year,
    #                                     type = "bar",
    #                                     y = ~most_visited_service, color = ~service_name,
    #                                     text = ~most_visited_service, textposition = 'outside',
    #                                     hovertext = paste("Date :",df_purchased_service$month_year,
    #                                                       "<br>Service name :", df_purchased_service$service_name,
    #                                                       "<br>Nber of Service :", df_purchased_service$most_visited_service,
    #                                                       "<br>Sold:",round(df_purchased_service$service_payed/1000000,
    #                                                                         digits = 1), "million CFA"),
    #                                     hoverinfo = 'text') %>%
    #   layout(title = "",
    #          legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Service name</b>')),
    #          uniformtext=list(minsize=15, mode='show'),
    #          xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
    #                       # change x-axix size
    #                       tickfont = list(size = 14),
    #                       # change x-title size
    #                       titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
    #                       tickangle= -45, tickvals = df_purchased_service$month_year),
    #          yaxis = list(title = "<b> Number of service </b>",
    #                       titlefont = list(size = 16),
    #                       # change x-axix size
    #                       tickfont = list(size = 14))
    #          #tickvals = df_purchased_service$most_visited_service)
    #          #hoverlabel=list(bgcolor="gainsboro")
    #          #width = 500, autosize=F,
    #          #bargap = 0.1, bargroupgap = 0.1,
    #   ) %>%
    #   config(displayModeBar = F, 
    #          scrollZoom = T)
    # return(plot_purchased_service_bar)
    
  }) # end purchased_service_plot
  
} # end purchased_service_server
