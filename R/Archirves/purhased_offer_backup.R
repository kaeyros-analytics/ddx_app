purchased_offer_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "map-plot3",
      shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = ns("purchased_offer_plot")), type = 8, color = 'grey')
    ) # end tags$div
  ) # end tagList
} # end purchased_offer_ui

purchased_offer_server <- function(input, output, session) {
  
  output$purchased_offer_plot <- renderPlotly({
   
    purchased_offer_plot <- ease_data_for_visualization %>%
      filter(years_ == input$years,
             purchase_y_n == "1",
             service_name == "Car rentals") %>%
      group_by(service_name, offer_name, month_year) %>%
      summarise( most_visited_offer = n(),
                 offer_purchased = sum(price_offer))
    
    fig_offer_year_filter_service <- plotly::plot_ly(purchased_offer_plot, x = ~month_year,
                                                     type = "bar",
                                                     y = ~offer_purchased, color = ~offer_name,
                                                     colors = "Paired",
                                                     #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                                                     text = ~most_visited_offer, textposition = 'outside',
                                                     hovertext = paste("Date :",purchased_offer_plot$month_year,
                                                                       "<br>Service name :", purchased_offer_plot$service_name,
                                                                       "<br>Offer name :", purchased_offer_plot$offer_name,
                                                                       "<br>Nber of times ", purchased_offer_plot$offer_name, 
                                                                       " purchased :", purchased_offer_plot$most_visited_offer,
                                                                       "<br>Purchased:",round(purchased_offer_plot$offer_purchased/1000000,
                                                                                              digits = 1), "million CFA"),
                                                     hoverinfo = 'text') %>%
      layout(title = "Total connection time per month",
             legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>Offer name</b>')),
             uniformtext=list(minsize=15, mode='show'),
             xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14), 
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= -45, tickvals = purchased_offer_plot$month_year),
             yaxis = list(title = "<b> connection time in seconds </b>",
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
    
    
    
    return(fig_offer_year_filter_service)
    
  }) # end purchased_offer_plot
  
} # end purchased_soffer_server