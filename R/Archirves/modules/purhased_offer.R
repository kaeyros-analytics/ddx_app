purchased_offer_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "form-group mt-2 ml-2",
      selectInput(inputId = ns("service_name"), label = "Service Name",
                               choices = sort(unique(ease_data_for_visualization$service_name)),
                               selected = 1, width = "auto")
    ),
    tags$div(
      class = "map-plot3",
      shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = ns("purchased_offer_plot")), type = 1, color = 'blue')
    ) # end tags$div
  ) # end tagList
} # end purchased_offer_ui

purchased_offer_server <- function(input, output, session) {
  
  observe({responsive = TRUE
  
  x <- input$years
  
  # Extracting updated list of available versions for the selected time period
  service_name <-  ease_data_for_visualization %>%
    filter(years_ == input$years) %>%
    select(service_name)
  
  service_name <- sort(unique(service_name$service_name))
  
  # Updating select input
  updateSelectInput(session, "service_name",label = "Service Name", choices = service_name,
                    selected = service_name[1])
  
  }) # end observe
  
  
  output$purchased_offer_plot <- renderPlotly({
   
    purchased_offer_plot <- ease_data_for_visualization %>%
      filter(years_ == input$years,
             purchase_y_n == "1",
             service_name == input$service_name) %>%
      group_by(service_name, offer_name, month_year) %>%
      summarise( most_visited_offer = n(),
                 offer_purchased = sum(price_offer))
    
    fig_offer_year_filter_service <- plotly::plot_ly(purchased_offer_plot, x = ~month_year,
                                                     type = "bar",
                                                     y = ~offer_purchased, color = ~offer_name,
                                                     #colors = "Paired",
                                                     #colors = c("#515F7A", "#8290AC", "#BCC7CC", "#D5D9E0"),
                                                     colors = c("#55508D", "#726DA8", "#7D8CC4", "#A0D2DB"),
                                                     #colors = c("#37536d", "#1a76ff", "#db4052", "#556B2F"),
                                                     #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                                                     #text = ~most_visited_offer, textposition = 'outside',
                                                     hovertext = paste("Date :",purchased_offer_plot$month_year,
                                                                       "<br>Service name :", purchased_offer_plot$service_name,
                                                                       "<br>Offer name :", purchased_offer_plot$offer_name,
                                                                       "<br>Nber of times ", purchased_offer_plot$offer_name, 
                                                                       " purchased :", purchased_offer_plot$most_visited_offer,
                                                                       "<br>Purchased:",round(purchased_offer_plot$offer_purchased/1000000,
                                                                                              digits = 1), "million CFA"),
                                                     hoverinfo = 'text') %>%
      layout(title = "",
             legend = list(x = 100, y = 0.60, title=list(color= "blue", text='<b>Offer name</b>')),
             uniformtext=list(minsize=15, mode='show'),
             xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                          # change x-axix size
                          tickfont = list(size = 14), 
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= -45, tickvals = purchased_offer_plot$month_year),
             yaxis = list(title = "<b> Purchased offer in CFA </b>",
                          titlefont = list(size = 16),
                          #range = c(0,60000000),
                          #range = purchased_offer_plot$offer_purchased,
                          #tickvals = purchased_offer_plot$offer_purchased,
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
    
    
    
    return(fig_offer_year_filter_service)
    
  }) # end purchased_offer_plot
  
} # end purchased_soffer_server