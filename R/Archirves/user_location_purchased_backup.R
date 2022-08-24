
user_location_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "map-plot1",
      shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = ns("user_location_plot")), type = 8, color = 'grey')
    ) # end tags$div
  ) # end tagList
} # end user_location_ui

user_location_server <- function(input, output, session) {
  
  output$user_location_plot <- renderPlotly({
    # calculate the total purchased  ánd number of user per location, per month 
    data_location_purchased <- ease_data_for_visualization %>%
      filter(years_ == "2019",
             purchase_y_n == "1") %>%
      group_by(user_location, month_year) %>%
      summarise(nbr_user = length(unique(user_tracking_id)),
                total_sale = sum(price_offer)) #%>%
      #arrange(desc(total_sale))
    
    plot_user_location_stack_ <- plotly::plot_ly(data_location_purchased, x = ~month_year,
                                                       type = "bar", 
                                                       y = ~total_sale, color = ~user_location,
                                                       colors = c("slateblue", "#008080", "#BDB76B", "#B8860B"),
                                                       #text = ~most_visited_service, textposition = 'outside',
                                                       hovertext = paste("Date :",data_location_purchased$month_year,
                                                                         "<br>User location :", data_location_purchased$user_location,
                                                                         "<br>Nber of user :", data_location_purchased$nbr_user,
                                                                         "<br>Purchased:",round(data_location_purchased$total_sale/1000000,
                                                                                                digits = 1), "million CFA"),
                                                       hoverinfo = 'text') %>%
      layout(title = "Total purchased of each location per month",  barmode="stack", bargap=0.3,
             legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b>User location</b>')),
             uniformtext=list(minsize=15, mode='show'),
             xaxis = list(title = "<b> Date </b>", #font = list(size = 0),
                          #categoryorder = "total descending",
                          # change x-axix size
                          tickfont = list(size = 14), 
                          # change x-title size
                          titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                          tickangle= -45, tickvals = data_location_purchased$month_year),
             yaxis = list(title = "<b> Purchased in CFA </b>",
                          titlefont = list(size = 16),
                          #categoryorder = "total descending",
                          # change x-axix size
                          tickfont = list(size = 14))
             #tickvals = df_most_visited_service_month_year$most_visited_service)
             #hoverlabel=list(bgcolor="gainsboro")
             #width = 500, autosize=F,
             #bargap = 0.1, bargroupgap = 0.1,
      ) %>%
      config(displayModeBar = F, 
             scrollZoom = T)
    
    
    return(plot_user_location_stack_)
    
    
    
  }) # end user_location_plot
  
  
} # end user_location_server