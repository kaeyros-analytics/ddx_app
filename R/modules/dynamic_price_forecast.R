
dynamic_price_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "plot1",
      shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = ns("dynamic_price")), type = 1, color = 'blue')
    ) # end tags$div
  ) # end tagList
} # end user_location_ui

dynamic_price_server <- function(input, output, session) {
  
  #observeEvent(input$actionFilter,{  })
  
  filter_action <- shiny::eventReactive(eventExpr = input$actionFilter,
                                        valueExpr = {
                                          # subset the data frame 
                                          df_filter <- df_testdata %>%
                                            dplyr::filter(client_id == input$selected_client_id,
                                                          machine_id == input$selected_machine_id,
                                                          date >= input$daterange[1] & date <= input$daterange[2]) %>%
                                            dplyr::select(dynamic_pricing, lot_health_index, GPR, avg_temperature_month, 
                                                          inflation_rate, DEU_dmd, fixed_premium_rate, date, year_)
                                          
                                        }, ignoreNULL = FALSE
                                        
  ) # end eventReactive
  
  average_event <- eventReactive(eventExpr = input$actionFilter,
                                  valueExpr = {
                                    input$average_fixed_price
                                  }, ignoreNULL = FALSE)
  
  forecast_event1 <- eventReactive(eventExpr = input$actionFilter,
                          valueExpr = {
                            input$selected_forecast_period
                            }, ignoreNULL = FALSE)
  
  output$dynamic_price <- plotly::renderPlotly({
    
    # # subset the data frame 
    # df_filter <- df_testdata %>%
    #   filter(client_id == input$selected_client_id,
    #          machine_id == input$selected_machine_id,
    #          date >= input$daterange[1] & date <= input$daterange[2]) %>%
    #   dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price, date, year_) %>% 
    #   relocate(dynamic_price, .before = lot_health_index)
    
    mts_df_filter <- stats::ts(filter_action()[- c(7,8,9)],
                               frequency = 12,
                               start = c(min(filter_action()$year_), 1),
                               end = c(max(filter_action()$year_), 12))
    
    # Differencing the whole mts
    stnry1 <- MTS::diffM(mts_df_filter) #difference operation on a vector of time series. Default order of differencing is 1.
    
    # Creating a VAR model with vars
    var.a.mts_df_filter <- vars::VAR(stnry1,
                                     lag.max = 9, #highest lag order for lag length selection according to the choosen ic
                                     ic = "AIC", #information criterion
                                     type = "none") #type of deterministic regressors to include
    
   
    # Forecasting VAR models test
    fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = forecast_event1())
    #browser() filter_action points
    
    # Forecasting the dynamic_price
    dynamic_price = fcast1$fcst[1] # type list
    
    # Extracting the forecast column
    x_predict = dynamic_price$dynamic_pricing[,1]
    
    # Inverting the differencing
    #To get the data to the original scale we invert the time series, to get the values on the original scale 
    #we add the last value from the variable dynamic_price time series to the predicted values.
    # Extract the last values from the variable dynamic_pricing
    y <- tail(mts_df_filter)[,1][6]
    x2_predict = cumsum(x_predict) + y
    
    # Adding data and forecast to one time series
    dynamic_price_inv =ts(c(filter_action()[,1], x2_predict),
                          start = c(min(filter_action()$year_), 1),
                          end = c(max(filter_action()$year_), 12),
                          frequency = 12)
    
    
    #convert dynamic_price_inv to data frame and set a name from the column 
    dynamic_price_inv_dataframe <- as.data.frame(dynamic_price_inv) 
    colnames(dynamic_price_inv_dataframe) <- c("x")
    #head(dynamic_price_inv_dataframe)
    #browser()
    # extract the date from the time series dynamic_price_inv
    date <- as.Date(time(dynamic_price_inv))
    
    #choose a part of data frame for each forecast period
    nbr_row <- nrow(dynamic_price_inv_dataframe)
    
    n_period <- as.numeric(forecast_event1())
    
    n_1 <- nbr_row - n_period
    n_2 <- nbr_row - n_period + 1
    
    # Create confidence interval
    date_ci <- date[n_2:nbr_row]
    lower <- round((x2_predict - 1.95), 2)
    upper <- round((x2_predict + 1.95), 2)
    #df_ci <- cbind.data.frame(date_ci, lower, upper)
    #colnames(df_ci) <- c("date_ci","lower", "upper")
    
    # ggplot2::theme_set(theme_bw()) 
    # plot_dynamic_price <- ggplot() + 
    #   geom_line(data = as.data.frame(dynamic_price_inv_dataframe[1:n_1,]), aes(y = get("dynamic_price_inv_dataframe[1:n_1, ]"), 
    #                                                                            x = seq(1, n_1)), color = "#5BB318", size=0.71) +
    #   geom_line(data = as.data.frame(dynamic_price_inv_dataframe[n_2:nbr_row,]), aes(y = get("dynamic_price_inv_dataframe[n_2:nbr_row, ]"), 
    #                                                                                  x = seq(n_2, nbr_row)), color = "red", size=0.71) +
    #   ggtitle("") + # Plot of forecast of the VAR model on  time series
    #   #scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + 
    #   theme(plot.title = element_text(hjust = 0.5)) +
    #   xlab("Time") + ylab("")
    plot_dynamic_price <- plot_ly() 
    plot_dynamic_price <- plot_dynamic_price %>% add_trace(data = as.data.frame(dynamic_price_inv_dataframe[1:n_1,]), x = ~date[1:n_1], 
                                                           y = ~get("dynamic_price_inv_dataframe[1:n_1, ]"),
                                                           mode = 'lines', line = list(color = "#59AB64"), #17B169
                                                           hovertext = paste('Date: ',"<b>",date[1:n_1], "</b>",
                                                                             '<br>Dynamic price:',"<b>",
                                                                             round(dynamic_price_inv_dataframe[1:n_1, ],2), " </b>"),
                                                           hoverinfo = 'text') 
    plot_dynamic_price <- plot_dynamic_price %>% add_trace(data = as.data.frame(dynamic_price_inv_dataframe[n_2:nbr_row,]), 
                                                           x = ~date[n_2:nbr_row], y = ~get("dynamic_price_inv_dataframe[n_2:nbr_row, ]"),
                                                           mode = 'lines', line = list(color = "#ff0000"),
                                                           hovertext = paste('Date: ',"<b>",date[n_2:nbr_row], "</b>",
                                                                             '<br>Predicted dynamic price:',"<b>",
                                                                             round(dynamic_price_inv_dataframe[n_2:nbr_row, ],2), " </b>"),
                                                           hoverinfo = 'text',
                                                           
                                                           showlegend = F)
    # Confidence interval with the function add_ribbons
    plot_dynamic_price <- plot_dynamic_price %>% add_ribbons(x = date_ci,
                                                             ymin = lower,
                                                             ymax = upper,
                                                             color = I("#87A2FB"),  #I('rgba(49,130,189, 1)'),
                                                             showlegend = F,
                                                             hoverinfo="text",
                                                             hovertext = paste('Date:',"<b>",date_ci, "</b> \n",
                                                                               '95%_Upper:',"<b>",upper, "</b> \n",
                                                                               '95%_Lower:',"<b>",lower, "</b>"))
    plot_dynamic_price <- plot_dynamic_price %>% layout(title = "",
                                                        xaxis = list(title = "Date"),
                                                        yaxis = list (title = "Dynamic price in Euros"))
    plot_dynamic_price <- plot_dynamic_price %>%
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
    
    if(average_event()){ 
      plot_dynamic_price <- plot_dynamic_price %>% add_lines(data =filter_action(), x = filter_action()$date, y = filter_action()$fixed_premium_rate,  
                                                             line = list(color = "#E8AA42"), hoverinfo = 'text',
                                                             inherit = FALSE, showlegend = FALSE)
      plot_dynamic_price <- plot_dynamic_price %>% add_annotations(x = filter_action()$date[30], y = filter_action()$fixed_premium_rate[1] + 0.25, 
                                                                   text = paste("Fixed premium rate = ", round(filter_action()$fixed_premium_rate[1], 2), sep = ""),
                                                                   showarrow  = F)
      # plot_dynamic_price <- plot_dynamic_price + 
      #   geom_line(data=filter_action(), aes(x=seq(1, nrow(filter_action())), y=filter_action()$fixed_price), color='#EB823D') +
      #   annotate("text", x = 50, y = filter_action()$fixed_price[1] + 0.028, size = 6, label = paste0("fixed premium = ", round(filter_action()$fixed_price[1], 2),
      #                                                                                           sep = "" ))  
    } 
    
    return(plot_dynamic_price)
    
  }) # end dynamic_price
  
 
} # end dynamic_price_server