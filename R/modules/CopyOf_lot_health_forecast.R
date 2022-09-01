
lot_health_index_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "plot2",
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = ns("lot_health")), type = 1, color = 'blue'),
    ) # end tags$div
  ) # end tagList
} # end user_location_ui


lot_health_index_server <- function(input, output, session) {
  
  filter_action <- shiny::eventReactive(eventExpr = input$actionFilter,
                                        valueExpr = {
                                          # subset the data frame 
                                          df_filter <- df_testdata %>%
                                            filter(client_id == input$selected_client_id,
                                                   machine_id == input$selected_machine_id,
                                                   date >= input$daterange[1] & date <= input$daterange[2]) %>%
                                            dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price, date, year_) %>% 
                                            relocate(dynamic_price, .before = lot_health_index)
                                          
                                        }, ignoreNULL = FALSE
                                        
  ) # end filter_action
  
  
  
  forecast_event <- eventReactive(eventExpr = input$actionFilter,
                          valueExpr = {
    input$selected_forecast_period
  }, ignoreNULL = FALSE)
  
  #shiny::observeEvent(input$actionFilter, {
    
    output$lot_health <- shiny::renderPlot({
      
      # # subset the data frame 
      # df_filter <- df_testdata %>%
      #   filter(client_id == input$selected_client_id,
      #          machine_id == input$selected_machine_id,
      #          date >= input$daterange[1] & date <= input$daterange[2]) %>%
      #   dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price, date, year_) %>% 
      #   relocate(dynamic_price, .before = lot_health_index)
      
      
      mts_df_filter <- stats::ts(filter_action()[, -c(4,5)],
                                 frequency = 12,
                                 start = c(min(filter_action()$year_), 1),
                                 end = c(max(filter_action()$year_), 12))
      
      
      
      # Creating a VAR model with vars
      var.a.mts_df_filter <- vars::VAR(log1p(mts_df_filter),
                                       lag.max = 1, #highest lag order for lag length selection according to the choose AIC
                                       ic = "AIC", #information criterion
                                       type = "none") #type of deterministic regressors to include
      
 
      #fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = input$selected_forecast_period) 
      fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = forecast_event())
      
      #browser() 
      
      # Forecasting the dynamic_price
      lot_health = fcast1$fcst[2] # type list
      
      # Extracting the forecast column
      x_predict. = lot_health$lot_health_index[,1]
      
      # Inverting the log1p
      #To get the data to the original scale we invert the time series, to get the values on the original scale 
      #we add the last value from the variable dynamic_price time series to the predicted values.
      x2_predict. = exp(x_predict.) - 1
      
      # Adding data and forecast to one time series
      lot_health_inv =ts(c(filter_action()[,2], x2_predict.),
                         start = c(min(filter_action()$year_), 1),
                         end = c(max(filter_action()$year_), 12),
                         frequency = 12)
      #plot(dynamic_price_inv22)
      
      #convert lot_health_inv to data frame and set a name from the column 
      lot_health_inv_dataframe <- as.data.frame(lot_health_inv) 
      colnames(lot_health_inv_dataframe) <- c("x")
      #head(dynamic_price_inv_dataframe)
      
      #choose a part of data frame for each forecast period
      nbr_row1 <- nrow(lot_health_inv_dataframe)
      n_period1 <- as.numeric(forecast_event())
      
      n_11 <- nbr_row1 - n_period1
      n_22 <- nbr_row1 - n_period1 + 1 
      #length(filter_action()$fixed_price)
      
      ggplot2::theme_set(theme_bw()) 
      plot_dynamic_price <- ggplot() + 
        geom_line(data = as.data.frame(lot_health_inv_dataframe[1:n_11,]), aes(y = get("lot_health_inv_dataframe[1:n_11, ]"), 
                                                                               x = seq(1, n_11)), color = "#7879FF", size=0.71) +
        geom_line(data = as.data.frame(lot_health_inv_dataframe[n_22:nbr_row1,]), aes(y = get("lot_health_inv_dataframe[n_22:nbr_row1, ]"), 
                                                                                      x = seq(n_22, nbr_row1)), color = "red", size=0.71) +
        ggtitle("") + # Plot of forecast of the VAR model on  time series
        #scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + 
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Time") + ylab("")
      return(plot_dynamic_price)
      
      
    }) # end lot_health
    
    
  #}, ignoreNULL = FALSE)
  
  
} # end lot_health_index_server