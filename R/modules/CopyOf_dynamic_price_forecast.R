
dynamic_price_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "plot1",
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = ns("dynamic_price")), type = 1, color = 'blue')
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
                                            dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price, year_, fixed_price) %>% 
                                            relocate(dynamic_price, .before = lot_health_index)
                                          
                                          #test <- as.numeric(input$selected_forecast_period)
                                          
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
  
  
  output$dynamic_price <- shiny::renderPlot({
    
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
                                     lag.max = 1, #highest lag order for lag length selection according to the choosen ic
                                     ic = "AIC", #information criterion
                                     type = "none") #type of deterministic regressors to include
    
   
    # Forecasting VAR models test
    fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = forecast_event1())
    #browser() filter_action points
    
    # Forecasting the dynamic_price
    dynamic_price = fcast1$fcst[1] # type list
    
    # Extracting the forecast column
    x_predict = dynamic_price$dynamic_price[,1]
    
    # Inverting the log1p
    #To get the data to the original scale we invert the time series, to get the values on the original scale 
    #we add the last value from the variable dynamic_price time series to the predicted values.
    x2_predict = exp(x_predict) - 1
    
    # Adding data and forecast to one time series
    dynamic_price_inv =ts(c(filter_action()[,1], x2_predict),
                          start = c(min(filter_action()$year_), 1),
                          end = c(max(filter_action()$year_), 12),
                          frequency = 12)
    
    
    #convert dynamic_price_inv to data frame and set a name from the column 
    dynamic_price_inv_dataframe <- as.data.frame(dynamic_price_inv) 
    colnames(dynamic_price_inv_dataframe) <- c("x")
    #head(dynamic_price_inv_dataframe)
    
    #choose a part of data frame for each forecast period
    nbr_row <- nrow(dynamic_price_inv_dataframe)
    
    n_period <- as.numeric(forecast_event1())
    
    n_1 <- nbr_row - n_period
    n_2 <- nbr_row - n_period + 1 
    
    
    ggplot2::theme_set(theme_bw()) 
    plot_dynamic_price <- ggplot() + 
      geom_line(data = as.data.frame(dynamic_price_inv_dataframe[1:n_1,]), aes(y = get("dynamic_price_inv_dataframe[1:n_1, ]"), 
                                                                               x = seq(1, n_1)), color = "#5BB318", size=0.71) +
      geom_line(data = as.data.frame(dynamic_price_inv_dataframe[n_2:nbr_row,]), aes(y = get("dynamic_price_inv_dataframe[n_2:nbr_row, ]"), 
                                                                                     x = seq(n_2, nbr_row)), color = "red", size=0.71) +
      ggtitle("") + # Plot of forecast of the VAR model on  time series
      #scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + 
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Time") + ylab("")
    
    if(average_event()){ 
      plot_dynamic_price <- plot_dynamic_price + 
        geom_line(data=filter_action(), aes(x=seq(1, nrow(filter_action())), y=filter_action()$fixed_price), color='#EB823D') +
        annotate("text", x = 50, y = filter_action()$fixed_price[1] + 0.028, size = 6, label = paste0("fixed premium = ", round(filter_action()$fixed_price[1], 2),
                                                                                                sep = "" ))  
    } 
    
    return(plot_dynamic_price)
    
  }) # end dynamic_price
  
 
} # end dynamic_price_server