
lot_health_index_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "plot2",
      shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = ns("lot_health")), type = 1, color = 'blue'),
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
                                            dplyr::select(dynamic_pricing, lot_health_index, GPR, avg_temperature_month, 
                                                          inflation_rate, DEU_dmd, fixed_premium_rate, date, year_)
                                          
                                        }, ignoreNULL = FALSE
                                        
  ) # end filter_action
  
  #browser()
  
  forecast_event <- eventReactive(eventExpr = input$actionFilter,
                                  valueExpr = {
                                    input$selected_forecast_period
                                  }, ignoreNULL = FALSE) # forecast_event
  
  forecast_method <- eventReactive(eventExpr = input$actionFilter,
                                   valueExpr = {
                                     input$select_forecast
                                   }, ignoreNULL = FALSE) # forecast_method
  
  
  shiny::observeEvent(input$actionFilter, {
    
    if (forecast_method() == "VAR"){
      
      output$lot_health <- plotly::renderPlotly({
        
        # # subset the data frame 
        # df_filter <- df_testdata %>%
        #   filter(client_id == input$selected_client_id,
        #          machine_id == input$selected_machine_id,
        #          date >= input$daterange[1] & date <= input$daterange[2]) %>%
        #   dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price, date, year_) %>% 
        #   relocate(dynamic_price, .before = lot_health_index)
        
        
        mts_df_filter <- stats::ts(filter_action()[- c(7,8)],
                                   frequency = 12,
                                   start = c(min(filter_action()$year_), 1),
                                   end = c(max(filter_action()$year_), 12))
        
        # Differencing the whole mts
        stnry1 <- MTS::diffM(mts_df_filter) #difference operation on a vector of time series. Default order of differencing is 1.
        
        
        
        # Creating a VAR model with vars
        var.a.mts_df_filter <- vars::VAR(stnry1,
                                         lag.max = 9, #highest lag order for lag length selection according to the choose AIC
                                         ic = "AIC", #information criterion
                                         type = "none") #type of deterministic regressors to include
        
        
        #fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = input$selected_forecast_period) 
        fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = forecast_event())
        
        #browser() 
        
        # Forecasting the dynamic_price
        lot_health = fcast1$fcst[2] # type list
        
        # Extracting the forecast column
        x_predict. = lot_health$lot_health_index[,1]
        
        # Inverting the differencing
        #To get the data to the original scale we invert the time series, to get the values on the original scale 
        #we add the last value from the variable lot_health_index time series to the predicted values.
        
        # Extract the last values from the variable lot_health_index 
        yy <- tail(mts_df_filter)[,2][6]
        #y <- test[,1][6]
        x2_predict. = cumsum(x_predict.) + yy
        
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
        
        # extract the date from the time series lot_health_inv
        date_ts <- as.Date(time(lot_health_inv))
        
        #choose a part of data frame for each forecast period
        nbr_row1 <- nrow(lot_health_inv_dataframe)
        n_period1 <- as.numeric(forecast_event())
        
        n_11 <- nbr_row1 - n_period1
        n_22 <- nbr_row1 - n_period1 + 1 
        
        # Create confidence interval
        date_ci1 <- date_ts[n_22:nbr_row1]
        
        x_lower <- round((lot_health$lot_health_index[,2] + yy),2)
        x_upper <- round((lot_health$lot_health_index[,3] + yy),2)
        
        # ggplot2::theme_set(theme_bw()) 
        # plot_lot_health_index <- ggplot() + 
        #   geom_line(data = as.data.frame(lot_health_inv_dataframe[1:n_11,]), aes(y = get("lot_health_inv_dataframe[1:n_11, ]"), 
        #                                                                          x = seq(1, n_11)), color = "#7879FF", size=0.71) +
        #   geom_line(data = as.data.frame(lot_health_inv_dataframe[n_22:nbr_row1,]), aes(y = get("lot_health_inv_dataframe[n_22:nbr_row1, ]"), 
        #                                                                                 x = seq(n_22, nbr_row1)), color = "red", size=0.71) +
        #   ggtitle("") + # Plot of forecast of the VAR model on  time series
        #   #scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + 
        #   theme(plot.title = element_text(hjust = 0.5)) +
        #   xlab("Time") + ylab("")
        plot_lot_health_index <- plot_ly() 
        plot_lot_health_index <- plot_lot_health_index %>% add_trace(data = as.data.frame(lot_health_inv_dataframe[1:n_11,]),
                                                                     x = ~date_ts[1: n_11], y = ~get("lot_health_inv_dataframe[1:n_11, ]"),
                                                                     #x = ~seq(1, n_11), y = ~get("lot_health_inv_dataframe[1:n_11, ]"),
                                                                     mode = 'lines', line = list(color = "#8C12FB"), # 6495ED
                                                                     hovertext = paste('Date: ',"<b>",date_ts[1: n_11], "</b>",
                                                                                       '<br>Iot health index:',"<b>",
                                                                                       round(lot_health_inv_dataframe[1:n_11,],2), "</b>"),
                                                                     hoverinfo = 'text') 
        plot_lot_health_index <- plot_lot_health_index %>% add_trace(data = as.data.frame(lot_health_inv_dataframe[n_22:nbr_row1,]), 
                                                                     x = ~date_ts[n_22:nbr_row1], y = ~get("lot_health_inv_dataframe[n_22:nbr_row1, ]"),
                                                                     #x = ~seq(n_22, nbr_row1), y = ~get("lot_health_inv_dataframe[n_22:nbr_row1, ]"),
                                                                     mode = 'lines', line = list(color = "#ff0000"),
                                                                     hovertext = paste('Date: ',"<b>",date_ts[n_22:nbr_row1], "</b>",
                                                                                       '<br>Predicted iot health index:',"<b>",
                                                                                       round(lot_health_inv_dataframe[n_22:nbr_row1,],2), "</b>"),
                                                                     hoverinfo = 'text', showlegend = F)
        # Confidence interval with the function add_ribbons
        plot_lot_health_index <- plot_lot_health_index %>% add_ribbons(x = date_ci1,
                                                                 ymin = x_lower,
                                                                 ymax = x_upper,
                                                                 color = I("#87A2FB"), #I('rgba(49,130,189, 1)'), 
                                                                 showlegend = F,
                                                                 hoverinfo="text",
                                                                 hovertext = paste('Date:',"<b>",date_ci1, "</b> \n",
                                                                                   '95%_Upper:',"<b>",x_upper, "</b> \n",
                                                                                   '95%_Lower:',"<b>",x_lower, "</b>"))
        plot_lot_health_index <- plot_lot_health_index %>% layout(title = "",
                                                                  xaxis = list(title = "Date"),
                                                                  yaxis = list (title = "Iot health index in percentage"))
        plot_lot_health_index <- plot_lot_health_index %>%
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
        
        
        return(plot_lot_health_index)
        
        
      }) # end lot_health
      
    } else if (forecast_method() == "VARMA"){
      
      output$lot_health <- plotly::renderPlotly({
        
        mts_df_filter <- stats::ts(filter_action()[- c(7,8)],
                                   frequency = 12,
                                   start = c(min(filter_action()$year_), 1),
                                   end = c(max(filter_action()$year_), 12))
        
        # Differencing the whole mts
        stnry1 <- MTS::diffM(mts_df_filter) #difference operation on a vector of time series. Default order of differencing is 1.
        
        
        
        # Creating a VAR model with vars
        var.a.mts_df_filter <- vars::VAR(stnry1,
                                         lag.max = 9, #highest lag order for lag length selection according to the choose AIC
                                         ic = "AIC", #information criterion
                                         type = "none") #type of deterministic regressors to include
        
        
        #fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = input$selected_forecast_period) 
        fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = forecast_event())
        
        #browser() 
        
        # Forecasting the dynamic_price
        lot_health = fcast1$fcst[2] # type list
        
        # Extracting the forecast column
        x_predict. = lot_health$lot_health_index[,1]
        
        # Inverting the differencing
        #To get the data to the original scale we invert the time series, to get the values on the original scale 
        #we add the last value from the variable lot_health_index time series to the predicted values.
        
        # Extract the last values from the variable lot_health_index
        yy <- tail(mts_df_filter)[,2][6]
        #y <- test[,1][6]
        x2_predict. = cumsum(x_predict.) + yy
        
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
        
        # extract the date from the time series lot_health_inv
        date_ts <- as.Date(time(lot_health_inv))
        
        #choose a part of data frame for each forecast period
        nbr_row1 <- nrow(lot_health_inv_dataframe)
        n_period1 <- as.numeric(forecast_event())
        
        n_11 <- nbr_row1 - n_period1
        n_22 <- nbr_row1 - n_period1 + 1 
        #length(filter_action()$fixed_price)
        
        # ggplot2::theme_set(theme_bw()) 
        # plot_lot_health_index <- ggplot() + 
        #   geom_line(data = as.data.frame(lot_health_inv_dataframe[1:n_11,]), aes(y = get("lot_health_inv_dataframe[1:n_11, ]"), 
        #                                                                          x = seq(1, n_11)), color = "#7879FF", size=0.71) +
        #   geom_line(data = as.data.frame(lot_health_inv_dataframe[n_22:nbr_row1,]), aes(y = get("lot_health_inv_dataframe[n_22:nbr_row1, ]"), 
        #                                                                                 x = seq(n_22, nbr_row1)), color = "red", size=0.71) +
        #   ggtitle("") + # Plot of forecast of the VAR model on  time series
        #   #scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + 
        #   theme(plot.title = element_text(hjust = 0.5)) +
        #   xlab("Time") + ylab("")
        plot_lot_health_index <- plot_ly() 
        plot_lot_health_index <- plot_lot_health_index %>% add_trace(data = as.data.frame(lot_health_inv_dataframe[1:n_11,]),
                                                                     x = ~date_ts[1: n_11], y = ~get("lot_health_inv_dataframe[1:n_11, ]"),
                                                                     #x = ~seq(1, n_11), y = ~get("lot_health_inv_dataframe[1:n_11, ]"),
                                                                     mode = 'lines', line = list(color = "#89554E"),
                                                                     hovertext = paste('Date: ',"<b>",date_ts[1: n_11], "</b>",
                                                                                       '<br>Iot health index:',"<b>",
                                                                                       round(lot_health_inv_dataframe[1:n_11,],2), "</b>"),
                                                                     hoverinfo = 'text') 
        plot_lot_health_index <- plot_lot_health_index %>% add_trace(data = as.data.frame(lot_health_inv_dataframe[n_22:nbr_row1,]), 
                                                                     x = ~date_ts[n_22:nbr_row1], y = ~get("lot_health_inv_dataframe[n_22:nbr_row1, ]"),
                                                                     #x = ~seq(n_22, nbr_row1), y = ~get("lot_health_inv_dataframe[n_22:nbr_row1, ]"),
                                                                     mode = 'lines', line = list(color = "#ff0000"),
                                                                     hovertext = paste('Date: ',"<b>",date_ts[n_22:nbr_row1], "</b>",
                                                                                       '<br>Predicted iot health index:',"<b>",
                                                                                       round(lot_health_inv_dataframe[n_22:nbr_row1,],2), "</b>"),
                                                                     hoverinfo = 'text', showlegend = F)
        plot_lot_health_index <- plot_lot_health_index %>% layout(title = "",
                                                                  xaxis = list(title = "Date"),
                                                                  yaxis = list (title = ""))
        plot_lot_health_index <- plot_lot_health_index %>%
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
        
        
        return(plot_lot_health_index)
      }) # end lot_health
      
    } else {
      output$lot_health <- plotly::renderPlotly({
        
        # # subset the data frame 
        # df_filter <- df_testdata %>%
        #   filter(client_id == input$selected_client_id,
        #          machine_id == input$selected_machine_id,
        #          date >= input$daterange[1] & date <= input$daterange[2]) %>%
        #   dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price, date, year_) %>% 
        #   relocate(dynamic_price, .before = lot_health_index)
        
        
        mts_df_filter <- stats::ts(filter_action()[- c(7,8)],
                                   frequency = 12,
                                   start = c(min(filter_action()$year_), 1),
                                   end = c(max(filter_action()$year_), 12))
        
        
        
        # Differencing the whole mts
        stnry1 <- MTS::diffM(mts_df_filter) #difference operation on a vector of time series. Default order of differencing is 1.
        
        
        
        # Creating a VAR model with vars
        var.a.mts_df_filter <- vars::VAR(stnry1,
                                         lag.max = 9, #highest lag order for lag length selection according to the choose AIC
                                         ic = "AIC", #information criterion
                                         type = "none") #type of deterministic regressors to include
        
        
        #fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = input$selected_forecast_period) 
        fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = forecast_event())
        
        #browser() 
        
        # Forecasting the dynamic_price
        lot_health = fcast1$fcst[2] # type list
        
        # Extracting the forecast column
        x_predict. = lot_health$lot_health_index[,1]
        
        # Inverting the differencing
        #To get the data to the original scale we invert the time series, to get the values on the original scale 
        #we add the last value from the variable lot_health_index time series to the predicted values.
        
        # Extract the last values from the variable lot_health_index
        yy <- tail(mts_df_filter)[,2][6]
        #y <- test[,1][6]
        x2_predict. = cumsum(x_predict.) + yy
        
        # Adding data and forecast to one time series
        lot_health_inv =ts(c(filter_action()[,2], x2_predict.),
                           start = c(min(filter_action()$year_), 1),
                           end = c(max(filter_action()$year_), 12),
                           frequency = 12)
        #plot(dynamic_price_inv22)
        
        #convert lot_health_inv to data frame and set a name from the column 
        lot_health_inv_dataframe <- as.data.frame(lot_health_inv) 
        colnames(lot_health_inv_dataframe) <- c("x")
        
        # extract the date from the time series lot_health_inv
        date_ts <- as.Date(time(lot_health_inv))
        
        #choose a part of data frame for each forecast period
        nbr_row1 <- nrow(lot_health_inv_dataframe)
        n_period1 <- as.numeric(forecast_event())
        
        n_11 <- nbr_row1 - n_period1
        n_22 <- nbr_row1 - n_period1 + 1 
        #length(filter_action()$fixed_price)
        
        # ggplot2::theme_set(theme_bw()) 
        # plot_lot_health_index <- ggplot() + 
        #   geom_line(data = as.data.frame(lot_health_inv_dataframe[1:n_11,]), aes(y = get("lot_health_inv_dataframe[1:n_11, ]"), 
        #                                                                          x = seq(1, n_11)), color = "brown", size=0.71) +
        #   geom_line(data = as.data.frame(lot_health_inv_dataframe[n_22:nbr_row1,]), aes(y = get("lot_health_inv_dataframe[n_22:nbr_row1, ]"), 
        #                                                                                 x = seq(n_22, nbr_row1)), color = "red", size=0.71) +
        #   ggtitle("") + # Plot of forecast of the VAR model on  time series
        #   #scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + 
        #   theme(plot.title = element_text(hjust = 0.5)) + 
        #   xlab("Time") + ylab("")
        
        plot_lot_health_index <- plot_ly() 
        plot_lot_health_index <- plot_lot_health_index %>% add_trace(data = as.data.frame(lot_health_inv_dataframe[1:n_11,]),  
                                                                     x = ~date_ts[1: n_11], y = ~get("lot_health_inv_dataframe[1:n_11, ]"),
                                                                     mode = 'lines', line = list(color = "#FCC557"),
                                                                     hovertext = paste('Date: ',"<b>",date_ts[1: n_11], "</b>",
                                                                                       '<br>Iot health index:',"<b>",
                                                                                       round(lot_health_inv_dataframe[1:n_11,],2), "</b>"),
                                                                     hoverinfo = 'text') 
        plot_lot_health_index <- plot_lot_health_index %>% add_trace(data = as.data.frame(lot_health_inv_dataframe[n_22:nbr_row1,]),  
                                                                     x = ~date_ts[n_22:nbr_row1], y = ~get("lot_health_inv_dataframe[n_22:nbr_row1, ]"),
                                                                     mode = 'lines', line = list(color = "#ff0000"),
                                                                     hovertext = paste('Time: ',"<b>", date_ts[n_22:nbr_row1], "</b>",
                                                                                       '<br>Predicted iot health index:',"<b>",
                                                                                       round(lot_health_inv_dataframe[n_22:nbr_row1,],2), "</b>"),
                                                                     hoverinfo = 'text', showlegend = F)
        plot_lot_health_index <- plot_lot_health_index %>% layout(title = "",
                                                                  xaxis = list(title = "Date"),
                                                                  yaxis = list (title = ""))
        plot_lot_health_index <- plot_lot_health_index %>%
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
        
        return(plot_lot_health_index)
        
        
      }) # end lot_health
      
    }
    
  }, ignoreNULL = FALSE)
  
  
} # end lot_health_index_server