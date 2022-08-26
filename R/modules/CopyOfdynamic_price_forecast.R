
dynamic_price_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "plot1",
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = ns("dynamic_price")), type = 1, color = 'blue'),
      #class = "filtre1",
      # selectclientid = selectInput(ns$session("selected_client_id"), "Client_Id",
      #                              choices = unique(sort(df_testdata$client_id)),
      #                              selected = 1,
      #                              selectize = TRUE)
    ) # end tags$div
  ) # end tagList
} # end user_location_ui

dynamic_price_server <- function(input, output, session) {
  
  
  output$dynamic_price <- shiny::renderPlot({
    
    # subset the data frame 
    df_filter <- df_testdata %>%
      filter(client_id == input$selected_client_id,
              machine_id == input$selected_machine_id,
             date >= input$daterange[1] & date <= input$daterange[2]) %>%
      dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price, year_) %>% 
      relocate(dynamic_price, .before = lot_health_index)
    
   
    
    mts_df_filter <- stats::ts(df_filter[, -4],
                               frequency = 12,
                               start = c(min(df_filter$year_), 1),
                               end = c(max(df_filter$year_), 12))
    # 
    # mts_df_filter <- stats::ts(df_filter[, -4],
    #                            frequency = 12,
    #                            start = c(2001, 1),
    #                            end = c(2021, 12))
    
    
    #browser() 
    # Creating a VAR model with vars
    var.a.mts_df_filter <- vars::VAR(log1p(mts_df_filter),
                                     lag.max = 1, #highest lag order for lag length selection according to the choosen ic
                                     ic = "AIC", #information criterion
                                     type = "none") #type of deterministic regressors to include
    
    
    # Forecasting VAR models
    fcast1 = stats::predict(var.a.mts_df_filter, n.ahead = input$selected_forecast_period)
   
    
    # Forecasting the dynamic_price
    dynamic_price = fcast1$fcst[1] # type list
    
    # Extracting the forecast column
    x_predict = dynamic_price$dynamic_price[,1]
    
    # Inverting the log1p
    #To get the data to the original scale we invert the time series, to get the values on the original scale 
    #we add the last value from the variable dynamic_price time series to the predicted values.
    x2_predict = exp(x_predict) - 1
    
    # Adding data and forecast to one time series
    dynamic_price_inv =ts(c(df_filter[,1], x2_predict),
                          start = c(min(df_filter$year_), 1),
                          end = c(max(df_filter$year_), 12),
                          frequency = 12)
    
    # dynamic_price_inv =ts(c(df_filter[,1], x2_predict),
    #                       start = c(2001, 1),
    #                       end = c(2021, 12),
    #                       frequency = 12)
    #plot(dynamic_price_inv)
    
    #convert dynamic_price_inv to data frame and set a name from the column 
    dynamic_price_inv_dataframe <- as.data.frame(dynamic_price_inv) 
    colnames(dynamic_price_inv_dataframe) <- c("x")
    #head(dynamic_price_inv_dataframe)
    
    if(input$selected_forecast_period == 3){
      plot_dynamic_price <- ggplot() + 
        geom_line(data = as.data.frame(dynamic_price_inv_dataframe[1:249,]), aes(y = get("dynamic_price_inv_dataframe[1:249, ]"), 
                                                                                 x = seq(1, 249)), color = "#59CE8F") +
        geom_line(data = as.data.frame(dynamic_price_inv_dataframe[250:252,]), aes(y = get("dynamic_price_inv_dataframe[250:252, ]"), 
                                                                                   x = seq(250, 252)), color = "red") +
        ggtitle("") + # Plot of forecast of the VAR model on  time series
        #scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + 
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Time") + ylab("")
      return(plot_dynamic_price)
      
    } else if(input$selected_forecast_period == 6){
      plot_dynamic_price <- ggplot() + 
        geom_line(data = as.data.frame(dynamic_price_inv_dataframe[1:246,]), aes(y = get("dynamic_price_inv_dataframe[1:246, ]"), 
                                                                                 x = seq(1, 246)), color = "#59CE8F") +
        geom_line(data = as.data.frame(dynamic_price_inv_dataframe[247:252,]), aes(y = get("dynamic_price_inv_dataframe[247:252, ]"), 
                                                                                   x = seq(247, 252)), color = "red") +
        ggtitle("") + # Plot of forecast of the VAR model on  time series
        #scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + 
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Time") + ylab("")
      return(plot_dynamic_price)
      
    } else
      plot_dynamic_price <- ggplot() + 
      geom_line(data = as.data.frame(dynamic_price_inv_dataframe[1:240,]), aes(y = get("dynamic_price_inv_dataframe[1:240, ]"), 
                                                                               x = seq(1, 240)), color = "#59CE8F") +
      geom_line(data = as.data.frame(dynamic_price_inv_dataframe[241:252,]), aes(y = get("dynamic_price_inv_dataframe[241:252, ]"), 
                                                                                 x = seq(241, 252)), color = "red") +
      ggtitle("") + # Plot of forecast of the VAR model on  time series
      #scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Time") + ylab("")
    return(plot_dynamic_price)
    
    
    
  }) # end dynamic_price
  
  
} # end dynamic_price_server+9