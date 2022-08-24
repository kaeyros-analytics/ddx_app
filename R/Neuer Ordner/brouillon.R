ex <- data.frame(treat.year = c("C.2015", "C.2015", "C.2015", "C.2015", "Ca.2015", "Ca.2015", "Ca.2015", "Ca.2015", "C.2009", "C.2009", "C.2009", "C.2009", "Ca.2009", "Ca.2009", "Ca.2009", "Ca.2009"),
                 treat = c("C", "C", "C", "C", "Ca", "Ca", "Ca", "Ca", "C", "C", "C", "C", "Ca", "Ca", "Ca", "Ca"),
                 year = c("2015", "2015", "2015", "2015", "2015", "2015", "2015", "2015", "2009", "2009", "2009", "2009", "2009", "2009", "2009", "2009"),
                 var.b = c(33, 18, 34, 28, 12, 11, 15, 13, 45, 52, 35, 39, 29, 27, 30, 27),
                 var.e = c(1.1, 0.5, 1.3, 1.2, 0, 0, 0, 0, 1.06, 0.96, 1.06, 0.58, 0.1, 0, 0, 0),
                 var.g = c(83.2, 53.6, 94.9, 80.8, 25.6, 29.2, 31.9, 29.8, 102.3, 83.9, 158.7, 126.6, 78.5, 186.9, 82.4, 62))

PCA1 <- prcomp(ex[ , 3:4], scale = TRUE)

library(ggfortify)
autoplot(PCA1, data = ex, colour = "year", 
         loadings.label = T,  frame.colour="treat.year", frame=T)+theme(legend.position = "none")
# 
# Registered S3 method overwritten by 'GGally':
#   method from   
# +.gg   ggplot2
# 
# Registered S3 method overwritten by 'GGally':
#   method from   
# +.gg   ggplot2
# Registered S3 method overwritten by 'quantmod':
#   method            from
# as.zoo.data.frame zoo

remove.packages("forecast")

# changing the scale initial vector
rescaled_vector <- scales::rescale(df_filter$lot_health_index, to = c(20, 100))

# visualize density distribution of the rscaled vector
den <- density(rescaled_vector) 
plot(den, frame = FALSE, col = "blue",main = "Density plot", ylim = c(0, 0.03))

data(AirPassengers)

class(AirPassengers)

mts_df_filter[, "dynamic_price" ]

loti_arima <- mts_df_filter[, "dynamic_price" ] %>% forecast::auto.arima()
loti_arima %>% forecast(h = 12) %>% forecast::autoplot()

flow_arima <- mts_df_filter[, "dynamic_price" ] %>% auto.arima(lambda = 0)
flow_arima %>% forecast(h = 12) %>% autoplot()

x <- 1:10
x1 <- c(2,4,6,8,7,8,14,16,18,20)
lo <- loess(y~x)
plot(y)

lines(smooth(y,'normal'),type='l',col='blue')
xl <- seq(min(x),max(x), (max(x) - min(x))/1000)
lines(xl, predict(lo,xl), col='red', lwd=2)


x3R <- smooth(x1, "3R") # 2 iterations of "3"

plot(x1)
lines(x3R, col = "red")

x2 <- df_filter$dynamic_price

x33R <- smooth(x2, "3R") # 2 iterations of "3"
plot(x2, type = "l")
lines(x33R, col = "red")


x33R <- smooth(x2, "3RSS") # 2 iterations of "3"
plot(x2, type = "l")
lines(x33R, col = "red")
lines(mean(df_filter$avg_market_premium_price), col = "green")


x <- sin(seq(0, 4 * pi, length.out = 100)) + rnorm(100, 0, 0.2)
plot(x, type = "l")
lines(smoothing(x, method = "smooth"), type = "l", col = "blue")
lines(smoothing(x, method = "loess"), type = "l", col = "red")


x34R <- modelbased::smoothing(x2, method = "smooth") # 2 iterations of "3"
plot(x2, type = "l", xlim = c(2001, 2020))
lines(, type = "l", col = "blue")
lines(smoothing(x, method = "loess"), type = "l", col = "red")


