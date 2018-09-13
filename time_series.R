library(readr)
rating <- read_csv("development/mtech/testR/rating.csv", 
                  col_names = FALSE, col_types = cols(`17-Jun-2007 (25)` = col_date(format = "%dd-%mmm-%YYY")))
rating_series <- ts(rating["X2"],frequency=52, start=c(2007,25))
rating_series_test <- ts(rating_test$y,frequency=52, start=c(2008,44))
rating_series_train <- ts(rating_train$y,frequency=52, start=c(2007,25))
plot.ts(rating_series)
log_rating_series <- log(rating_series)
require(TTR)


## prophet ###
rating_train <- data.frame(ds=rating[[2]][1:72],y=rating[[1]][1:72])
rating_test <- data.frame(ds=rating[[2]][72:92],y=rating[[1]][72:92])

m <- prophet(rating_train,yearly.seasonality=TRUE)

future <- make_future_dataframe(m,periods = 20,freq = 'week')

forecast <- predict(m, future)

tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
mape<-sum((abs(rating_test["y"]-forecast[[c('yhat')]][72:92]))/
            rating_test["y"])*100/length(rating_test[["y"]])
mad<-sum(abs(rating_test["y"]-forecast[[c('yhat')]][72:92]))*1/21

plot(m, forecast)
prophet_plot_components(m, forecast)
dyplot.prophet(m, forecast)


### getting trend #####
trend<-lm(formula = y ~ X4,data=(rating_train[c("X4","y")]))
pred<-302.8-1.4*rating["X4"]
sum(abs(pred[["X4"]][72:92]-rating_test["y"])/rating_test["y"])*100/21  #mape


### dummy variable  ######
rating["month"]<-months(as.POSIXlt(rating[["X3"]], format="%m/%d/%Y"))
dummies<-lm(rating$X2 ~ rating$X4+rating$month, data=rating)
ggplot(rating, aes(X3)) +
  +     geom_line(aes(y =X2, colour = "actual")) +
  +     geom_line(aes(y = predicted, colour = "predicted"))

rating_train["month"]<-months(as.POSIXlt(rating_train[["ds"]], format="%m/%d/%Y"))
rating_test["month"]<-months(as.POSIXlt(rating_test[["ds"]], format="%m/%d/%Y"))
dummies<-lm(rating_train$y ~ rating_train$X4+rating_train$month, data=rating_train)

#### Exponential smoothing  ####

Sexp<-HoltWinters(rating_series_train,gamma = FALSE,beta = FALSE)  #Simple
holts<-HoltWinters(rating_series_train,gamma = FALSE)  #holt's
holt_winter<-HoltWinters(rating_series_train)  #holt-winter's
plot(Sexp)
forecast<-forecast(Sexp,h=21)
forecast<-forecast(holts,h=21)
forecast<-forecast(holt_winter)

sum(abs(forecast$mean[1:21]-rating_series_test[1:21])/rating_series_test[1:21])*100/21 #mape

### acf pacf  #####
adfTest(x=rating_series,type = "c")
rating_series_dif1<-diff(rating_series,differences=1)
Acf(rating_series_dif1,lag.max = 90)
Pacf(rating_series_dif1,lag.max = 90)

### arima ####


