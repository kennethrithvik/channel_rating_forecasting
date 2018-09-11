## Time Series and Forecasting

R has [extensive facilities](http://cran.r-project.org/web/views/TimeSeries.html) for analyzing time series data. This section describes the creation of a time series, seasonal decomposition, modeling with exponential and ARIMA models, and forecasting with the [forecast](http://cran.r-project.org/web/packages/forecast/index.html) package.

### Creating a time series

The **ts()** function will convert a numeric vector into an R time series object. The format is **ts(**_vector_,** start=, end=, frequency=)** where start and end are the times of the first and last observation and frequency is the number of observations per unit time (1=annual, 4=quartly, 12=monthly, etc.).

# save a numeric vector containing 72 monthly observations  
# from Jan 2009 to Dec 2014 as a time series object  
myts &lt;- ts(myvector, start=c(2009, 1), end=c(2014, 12), frequency=12)   
  
# subset the time series (June 2014 to December 2014)  
myts2 &lt;- window(myts, start=c(2014, 6), end=c(2014, 12))   
  
# plot series  
plot(myts)

### Seasonal Decomposition

A time series with additive trend, seasonal, and irregular components can be decomposed using the **stl()** function. Note that a series with multiplicative effects can often by transformed into series with additive effects through a log transformation (i.e., _newts_ &lt;- **log(**_myts_**)**).

# Seasonal decomposition  
fit &lt;- stl(myts, s.window="period")  
plot(fit)  
  
# additional plots  
monthplot(myts)  
library(forecast)  
seasonplot(myts)

### Exponential Models

Both the **HoltWinters()** function in the base installation, and the **ets()** function in the forecast package, can be used to fit exponential models.

# simple exponential - models level  
fit &lt;- HoltWinters(myts, beta=FALSE, gamma=FALSE)  
# double exponential - models level and trend  
fit &lt;- HoltWinters(myts, gamma=FALSE)  
# triple exponential - models level, trend, and seasonal components  
fit &lt;- HoltWinters(myts)  
  
# predictive accuracy  
library(forecast)  
accuracy(fit)  
  
# predict next three future values  
library(forecast)  
forecast(fit, 3)  
plot(forecast(fit, 3))

### ARIMA Models

The **arima()** function can be used to fit an autoregressive integrated moving averages model. Other useful functions include:

 **lag(**_ts_, _k_**)** lagged version of time series, shifted back _k_observations

 **diff(**_ts_, **differences=**_d_**)** difference the time series _d_ times

 **ndiffs(**_ts_**)** Number of differences required to achieve stationarity (from the [forecast](http://cran.r-project.org/web/packages/forecast/index.html) package)

 **acf(**_ts_**)** autocorrelation function

 **pacf(**_ts_**)** partial autocorrelation function

 **adf.test(**_ts_**)** Augemented Dickey-Fuller test. Rejecting the null hypothesis suggests that a time series is stationary (from the [tseries](http://cran.r-project.org/web/packages/tseries/index.html) package)

 **Box.test(**_x_, **type="Ljung-Box")** Pormanteau test that observations in vector or time series _x_ are independent

Note that the [forecast](http://cran.r-project.org/web/packages/forecast/index.html) package has somewhat nicer versions of **acf()** and **pacf()** called **Acf()** and Pacf() respectively.

# fit an ARIMA model of order P, D, Q  
fit &lt;- arima(myts, order=c(p, d, q)  
  
# predictive accuracy  
library(forecast)  
accuracy(fit)  
  
# predict next 5 observations  
library(forecast)  
forecast(fit, 5)  
plot(forecast(fit, 5))

### Automated Forecasting

The [forecast](http://cran.r-project.org/web/packages/forecast/index.html) package provides functions for the automatic selection of exponential and ARIMA models. The **ets()** function supports both additive and multiplicative models. The auto.arima() function can handle both seasonal and nonseasonal ARIMA models. Models are chosen to maximize one of several fit criteria.

library(forecast)  
# Automated forecasting using an exponential model  
fit &lt;- ets(myts)  
  
# Automated forecasting using an ARIMA model  
fit &lt;- auto.arima(myts)

### Going Further

There are many good online resources for learning time series analysis with R. These include [A little book of R for time series](http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/) by Avril Chohlan and DataCamp's [manipulating time series in R course](https://www.datacamp.com/courses/manipulating-time-series-data-in-r-with-xts-zoo) by Jeffrey Ryan.
