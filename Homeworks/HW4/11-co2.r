# install.packages("TSA") 
library(ggplot2)
library(dplyr)
big_font <- theme_grey(base_size =  24)

data(co2, package = "TSA")

source(url("http://stat565.cwick.co.nz/code/fortify-ts.r"))
co2 <- fortify(co2)
co2 <- rename(co2, CO2 = x)

qplot(time, CO2, data = co2, geom = "line") +
  big_font

# taking first differences removes the trend
co2$diff <- c(rep(NA, 12), diff(co2$CO2, lag = 12))
qplot(time, diff, data = co2, geom = "line") +
  big_font

# but there is still strong seasonality
# could also remove the seasonality by differencing
# 12 months per year, so difference at lag 12 in original data
# taking first differences removes the trend
co2$diff_12 <- c(rep(NA, 12), diff(co2$diff, lag = 12))
qplot(time, diff_12, data = co2, geom = "line") +
  big_font 
# now we have something that looks stationary


co2$diff_12test <- c(rep(NA, 12), diff(co2$CO2, lag = 12))
co2$diff_1test <- c(NA, diff(co2$diff_12test))
qplot(time, diff_1test, data = co2, geom = "line") +
  big_font
qplot(time, diff_12^2, data = co2) +
  geom_smooth()+
  big_font

qplot(time %/%1, diff_12^2, data = co2) +
  geom_smooth()+
  big_font

source(url("http://stat565.cwick.co.nz/code/get_acf.R")) # my code for examine_corr
# raw acf
examine_corr(co2$CO2, lag.max = 40)
# acf of 1st diff
examine_corr(co2$diff, na.action = na.pass, lag.max = 40)
# acf of 1st and seasonsal diff
examine_corr(co2$diff_12, na.action = na.pass, lag.max = 40)


acf(co2$diff_12, na.action = na.pass)
# significant at lag 1 and 12, maybe 11 and 13 
pacf(co2$diff_12, na.action = na.pass)
# significant at lag 1, 2, 12, 13
# try ARIMA (0, 1, 1) x (0, 1, 1)_12 to start
#     ARIMA (1, 1, 0) x (0, 1, 1)_12 to start
#     ARIMA (1, 1, 1) x (0, 1, 1)_12 to start


(fit_ma <- arima(co2$CO2, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)))
(fit_ar <- arima(co2$CO2, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 1), period = 12)))
(fit_sarma <- arima(co2$CO2, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)))
# choose ARIMA (0, 1, 1) x (0, 1, 1)_12

# what does the automatic procedure say
library(forecast)
# need to make it a ts to get seasonality
# freq = 12 measurements per cycle
auto.arima(ts(co2$CO2, freq =  12))
# picks SARIMA(1, 0, 1)x(0, 1, 1)[12] but AIC is within 
# 3 of our model.

# diagnostics
examine_corr(residuals(fit_ma)) 
last_plot()+ big_font

qqnorm(residuals(fit_ma))
qqline(residuals(fit_ma))

#forecast
pred.df <- as.data.frame(predict(fit_ma, n.ahead = 4*12))
pred.df$time <- max(co2$time) + (1:(4*12))/12

qplot(time, CO2, data = co2, geom = "line") + 
  geom_ribbon(aes(ymin = pred- 2*se, ymax = pred + 2*se, y = NULL), data = pred.df, alpha = 0.2) +
  geom_line(aes(y = pred), data = pred.df) +
  big_font
