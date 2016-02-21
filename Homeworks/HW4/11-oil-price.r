library(ggplot2)
library(dplyr)
big_font <- theme_grey(base_size =  24)

# install.packages("TSA") 

data(oil.price, package = "TSA")

source(url("http://stat565.cwick.co.nz/code/fortify-ts.r"))
oil <- fortify(oil.price)
oil <- rename(oil, price = x)

qplot(time, price, data = oil, geom = "line") +
  big_font

# differencing can remove trends
oil$diff1 <- c(NA, diff(oil$price))

qplot(time, diff1, data = oil, geom = "line") +
  big_font

oil$diff2 <- c(NA, diff(oil$diff1))
qplot(time, diff2, data = oil, geom = "line") +
  big_font

# the trend is gone, but the variation looks like it increases, try log transform

oil$diff1_log <- c(NA, diff(log(oil$price)))

qplot(time, diff1_log, data = oil, geom = "line") +
  big_font
# better


# orignal plot with log transform
qplot(time, log(price), data = oil, geom = "line") +
  big_font

oil$diff2_log <- c(NA, diff(oil$diff1_log))
qplot(time, diff2_log, data = oil, geom = "line") +
  big_font

# so the first difference of log price looks stationary in mean, but what about
# variance
qplot(time, diff1_log^2, data = oil) +
  geom_smooth() +
  big_font
# a few outliers at the start but otherwise looks okish

qplot(time %% 1, diff1_log^2, data = oil, geom = c("boxplot", "point"), group = time %% 1) +
  big_font
# looks ok, %% is the modolu operator, time %% 1 will give the remainder after
# dividing by 1, in this case the decimal part of the year, which is equivalent 
# to the month

# looks stationary, let's choose an ARMA(p, q) model
acf(oil$diff1_log, lag.max = 24, na.action = na.pass)
# significant at lag 1
pacf(oil$diff1_log, lag.max = 24, na.action = na.pass)
# significant at lag 1, maybe something happening at lag 2 and 15
# models to try MA(1), AR(2), ARMA(1, 1)

# now fit an ARIMA(0, 1, 1) model to difference of log price, xrg term
# is to include a constant in the differenced series.
n <- nrow(oil)
(fit_ma1 <- arima(log(oil$price), order = c(0, 1, 1), xreg = 1:n))
(fit_ar2 <- arima(log(oil$price), order = c(2, 1, 0), xreg = 1:n))
(fit_arma1 <- arima(log(oil$price), order = c(1, 1, 1), xreg = 1:n))
(fit_ma2 <- arima(log(oil$price), order = c(0, 1, 2), xreg = 1:n))
# MA(1) seems best, check residuals (a.k.a innovations)

# diagnostics
# is there any correlation left in the residuals
acf(residuals(fit_ma1))
pacf(residuals(fit_ma1))
# look good

# check normality
qqnorm(residuals(fit_ma1))
qqline(residuals(fit_ma1))
# a few outliers but mostly good

# a time plot of residuals
oil$residuals <- residuals(fit_ma1)
qplot(time, residuals, data = oil, geom = "line")

# outliers
subset(oil, abs(residuals) > 0.3)

# our model looks good let's forecast log oil price 
pred.df <- as.data.frame(predict(fit_ma1, n.ahead = 48, newxreg = (n + 1):(n+48)))
pred.df$time <- max(oil$time) + (1:48)/12

qplot(time, log(price), data = oil, geom = "line") + 
  geom_ribbon(aes(ymin = pred- 2*se, ymax = pred + 2*se, y = NULL), data = pred.df, alpha = 0.2) +
  geom_line(aes(y = pred), data = pred.df) +
  big_font

qplot(time, price, data = oil, geom = "line") + 
  geom_ribbon(aes(ymin = exp(pred- 2*se), ymax = exp(pred + 2*se), y = NULL), data = pred.df, alpha = 0.2) +
  geom_line(aes(y = exp(pred)), data = pred.df) +
  big_font

# check against truth
oil_new <- read.csv("../data/oil_update.csv")
oil_new$time <- seq(2006 + 1/12, 2012 + 4/12, 1/12)

qplot(time, price, data = oil, geom = "line") + 
  geom_ribbon(aes(ymin = exp(pred- 2*se), ymax = exp(pred + 2*se), y = NULL), data = pred.df, alpha = 0.2) +
  geom_line(aes(y = exp(pred)), data = pred.df) +
  geom_line(data = oil_new, linetype = "dashed") +
  big_font

