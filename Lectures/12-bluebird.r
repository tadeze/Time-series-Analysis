# install.packages("TSA")  # if you need to
library(TSA)
library(ggplot2)
source(url("http://stat565.cwick.co.nz/code/fortify-ts.r"))
source(url("http://stat565.cwick.co.nz/code/get_acf.R"))

#' # Blue bird chips
#' ## How does price relate to sales?

data(bluebird)
sales <- bluebird[, 1]
price <- bluebird[, 2]

plot(log(sales))
plot(price)
# these both look fairly non-stationary to me, so I won't difference

# the "ignore correlation" approach - WRONG approach
fit_ols <- lm(log(sales) ~ price)
summary(fit_ols)

# backtransform and use % decrease with price increase of 0.1
est_ols <- 100*(1 - exp(0.1 * coef(fit_ols)["price"]))
ci_ols <- 100*(1 - exp(0.1 * confint(fit_ols)["price", ]))
#' It is estimated that an increase in price of 10 cents is associated with decrease in median sales of `r round(est_ols, 2)` (95% CI `r round(ci_ols[1],2)` to `r round(ci_ols[2],2)`).

# fit model with no correlation
fit <- arima(log(sales), xreg = price, order = c(0,0,0))

examine_corr(residuals(fit), lag.max = 30)
# seasonal? something at lag 4, and maybe decaying slowly at lags 8, 12 and 16 in acf, drops off in pacf
# non-seasonal, something at lags 1 through 3 in acf, decaying AR(3)

# try a (0, 0, 0) x (1, 0, 0)_4 model
fit.2 <- arima(log(sales), xreg = price, order = c(0,0,0), seasonal = list(order = c(1, 0, 0), period = 4))
examine_corr(residuals(fit.2))
qplot(sample = residuals(fit.2))
# seems to work pretty well

fit.2
coef(fit.2)["xreg"] 
confint(fit.2)["xreg", ]

# backtransform and use % decrease with price increase of 0.1
(est <- 100*(1 - exp(0.1 * coef(fit.2)["xreg"])))
(ci <- 100*(1 - exp(0.1 * confint(fit.2)["xreg", ])))

#' It is estimated that an increase in price of 10 cents
#'  is associated with decrease in median sales of `r round(est, 2)` (95% CI `r round(ci[1], 2)` to `r round(ci[2], 2)`).

#' Interestingly, accounting for the correlation changes our
#'  estimate only slightly here.  


# Out of curiosity lets look at some other models
## d = 0, D = 0, p = 4
fit.3 <- arima(log(sales), xreg = price, order = c(4,0,0), 
  seasonal = list(order = c(0, 0, 0), period = 4))
## d = 1, D = 0, p = 3
fit.4 <- arima(log(sales), xreg = price, order = c(3,1,0), 
  seasonal = list(order = c(0, 0, 0), period = 4))
## d = 1, D = 1, P = 1
fit.5 <- arima(log(sales), xreg = price, order = c(0,1,1), 
  seasonal = list(order = c(1, 1, 0), period = 4))

rbind(
  100*(1 - exp(0.1 * confint(fit.3)["xreg", ])),
  100*(1 - exp(0.1 * confint(fit.4)["xreg", ])),
  100*(1 - exp(0.1 * confint(fit.5)["xreg", ])))

# All pretty similar confidence intervals.

