# install.packages("TSA")  # if you need to
library(TSA)
library(ggplot2)
source(url("http://stat565.cwick.co.nz/code/fortify-ts.r"))
source(url("http://stat565.cwick.co.nz/code/get_acf.R"))

# == Public transit boardings == #
# how does the price of gas relate to the boardings on public transit?

data(boardings)
log.boardings = boardings[ , 1]
log.price = boardings[ , 2]

summary(lm(log.boardings ~ log.price))

plot(log.boardings)
plot(log.price)
# price specifically looks non-stationary, try diffferencing
plot(diff(log.price))
# better

# fit model with uncorrelated errors, but differenced once
fit <- arima(log.boardings, xreg = log.price, order = c(0,1,0))
examine_corr(residuals(fit), lag.max = 30)
# peak at 1, i.e. seasonal.  decays in acf, cuts off in pacf, so AR
# something happening short term, try MA

fit.2 <- arima(log.boardings, xreg = log.price, order = c(0,1,0),
  seasonal = c(1, 0, 0))
examine_corr(residuals(fit.2), lag.max = 30)
qplot(sample = residuals(fit.2))
plot(residuals(fit.2))

fit.2

# test for parameter = 0
2*(1 - pnorm(abs( fit.2$coef["xreg"] / 
    sqrt(diag(fit.2$var.coef)["xreg"]) )))

# backtransform and interpret
(est <- 100*((2^coef(fit.2)["xreg"]) - 1))
(ci <- 100*((2^(confint(fit.2)["xreg", ]))- 1))

# There is no evidence that gas price is related to boardings on public transit (p-value = 0.35, from Wald test on the regression parameter, assuming SARIMA (0, 1, 0) x (1, 0, 0)_12) errors). It is estimated that a doubling in the price of gas is associated with an increase in median public transit boardings of 2.5% (95% CI 2.7% decrease to 8.0% increase).

# Caveat:  The book this series comes from identifes and corrects for an outlier which substantially changes the conclusions.  Read Cryer & Chan Chapter 11 if you are interested.
