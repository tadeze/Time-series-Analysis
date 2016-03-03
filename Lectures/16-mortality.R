# HW6 - Return to mortality example
load(url("http://www.stat.pitt.edu/stoffer/tsa3/tsa3.rda"))
source(url("http://stat565.cwick.co.nz/code/fortify-ts.r"))
source(url("http://stat565.cwick.co.nz/code/get_acf.R")) # my code for examine_corr

library(ggplot2)
library(dplyr)
library(tidyr)
library(nlme)

mort <- data.frame(mortality = cmort,
  part = part, temp = tempr)
mort$time <- fortify(cmort)$time

# My thought process:
# 1. Series are seasonal, we probably should deal with that
#    to get a relationship beyond just common seasonal period
# 2. Difference seasonally

# What you found:

mort <- mutate(mort,
  mort_diff = c(rep(NA, 52), diff(mortality, lag = 52)),
  part_diff = c(rep(NA, 52), diff(part, lag = 52)),
  temp_diff = c(rep(NA, 52), diff(temp, lag = 52)))

lm_fit <- lm(mort_diff ~ part_diff + temp_diff, data = mort)
examine_corr(residuals(lm_fit), lag.max = 104)
examine_corr(mort$mort_diff, na.action = na.pass, lag.max = 104)
# big negative correlation at lag 52

# Talk about overdifferencing

# Would first differencing be better?
examine_corr(diff(mort$mortality, lag = 1), 
  na.action = na.pass, lag.max = 104)

# == How else could we deal with seasonality?
periodic <- function(x, frequency = 1, order = 1){
  do.call(cbind, lapply(1:order, function(ord){
    cbind(cos(2*pi*ord*frequency*x), sin(2*pi*ord*frequency*x))    
  }))
}
# remove trend - to help visualize seasonality
fit_mort <- lm(mortality ~ time, data = mort)
mort$detrend <- residuals(fit_mort)

qplot(time %% 1, detrend, data = mort, geom = "line",
    group = time %/% 1) + 
  geom_smooth(aes(group = 1), method = "lm", 
    formula = y ~ periodic(x, freq = 1, order = 4))

# Also deseasonalize everything else
mort$mort_deseas <- residuals(
  lm(detrend ~ periodic(time, freq = 1, order = 4), data = mort))
mort$temp_deseas <- residuals(
  lm(temp ~ periodic(time, freq = 1, order = 4), data = mort))
mort$part_deseas <- residuals(
  lm(part ~ periodic(time, freq = 1, order = 4), data = mort))

# should check this is reasonable...

fit_deseas <- lm(mort_deseas ~ part_deseas + temp_deseas, data = mort)
summary(fit_deseas)
examine_corr(residuals(fit_deseas), lag.max = 104)

fit_gls<- gls(mort_deseas ~ part_deseas + temp_deseas, data = mort,
  correlation = corARMA(p = 2))
summary(fit_gls)
# interpret coefficients as: unit increase in **seasonally adjusted** 
# explanatory, is associated with beta increase in mean 
# **seasonally adjusted**  mortality

# equivalent too fitting model on raw mortality
# with linear trend and seasonal model on RHS
fit_raw <- gls(mortality ~ time + periodic(time, freq = 1, order = 4) + 
    part_deseas + temp_deseas, data = mort,
  correlation = corARMA(p = 2)))

fit_raw <- gls(mortality ~ time + periodic(time, freq = 1, order = 4) + 
    part + temp, data = mort,
  correlation = corARMA(p = 2))

# == Some simuation examples ====

# series with SARIMA structure


# series with fixed seasonality plus noise
