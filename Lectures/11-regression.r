library(ggplot2)
library(dplyr)
library(tidyr)
load(url("http://www.stat.pitt.edu/stoffer/tsa3/tsa3.rda"))
source(url("http://stat565.cwick.co.nz/code/fortify-ts.r"))
source(url("http://stat565.cwick.co.nz/code/get_acf.R")) # my code for examine_corr
big_font <- theme_grey(base_size =  24)

mort <- data.frame(mortality = cmort,
  part = part, temp = tempr)
mort$time <- fortify(cmort)$time

qplot(time, value, data = gather(mort, variable, value, -time), geom = "line") +
  facet_grid(variable ~ ., scale = "free")

qplot(temp, mortality, data = mort)
qplot(part, mortality, data = mort)
qplot(temp, part, data = mort)

qplot(temp, mortality, data = mort) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, colour = "red") +
  geom_smooth(se = FALSE)  +
  big_font

qplot(part, mortality, data = mort) + geom_smooth(se = FALSE) + 
  big_font

mort <- mutate(mort, temp_sc = temp - mean(temp),
               temp_2 = temp_sc^2,
               time0 = time - min(time))

fit_lm <- lm(mortality ~ time0 + temp_sc + temp_2 + part, data = mort)
summary(fit_lm)

# assumptions
# residuals versus covariates
mort_lm <- fortify(fit_lm)
qplot(time0, .resid, data = mort_lm, geom= "line")
qplot(temp_sc, .resid, data = mort_lm)
qplot(part, .resid, data = mort_lm)

# residuals versus fitted
qplot(.fitted, .resid, data = mort_lm)

# normality of residuals
qqnorm(mort_lm$.resid)
qqline(mort_lm$.resid)

# correlation of residuals
examine_corr(residuals(fit_lm)) 
last_plot() + big_font
# AR (2)? violates regression assumptions

# two ways to fit
library(nlme)
gls_fit <- gls(mortality ~ time0 + temp_sc + temp_2 + part, data = mort,
    correlation = corARMA(p = 2), method = "ML")
summary(gls_fit)

# or

arima_fit <- with(mort, 
  arima(mortality, order = c(2, 0, 0), xreg = cbind(time0, temp_sc, temp_2, part)))
arima_fit

# diagnostics

mort$residuals <- residuals(gls_fit, type = "normalized")
mort$fitted <- fitted(gls_fit)

examine_corr(mort$residuals)

qplot(fitted, residuals, data = mort) + geom_hline(yintercept = 0)
qplot(time, residuals, data = mort) + geom_hline(yintercept = 0)
qplot(temp, residuals, data = mort) + geom_hline(yintercept = 0)
qplot(part, residuals, data = mort) + geom_hline(yintercept = 0)

qqnorm(mort$residuals)
qqline(mort$residuals)

confint(arima_fit)
confint(fit_lm)
intervals(gls_fit)

# if you refit the model with white noise errors using gls, you can actually 
# test to see if the correlation structure improved the fit
gls_wn <- gls(mortality ~ time0 + temp_sc + temp_2 + part, data = mort, method = "ML")
anova(gls_wn, gls_fit)

# plot prediction on temp versus mortality plot
mort$pred <- predict(gls_fit, newdata = 
  data.frame(time0 = 0, temp_sc = mort$temp_sc,
             temp_2 = mort$temp_2, part = mean(mort$part)))

qplot(temp, mortality, data = mort) +
  geom_line(aes(y = pred))

