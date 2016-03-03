library(ggplot2)
load(url("http://www.stat.pitt.edu/stoffer/tsa3/tsa3.rda"))
source(url("http://stat565.cwick.co.nz/code/fortify-ts.r"))
source(url("http://stat565.cwick.co.nz/code/get_acf.R"))

soi_df <- fortify(soi)

periodic <- function(x, frequency = 1, order = 1){
  do.call(cbind, lapply(1:order, function(ord){
    cbind(cos(2*pi*ord*frequency*x), sin(2*pi*ord*frequency*x))    
  }))
}

spectrum(soi_df$x, taper = 0, span = c(5, 5)) 
abline(v = c(1/12, 1/48))

abline(v = c(1/12, 2/12, 3/12, 4/12), col = "#9E0142")

qplot(time, soi, data = soi_df, geom = "line")

qplot(time %% 1, x, data = soi_df, geom = "line", 
    group = time %/% 1) + xlab("Time of year")

qplot(time %% 1, x, data = soi_df, geom = "line", 
  group = time %/% 1) +
  geom_smooth(aes(group = 1), method = "lm", 
    formula = y ~ periodic(x, frequency = 1, order = 1))

fit_harm <- lm(x ~ time + periodic(time, frequency = 1, order = 3) +
  periodic(time, freq = 1/4, order = 1), data = soi_df)

soi_df$res <- residuals(fit_harm)
qplot(time, res, data = soi_df, geom = "line") + geom_smooth()
qplot(time %% 1, res, data = soi_df, geom = "line", group = time%/% 1)
examine_corr(soi_df$res)
# still temporal correlation

library(nlme)
fit_gls <- gls(x ~ time + periodic(time, frequency = 1, order = 3) +
  periodic(time, freq = 1/4, order = 1), data = soi_df,
               correlation = corARMA(p = 1, q = 1))
examine_corr(residuals(fit_gls, type = "normalized"))

summary(fit_gls)
# be careful with these p-values, we did some model selection by choosing the 
# harmonic components with large power.

qplot(time, soi, data = soi_df) +
  geom_line(aes(y = fitted(fit_gls)))

spectrum(arima.sim(model = list(ar = 0.86, ma = -0.57), 500), span = c(7,7))
