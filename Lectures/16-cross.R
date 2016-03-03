load(url("http://www.stat.pitt.edu/stoffer/tsa3/tsa3.rda"))
source(url("http://stat565.cwick.co.nz/code/get_acf.R")) 
source(url("http://stat565.cwick.co.nz/code/fortify-ts.r"))
library(dplyr)
library(tidyr)
library(ggplot2)

soi_df <- fortify(soi)
soi_df <- rename(soi_df, soi = x)
soi_df <- inner_join(soi_df, fortify(rec) %>% rename(rec = x))

head(soi_df)

qplot(time, value, 
  data = gather(soi_df, variable, value, -time), geom = "line") +
  facet_grid(variable ~ ., scale = "free")

qplot(time %% 1, value, group = time %/% 1,
    data = gather(soi_df, variable, value, -time), geom = "line") +
  facet_grid(variable ~ ., scale = "free") +
  xlab("Fraction of year")

# Individual ACFs
examine_corr(soi_df$soi, lag.max = 36)
examine_corr(soi_df$rec, lag.max = 36)

examine_corr(diff(soi_df$rec, lag = 12), lag.max = 36)

# Cross correlation function
with(soi_df, ccf(soi, rec, ylab = "CCF"))
# sample cross correlation on raw series
# structure may be due to auto-correlation structure

# == Pre-whitening ====

#fit ARIMA model to one series
fit_rec <- arima(soi_df$rec, order = c(2, 0, 0))
examine_corr(residuals(fit_rec), lag.max = 36)
# looking good
rec_prewhitened <- residuals(fit_rec)

# Then apply same model to other series
# pre-whiten soi, using same parameters as rec fit
soi_prewhitened <- residuals(
  arima(soi_df$soi, order = c(2, 0, 0),
    fixed = c(coef(fit_rec)[-3], NA)))
examine_corr(soi_prewhitened)
# it won't remove all autocorrelation in the other series but that is ok

# pattern much clearer!
ccf(soi_prewhitened, rec_prewhitened)

# == similar ideas in the frequency domain ====
# co-spectrum
spec1 <- spectrum(cbind(soi, rec), span = c(3, 3))

plot(spec1, plot.type = "coherency")
plot(spec1, plot.type = "phase")
