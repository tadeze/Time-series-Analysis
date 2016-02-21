library(ggplot2)
library(plyr)
library(lubridate)

# for slides
big_font <- theme_grey(base_size =  24)
load(url("http://stat565.cwick.co.nz/data/corv.rda"))

# clean up our data so that is has all the dates and is in order.
all_days <- data.frame(date = seq(ymd("2000/01/01"), ymd("2013/12/31"),  
  by = "day"))
# merge them together
corv <- join(corv, all_days, type = "full")

# redo years, months and ydays
corv$year <- year(corv$date)
corv$month <- month(corv$date)
corv$yday <- yday(corv$date)

# force it to be in order
corv <- corv[order(corv$date), ]


# == Aggregate == #
# linear fit (think one-way ANOVA)
year_fit <- lm(temp ~ factor(year), data = corv, na.action = na.exclude)
corv$annual_avg <- predict(year_fit, newdata = corv)

qplot(year, annual_avg, data = corv, geom = "line") 
qplot(date, temp, data = corv, geom = "line") +
  geom_line(aes(y = annual_avg), colour = "blue")

# == Smooth == #

# the ma function expects a time series object so we first need to 
corv_ts <- ts(corv$temp, start = 2000, freq = 365.25)

# n is number of time points to average
n <- 7 # we have daily data so this is a week
corv$weekly_ma <- filter(corv_ts, filter = rep(1, n)/n)
n <- 30 # approximately a month
corv$monthly_ma <- filter(corv_ts, filter = c(1/2, rep(1, n-1), 1/2)/n)
n <- 365 # approximately a year
corv$annual_ma <- filter(corv_ts, filter = rep(1, n)/n)

qplot(date, temp, data = corv, geom = "line", alpha = I(0.5)) +
  geom_line(aes(y = weekly_ma, colour = "weekly_ma"), size = 1) +
  geom_line(aes(y = monthly_ma, colour = "monthly_ma"), size = 1) +
  geom_line(aes(y = annual_ma, colour = "annual_ma"), size = 1) +
  scale_colour_brewer("Moving average", type = "qual", palette = 2) +
  big_font
# missing data propagates

last_plot() + xlim(ymd("2011-01-01", "2011-12-31"))


# add a straight line
qplot(date, temp, data = corv, geom = "line") +
  geom_smooth(method = "lm")

# add a locally weighted regression line (loess) 
qplot(date, temp, data = corv, geom = "line") +
  geom_smooth(method = "loess")

# change span of loess line
qplot(date, temp, data = corv, geom = "line") +
  geom_smooth(method = "loess", span = 0.2)


# == Subtract == #
# calculate monthly means
month_fit <- lm(temp ~ factor(month), data = corv, na.action = na.exclude)
corv$month_avg <- predict(month_fit)
corv$res <- residuals(month_fit)

qplot(date, month_avg, data = corv, geom = "line", group = year)
qplot(date, res, data = corv, geom = "line") +
  geom_smooth(method = "loess", span = 0.2)


# === Removing seasonal pattern ex. 2 === #
qplot(yday, temp, data = corv, geom = "line", group = year, alpha = I(.3)) +
  geom_smooth(method = "loess", aes(group = 1), size = 1) +
  big_font

seas_fit <- loess(temp ~ yday, data = corv, na.action = na.exclude)
corv$seas_fit <- predict(seas_fit, newdata = corv)
corv$deseas <- residuals(seas_fit)

# what did the fit look like
qplot(yday, seas_fit, data = corv, geom = "line")

qplot(date, deseas, data = corv, geom = "line") +
  geom_smooth(method = "loess", span = 0.2) +
  big_font

# === automatic approaches === #
# just for demonstration fill in missings with seasonal smooth
temp_nomiss <- corv$temp
temp_nomiss[is.na(temp_nomiss)] <- corv$seas_fit[is.na(temp_nomiss)]
corv_ts2 <- ts(temp_nomiss, start = 2000, freq = 365.25)

plot(stl(corv_ts2, 365.25))
plot(decompose(corv_ts2))

source(url("http://stat565.cwick.co.nz/code/fortify-ts.r"))
load(url("http://www.stat.pitt.edu/stoffer/tsa3/tsa3.rda"))
jj.df <- fortify(jj)
qplot(time, x, data = jj.df, geom ="line") 
qplot(time, log(x), data = jj.df, geom ="line") +
ylab("log (Earnings per share)")

# a quadratic trend
qplot(time, x, data = jj.df, geom ="line") +
  ylab("Earnings per share")  +
  xlab("Year") + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))

jj_trend <- loess(x ~ time, data = jj.df, na.action = na.exclude)

jj.df$trend <- predict(jj_trend, newdata = jj.df)
jj.df$de_trend <- residuals(jj_trend)
qplot(time, de_trend, data = jj.df, geom ="line") +
  ylab("Earnings per share")  +
  xlab("Year")

# a multiplicative trend?
jj.df$de_trend_mult <- jj.df$x/jj.df$trend

qplot(time, de_trend_mult, data = jj.df, geom ="line") +
  ylab("Earnings per share")  +
  xlab("Year")
