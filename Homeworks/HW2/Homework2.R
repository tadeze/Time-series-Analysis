#Homework 
library(ggplot2)
library(dplyr)
library(lubridate)
load(url("http://stat565.cwick.co.nz/data/sales.rda")) #load data
nrow(sales)
str(sales)
# f#ormat date # 
sales <- sales %>% mutate(yday=yday(date),year=year(date),month=month(date))

#Introduction 
qplot(date,value,data=sales,geom="line")


#seasonality 
# seasonality dominates so start there...

# == seasonality == # 
qplot(date, value, data = sales, geom = "line", group = year, alpha = I(.3)) +
	geom_smooth(aes(group = 1), method = "loess", se = FALSE, size = 2) +
	ylab("Average monthly sales") +
	xlab("Years")

# fit the seasonal model in preparation for subtraction
lo_fit <- loess(value ~ month, data = sales, na.action = na.exclude)
sales$seasonal_smooth <- fitted(lo_fit)

# check it looks ok
qplot(date,value, data = sales, geom = "line", group = year, alpha = I(.3)) +
	geom_line(aes(y = seasonal_smooth), colour = "blue",  size = 2)
# subtract off pattern
sales$deseasonalised <- sales$value - sales$seasonal_smooth

#Trend check the trend 

qplot(date, deseasonalised, data = sales, geom = "line") +
	geom_smooth(se = FALSE, method = "loess", span = 0.4, size = 2)+
	ylab("Deseasonalised sales value") +
	xlab("Year")
#Residual
# fit the trend model in preparation for subtraction
lo_fit_trend <- loess(deseasonalised ~ as.numeric(date), data = sales, na.action = na.exclude,
											span = 0.4)
sales$trend_smooth <- fitted(lo_fit_trend)
sales$residual <- sales$deseasonalised - sales$trend_smooth

#fit residual graph 
#plot the residual
qplot(date, residual, data = sales, geom  = "line")
# force to all be in order still
sales <- sales[order(sales$date), ]

# correlation of residuals
sales_acf <- acf(sales$residual, na.action = na.pass)

# qplotize it, for uniform look.
acf_df <- data.frame(ACF = sales_acf$acf, Lag = sales_acf$lag)
ci <- qnorm((1 + 0.95)/2)/sqrt(sales_acf$n.used)

qplot(Lag, ymin = 0, ymax = ACF, data = acf_df, geom = "linerange")+
	geom_hline(aes(yintercept = c(-ci, ci)), colour = "blue", linetype = "dashed") +
	ylab("Autocorrelation")



qplot(month, residual^2, data = sales, alpha = I(0.2)) +
	geom_smooth(method = "loess") +
	ylab("Squared residual \n value") +
	xlab("Year") +
	theme_grey(10)

qplot(date, residual^2, data = sales, alpha = I(0.2)) +
	geom_smooth(method = "loess") +
	ylab("Squared residual \n temperature") +
	xlab("Day of the year") +
	theme_grey(10)
