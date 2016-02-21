# Question 2 b

library(ggplot2)
library(dplyr)
library(TSA)
data(robot)

big_font <- theme_grey(base_size =  24)
source(url("http://stat565.cwick.co.nz/code/fortify-ts.r"))
robot <- fortify(robot)
robot <- rename(robot, distance = x)
# Timeseries of distance travelled in time
qplot(time, distance, data = robot, geom = "line") +
	big_font

#difference 
robot$diff1<-c(NA,diff(robot$distance))
qplot(time, diff1, data = robot, geom = "line") +
	big_font

#difference 
robot$diff1<-c(NA,diff(robot$distance))
qplot(time, diff1, data = robot, geom = "line") +
	big_font

#different 2
robot$diff2<- c(rep(NA,1),diff(robot$diff1,lag=1))
qplot(time, diff2, data = robot, geom = "line") +
	big_font

# Looks good with little bit variance.
qplot(time, diff2^2, data = robot) +
	geom_smooth() +
	big_font
# Variance of the 
qplot(time %/%1, diff2^2, data = robot,group=time%/%1,geom="boxplot") +
	geom_smooth()+
	big_font
# looks stationary, let's choose an ARMA(p, q) model
acf(robot$diff2, lag.max = 50, na.action = na.pass)
# Looks AR(1) with signficant lag at 1 
# significant at lag 1, 2,3 and 5
pacf(robot$diff2, lag.max = 50, na.action = na.pass)


# Check the 
n <- nrow(robot)
#Grid search  for good fit
min.aic <- 9999
p<- -4 
q<- -4
for(i in 0:4)
{
		for(j in 0:4)
	{
		fit<-arima(robot$distance, order = c(i, 1, j), xreg = 1:n)
		if(fit$aic<min.aic)
		{
			min.aic <- fit$aic
			p<- i
			q<- j
		}
	}
}
# ARMA(1,2) seems best
fit_arima <- arima(robot$distance, order = c(p, 1, q), xreg = 1:n)

# diagnostics
# is there any correlation left in the residuals
acf(residuals(fit_arima),na.action = na.pass)
pacf(residuals(fit_arima),na.action = na.pass)
# looks good

# check normality
qqnorm(residuals(fit_arima))
qqline(residuals(fit_arima))
# Looks normal with few expected outliers 

# a time plot of residuals
robot$residuals <- residuals(fit_arima)
qplot(time, residuals, data = robot, geom = "line")

# outliers
subset(robot, abs(residuals) > 0.3)
# We don't have outliers forthis dataset


