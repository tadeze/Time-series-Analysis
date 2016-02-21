########## Question 2 #################################3
#
# Find ARIMA model to deere2 dataset
########################################################
library(ggplot2)
library(dplyr)
library(TSA)


data("deere2")

big_font <- theme_grey(base_size =  24)
source(url("http://stat565.cwick.co.nz/code/fortify-ts.r"))
deere <- fortify(deere2)
deere <- rename(deere, deviation = x)

qplot(time, deviation, data = deere, geom = "line") +
	big_font

# differencing can remove trends

deere$diff1 <- c(NA, diff(deere$deviation))
qplot(time, diff1, data = deere, geom = "line") +
	big_font


deere$diff2 <- c(NA, diff(deere$diff1))
qplot(time, diff2, data = deere, geom = "line") +
	big_font
# The trend looks removed but still there is high variance,
#log transform with small 

min.dev <- min(deere$deviation)
deere$diff1_log <- c(NA,diff(log(deere$deviation +1-min.dev)))
qplot(time, diff1_log, data = deere, geom = "line") +
	big_font
#Variance removed with few outlies at the beginning 

qplot(time, diff1_log^2, data = deere) +
	geom_smooth() +
	big_font
# a few outliers in the beginning 

# looks stationary, let's choose an ARMA(p, q) model
acf(deere$diff1_log, lag.max = 50, na.action = na.pass)
# Looks MA process with significant MA(1)
# significant at lag 1,3 and 4 and may be 2
pacf(deere$diff1_log, lag.max = 50, na.action = na.pass)

#significant at lag 1, little bit at 2, 3,4
# models to try MA(1), AR(1), ARMA(1, 1), MA(2)
#lets do grid search till ARMA(5,5)
n <- nrow(deere)

#Grid search  for good fit
min.aic <- 9999
p<- -4 
q<- -4

for(i in 0:3)
{
	for(j in 0:3)
	{
		fit<-arima(log(deere$deviation-min.dev+1), order = c(i, 1, j), xreg = 1:n)
		if(fit$aic<min.aic)
		{
			min.aic <- fit$aic
			p<- i
			q<- j
		}
	}
}
# ARMA(1,3) seems best, check residuals (a.k.a innovations)
fit_arma <- arima(log(deere$deviation-min.dev+1), order = c(p, 1, q), xreg = 1:n)

# diagnostics
# is there any correlation left in the residuals
acf(residuals(fit_arma),na.action=na.pass)
pacf(residuals(fit_arma),na.action=na.pass)
# looks good

# check normality
qqnorm(residuals(fit_arma))
qqline(residuals(fit_arma))
# Looks normal with few expected outliers 

# a time plot of residuals
deere$residuals <- residuals(fit_arma)
qplot(time, residuals, data = deere, geom = "line")

# outliers
num_out<-subset(deere, abs(residuals) > 0.3) %>% nrow
#We have many outliers around the middle