
seed(88)

one_sim <- function(alphax=0.5,alphaz=0.5){
	n <- 100
	x <- arima.sim(list(ar=alphax), n = n)
	z <- arima.sim(list(ar=alphaz), n = n)
	y <- 1 + 0*x + z
	fit <- arima(y, order = c(1, 0, 0), xreg = x)
	
	abs( fit$coef["x"] / 
			 	sqrt(diag(fit$var.coef)["x"]) ) > 1.96
}

#plot the 

#fix fit the residual to AR(1)
#' check the residual fit from arima
#' fit AR(1)
ff <- arima(fit$residuals,order=c(1,0,0),xreg=x)


reject <- replicate(1000, one_sim(0.9,0.9))
table(reject)

#'
#' Large Ar parameter for z  rejects nulll hypothesis many times,
#' for small ar parameter the model rejects close to 5% of the hypothesis 
#' correlation increases the error increases. 
#' 
library('TSA')
?b



