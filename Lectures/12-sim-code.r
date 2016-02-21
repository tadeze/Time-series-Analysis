one_sim <- function(){
  n <- 100
  x <- arima.sim(list(), n = n)
  z <- arima.sim(list(), n = n)
  y <- 1 + 0*x + z
  fit <- arima(y, order = c(0, 0, 0), xreg = x)
  
  abs( fit$coef["x"] / 
      sqrt(diag(fit$var.coef)["x"]) ) > 1.96
}

reject <- replicate(1000, one_sim())
table(reject)
