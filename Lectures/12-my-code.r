library(ggplot2)

# x is ar
one_sim2 <- function(){
  x <- arima.sim(list(ar = 0.8), n = 100)
  y <- 1 + arima.sim(list(), n = 100)
  fit <- arima(y, order = c(0, 0, 0), xreg = x)
  
  abs( fit$coef["x"] / 
      sqrt(diag(fit$var.coef)["x"]) ) > 1.96
}

reject2 <- replicate(1000, one_sim2())
table(reject2)

# z is ar
one_sim3 <- function(){
  x <- arima.sim(list(), n = 100)
  y <- 1 + arima.sim(list(ar = 0.8), n = 100)
  fit <- arima(y, order = c(0, 0, 0), xreg = x)
  
  abs( fit$coef["x"] / 
      sqrt(diag(fit$var.coef)["x"]) ) > 1.96
}

reject3 <- replicate(1000, one_sim3())
table(reject3)

# both are ar
one_sim4 <- function(){
  x <- arima.sim(list(ar = 0.8), n = 100)
  y <- 1 + arima.sim(list(ar = 0.8), n = 100)
  fit <- arima(y, order = c(0, 0, 0), xreg = x)
  
  abs( fit$coef["x"] / 
      sqrt(diag(fit$var.coef)["x"]) ) > 1.96
}

reject4 <- replicate(1000, one_sim4())
table(reject4)

# x is ar, alpha = 0.5, z is ar, alpha = 0.2
one_sim5 <- function(){
  x <- arima.sim(list(ar = 0.5), n = 100)
  z <- arima.sim(list(ar = 0.2), n = 100)
  y <- 1 + 0 * x + z
  fit <- arima(y, order = c(0, 0, 0), xreg = x)
  
  abs( fit$coef["x"] / 
      sqrt(diag(fit$var.coef)["x"]) ) > 1.96
}
reject5 <- replicate(1000, one_sim5())
table(reject5)

one_sim6 <- function(){
  x <- arima.sim(list(ar = 0.5), n = 100)
  y <- 1 + arima.sim(list(ar = 0.8), n = 100)
  fit <- arima(y, order = c(0, 0, 0), xreg = x)
  
  abs( fit$coef["x"] / 
      sqrt(diag(fit$var.coef)["x"]) ) > 1.96
}

reject6 <- replicate(1000, one_sim6())
table(reject6)

# examine one fit
x <- arima.sim(list(ar = 0.9), n = 100)
y <- 1 + arima.sim(list(ar = 0.9), n = 100)
fit <- arima(y, order = c(0, 0, 0), xreg = x)
source(url("http://stat565.cwick.co.nz/code/get_acf.R"))
examine_corr(residuals(fit)) # looks like AR(1)!
fit.2 <- arima(y, order = c(1, 0, 0), xreg = x)
examine_corr(residuals(fit.2)) # looks like white noise!

one_sim7 <- function(){
  x <- arima.sim(list(ar = 0.9), n = 100)
  y <- 1 + arima.sim(list(ar = 0.9), n = 100)
  fit <- arima(y, order = c(1, 0, 0), xreg = x)
  
  abs( fit$coef["x"] / 
      sqrt(diag(fit$var.coef)["x"]) ) > 1.96
}

reject7 <- replicate(1000, one_sim7())
table(reject7)

# random walks
one_sim8 <- function(){
  x <- cumsum(rnorm(100))
  y <- 1 +  cumsum(rnorm(100))
  fit <- arima(y, order = c(0, 0, 0), xreg = x)
  
  abs( fit$coef["x"] / 
      sqrt(diag(fit$var.coef)["x"]) ) > 1.96
}

reject8 <- replicate(1000, one_sim8())
table(reject8)

# differencing solves the problem
one_sim9 <- function(){
  x <- cumsum(rnorm(100))
  y <- 1 +  cumsum(rnorm(100))
  fit <- arima(y, order = c(0, 1, 0), xreg = x)
  
  abs( fit$coef["x"] / 
      sqrt(diag(fit$var.coef)["x"]) ) > 1.96
}

reject9 <- replicate(1000, one_sim9())
table(reject9)

# integrated random walk
one_sim10 <- function(){
  x <- cumsum(cumsum(rnorm(100)))
  y <- 1 +  cumsum(cumsum(rnorm(100)))
  fit <- arima(y, order = c(0, 0, 0), xreg = x)
  
  abs( fit$coef["x"] / 
      sqrt(diag(fit$var.coef)["x"]) ) > 1.96
}

reject10 <- replicate(1000, one_sim10())
table(reject10)

# double differecing
one_sim11 <- function(){
  x <- cumsum(cumsum(rnorm(100)))
  y <- 1 +  cumsum(cumsum(rnorm(100)))
  fit <- arima(y, order = c(0, 2, 0), xreg = x)
  
  abs( fit$coef["x"] / 
      sqrt(diag(fit$var.coef)["x"]) ) > 1.96
}

reject11 <- replicate(1000, one_sim11())
table(reject11)
