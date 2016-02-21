#Home work 3
#Arima model 
#for x_t = 0.9x_{t-1} +0.5Z_t-1 +z_t
library('ggplot2')

#Theoretical ACF using ARMAacf
arma11 <- ARMAacf(ar=c(0.8,-.2),lag.max = 10)

qplot(x=0:10,ymin=0,
			ymax=arma11,geom="linerange")+
	geom_hline(yintercept=0,
						 linetype="dashed")+
	ylim(c(-1,1))

#ACF using simulation 

#simulate 
# 1 simulation 
arm1<-arima.sim(model=list(ar=c(0.8,-0.2),ma=1,sd=1),5)
ar.acf <-acf(arm1,lag.max = 1)
cat("ACF at lag1 =",ar.acf$acf[2])
ts.length=10
#1000 simulation 
arm1000<-replicate(1000,arima.sim(model=list(ar=c(0.8,-0.2),ma=1,sd=1),ts.length))

#estimate of simulated data 
acf.as <- apply(arm1000,2,function(x){acf(x,lag.max =1)$acf[2]})
acf.as.mean <- mean(acf.as)  #average overall simulation 
cat("\nACF at lag1 after 1000 simulation ",acf.as.mean,var(acf.as))

