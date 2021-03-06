#------------------------
#  Homework #4
#-------------------------

#----------------------------
# Simulate AR(1) process with random parameter value of different length
#------------------------------
library('dplyr')

#Simulate AR(1) with len=length and alpha1 parameter
# returns generated timeseries data
sim.dist<-function(len,alpha1)
{
	ar1<-arima.sim(model=list(ar=alpha1,ma=0,sd=1),len)
	return(ar1)
}

# Fit AR of or order to X
# return aic of the fitted model 
fit.model <-function(x,or)
{
	#pass argument and return aic
	fit<-arima(x,c(or,0,0))
	return(fit$aic)
}

#Simulate and fit length with paramter value 
# Return number of times the data fits the true value
simulate.and.fit<-function(len,alpha1)
{

sim.arima<-failwith(NA,sim.dist)
#simulate 500 runs 
ar1.sim <- replicate(500,sim.arima(len,alpha1))

#fit model starting from 0 to 6 order of 500 simulation
aic.model <-matrix(NA,nrow=7,ncol=500)
fit.fail <-failwith(NA,fit.model)
for(i in 0:6)
{
	aic.model[i+1,]<-apply(ar1.sim,2,function(x){fit.fail(x,i)})
}

### check how often the model with lowest AIC is the true model 

ar1.true.fit <- sum(apply(aic.model,2,which.min)==2)  #count number of AR(1) bits other model
return(ar1.true.fit)
}

#Simulate the AR(1) model and fit different AR models

alpha1=0.7 #alpha1 parameter
len=30  #length
best.fit<- simulate.and.fit(len,alpha1)
cat(" Out of 500 simulation generated by AR(1), AR(1) fits as best ",best.fit," times")

#### Repeat with longer length ###
len2=100
best.fit2<- simulate.and.fit(len2,alpha1)

## Repeat with different parameter (alpha1)
alph2 = 0.3
best.fi3<- simulate.and.fit(len,alpha1)  # short timeseries
best.fit4<- simulate.and.fit(len2,alpha1) # for longer timeseries
#Negative alpha value 
alpha3 = -0.3
best.fit5<- simulate.and.fit(len,alpha3)  # short timeseries
best.fit6<- simulate.and.fit(len2,alpha3) # for longer timeseries
