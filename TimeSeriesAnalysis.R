install.packages("astsa")
library(astsa)


#  White noise and moving average 
w=rnorm(500,0,1) 
#500 N(0,1)variates
# moving average,A linear combination of values in a time series is referred to
# as a filtered series, hence the command filter
par(mfrow=c(2,1))
v=filter(w,sides=2,rep(1/3,3)) 
plot.ts(w,main="white noise")
plot.ts(v,main="moving average")

dev.new() #open a new graphic device
ts.plot(w,v,lty=2:1,col=1:2,lwd=1:2)

# auto regression example
# xt=xt-1 - .9xt-2 +wt wt=white noise
w=rnorm(550,0,1) #50 extra to avoid startup problems
x=filter(w,filter=c(1,-.9),method="recursive")[-(1:50)]
plot.ts(x,main="autoregression")

#random walk with drift
#xt=drift + xt-1 +wt
#if drift = 0, then xt is random walk
set.seed(154) #so you can reproduce the results
w=rnorm(200,0,1); x=cumsum(w)
wd=w+.2; xd=cumsum(wd)
plot.ts(xd,ylim=c(-5,55),main="random walk")
lines(x);
lines(.2*(1:200),lty="dashed")

#signal in noise
#xt=2cos(2pi*t/50+0.6*pi)+wt
cs=2*cos(2*pi*1:500/50+0.6*pi)
w=rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50 + 0.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50 + 0.6*pi)+ N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50 + 0.6*pi)+ N(0,25)))
#as we can see, greater noise can obscure the periodic signal pattern
#spectrual analysis is used to detecting regular or periodic signals

summary(fit <- lm(gtemp~time(gtemp))) #regress gtemp on time
plot(gtemp, type="o", ylab="Global Temperature Deviation")
abline(fit) #add regression line to the plot

#Example 2.2 Pollution, Temperature and Mortality
par(mfrow=c(3,1))
plot(cmort, main="Cardiovascular Mortality", xlab="", ylab="")
plot(tempr, main="Temerature", xlab="", ylab="")
plot(part, main="Particulates", xlab="", ylab="")
dev.new() #open a new graphic device for the scatterplot matrix
pairs(cbind(Mortality=cmort, Temprature=tempr, Particulates=part))
temp=tempr-mean(tempr) # center temperature
temp2=temp^2
trend=time(cmort) # time
fit=lm(cmort~trend + temp + temp2 + part, na.action=NULL)# the use of na.action in lm() is to retain the time series attributes for the residuals and fitted values
summary(fit) #regression results
summary(aov(fit)) # ANOVA table
summary(aov(lm(cmort~cbind(trend, temp, temp2, part)))) 
num=length(cmort) #sample size
AIC(fit)/num - log(2*pi) # AIC
AIC(fit, k=log(num))/num - log(2*pi) #BIC
(AICc=log(sum(resid(fit)^2)/num)+(num+5)/(num-5-2)) #AICc

#example with lagged variables
#performing lagged regression in R is a little difficult because the series must be aligned prior to running the regression
#the easiest way to do this is to create a data frame that we call fish using ts.intersect, which aligns the lagged series
fish=ts.intersect(rec, soiL6=lag(soi,-6), dframe=TRUE)
summary(lm(rec~soiL6,data=fish, na.action=NULL))
plot(fish$rec) 
lines(fitted(fit), col=2)

#example with detrending global temperature
fit=lm(gtemp~time(gtemp),na.action=NULL) # regress gtemp on time
par(mfrow=c(2,1))
plot(resid(fit), type="o", main="detrended")
plot(diff(gtemp), type="o", main="first difference")
par(mfrow=c(3,1))
acf(gtemp, 48, main="gtemp")
acf(resid(fit), 48, main="detrended")
acf(diff(gtemp), 48, main="first difference")

#example of differencing global temperature
#In this case it appears that the differenced process shows minimal autocorrelation, which may imply the global temperature 
#series is a random walk with drift, it's interesting to note that if the series is a random walk with drift, the mean of 
#the differenced series, which is an estimate of the drift
mean(diff(gtemp))
sd(diff(gtemp))/sqrt(length(diff(gtemp)))
