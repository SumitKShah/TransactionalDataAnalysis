setwd("C:/Users/Pankaj/Desktop/intern")
train=read.csv("payment.csv")
train[order(as.Date(train$VALUE_DATE,format="%m/%d/%Y")),]
library(lubridate)
library(plyr)
train$VALUE_DATE=mdy(train$VALUE_DATE)
m=arrange(train,VALUE_DATE)
str(m)
table(m$VALUE_DATE)
plot(table(m$VALUE_DATE))
p=as.data.frame(table(m$VALUE_DATE))
library(plotly)
s=plot_ly(p,x=Var1,y=Freq,name="Frequency of Transactions")
s %>% add_trace(y=fitted(loess(Freq~as.numeric(Var1))),x=Var1)
str(p)
head(p)
library(xts)
library(forecast)
p.xts=xts(p[,2], as.POSIXct(p[,1], format="%Y-%m-%d"))
head(p.xts)
daily_rate <- lag(p.xts) / diff(index(p.xts))
dummy_dates <- seq(from=index(p.xts)[1], to=tail(index(p.xts), 1), by="day")
m.xts <- merge(daily_rate, dummy_dates)
m.xts.interpolate <- na.approx(m.xts)
m.ts <- ts(m.xts.interpolate, freq=365, start=c(2016, 122))
dim(m.ts) <- NULL
fit <- ets(m.ts)
plot(forecast(fit, h=30))


library(stats)
plot(forecast(fit, h=90))
library(xts)
library(forecast)
plot(forecast(fit, h=30))


plot(forecast(fit, h=90))



library(timeSeries)
y=ts(p,frequency = 7)
str(y)
y=ts(p[,2],frequency = 7)
str(y)
fit=ets(y)
fc=forecast(fit)
plot(fc)
plot.ts(y)
plot(y)
plot(fit)
accuracy(fit)

library(TTR)
ySMA=SMA(y,n=3)
plot.ts(ySMA)
ySMA=SMA(y,n=5)
plot.ts(ySMA)
ySMA=SMA(y,n=8)
plot.ts(ySMA)
yforecast=HoltWinters(y,beta=FALSE,gamma=FALSE)
yforecast
yforecast$fitted
plot(yforecast)
yforecast$SSE
HoltWinters(y, beta=FALSE, gamma=FALSE, l.start=1)
yforecast2=forcast.HoltWinters(yforecast)
yforecast2=forecast.HoltWinters(yforecast,h=8)
plot.forecast(yforecast2)
head(yforecast2)
acf(yforecasts2$residuals, lag.max=20)
Box.test(yforecast2$residuals, lag=20, type="Ljung-Box")
plot.ts(yforecast2$residuals)
plotForecastErrors <- function(forecasterrors)
{
# make a histogram of the forecast errors:
mybinsize <- IQR(forecasterrors)/4
mysd   <- sd(forecasterrors)
mymin  <- min(forecasterrors) - mysd*5
mymax  <- max(forecasterrors) + mysd*3
# generate normally distributed data with mean 0 and standard deviation mysd
mynorm <- rnorm(10000, mean=0, sd=mysd)
mymin2 <- min(mynorm)
mymax2 <- max(mynorm)
if (mymin2 < mymin) { mymin <- mymin2 }
if (mymax2 > mymax) { mymax <- mymax2 }
# make a red histogram of the forecast errors, with the normally distributed data overlaid:
mybins <- seq(mymin, mymax, mybinsize)
hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
# freq=FALSE ensures the area under the histogram = 1
# generate normally distributed data with mean 0 and standard deviation mysd
myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
# plot the normal curve as a blue line on top of the histogram of forecast errors:
points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(yforecast2$residuals)
yforecast3 <- HoltWinters(y, gamma=FALSE)
yforecast3
plot(yforecast3)
yforecast4=forecast.HoltWinters(yforecast3,h=10)
plot.forecast(yforecast4)


