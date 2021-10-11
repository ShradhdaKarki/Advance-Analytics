library(forecast)
library(dbplyr)
library(dplyr)
library(tidyverse)
library(zoo)


data_sk<-read.csv('AustralianWines.csv')
str(data_sk)

x<-ts(data_sk$Red,start=c(1980,1),frequency=12)
#x<-ts(data_sk$Red,start=c(1993,1),end=c(1994,12),frequency=12)

plot(x)

data.lm<-tslm(x~trend)
summary(data.lm)


nvalid<-24
ntrain<-length(x)-nvalid

train.ts<-window(x,start=c(1980,1),end=c(1980,ntrain))
valid.ts<-window(x,start=c(1980,ntrain+1),end=c(1980,ntrain+nvalid))

### Linear model

train.lm<-tslm(train.ts~trend)
summary(train.lm)


train.lm.pred<-forecast(train.lm,h=nvalid,level=0)  #yt hat
accuracy(train.lm.pred,valid.ts)


## Polynomial model

train.lm.poly.trend <- tslm(train.ts ~ trend + I(trend^2))
summary(train.lm.poly.trend)
train.lm.poly.trend.pred <- forecast(train.lm.poly.trend, h = nvalid, level = 0)
accuracy(train.lm.poly.trend.pred,valid.ts)

## Seasonality model

train.lm.season <- tslm(train.ts ~ season)
summary(train.lm.season)
train.lm.season.pred <- forecast(train.lm.season, h = nvalid, level = 0)
accuracy(train.lm.season.pred,valid.ts)

# A model with trend and seasonality
train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.lm.trend.season)
train.lm.trend.season.pred <- forecast(train.lm.trend.season, h = nvalid, level = 0)
accuracy(train.lm.trend.season.pred,valid.ts)



#Simple exponential smoothing model
ma <- rollmean(x,k=12,align="right")
summary(ma)

ma

# Calculate MAPE
MAPE = mean(abs((ma-x)/x),na.rm=T)
MAPE

# run simple exponential smoothing
# and alpha = 0.2 to fit simple exponential smoothing.
ses <- ses(train.ts, alpha = 0.2, h=24)
autoplot(ses)
accuracy(ses,valid.ts)

# Use ses function to estimate alpha
ses1 <- ses(train.ts, alpha = NULL, h=24)
summary(ses1)
accuracy(ses1,valid.ts)































