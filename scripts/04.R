################################################################################
################################################################################
##################  Working on ARIMA model #####################################
################## for lupa water spring    ####################################
################################################################################
################################################################################


#### libraries ####

library(glmnet)
library(FitAR)
library(forecast)
library(pracma)
library(mFilter)
library(tidyverse)
library(lubridate)
library(xts)

#### lupa file ####

lupa <- read.csv("processed_data/LUPA_to_model.csv") %>%
  select(-X) %>%
  mutate(Date = ymd(Date),
         Month = format(Date, format ="%m"))

View(lupa)

#### turning to ts/xts format ####

str(lupa)

#lupa <- xts(lupa[,-1],order.by = lupa[,1])

lupa_ts <- ts(lupa$imp_flow_rate, frequency = 12,start=min(lupa$Date))
View(lupa_ts)

#plot(lupa$imp_flow_rate, main = "Lupa Water Spring Flow Rate (L/s)")

plot.ts(lupa_ts)

  # one feature that would violate any assumption of stationarity 
  #1. increasing/decreasing (perhaps non-linear) trends over time

#### identifying and removing characteristics, such as trend and seasonal variation ####
# decomposition model reduces time series into 3 components: 
# trend 
# seasonal effects
# random errors

## weights for moving avg 
fltr <- c(1/2, rep(1, times =12), 1/2)/12

## estimate of the trend 
lupa_trend <- stats::filter(lupa_ts, filter = fltr, method = "convo", sides =2)

## plot the trend
plot.ts(lupa_trend, ylab = "Trend", cex = 1)

# estimating seasonal effects
lupa_seas <- lupa_ts - lupa_trend

plot.ts(lupa_seas, cex =1)

ll <- length(lupa_seas)
ff <- frequency(lupa_seas)

periods <- ll%/%ff

index <- seq(1, ll, by = ff) -1


mm <- numeric(ff)
for (i in 1:ff){
  mm[i] <- mean(lupa_seas[index+i], na.rm = T)
}

mm <- mm - mean(mm)

## plot monthly seasonal effects
plot.ts(mm, cex = 1)

#### create ts object for season ####
lupa_seas_ts <- ts(rep(mm, periods + 1)[seq(ll)],
                   start(lupa_seas),frequency = ff)

#### random errors over time ####

lupa_err <- lupa_ts - lupa_trend - lupa_seas_ts

## plot obs ts, trend and seasonal effect 
jpeg("img/lupa_seasonality.jpg", width = 1000, height = 700,
     quality = 100)
plot(cbind(lupa_ts, lupa_trend, lupa_seas_ts,lupa_err),
     main = "", yax.flip = T)
dev.off()

#### or using decompose() for decomposition ####

## decomposition of lupa data
lupa_decomp <- decompose(lupa_ts)

plot(lupa_decomp, yax.flip= T)

## similar results but not identical ... 


#### differencing to remove trend or seasonal effects ####

## twice-difference lupa data 
lupa_d2 <- diff(lupa_ts, differences = 2)

## plot the differenced data 
plot(lupa_d2)

# successfully removed trend in central years, but not at beginning nor end of time series ....

## difference the differenced data (removing "seasonality")

lupa_d2d12 <- diff(lupa_d2, lag = 1)

plot(lupa_d2d12)

# now - time series that appears to be random errors without any obvious trend or
# seasonal component

#### correlation within and among time series ####

## ACF
lupa_acf <-acf(lupa$imp_flow_rate, lag.max = 50, type = "correlation")
# there is very high autocorrelation even out of lags of 50+ months 


plot.acf <- function(ACFobj) {
  rr <- ACFobj$acf[-1]
  kk <- length(rr)
  nn <- ACFobj$n.used
  plot(seq(kk), rr, type = "h", lwd = 2, yaxs = "i", xaxs = "i", 
       ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylab = "Correlation", las = 1)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "blue")
  abline(h = 0)
}


plot.acf(lupa_acf)


### partial autocorellation - PACF 

lupa_pacf <- pacf(lupa$imp_flow_rate, lag.max = 50)

plot.pacf <- function(PACFobj) {
  rr <- PACFobj$acf
  kk <- length(rr)
  nn <- PACFobj$n.used
  plot(seq(kk), rr, type = "h", lwd = 2, yaxs = "i", xaxs = "i", 
       ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylab = "PACF", las = 1)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "blue")
  abline(h = 0)
}

plot.pacf(lupa_pacf)

# PACF very high - equals ACF at lag-1 - but values at lags > 1 are relatively small
# unlike the graph before 

# plot doesnt include any value at lag-0, because it is impossible 
# to remove any intermediate autocorrelation between t and t-k when k = 0


#### arima ####

acf(lupa_d2d12, type = "correlation", lag.max = 12)

## from the results of the new acf (post de-trending)
## 3 lags are adequate

## modelling- lag length selection

fit_lupa3 <- arima(lupa_d2d12, order = c(3,0,0))
fit_lupa3

fit_lupa2 <- arima(lupa_d2d12, order = c(2,0,0)) 
fit_lupa2

fit_lupa1 <- arima(lupa_d2d12, order = c(1,0,0))
fit_lupa1

AIC <- SelectModel(as.numeric(lupa_ts),lag.max = 12, ARModel = "AR",
                   Best = 1, Criterion = "AIC")
AIC

fit_lupa8 <- arima(lupa_d2d12, order = c(8,0,0))
fit_lupa8

accuracy(fit_lupa8) # going for fit_lupa8

#### forecast one step ahead ####

# with lag-3

forecast_lupa <- forecast(fit_lupa3)
plot(forecast_lupa)


# with lag-8

forecast_lupa8 <- forecast(fit_lupa8)
plot(forecast_lupa8)


################################################################################

# doganella aquifer # 

doganella <- read.csv("processed_data/DOGANELLA_to_model.csv") %>%
  select(-X, -Rainfall_Velletri,-Rainfall_Monteporzio,-Temperature_Velletri,
         -Temperature_Monteporzio,-Pozzo_1:-Pozzo_9) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  spread(key = imp, value = depth_to_gw.m)

# pozzo 1

doganella_ts <- ts(doganella$imp1, frequency = 12,start= c(2012,6))

doganella_decomp <- decompose(doganella_ts)

plot(doganella_decomp, yax.flip= T)

# removed trend

doganella_d2 <- diff(doganella_ts, differences = 2)

## plot the differenced data 
plot(doganella_d2)

# successfully removed trend in central years, but not at beginning nor end of time series ....

## difference the differenced data (removing "seasonality")

doganella_d12 <- diff(doganella_d2, lag = 12)

plot(doganella_d12)

### 

acf(doganella_d12, type = "correlation", lag.max = 12)


AIC <- SelectModel(as.numeric(doganella_ts),lag.max = 12, ARModel = "AR",
                   Best = 1, Criterion = "AIC")
AIC


doganella_target1 <- doganella %>% select(Date, imp1)
doganellaxts <- xts(doganella_target1[,-1], order.by = doganella[,1])
doganella1 = detrend(doganellaxts, tt = 'linear', bp = c()) 
plot(doganella1)


acf(doganella1, lag.max = 30, type = "correlation")

AIC <- SelectModel(as.numeric(doganella1),
                   lag.max = 12, ARModel = "AR",
                   Best = 1, Criterion = "AIC")
AIC


fit_doganella8 <- arima(doganella1, order = c(8,0,0))
fit_doganella8

forecast_doganella <- forecast(fit_doganella8)
plot(forecast_doganella)


tsdiag(fit_doganella8)


accuracy(fit_doganella8)


seasonplot(doganella_ts)

dec <- stl(doganella_ts, s.window =  "periodic")
plot(dec)

tsdisplay(doganella_ts)


### SARIMA ###

fit <- Arima(window(doganella_ts, end = 2019), order = c(2,0,0),
             seasonal = list(order = c(2,0,0), period = 12))

tsdisplay(residuals(fit))

plot(forecast(fit, h = 12))
lines(doganella_ts)

## seasonal dummy 

month <- seasonaldummy(window(doganella_ts, end = 2019))
fit_s <- Arima(window(doganella_ts, end = 2019), order = c(2,0,0),
               xreg = month)
plot(forecast(fit_s, h = 12, xreg = month[3:13,]))
lines(doganella_ts)

accuracy(fit)

accuracy(fit_s)



#### madonna di canneto ####

canneto <- read.csv("processed_data/MADONNA_DI_CANNETO_to_model.csv")

str(canneto)

canneto1 <- canneto %>% 
  select(-X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

str(canneto1)

canneto_target <- canneto1 %>%select(Date, imp_flow_rate)
# to xts file 
canneto_xts <- xts(canneto_target[,-1], order.by = canneto1[,1])

plot(canneto_xts)

# removing trend 
canneto_det <- detrend(canneto_xts, tt = "linear",
                       bp = c())
plot(canneto_det)

# opt 2 
fit1 <- lm(canneto_xts~time(canneto_xts), na.action=NULL)
plot(fit1)

# quadratic
time <- as.numeric(time(canneto_xts))
fit2 <- lm(canneto_xts~time+I(time^2), na.action=NULL)

# plot 
par(mfrow = c(2,1))
plot(canneto_xts)
lines(fitted.values(fit1),col=2)

lines(fitted.values(fit2),col=3)

plot(as.xts(rep(0,length(canneto_xts)),order.by = time(canneto_xts)),
     type = "l", main = "linear and quadratic detrend")
lines(resid(fit2),col=3)
lines(resid(fit1),col=2)

## diff()
par(mfrow = c(1,1))
canneto3 <- diff(canneto_xts)
plot(canneto3,as.xts(rep(0,length(canneto_xts)),
                     order.by = time(canneto_xts)),
     type = "l")
lines(resid(fit1),col=3)


### hp filter 
canneto4 <- hpfilter(canneto_xts, freq = 1600)$cycle
canneto4 <- xts(canneto4, order.by = time(canneto_xts))

plot(canneto4, as.xts(rep(0,length(canneto_xts)),
                      order.by = time(canneto_xts)),
     type = "l")
lines(resid(fit1),col=3)


## log transform

canneto5 <- log(canneto_xts)
canneto5growth <- diff(canneto5)

par(mfrow = c(2,1))
plot(canneto_xts)
par(new = T)
plot(canneto5,col="2")
axis(4, col = "2")

plot(canneto3)
par(new = T)
plot(canneto5growth, type ="l", col = 2)
axis(4,col ="2")

## acf 

par(mfrow = c(1,1))
acf(canneto5, lag.max = 12, type = "correlation")
  # strong autocorrelation it seems


acf(na.omit(canneto5growth), lag.max = 12,
    type ="correlation")


acf(canneto_det, lag.max = 12, type = "correlation")
  # still strongly autocorrelated

acf(na.omit(canneto3), lag.max = 12, type = "correlation")

acf(canneto4, lag.max = 12, type = "correlation")

## arima ##

## with hp filter 
fit_canneto4 <- arima(canneto4, order = c(4,0,0))
fit_canneto4

fit_canneto3 <- arima(canneto4, order = c(3,0,0))
fit_canneto3

fit_canneto2 <- arima(canneto4, order = c(2,0,0))
fit_canneto2

fit_canneto1 <- arima(canneto4, order = c(1,0,0))
fit_canneto1

AIC4 <- SelectModel(as.numeric(canneto4),
                    lag.max = 12, 
                    ARModel = "AR", 
                    Best = 1, Criterion = "AIC")
AIC4


fit_canneto11 <- arima(canneto4, order = c(11,0,0))
fit_canneto11


## with diff()

AIC3 <- SelectModel(as.numeric(na.omit(canneto3)),
                    lag.max = 12, 
                    ARModel = "AR",
                    Best = 1, Criterion = "AIC")
AIC3

fit_canneto1 <- arima(canneto3, order = c(1,0,0))
fit_canneto1


### forecast ### 

forecast_canneto4 <- forecast(fit_canneto11)
plot(forecast_canneto4)


forecast_canneto3 <- forecast(fit_canneto1)
plot(forecast_canneto3)


### tsdiag 

tsdiag(fit_canneto11)

tsdiag(fit_canneto1)


accuracy(fit_canneto11)
accuracy(fit_canneto1)

min(canneto_target$Date)
canneto_ts <- ts(canneto_target$imp_flow_rate, frequency = 12, start = c(2015,3))
plot(canneto_ts)

seasonplot(canneto_ts)

dec_canneto <- stl(canneto_ts, s.window = "periodic")
plot(dec_canneto)

tsdisplay(canneto_ts)

## sarima ##

fit_canneto <- Arima(window(canneto_ts,end=2019), method = "CSS",
             order=c(2,0,0),
             seasonal=list(order=c(2,0,0), period = 12))
tsdisplay(residuals(fit_canneto))

plot(forecast(fit_canneto, h = 12))
lines(canneto_ts)

## seasonal dummy 

month <- seasonaldummy(window(canneto_ts, end = 2019))
fit_s <- Arima(window(canneto_ts, end = 2019),
               order = c(2,0,0), xreg = month)
plot(forecast(fit_s, h = 12, xreg = month[3:13,]))
lines(canneto_ts)


tsdisplay(residuals(fit_s))

fit_s2 <- Arima(window(canneto_ts, end = 2019), order = c(4,0,0),
                xreg = NULL)
plot(forecast(fit_s2, h = 12, xreg = NULL))
lines(canneto_ts)

tsdisplay(residuals(fit_s2))

accuracy(fit_s)
accuracy(fit_s2)
accuracy(fit_canneto) # best model performance -- best MAPE and MASE values, and lowest RMSE 
accuracy(fit_canneto11)
accuracy(fit_canneto1)
