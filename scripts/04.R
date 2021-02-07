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


