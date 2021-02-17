## libraries

library(tidyverse)
library(autoTS)
library(rsample)
library(forecast)
#install.packages("MLmetrics")
library(MLmetrics)
library(lubridate)
library(stats)


## data ##

## canneto orig 

canneto_orig <- read.csv("processed_data/MADONNA_DI_CANNETO_to_model+lags.csv") %>%
  dplyr::select(-X, -Fl_rate.Tri, -Fl_rate.Quar, 
                -Fl_rate.Sem,-Y_m, -Year,-Month,-Day,
                -Trimonthly,
                -Semester, -Quarters) %>% 
  mutate(Date = as_date(Date))

res <- getBestModel(
  canneto_orig$Date[1:500],
  canneto_orig$fl_rate.Ls[1:500],
  "day",
  complete = 0,
  n_test = 7, # prediction horizon (i.e. n. of days)
  graph = TRUE,
  algos = list("my.prophet", "my.sarima","my.tbats",
               "my.bats", "my.stlm", "my.stlf","my.stl",
               "my.shortterm"),
  bagged = "auto",
  metric.error = my.rmse)

res$best # TBATS

# getting predictions of best model
pred <- res %>%
  my.predictions()

tail(pred)

# plotting predicted VS Actual values
dates_test <- as.Date(canneto_orig$Date, format = "%Y-%m-%d")[501:507]
values_test <- canneto_orig$fl_rate.Ls[501:507]

ggplot() +
  geom_line(data=pred[!is.na(pred$tbats) & pred$type == "mean",], aes(x=dates, 
                                                                       y=tbats), 
            color = "blue") +
  geom_line(aes(x = dates_test, y = values_test), color = "red") +
  xlab('Dates') +
  ylab('Flow Rate Madonna di Canneto') 



### tbats ###
canneto_ts <- ts(canneto_orig$fl_rate.Ls, 
                 frequency =365, 
                 start = c(2015, as.numeric(format(canneto_orig$Date, "%j"))))
canneto_ts

#Create samples
training=window(canneto_ts, start = c(2015,72), end = c(2017,365))
validation=window(canneto_ts, start = c(2018,1))

tbats_model <- tbats(training)
tbats_forecast <- forecast(tbats_model, h = length(validation))

MAPE(tbats_forecast$mean,validation) *100 # 15%
accuracy(tbats_model)
RMSE(tbats_forecast$mean,validation) # 45

plot(tbats_forecast)
lines(canneto_ts)

### sarima ###

fit <- Arima(window(canneto_ts, end = c(2020,7)), order = c(2,0,0),
             seasonal = list(order = c(2,0,0), period = 12))

tsdisplay(residuals(fit))

plot(forecast(fit, h = 12))
lines(canneto_ts)

forecast_fit <- forecast(fit)

accuracy(fit)

RMSE(forecast_fit$mean,validation) # 1.56
MAPE(forecast_fit$mean,validation) * 100 # 0.48


