## libraries

library(tidyverse)
library(autoTS)


## data 

canneto_orig <- read.csv("processed_data/MADONNA_DI_CANNETO_to_model+lags.csv") %>%
  dplyr::select(-X, -Fl_rate.Tri, -Fl_rate.Quar, 
                -Fl_rate.Sem,-Y_m, -Year,-Month,-Day,
                -Trimonthly,
                -Semester, -Quarters)

# n_test is a prediction horizon, predict for 7 days  
res <- getBestModel(
  canneto_orig$Date[1:500],
  canneto_orig$fl_rate.Ls[1:500],
  "day",
  complete = 0,
  n_test = 7,
  graph = TRUE,
  algos = list("my.prophet", "my.sarima","my.tbats",
               "my.bats", "my.stlm", "my.stlf","my.stl",
               "my.shortterm"),
  bagged = "auto",
  metric.error = my.rmse)

# getting predictions of best model
pred <- res %>%
  my.predictions()

# plotting predicted VS Actual values
dates_test <- as.Date(canneto_orig$Date, format = "%Y-%m-%d")[501:507]
values_test <- canneto_orig$fl_rate.Ls[501:507]

ggplot() +
  geom_line(data=pred[!is.na(pred$sarima) & pred$type == "mean",], aes(x=dates, 
                                                                       y=sarima), 
            color = "blue") +
  geom_line(aes(x = dates_test, y = values_test), color = "red") +
  xlab('Dates') +
  ylab('Flow Rate Madonna di Canneto') 






