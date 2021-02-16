## libraries

library(tidyverse)
library(autoTS)


## data 

canneto_orig <- read.csv("processed_data/MADONNA_DI_CANNETO_to_model+lags.csv") %>%
  dplyr::select(-X, -Fl_rate.Tri, -Fl_rate.Quar, 
                -Fl_rate.Sem,-Y_m, -Year,-Month,-Day,
                -Trimonthly,
                -Semester, -Quarters)

## autoTS

getBestModel(
  canneto_orig$Date,
  canneto_orig$fl_rate.Ls,
  "day",
  complete = 0,
  n_test = NA,
  graph = TRUE,
  algos = list("my.prophet", "my.sarima","my.tbats", 
               "my.bats", "my.stlm", "my.stlf","my.stl",
               "my.shortterm"),
  bagged = "auto",
  metric.error = my.rmse
) # error 