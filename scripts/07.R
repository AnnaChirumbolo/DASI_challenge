################################
######### only canneto #########
################################

#### libraries ####


#install.packages("rsample")
library(rsample)
library(caret)
library(ggthemes)
library(scales)
#install.packages("wesanderson")
library(wesanderson)
library(tidyverse)
library(gbm)
#install.packages("Metrics")
library(Metrics)
#install.packages("here")
library(here)
library(MASS)
library(leaps)
library(purrr)


#### uploading all the files ####

canneto_orig <- read.csv("processed_data/MADONNA_DI_CANNETO_to_model+lags.csv") %>%
  dplyr::select(-X, -Date, -Fl_rate.Tri, -Fl_rate.Quar, 
                -Fl_rate.Sem,-Y_m, -Year,-Month,-Day,
                -Trimonthly,
                -Semester, -Quarters)
str(canneto_orig)

#canneto_orig[,4:17] <- factor(canneto_orig[,4:17])
#str(canneto_orig)

canneto0.5 <- read.csv("processed_data/canneto_rain0.5.csv")%>%
  dplyr::select(-X, -Date, -Fl_rate.Tri, -Fl_rate.Quar, 
                -Fl_rate.Sem,-Y_m, -Year,-Month,-Day,
                -seq.rain.val,-Semester,-Quarters,-Trimonthly,
                -Rainfall_Settefrati)
str(canneto0.5)
canneto1.5 <- read.csv("processed_data/canneto_rain1.5.csv") %>%
  dplyr::select(-X, -Date, -Fl_rate.Tri, -Fl_rate.Quar, 
                -Fl_rate.Sem,-Y_m, -Year,-Month,-Day,
                -seq.rain.val,-Semester,-Quarters,
                -Trimonthly,
                -Rainfall_Settefrati)

canneto3 <- read.csv("processed_data/canneto_rain3.csv") %>% 
  dplyr::select(-X, -Date, -Fl_rate.Tri, -Fl_rate.Quar, 
                -Fl_rate.Sem,-Y_m, -Year,-Month,-Day,
                -seq.rain.val,-Semester,-Trimonthly,
                -Quarters,-Rainfall_Settefrati)

canneto5 <- read.csv("processed_data/canneto_rain5.csv") %>% 
  dplyr::select(-X, -Date, -Fl_rate.Tri, -Fl_rate.Quar, 
                -Fl_rate.Sem,-Y_m, -Year,-Month,-Day,
                -seq.rain.val,-Semester,-Trimonthly,
                -Quarters,-Rainfall_Settefrati)

canneto7 <- read.csv("processed_data/canneto_rain7.csv") %>% 
  dplyr::select(-X, -Date, -Fl_rate.Tri, -Fl_rate.Quar, 
                -Fl_rate.Sem,-Y_m, -Year,-Month,-Day,
                -seq.rain.val,-Semester,-Quarters,
                -Trimonthly,
                -Rainfall_Settefrati)

#### starting with stepwise model selection (regr.) ####

## function ##

step.wisef <- function(x, DATA){
  set.seed(123)
  train.control <- trainControl(method = "cv", number = 10)
  step.model <- train(as.formula(paste(x,"~.")), data = DATA, 
                      method = "leapSeq", 
                      tuneGrid = data.frame(nvmax = 1:13),
                      trControl = train.control,
                      na.action = na.omit)
  return(step.model)
}

## running the function over the various datasets 

# temp removing trimonthly data 
canneto_orig.sw <- step.wisef("fl_rate.Ls", canneto_orig)
canneto_orig.sw$bestTune
coef(canneto_orig.sw$finalModel,3)

## canneto 0.5 mm

canneto0.5.sw <- step.wisef("fl_rate.Ls",canneto0.5)
canneto0.5.sw$bestTune
coef(canneto0.5.sw$finalModel, 4)

## canneto 1.5 mm

canneto1.5.sw <- step.wisef("fl_rate.Ls",canneto1.5)
canneto1.5.sw$bestTune
coef(canneto1.5.sw$finalModel,3) # rain is absent (will still put it in the model for comparison)


## canneto 3 mm

canneto3.sw <- step.wisef("fl_rate.Ls",canneto3)
canneto3.sw$bestTune
coef(canneto3.sw$finalModel,3)

## canneto 5mm 

canneto5.sw <- step.wisef("fl_rate.Ls",canneto5)
canneto5.sw$bestTune
coef(canneto5.sw$finalModel,3)

## canneto 7 mm

canneto7.sw <- step.wisef("fl_rate.Ls",canneto7)
canneto7.sw$bestTune
coef(canneto7.sw$finalModel,3)


#### selecting variables to model ####

str(canneto_orig)
canneto_orig_tm <- canneto_orig %>% 
  dplyr::select(Rainfall_Settefrati,Temperature_Settefrati,
                fl_rate.Ls, Autumn,snow.no)



#### model: gbm ####

#### canneto original ####

##testing and training split

set.seed(123)
canneto_orig_tm.split <- initial_split(canneto_orig_tm, prop = .7)
canneto_orig_tm.train <- training(canneto_orig_tm.split)
canneto_orig_tm.test <- testing(canneto_orig_tm.split)

canneto_orig_tm.fit <- gbm::gbm(fl_rate.Ls ~ .,
                        data = canneto_orig_tm,
                        verbose = T, 
                        shrinkage = 0.01,
                        interaction.depth = 3, 
                        n.minobsinnode = 5,
                        n.trees = 2000,
                        cv.folds = 12)
canneto_orig_tm.fit.perf <- gbm.perf(canneto_orig_tm.fit, method = "cv")

## make predictions 

canneto_orig_tm.fit.pred <- stats::predict(object = canneto_orig_tm.fit,
                               newdata = canneto_orig_tm.test,
                               n.trees = canneto_orig_tm.fit.perf)
canneto_orig_tm.fit.rmse <- Metrics::rmse(actual = canneto_orig_tm.test$fl_rate.Ls,
                           predicted = canneto_orig_tm.fit.pred)
print(canneto_orig_tm.fit.rmse) # 26.59

#### canneto at rain: 0.5 = 0 ####

canneto0.5.tm <- canneto0.5 %>% 
  dplyr::select(rain1, Temperature_Settefrati,
                fl_rate.Ls,Spring,Summer,snow.no)

## testing and training split 

canneto0.5.tm.split <- initial_split(canneto0.5.tm, prop = .7)
canneto0.5.tm.train <- training(canneto0.5.tm.split)
canneto0.5.tm.test <- testing(canneto0.5.tm.split)

canneto0.5.tm.fit <- gbm::gbm(fl_rate.Ls ~ .,
                                data = canneto0.5.tm,
                                verbose = T, 
                                shrinkage = 0.01,
                                interaction.depth = 3, 
                                n.minobsinnode = 5,
                                n.trees = 5000,
                                cv.folds = 10)
canneto0.5.tm.fit.perf <- gbm.perf(canneto0.5.tm.fit, method = "cv")

## make predictions 

canneto0.5.tm.fit.pred <- stats::predict(object = canneto0.5.tm.fit,
                                           newdata = canneto0.5.tm.test,
                                           n.trees = canneto0.5.tm.fit.perf)
canneto0.5.tm.fit.rmse <- Metrics::rmse(actual = canneto0.5.tm.test$fl_rate.Ls,
                                          predicted = canneto0.5.tm.fit.pred)
print(canneto0.5.tm.fit.rmse) # 26.23








