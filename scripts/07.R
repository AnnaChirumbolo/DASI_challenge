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
  dplyr::select(-X, -Date,-Fl_rate.Tri, -Fl_rate.Quar, 
                -Fl_rate.Sem,-Y_m, -Year,-Month,-Day,
                -Trimonthly,
                -Semester, -Quarters)
str(canneto_orig)

#canneto_orig[,4:17] <- factor(canneto_orig[,4:17])
#str(canneto_orig)

canneto0.5 <- read.csv("processed_data/canneto_rain0.5.csv")%>%
  dplyr::select(-X, -Date,-Fl_rate.Tri, -Fl_rate.Quar, 
                -Fl_rate.Sem,-Y_m, -Year,-Month,-Day,
                -Trimonthly,
                -Semester, -Quarters,
                -Rainfall_Settefrati)
str(canneto0.5)

canneto1.5 <- read.csv("processed_data/canneto_rain1.5.csv") %>%
  dplyr::select(-X, -Date, -Fl_rate.Tri, -Fl_rate.Quar, 
                -Fl_rate.Sem,-Y_m, -Year,-Month,-Day,
                -Trimonthly,
                -Semester, -Quarters,
                -Rainfall_Settefrati)

canneto3 <- read.csv("processed_data/canneto_rain3.csv") %>% 
  dplyr::select(-X, -Date,-Fl_rate.Tri, -Fl_rate.Quar, 
                -Fl_rate.Sem,-Y_m, -Year,-Month,-Day,
                -Trimonthly,
                -Semester, -Quarters,-Rainfall_Settefrati)

canneto5 <- read.csv("processed_data/canneto_rain5.csv") %>% 
  dplyr::select(-X, -Date,-Fl_rate.Tri, -Fl_rate.Quar, 
                -Fl_rate.Sem,-Y_m, -Year,-Month,-Day,
                -Trimonthly,
                -Semester, -Quarters,-Rainfall_Settefrati)

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
coef(canneto_orig.sw$finalModel,6)

## canneto 0.5 mm

canneto0.5.sw <- step.wisef("fl_rate.Ls",canneto0.5)
canneto0.5.sw$bestTune
coef(canneto0.5.sw$finalModel, 6)

## canneto 1.5 mm

canneto1.5.sw <- step.wisef("fl_rate.Ls",canneto1.5)
canneto1.5.sw$bestTune
coef(canneto1.5.sw$finalModel,5) # rain is absent (will still put it in the model for comparison)


## canneto 3 mm

canneto3.sw <- step.wisef("fl_rate.Ls",canneto3)
canneto3.sw$bestTune
coef(canneto3.sw$finalModel,11)

## canneto 5mm 

canneto5.sw <- step.wisef("fl_rate.Ls",canneto5)
canneto5.sw$bestTune
coef(canneto5.sw$finalModel,8)


#### selecting variables to model ####

str(canneto_orig)
canneto_orig_tm <- canneto_orig %>% 
  dplyr::select(Rainfall_Settefrati,Temperature_Settefrati,
                Spring,Summer,lag1,lag7,
                fl_rate.Ls, Autumn,snow.no)


#### correlation matrix ####
library(corrplot)


canneto_orig.cor <- canneto_orig %>% 
  cor(., use = "complete.obs") %>%
  corrplot(., method = "circle",
           tl.col = "black",tl.srt = 35, 
           tl.cex = 0.72)

canneto0.5.cor <- canneto0.5 %>% 
  cor(., use = "complete.obs") %>% 
  corrplot(., method = "circle")


#### model: gbm ####

#### canneto original ####

##testing and training split

set.seed(123)
canneto_orig_tm.split <- initial_split(canneto_orig, prop = .7)
canneto_orig_tm.train <- training(canneto_orig_tm.split)
canneto_orig_tm.test <- testing(canneto_orig_tm.split)

canneto_orig_tm.fit <- gbm::gbm(fl_rate.Ls ~ .,
                        data = canneto_orig,
                        verbose = T, 
                        shrinkage = 0.01,
                        interaction.depth = 3, 
                        n.minobsinnode = 5,
                        n.trees = 1000,
                        cv.folds = 12)
canneto_orig_tm.fit.perf <- gbm.perf(canneto_orig_tm.fit, method = "cv")

## make predictions 

canneto_orig_tm.fit.pred <- stats::predict(object = canneto_orig_tm.fit,
                               newdata = canneto_orig_tm.test,
                               n.trees = canneto_orig_tm.fit.perf)
canneto_orig_tm.fit.rmse <- Metrics::rmse(actual = canneto_orig_tm.test$fl_rate.Ls,
                           predicted = canneto_orig_tm.fit.pred)
print(canneto_orig_tm.fit.rmse) # 26.59


# summarise model 

p9.effects <- tibble::as_tibble(gbm::summary.gbm(canneto_orig_tm.fit,
                                                 plotit = F))
p9.effects %>% utils::head() # 100 ... of course it's the only one 
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

canneto_orig_tm.test$predicted <- as.integer(predict(canneto_orig_tm.fit,
                                        newdata = canneto_orig_tm.test,
                                        n.trees = canneto_orig_tm.fit.perf))

# plot predicted vs actual

ggplot(canneto_orig_tm.test) +
  geom_point(aes(x = predicted,
                 y = fl_rate.Ls,
                 color = predicted - fl_rate.Ls),
             alpha = .7, size = 2) +
  theme_classic()


## plotting top 6 features

p9.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2") +
  theme_classic()

## re-doing the model with top 6 features 

canneto_top6 <- canneto_orig %>% 
  dplyr::select(fl_rate.Ls, Temperature_Settefrati, 
                lag7,Rainfall_Settefrati,Spring,lag1,
                Summer)

set.seed(123)
canneto_orig_tm.split <- initial_split(canneto_top6, prop = .7)
canneto_orig_tm.train <- training(canneto_orig_tm.split)
canneto_orig_tm.test <- testing(canneto_orig_tm.split)

canneto_orig_tm.fit <- gbm::gbm(fl_rate.Ls ~ .,
                                data = canneto_top6,
                                verbose = T, 
                                shrinkage = 0.01,
                                interaction.depth = 3, 
                                n.minobsinnode = 5,
                                n.trees = 500,
                                cv.folds = 12)
canneto_orig_tm.fit.perf <- gbm.perf(canneto_orig_tm.fit, method = "cv")

## make predictions 


canneto_orig_tm.fit.pred <- stats::predict(object = canneto_orig_tm.fit,
                                           newdata = canneto_orig_tm.test,
                                           n.trees = canneto_orig_tm.fit.perf)
canneto_orig_tm.fit.rmse <- Metrics::rmse(actual = canneto_orig_tm.test$fl_rate.Ls,
                                          predicted = canneto_orig_tm.fit.pred)
print(canneto_orig_tm.fit.rmse) # 29 still larger error with only those vals... 


### comparing with values suggested by stepwise ###


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
                                n.trees = 500,
                                cv.folds = 12)
canneto_orig_tm.fit.perf <- gbm.perf(canneto_orig_tm.fit, method = "cv")

## make predictions 

canneto_orig_tm.fit.pred <- stats::predict(object = canneto_orig_tm.fit,
                                           newdata = canneto_orig_tm.test,
                                           n.trees = canneto_orig_tm.fit.perf)
canneto_orig_tm.fit.rmse <- Metrics::rmse(actual = canneto_orig_tm.test$fl_rate.Ls,
                                          predicted = canneto_orig_tm.fit.pred)
print(canneto_orig_tm.fit.rmse) # 28.83


# summarise model 

p9.effects <- tibble::as_tibble(gbm::summary.gbm(canneto_orig_tm.fit,
                                                 plotit = F))
p9.effects %>% utils::head() # 100 ... of course it's the only one 
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

canneto_orig_tm.test$predicted <- as.integer(predict(canneto_orig_tm.fit,
                                                     newdata = canneto_orig_tm.test,
                                                     n.trees = canneto_orig_tm.fit.perf))

# plot predicted vs actual

ggplot(canneto_orig_tm.test) +
  geom_point(aes(x = predicted,
                 y = fl_rate.Ls,
                 color = predicted - fl_rate.Ls),
             alpha = .7, size = 2) +
  theme_classic()


## plotting top 6 features

p9.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2") +
  theme_classic()


############-------------------------------------


#### canneto at rain: 0.5 = 0 ####

## testing and training split 
str(canneto0.5)
canneto0.5.tm.split <- initial_split(canneto0.5, prop = .7)
canneto0.5.tm.train <- training(canneto0.5.tm.split)
canneto0.5.tm.test <- testing(canneto0.5.tm.split)

canneto0.5.tm.fit <- gbm::gbm(fl_rate.Ls ~ .,
                                data = canneto0.5,
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
print(canneto0.5.tm.fit.rmse) # 28



# summarise model 

p9.effects <- tibble::as_tibble(gbm::summary.gbm(canneto_orig_tm.fit,
                                                 plotit = F))
p9.effects %>% utils::head() # 100 ... of course it's the only one 
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

canneto_orig_tm.test$predicted <- as.integer(predict(canneto_orig_tm.fit,
                                                     newdata = canneto_orig_tm.test,
                                                     n.trees = canneto_orig_tm.fit.perf))

# plot predicted vs actual

ggplot(canneto_orig_tm.test) +
  geom_point(aes(x = predicted,
                 y = fl_rate.Ls,
                 color = predicted - fl_rate.Ls),
             alpha = .7, size = 2) +
  theme_classic() ### same results as 1.5mm

### canneto 3mm

## testing and training split 

canneto3.tm.split <- initial_split(canneto3, prop = .7)
canneto3.tm.train <- training(canneto3.tm.split)
canneto3.tm.test <- testing(canneto3.tm.split)

canneto3.tm.fit <- gbm::gbm(fl_rate.Ls ~ .,
                              data = canneto3,
                              verbose = T, 
                              shrinkage = 0.01,
                              interaction.depth = 3, 
                              n.minobsinnode = 5,
                              n.trees = 5000,
                              cv.folds = 10)
canneto3.tm.fit.perf <- gbm.perf(canneto3.tm.fit, method = "cv")

## make predictions 

canneto3.tm.fit.pred <- stats::predict(object = canneto3.tm.fit,
                                         newdata = canneto3.tm.test,
                                         n.trees = canneto3.tm.fit.perf)
canneto3.tm.fit.rmse <- Metrics::rmse(actual = canneto3.tm.test$fl_rate.Ls,
                                        predicted = canneto3.tm.fit.pred)
print(canneto3.tm.fit.rmse) # 28.55

## greater error so far 

### canneto 5mm 

canneto5.tm.split <- initial_split(canneto5, prop = .7)
canneto5.tm.train <- training(canneto5.tm.split)
canneto5.tm.test <- testing(canneto5.tm.split)

canneto5.tm.fit <- gbm::gbm(fl_rate.Ls ~ .,
                            data = canneto5,
                            verbose = T, 
                            shrinkage = 0.01,
                            interaction.depth = 3, 
                            n.minobsinnode = 5,
                            n.trees = 5000,
                            cv.folds = 10)
canneto5.tm.fit.perf <- gbm.perf(canneto5.tm.fit, method = "cv")

## make predictions 

canneto5.tm.fit.pred <- stats::predict(object = canneto5.tm.fit,
                                       newdata = canneto5.tm.test,
                                       n.trees = canneto5.tm.fit.perf)
canneto5.tm.fit.rmse <- Metrics::rmse(actual = canneto5.tm.test$fl_rate.Ls,
                                      predicted = canneto5.tm.fit.pred)
print(canneto5.tm.fit.rmse) # 29.266

## canneto_orig still best one 

