################################################################################
################################################################################
######################### GRADIENT BOOST MACHINE  ##############################
################################################################################
################################################################################

## libraries 

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

## reading files 

auser <- read.csv("processed_data/AUSER_to_model.csv") 
# %>%
.# dplyr::select(-X)
  #,-Rainfall_Velletri, -Rainfall_Monteporzio,
  #              -Pozzo_1:-Pozzo_9) %>% 
#  spread(key = imp, value = depth_to_gw.m) # in regression date doesnt really matter 
str(auser)

## prepping objects per target ##

meteo <- c("rain","temp")

pozzo1 <- doganella %>% dplyr::select(., contains("1"), contains(meteo))
pozzo2 <- doganella %>% dplyr::select(., contains("2"), contains(meteo))
pozzo3 <- doganella %>% dplyr::select(., contains("3"), contains(meteo))
pozzo4 <- doganella %>% dplyr::select(., contains("4"), contains(meteo))
pozzo5 <- doganella %>% dplyr::select(., contains("5"), contains(meteo))
pozzo6 <- doganella %>% dplyr::select(., contains("6"), contains(meteo))
pozzo7 <- doganella %>% dplyr::select(., contains("7"), contains(meteo))
pozzo8 <- doganella %>% dplyr::select(., contains("8"), contains(meteo))
pozzo9 <- doganella %>% dplyr::select(., contains("9"), contains(meteo))

#### computing stepwise regression for variable selection ####
# creating function
step.wisef <- function(x, DATA){
  set.seed(123)
  train.control <- trainControl(method = "cv", number = 10)
  step.model <- train(as.formula(paste(x,"~.")), data = DATA, 
                      method = "leapSeq", 
                      tuneGrid = data.frame(nvmax = 1:5),
                      trControl = train.control,
                      na.action = na.omit)
  return(step.model)
}

#### pozzo 1 ####

pozzo1_sw <- step.wisef("imp1", pozzo1)
pozzo1_sw$bestTune 
pozzo1_sw$finalModel
coef(pozzo1_sw$finalModel, 5)

### let's stick to three variables (?) ###
## question: model chooses the two temperatures even if they're highly correlated to one another...?
# why? 

#### testing and training split ####

set.seed(123)
pozzo1.split <- initial_split(pozzo1, prop = .7)
pozzo1.train <- training(pozzo1.split)
pozzo1.test <- testing(pozzo1.split)

pozzo1_fit1 <- gbm::gbm(imp1 ~ .,
                        data = pozzo1,
                        verbose = T, 
                        shrinkage = 0.01,
                        interaction.depth = 3, 
                        n.minobsinnode = 5,
                        n.trees = 5000,
                        cv.folds = 10)
perf_gbm1 <- gbm.perf(pozzo1_fit1, method = "cv")

## make predictions 

pozzo1_pred1 <- stats::predict(object = pozzo1_fit1,
                               newdata = pozzo1.test,
                               n.trees = perf_gbm1)
rmse_fit1 <- Metrics::rmse(actual = pozzo1.test$imp1,
                           predicted = pozzo1_pred1)
print(rmse_fit1) # 9.313319
#(higher error than when keeping all variables)

#plot - rain velletri
gbm::plot.gbm(pozzo1_fit1, i.var = 1)
# plot - temp monteporzio
plot.gbm(pozzo1_fit1, i.var = 2)
# plot - temp velletri
plot.gbm(pozzo1_fit1, i.var = 3)

## interactions of two features on the variable 

gbm::plot.gbm(pozzo1_fit1, i.var = c(1,3)) # vol-rain
plot.gbm(pozzo1_fit1, i.var = c(1,2)) # vol-temp
plot.gbm(pozzo1_fit1, i.var = c(2,3)) # temp-rain

### impact of different features on predicting depth to gw 

# summarise model 

pozzo1_effects <- tibble::as_tibble(gbm::summary.gbm(pozzo1_fit1,
                                                     plotit = F))
pozzo1_effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
pozzo1_effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

pozzo1.test$predicted <- as.integer(predict(pozzo1_fit1,
                                            newdata = pozzo1.test,
                                            n.trees = perf_gbm1))

# plot predicted vs actual

ggplot(pozzo1.test) +
  geom_point(aes(x = predicted,
                 y = imp1,
                 color = predicted - imp1),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()

#### pozzo 2 #### 

## stepwise 

pozzo2_sw <- step.wisef("imp2", pozzo2)
pozzo2_sw$bestTune # 5
coef(pozzo2_sw$finalModel, 3)

## train and test  ##### how to automate this??? 
#gmb.f(pozzo2)...

# split 
set.seed(123)
p2.split <- initial_split(pozzo2, prop = .7)
p2.train <- training(p2.split)
p2.test <- testing(p2.split)

p2.fit1 <- gbm(imp2 ~ .,
               data = pozzo2,
               verbose = T, 
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 10)

p2.fit1_perf <- gbm.perf(p2.fit1, method = "cv")
p2.fit1_perf

## make predictions 

p2_pred1 <- stats::predict(object = p2.fit1,
                           newdata = p2.test,
                           n.trees = p2.fit1_perf)
p2_rmse <- Metrics::rmse(actual = p2.test$imp2,
                         predicted = p2_pred1)
print(p2_rmse) # 0.98

gbm::plot.gbm(p2.fit1, i.var = 1)

plot.gbm(p2.fit1, i.var = 2)

plot.gbm(p2.fit1, i.var = 3)

## interactions of two features on the variable 

gbm::plot.gbm(p2.fit1, i.var = c(1,3))
plot.gbm(p2.fit1, i.var = c(1,2))
plot.gbm(p2.fit1, i.var = c(2,3))

### impact of different features on predicting depth to gw 

# summarise model 

p2.effects <- tibble::as_tibble(gbm::summary.gbm(p2.fit1,
                                                 plotit = F))
p2.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
p2.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p2.test$predicted <- as.integer(predict(p2.fit1,
                                        newdata = p2.test,
                                        n.trees = p2.fit1_perf))

# plot predicted vs actual

ggplot(p2.test) +
  geom_point(aes(x = predicted,
                 y = imp2,
                 color = predicted - imp2),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()


#### pozzo 3 ####

# stepwise

pozzo3_sw <- step.wisef("imp3",pozzo3)
pozzo3_sw$bestTune # 5
coef(pozzo3_sw$finalModel, 4)


# split 
set.seed(123)
p3.split <- initial_split(pozzo3, prop = .7)
p3.train <- training(p3.split)
p3.test <- testing(p3.split)

p3.fit1 <- gbm(imp3 ~ .,
               data = pozzo3,
               verbose = T, 
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 10)

p3.fit1_perf <- gbm.perf(p3.fit1, method = "cv")

## make predictions 

p3_pred1 <- stats::predict(object = p3.fit1,
                           newdata = p3.test,
                           n.trees = p3.fit1_perf)
p3_rmse <- Metrics::rmse(actual = p3.test$imp3,
                         predicted = p3_pred1)
print(p3_rmse) # 1.89

gbm::plot.gbm(p3.fit1, i.var = 1)

plot.gbm(p3.fit1, i.var = 2)

plot.gbm(p3.fit1, i.var = 3)

# ecc...

## interactions of two features on the variable 

gbm::plot.gbm(p3.fit1, i.var = c(1,3))
plot.gbm(p3.fit1, i.var = c(1,2))
plot.gbm(p3.fit1, i.var = c(2,3))

### impact of different features on predicting depth to gw 

# summarise model 

p3.effects <- tibble::as_tibble(gbm::summary.gbm(p3.fit1,
                                                 plotit = F))
p3.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
p3.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p3.test$predicted <- as.integer(predict(p3.fit1,
                                        newdata = p3.test,
                                        n.trees = p3.fit1_perf))

# plot predicted vs actual

ggplot(p3.test) +
  geom_point(aes(x = predicted,
                 y = imp3,
                 color = predicted - imp3),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()



#### pozzo 4 ####

# stepwise
pozzo4_sw <- step.wisef("imp4", pozzo4)
pozzo4_sw$bestTune # 5
coef(pozzo4_sw$finalModel, 3)

# split, train and test
set.seed(123)
p4.split <- initial_split(pozzo4, prop = .7)
p4.train <- training(p4.split)
p4.test <- testing(p4.split)

p4.fit1 <- gbm(imp4 ~ .,
               data = pozzo4,
               verbose = T, 
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 10)

p4.fit1_perf <- gbm.perf(p4.fit1, method = "cv")

## make predictions 

p4_pred1 <- stats::predict(object = p4.fit1,
                           newdata = p4.test,
                           n.trees = p4.fit1_perf)
p4_rmse <- Metrics::rmse(actual = p4.test$imp4,
                         predicted = p4_pred1)
print(p4_rmse) # 0.82

gbm::plot.gbm(p4.fit1, i.var = 1)

plot.gbm(p4.fit1, i.var = 2)

plot.gbm(p4.fit1, i.var = 3)

# ecc...

## interactions of two features on the variable 

gbm::plot.gbm(p4.fit1, i.var = c(1,3))
plot.gbm(p4.fit1, i.var = c(1,2))
plot.gbm(p4.fit1, i.var = c(2,3))

### impact of different features on predicting depth to gw 

# summarise model 

p4.effects <- tibble::as_tibble(gbm::summary.gbm(p4.fit1,
                                                 plotit = F))
p4.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
p4.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p4.test$predicted <- as.integer(predict(p4.fit1,
                                        newdata = p4.test,
                                        n.trees = p4.fit1_perf))

# plot predicted vs actual

ggplot(p4.test) +
  geom_point(aes(x = predicted,
                 y = imp4,
                 color = predicted - imp4),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()



#### pozzo 5 ####

#stepwise
pozzo5_sw <- step.wisef("imp5", pozzo5)
pozzo5_sw$bestTune # 5
coef(pozzo5_sw$finalModel, 3)

# split 
set.seed(123)
p5.split <- initial_split(pozzo5, prop = .7)
p5.train <- training(p5.split)
p5.test <- testing(p5.split)

p5.fit1 <- gbm(imp5 ~ .,
               data = pozzo5,
               verbose = T, 
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 10)

p5.fit1_perf <- gbm.perf(p5.fit1, method = "cv")

## make predictions 

p5_pred1 <- stats::predict(object = p5.fit1,
                           newdata = p5.test,
                           n.trees = p5.fit1_perf)
p5_rmse <- Metrics::rmse(actual = p5.test$imp5,
                         predicted = p5_pred1)
print(p5_rmse) # 2.36

gbm::plot.gbm(p5.fit1, i.var = 1)

# ecc...

## interactions of two features on the variable 

gbm::plot.gbm(p5.fit1, i.var = c(1,3))
plot.gbm(p5.fit1, i.var = c(1,2))
plot.gbm(p5.fit1, i.var = c(2,3))

### impact of different features on predicting depth to gw 

# summarise model 

p5.effects <- tibble::as_tibble(gbm::summary.gbm(p5.fit1,
                                                 plotit = F))
p5.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
p5.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p5.test$predicted <- as.integer(predict(p5.fit1,
                                        newdata = p5.test,
                                        n.trees = p5.fit1_perf))

# plot predicted vs actual

ggplot(p5.test) +
  geom_point(aes(x = predicted,
                 y = imp5,
                 color = predicted - imp5),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()


#### pozzo 6 ####

#stepwise
pozzo6_sw <- step.wisef("imp6", pozzo6)
pozzo6_sw$bestTune # 5
coef(pozzo6_sw$finalModel, 2)

# split 
set.seed(123)
p6.split <- initial_split(pozzo6,prop = .7)
p6.train <- training(p6.split)
p6.test <- testing(p6.split)

p6.fit1 <- gbm(imp6 ~ .,
               data = pozzo6,
               verbose = T, 
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 10)

p6.fit1_perf <- gbm.perf(p6.fit1, method = "cv")

## make predictions 

p6_pred1 <- stats::predict(object = p6.fit1,
                           newdata = p6.test,
                           n.trees = p6.fit1_perf)
p6_rmse <- Metrics::rmse(actual = p6.test$imp6,
                         predicted = p6_pred1)
print(p6_rmse) # 0.93

gbm::plot.gbm(p6.fit1, i.var = 1)

plot.gbm(p6.fit1, i.var = 2)

plot.gbm(p6.fit1, i.var = 3)

# ecc...

## interactions of two features on the variable 

gbm::plot.gbm(p6.fit1, i.var = c(1,3))
plot.gbm(p6.fit1, i.var = c(1,2))
plot.gbm(p6.fit1, i.var = c(2,3))

### impact of different features on predicting depth to gw 

# summarise model 

p6.effects <- tibble::as_tibble(gbm::summary.gbm(p6.fit1,
                                                 plotit = F))
p6.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
p6.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p6.test$predicted <- as.integer(predict(p6.fit1,
                                        newdata = p6.test,
                                        n.trees = p6.fit1_perf))

# plot predicted vs actual

ggplot(p6.test) +
  geom_point(aes(x = predicted,
                 y = imp6,
                 color = predicted - imp6),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()


#### pozzo 7 ####

#stepwise
pozzo7_sw <- step.wisef("imp7", pozzo7)
pozzo7_sw$bestTune # 5
coef(pozzo7_sw$finalModel, 3)

# split 
set.seed(123)
p7.split <- initial_split(pozzo7, prop = .7)
p7.train <- training(p7.split)
p7.test <- testing(p7.split)

p7.fit1 <- gbm(imp7 ~ .,
               data = pozzo7,
               verbose = T, 
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 10)

p7.fit1_perf <- gbm.perf(p7.fit1, method = "cv")

## make predictions 

p7_pred1 <- stats::predict(object = p7.fit1,
                           newdata = p7.test,
                           n.trees = p7.fit1_perf)
p7_rmse <- Metrics::rmse(actual = p7.test$imp7,
                         predicted = p3_pred1)
print(p7_rmse) # 16.32

gbm::plot.gbm(p7.fit1, i.var = 1)

plot.gbm(p7.fit1, i.var = 2)

plot.gbm(p7.fit1, i.var = 3)

# ecc...

## interactions of two features on the variable 

gbm::plot.gbm(p7.fit1, i.var = c(1,3))
plot.gbm(p7.fit1, i.var = c(1,2))
plot.gbm(p7.fit1, i.var = c(2,3))

### impact of different features on predicting depth to gw 

# summarise model 

p7.effects <- tibble::as_tibble(gbm::summary.gbm(p7.fit1,
                                                 plotit = F))
p7.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
p7.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p7.test$predicted <- as.integer(predict(p7.fit1,
                                        newdata = p7.test,
                                        n.trees = p7.fit1_perf))

# plot predicted vs actual

ggplot(p7.test) +
  geom_point(aes(x = predicted,
                 y = imp7,
                 color = predicted - imp7),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()


#### pozzo 8 #### 

#stepwise
pozzo8_sw <- step.wisef("imp8", pozzo8)
pozzo8_sw$bestTune # 5
coef(pozzo8_sw$finalModel, 3)

# split 
set.seed(123)
p8.split <- initial_split(pozzo8, prop = .7)
p8.train <- training(p8.split)
p8.test <- testing(p8.split)

p8.fit1 <- gbm(imp8 ~ .,
               data = pozzo8,
               verbose = T, 
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 10)

p8.fit1_perf <- gbm.perf(p8.fit1, method = "cv")

## make predictions 

p8_pred1 <- stats::predict(object = p8.fit1,
                           newdata = p8.test,
                           n.trees = p8.fit1_perf)
p8_rmse <- Metrics::rmse(actual = p8.test$imp8,
                         predicted = p8_pred1)
print(p8_rmse) # 0.87

gbm::plot.gbm(p8.fit1, i.var = 1)

plot.gbm(p8.fit1, i.var = 2)

plot.gbm(p8.fit1, i.var = 3)

# ecc...

## interactions of two features on the variable 

gbm::plot.gbm(p8.fit1, i.var = c(1,3))
plot.gbm(p8.fit1, i.var = c(1,2))
plot.gbm(p8.fit1, i.var = c(2,3))

### impact of different features on predicting depth to gw 

# summarise model 

p8.effects <- tibble::as_tibble(gbm::summary.gbm(p8.fit1,
                                                 plotit = F))
p8.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
p8.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p8.test$predicted <- as.integer(predict(p8.fit1,
                                        newdata = p8.test,
                                        n.trees = p8.fit1_perf))

# plot predicted vs actual

ggplot(p8.test) +
  geom_point(aes(x = predicted,
                 y = imp8,
                 color = predicted - imp8),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()


#### pozzo 9 #### 

#stepwise
pozzo9_sw <- step.wisef("imp9", pozzo9)
pozzo9_sw$bestTune # 1!
coef(pozzo9_sw$finalModel, 1)
# others "subscript out of bounds", as best model is already 1?

# split 
set.seed(123)
p9.split <- initial_split(pozzo9, prop = .7)
p9.train <- training(p9.split)
p9.test <- testing(p9.split)

p9.fit1 <- gbm(imp9 ~ imp_rain_velletri, # in this case just one var...
               data = pozzo9,
               verbose = 1,
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 1) # can't go larger or i get error:
# >1 nodes produced errors; first error: incorrect number of dimensions

p9.fit1_perf <- gbm.perf(p9.fit1)

## make predictions 

p9_pred1 <- stats::predict(object = p9.fit1,
                           newdata = p9.test,
                           n.trees = p9.fit1_perf)
p9_rmse <- Metrics::rmse(actual = p9.test$imp9,
                         predicted = p9_pred1)
print(p9_rmse) # 3.88

gbm::plot.gbm(p9.fit1, i.var = 1) # only the one var

## interactions of two features on the variable not possible

### impact of different features on predicting depth to gw 

# summarise model 

p9.effects <- tibble::as_tibble(gbm::summary.gbm(p9.fit1,
                                                 plotit = F))
p9.effects %>% utils::head() # 100 ... of course it's the only one 
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p9.test$predicted <- as.integer(predict(p9.fit1,
                                        newdata = p9.test,
                                        n.trees = p9.fit1_perf))

# plot predicted vs actual

ggplot(p9.test) +
  geom_point(aes(x = predicted,
                 y = imp9,
                 color = predicted - imp9),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()


#### XGBOOST ####

#install.packages("xgboost")
library(xgboost)
library(lubridate)
library(tibble)

View(pozzo1)
max(pozzo1$Date)
str(pozzo1)

pozzo1_ext <- pozzo1 %>%
  mutate(Date = as_date(Date)) %>%
  bind_rows(tibble(Date = seq(start = as_date("2020-07-01"),
                              from = as_date("2020-07-01"),
                              by = "day", length.out = 31),
                   imp1 = rep(NA, 31)))
tail(pozzo1_ext)

pozzo1_xgb <- pozzo1_ext %>%
  mutate(months = lubridate::month(Date),
         years = lubridate::year(Date),
         days = lubridate::day(Date))

str(pozzo1_xgb)

## split - train and prediction sets

p1.xgb.train <- pozzo1_xgb[1:nrow(pozzo1),]
p1.xgb.pred <- pozzo1_xgb[(nrow(pozzo1)+1):nrow(pozzo1_xgb),]

p1.x_train <- xgboost::xgb.DMatrix(as.matrix(p1.xgb.train %>% 
                                               dplyr::select(months, years)))
p1.x_pred <- xgboost::xgb.DMatrix(as.matrix(p1.xgb.pred %>% 
                                              dplyr::select(months, years)))
p1.y_train <- p1.xgb.train$imp1


p1.xgb_trcontrol <- caret::trainControl(
  method ="cv",
  number = 5,
  
)


#### canneto ####

canneto <- read.csv("processed_data/MADONNA_DI_CANNETO_to_model.csv")
canneto0.5 <- read.csv("processed_data/canneto_rain0.5.csv")
canneto1.5 <- read.csv("processed_data/canneto_rain1.5.csv")
canneto3 <- read.csv("processed_data/canneto_rain3.csv")
canneto5 <- read.csv("processed_data/canneto_rain5.csv")
canneto7 <- read.csv("processed_data/canneto_rain7.csv")
# uploaded new datasets with new features 



