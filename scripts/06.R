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

## reading files 

doganella <- read.csv("processed_data/DOGANELLA_to_model.csv") %>%
  select(-X,-Rainfall_Velletri, -Rainfall_Monteporzio,
         -Pozzo_1:-Pozzo_9,-imp_rain_monteporzio,
         -Temperature_Monteporzio) %>% 
  spread(key = imp, value = depth_to_gw.m) # in regression date doesnt really matter 
str(doganella)


cor(pozzo1$Temperature_Monteporzio, pozzo1$Temperature_Velletri) # 0.98
cor(pozzo1$imp_rain_monteporzio, pozzo1$imp_rain_monteporzio)  #1

## prepping objects per target ##

pozzo1 <- doganella %>% select(., contains("1"), contains("velletri"))
head(pozzo1)

pozzo2 <- doganella %>% select(., contains("2"), contains("velletri"))
pozzo3 <- doganella %>% select(., contains("3"), contains("velletri"))
pozzo4 <- doganella %>% select(., contains("4"), contains("velletri"))
pozzo5 <- doganella %>% select(., contains("5"), contains("velletri"))
pozzo6 <- doganella %>% select(., contains("6"), contains("velletri"))
pozzo7 <- doganella %>% select(., contains("7"), contains("velletri"))
pozzo8 <- doganella %>% select(., contains("8"), contains("velletri"))
pozzo9 <- doganella %>% select(., contains("9"), contains("velletri"))

#### pozzo 1 ####

# testing and training split 

set.seed(123)
pozzo1.split <- initial_split(pozzo1, prop = .7)
pozzo1.train <- training(pozzo1.split)
pozzo1.test <- testing(pozzo1.split)

pozzo1_fit1 <- gbm::gbm(imp1 ~.,
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
print(rmse_fit1) # 7.735155


gbm::plot.gbm(pozzo1_fit1, i.var = 2)


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

doganella_test$predicted <- as.integer(predict(pozzo1_fit1,
                                               newdata = doganella_test,
                                               n.trees = perf_gbm1))

# plot predicted vs actual

ggplot(doganella_test) +
  geom_point(aes(x = predicted,
                y = imp1,
                color = predicted - imp1),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()

#### pozzo 2 #### 















