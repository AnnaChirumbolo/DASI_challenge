################################################################################
################################################################################
######## 03 - MODELLING: RANDOM FOREST                    ######################
################################################################################
################################################################################

#### libraries ####

#install.packages("h2o")
library(h2o)
library(tidyverse)
#install.packages("randomForest")
library(randomForest)
library(caret)
#install.packages("tree")
library(tree)
#install.packages("e1071")
library(e1071)
require(caTools)
#install.packages("tsibble")
library(tsibble)
library(lubridate)
#install.packages("gbm")
library(gbm) # boosting
library(forecast)

?`h2o-package`


#### let's start "easy": luco 

luco <- read.csv("processed_data/luco_featured.csv") %>%
  select(-X)

luco_base_val <- luco %>% 
  group_by(Year) %>% 
  summarise(mean_depth = mean(imp1))

str(luco)

summary(luco_base_val)

# id col: X
# date: date character
# rainfall: feature variable, continuous numeric
# flow rate: target variable, continuous numeric 

head(luco)
tail(luco)

summary(luco)

#### splitting dataset between train and test

lucoRF1 <- luco %>%
  select(-Pozzo_1,-Pozzo_3, -Volume_Pozzo_3, -Date, -Day, -Month, -Year, -imp3)

Train_Test <- sample(c("Train","Test"), nrow(lucoRF1), replace = T,
                     prob = c(0.75,0.25))
luco_train <- lucoRF1[Train_Test =="Train",]
luco_test <- lucoRF1[Train_Test == "Test",]

luco_train1 <- sample(1:nrow(lucoRF1),nrow(lucoRF1)/2)
luco_test1 <- luco[-luco_train1,"imp1"]

summary(luco_train)

#### random forest model ####

rf_formula <- as.formula("imp1 ~.")
luco_rf <- randomForest(rf_formula, 
                        data = luco_train,
                        ntree = 200,
                        importance = T,
                        replace = T)
print(luco_rf)
# MSE = 10.61
# % var expl = 96.31 %

plot(luco_rf)
# shows error change with increasing number of trees 
# Y - corresp MSE 
# X - tree number 

luco_rf_margin <- predict(luco_rf, newdata =luco[-luco_train1,])

mean((luco_rf_margin - luco_test1)^2) # MSE = 7.85 / 8.51
sqrt(mean((luco_rf_margin - luco_test1)^2)) # MSE = 7.85 / 8.51


#var(luco$imp_flow_rate)

importance(luco_rf)
varImpPlot(luco_rf)

#### 


#### comparing with regression tree ####

luco_tree <- tree(imp1 ~., data = lucoRF1, subset = luco_train1)
summary(luco_tree)


plot(luco_tree)
text(luco_tree, pretty = 0)

cv_luco <- cv.tree(luco_tree)
cv_luco

plot(cv_luco$size, cv_luco$dev, type = "b")
# could go down to 9 leaves

prune_luco <- prune.tree(luco_tree, best = 9)
summary(prune_luco)
cv.tree(luco_tree,,prune.tree)$dev

plot(prune_luco)
text(prune_luco, pretty = 0)

tree_luco_margin <- predict(luco_tree, newdata = luco[-luco_train1,])
mean((tree_luco_margin - luco_test1)^2) # MSE = 99.82
## worse than random forest mse 