################################################################################
################################################################################
######## MODELLING: RANDOM FOREST                         ######################
################################################################################
################################################################################

#### libraries ####

library(tidyverse)
#install.packages("randomForest")
library(randomForest)
library(caret)
#install.packages("tree")
library(tree)
#install.packages("e1071")
library(e1071)
require(caTools)

#install.packages("gbm")
library(gbm) # boosting


#### let's start "easy": lupa water spring 

lupa <- read.csv("processed_data/LUPA_to_model.csv") %>%
  select(-X,-Date)

str(lupa)

# id col: X
# date: date character
# rainfall: feature variable, continuous numeric
# flow rate: target variable, continuous numeric 

head(lupa)
tail(lupa)

summary(lupa)

#### splitting dataset between train and test 

Train_Test <- sample(c("Train","Test"), nrow(lupa), replace = T,
                     prob = c(0.75,0.25))
lupa_train <- lupa[Train_Test =="Train",]
lupa_test <- lupa[Train_Test == "Test",]

lupa_train1 <- sample(1:nrow(lupa),nrow(lupa)/2)
lupa_test1 <- lupa[-lupa_train1,"imp_flow_rate"]


#### random forest model ####

rf_formula <- as.formula("imp_flow_rate ~.")
lupa_rf <- randomForest(rf_formula, 
                        data = lupa_train,
                        ntree = 500,
                        importance = T,
                        replace = T)
print(lupa_rf)
# MSE = 10.61
# % var expl = 96.31 %

plot(lupa_rf)
# shows error change with increasing number of trees 
# Y - corresp MSE 
# X - tree number 

plot(lupa_rf)

lupa_rf_margin <- as.numeric(predict(lupa_rf, newdata =lupa[-lupa_train1,]))

mean((lupa_rf_margin - lupa_test1)^2) # MSE = 7.85

importance(lupa_rf)
varImpPlot(lupa_rf)


#### comparing with regression tree ####

lupa_tree <- tree(imp_flow_rate ~., data = lupa, subset = lupa_train1)
summary(lupa_tree)


plot(lupa_tree)
text(lupa_tree, pretty = 0)

cv_lupa <- cv.tree(lupa_tree)
cv_lupa

plot(cv_lupa$size, cv_lupa$dev, type = "b")
# could go down to 9 leaves

prune_lupa <- prune.tree(lupa_tree, best = 9)
summary(prune_lupa)
cv.tree(lupa_tree,,prune.tree)$dev

plot(prune_lupa)
text(prune_lupa, pretty = 0)

tree_lupa_margin <- predict(lupa_tree, newdata = lupa[-lupa_train1,])
mean((tree_lupa_margin - lupa_test1)^2) # MSE = 99.82
## worse than random forest mse 



