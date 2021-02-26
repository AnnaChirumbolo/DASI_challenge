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

auser <- read.csv("processed_data/AUSER_to_model.csv")%>%
dplyr::select(-X, -Date,-Temperature_Ponte_a_Moriano, -DIEC, -PAG,-CoS, -LT2,-SAL)
# in regression date doesnt really matter 
#Temperature_Ponte_a_Moriano ho dovuto cancellarla per un problema sul sensore dal 2017, con tutti dati uguali a 0
str(auser)

(
  auser$Season<-factor(auser$Season, 
                        levels=c("Winter","Spring", "Summer", "Autumn"))
)
#### Correlation Matrix ####
df <- auser
df$Date <- NULL
ggcorr(df, label = TRUE, label_round = 2, hjust = 1, size = 4, layout.exp = 4, label_size = 3)
rm(df)
#dalla matrice di correlazione, vedo che le piogge sono fortemente correlate 
#prendo le localita' piu' rappresentative:
#Rainfall_Monte_Serra Rainfall_Croce_Arcana Rainfall_Calavorno Rainfall_Tereglio_Coreglia_Antelminelli
# stesso discorso per le temperature: le piu' rappresenative sono:
#Temperature_Monte_Serra, Temperature_Lucca_Orto_Botanico
#stesso discorso per i volumi e idrometria

auser<-auser%>%
  dplyr::select(Rainfall_Monte_Serra, Rainfall_Croce_Arcana, Rainfall_Calavorno,
                Rainfall_Tereglio_Coreglia_Antelminelli, Temperature_Monte_Serra,
                Temperature_Lucca_Orto_Botanico, Volume_POL, Volume_CC2, Volume_CSAL,
                Hydrometry_Monte_S_Quirico, imp1,imp2, imp3, imp4, imp5, Season)


#imp1=CoS target
#imp2=DIEC 
#imp3=LT2 target
#imp4=PAG
#imp5=SAL taarget
## vis with imputation 
## prepping objects per target ##

#meteo <- c("rain","temp")

#pozzo1 <- auser %>% dplyr::select(-imp3, -Volume_Pozzo_3)
#pozzo3 <- auser %>% dplyr::select(-imp1, -Volume_Pozzo_1)

pozzo_SAL <- auser %>% dplyr::select(-imp3, -imp1)%>%   
  dplyr::rename(pozzo_PAG=imp4, pozzo_DIEC=imp2 )
pozzo_LT2 <- auser %>% dplyr::select(-imp1, -imp5)%>%   
  dplyr::rename(pozzo_PAG=imp4, pozzo_DIEC=imp2 )
pozzo_CoS <- auser %>% dplyr::select(-imp3, -imp5)%>%   
  dplyr::rename(pozzo_PAG=imp4, pozzo_DIEC=imp2 )



#### computing stepwise regression for variable selection ####
# creating function
step.wisef <- function(x, DATA){
  set.seed(123)
  train.control <- trainControl(method = "cv", number = 10)
  step.model <- train(as.formula(paste(x,"~.")), data = DATA, 
                      method = "leapSeq", 
                      tuneGrid = data.frame(nvmax = 1:11),
                      trControl = train.control,
                      na.action = na.omit)
  return(step.model)
}

#### pozzo 1 CoS ####

pozzo_cos_gb <- pozzo_CoS  
pozzo_cos_gb_Season <- dummyVars(~Season, data = pozzo_cos_gb, fullRank = F)
pozzo_cos_gb_Season <- as.data.frame(predict(pozzo_cos_gb_Season, newdata = pozzo_cos_gb))
pozzo_cos_gb <- cbind(pozzo_cos_gb, pozzo_cos_gb_Season)
pozzo_cos_gb<-pozzo_cos_gb%>% dplyr::select(-Season)

#### GBM con target impCoS####

pozzo_CoS_sw <- step.wisef("imp1", pozzo_cos_gb)
pozzo_CoS_sw$bestTune 
pozzo_CoS_sw$finalModel
coef(pozzo_CoS_sw$finalModel, 10)

#### testing and training split CoS ####

set.seed(123)
pozzo_CoS.split <- initial_split(pozzo_cos_gb, prop = .7)
pozzo_CoS.train <- training(pozzo_CoS.split)
pozzo_CoS.test <- testing(pozzo_CoS.split)

pozzo_CoS_fit1 <- gbm::gbm(imp1 ~ .,
                           data = pozzo_cos_gb,
                           verbose = T, 
                           shrinkage = 0.01,
                           interaction.depth = 3, 
                           n.minobsinnode = 5,
                           n.trees = 5000,
                           cv.folds = 10)
perf_gbm1 <- gbm.perf(pozzo_CoS_fit1, method = "cv")
#ggsave("img/auser/43pozzo_cos_GB.jpg",dpi = 500, width = 10, height=7)

## make predictions 

pozzo_CoS_pred1 <- stats::predict(object = pozzo_CoS_fit1,
                                  newdata = pozzo_CoS.test,
                                  n.trees = perf_gbm1)
rmse_fit1 <- Metrics::rmse(actual = pozzo_CoS.test$imp1,
                           predicted = pozzo_CoS_pred1)
print(rmse_fit1) 
#### RMSE 0.2248 pozzo CoS GB ####
# verificare il confronto rmse
#### plot pozzo cos ####
#plot - rain monte serra
gbm::plot.gbm(pozzo_CoS_fit1, i.var = 1)
# plot - rain croce arcano
plot.gbm(pozzo_CoS_fit1, i.var = 2)
# plot - rain calavorno
plot.gbm(pozzo_CoS_fit1, i.var = 3)
# plot - volume CC2
plot.gbm(pozzo_CoS_fit1, i.var = 8)
# plot - temp lucca
plot.gbm(pozzo_CoS_fit1, i.var = 6)


## interactions of two features on the variable 
gbm::plot.gbm(pozzo_CoS_fit1, i.var = c(1,3)) # rain-rain
plot.gbm(pozzo_CoS_fit1, i.var = c(1,2)) # rain-temp
plot.gbm(pozzo_CoS_fit1, i.var = c(6,7)) # temp-vol

### impact of different features on predicting depth to gw 

# summarise model 

pozzo1_effects <- tibble::as_tibble(gbm::summary.gbm(pozzo_CoS_fit1,
                                                     plotit = F))
pozzo1_effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 6 features
pozzo1_effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")
ggsave("img/auser/44auser_pozzocos_features.jpg",
       dpi = 500, width = 10, height=7)

## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

pozzo_CoS.test$predicted <- as.integer(predict(pozzo_CoS_fit1,
                                            newdata = pozzo_CoS.test,
                                            n.trees = perf_gbm1))

# plot predicted vs actual

ggplot(pozzo_CoS.test) +
  geom_point(aes(x = predicted,
                 y = imp1,
                 color = predicted - imp1),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()
ggsave("img/auser/45pozzo_cos_pred.jpg",
       dpi = 500, width = 10, height=7)



## plotting pred vs actual 

reg <- lm(predicted ~ imp1, data = pozzo_CoS.test)
reg
#Coefficients:
#(Intercept)         imp1  
#-0.2432       0.9590 

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),
             "\nr.squared = ",r.sq)
eq
# plot
(gbm_actualvspred <- ggplot(pozzo_CoS.test) +
    geom_point(aes(x = predicted,
                   y = imp1,
                   color = predicted - imp1),
               alpha = .7, size = 2) +
    geom_abline(intercept = 8.33,slope = 0.78, 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 50, y = 40, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (GBM): Pozzo CoS auser\n",
         subtitle = "Minimum rainfall threshold at 0.5 mm\n")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())

















#### pozzo 3 LT2 ####
pozzo_lt2_gb <- pozzo_LT2  
pozzo_lt2_gb_Season <- dummyVars(~Season, data = pozzo_lt2_gb, fullRank = F)
pozzo_lt2_gb_Season <- as.data.frame(predict(pozzo_lt2_gb_Season, newdata = pozzo_lt2_gb))
pozzo_lt2_gb <- cbind(pozzo_lt2_gb, pozzo_lt2_gb_Season)
pozzo_lt2_gb<-pozzo_lt2_gb%>% dplyr::select(-Season)



pozzo_LT2_sw <- step.wisef("imp3", pozzo_lt2_gb)
pozzo_LT2_sw$bestTune 
pozzo_LT2_sw$finalModel
coef(pozzo_LT2_sw$finalModel, 10)

#### testing and training split LT2 ####

set.seed(123)
pozzo_LT2.split <- initial_split(pozzo_lt2_gb, prop = .7)
pozzo_LT2.train <- training(pozzo_LT2.split)
pozzo_LT2.test <- testing(pozzo_LT2.split)

pozzo_LT2_fit1 <- gbm::gbm(imp3 ~ .,
                           data = pozzo_lt2_gb,
                           verbose = T, 
                           shrinkage = 0.01,
                           interaction.depth = 3, 
                           n.minobsinnode = 5,
                           n.trees = 5000,
                           cv.folds = 10)
perf_gbm1 <- gbm.perf(pozzo_LT2_fit1, method = "cv")

#ggsave("img/auser/46pozzo_LT2_GB.jpg",dpi = 500, width = 10, height=7)

## make predictions 

pozzo_LT2_pred1 <- stats::predict(object = pozzo_LT2_fit1,
                                  newdata = pozzo_LT2.test,
                                  n.trees = perf_gbm1)
rmse_fit1 <- Metrics::rmse(actual = pozzo_LT2.test$imp3,
                           predicted = pozzo_LT2_pred1)
print(rmse_fit1) 
#### RMSE 0.0865 pozzo LT2 GB ####
# verificare il confronto rmse
#### plot pozzo LT2 ####
#plot - rain monte serra
gbm::plot.gbm(pozzo_LT2_fit1, i.var = 1)
# plot - rain croce arcano
plot.gbm(pozzo_LT2_fit1, i.var = 2)
# plot - rain calavorno
plot.gbm(pozzo_LT2_fit1, i.var = 3)
# plot - volume CC2
plot.gbm(pozzo_LT2_fit1, i.var = 8)
# plot - temp lucca
plot.gbm(pozzo_LT2_fit1, i.var = 6)


## interactions of two features on the variable 
gbm::plot.gbm(pozzo_LT2_fit1, i.var = c(1,3)) # rain-rain
plot.gbm(pozzo_LT2_fit1, i.var = c(1,2)) # rain-temp
plot.gbm(pozzo_LT2_fit1, i.var = c(6,7)) # temp-vol

### impact of different features on predicting depth to gw 

# summarise model 

pozzo3_effects <- tibble::as_tibble(gbm::summary.gbm(pozzo_LT2_fit1,
                                                     plotit = F))
pozzo3_effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 6 features
pozzo3_effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")
ggsave("img/auser/47auser_pozzolt2_features.jpg",
       dpi = 500, width = 10, height=7)

## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

pozzo_LT2.test$predicted <- as.integer(predict(pozzo_LT2_fit1,
                                               newdata = pozzo_LT2.test,
                                               n.trees = perf_gbm1))

# plot predicted vs actual

ggplot(pozzo_LT2.test) +
  geom_point(aes(x = predicted,
                 y = imp3,
                 color = predicted - imp3),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()
ggsave("img/auser/48pozzo_lt2_pred.jpg",
       dpi = 500, width = 10, height=7)



## plotting pred vs actual 

reg <- lm(predicted ~ imp3, data = pozzo_LT2.test)
reg
#Coefficients:
#(Intercept)         imp3  
#0.483        0.921 

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),
             "\nr.squared = ",r.sq)
eq
# plot
(gbm_actualvspred <- ggplot(pozzo_LT2.test) +
    geom_point(aes(x = predicted,
                   y = imp3,
                   color = predicted - imp3),
               alpha = .7, size = 2) +
    geom_abline(intercept = 8.33,slope = 0.78, 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 50, y = 40, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (GBM): Pozzo LT2 auser\n",
         subtitle = "Minimum rainfall threshold at 0.5 mm\n")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())














#### pozzo 5 SAL  ####
pozzo_sal_gb <- pozzo_SAL  
pozzo_sal_gb_Season <- dummyVars(~Season, data = pozzo_sal_gb, fullRank = F)
pozzo_sal_gb_Season <- as.data.frame(predict(pozzo_sal_gb_Season, newdata = pozzo_sal_gb))
pozzo_sal_gb <- cbind(pozzo_sal_gb, pozzo_sal_gb_Season)
pozzo_sal_gb<-pozzo_sal_gb%>% dplyr::select(-Season)

pozzo_SAL_sw <- step.wisef("imp5", pozzo_sal_gb)
pozzo_SAL_sw$bestTune 
pozzo_SAL_sw$finalModel
coef(pozzo_SAL_sw$finalModel, 7)

 
#### testing and training split ####
#### pozzo 5 SAL tain/test####

#### testing and training split CoS ####

set.seed(123)
pozzo_SAL.split <- initial_split(pozzo_sal_gb, prop = .7)
pozzo_SAL.train <- training(pozzo_SAL.split)
pozzo_SAL.test <- testing(pozzo_SAL.split)

pozzo_SAL_fit1 <- gbm::gbm(imp5 ~ .,
                           data = pozzo_sal_gb,
                           verbose = T, 
                           shrinkage = 0.01,
                           interaction.depth = 3, 
                           n.minobsinnode = 5,
                           n.trees = 5000,
                           cv.folds = 10)
perf_gbm1 <- gbm.perf(pozzo_SAL_fit1, method = "cv")
#ggsave("img/auser/49pozzo_SAL_GB.jpg",dpi = 500, width = 10, height=7)

## make predictions 

pozzo_SAL_pred1 <- stats::predict(object = pozzo_SAL_fit1,
                                  newdata = pozzo_SAL.test,
                                  n.trees = perf_gbm1)
rmse_fit1 <- Metrics::rmse(actual = pozzo_SAL.test$imp5,
                           predicted = pozzo_SAL_pred1)
print(rmse_fit1) 
#### RMSE 0.1139 pozzo SAL GB ####

#### plot pozzo cos ####
#plot - rain monte serra
gbm::plot.gbm(pozzo_SAL_fit1, i.var = 1)
# plot - rain croce arcano
plot.gbm(pozzo_SAL_fit1, i.var = 2)
# plot - rain calavorno
plot.gbm(pozzo_SAL_fit1, i.var = 3)
# plot - volume CC2
plot.gbm(pozzo_SAL_fit1, i.var = 8)
# plot - temp lucca
plot.gbm(pozzo_SAL_fit1, i.var = 6)


## interactions of two features on the variable 
gbm::plot.gbm(pozzo_SAL_fit1, i.var = c(1,3)) # rain-rain
plot.gbm(pozzo_SAL_fit1, i.var = c(1,2)) # rain-temp
plot.gbm(pozzo_SAL_fit1, i.var = c(6,7)) # temp-vol

### impact of different features on predicting depth to gw 

# summarise model 

pozzo5_effects <- tibble::as_tibble(gbm::summary.gbm(pozzo_SAL_fit1,
                                                     plotit = F))
pozzo5_effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 6 features
pozzo5_effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")
ggsave("img/auser/50auser_pozzosal_features.jpg",
       dpi = 500, width = 10, height=7)

## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

pozzo_SAL.test$predicted <- as.integer(predict(pozzo_SAL_fit1,
                                               newdata = pozzo_SAL.test,
                                               n.trees = perf_gbm1))

# plot predicted vs actual

ggplot(pozzo_SAL.test) +
  geom_point(aes(x = predicted,
                 y = imp5,
                 color = predicted - imp5),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()
ggsave("img/auser/51pozzo_sal_pred.jpg",
       dpi = 500, width = 10, height=7)



## plotting pred vs actual 

reg <- lm(predicted ~ imp5, data = pozzo_SAL.test)
reg
#Coefficients:
#(Intercept)         imp15 
#-0.05032      0.91663 

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),
             "\nr.squared = ",r.sq)
eq
# plot
(gbm_actualvspred <- ggplot(pozzo_SAL.test) +
    geom_point(aes(x = predicted,
                   y = imp5,
                   color = predicted - imp5),
               alpha = .7, size = 2) +
    geom_abline(intercept = 8.33,slope = 0.78, 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 50, y = 40, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (GBM): Pozzo SAL auser\n",
         subtitle = "Minimum rainfall threshold at 0.5 mm\n")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())


#### modelli con i lag ####



#######fine##########



