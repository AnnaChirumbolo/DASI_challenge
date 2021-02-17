
#### uploading libraries ####

library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(corrplot)
library(imputeTS)
library(zoo)
library(data.table)
library(outliers)
library(tidyselect)
library(Hmisc)
library(caret)
library(h2o)
library(randomForest)
library(caret)
library(tree)
library(e1071)
require(caTools)
library(tsibble)
library(gbm)
library(forecast)
library(naniar)


#### uploading files ####

luco <- read.csv("data/Aquifer_Luco.csv")

#### data cleaning ####

#### luco ####

luco_missing <- luco %>% 
  miss_var_summary()
print(luco_missing)


luco1 <- luco %>% 
  gather(key = "well", value = "depth_to_gw.m", Depth_to_Groundwater_Podere_Casetta:Depth_to_Groundwater_Pozzo_4) %>%
  rename(Date = ï..Date) %>%
  mutate(Date = dmy(Date),
         well = gsub("Depth_to_Groundwater_","",well))

#visualizzo le variabili target in funzione del tempo
(first_look <- ggplot(luco1, aes(x =Date, y = abs(depth_to_gw.m), color = well))+
    geom_line(size = .5)+
    theme_classic())


min(luco1$Date[!is.na(luco1$depth_to_gw.m)])

#decido di prendere solo Pozzo 1 e Pozzo 3 in quanto Podere casetta non ha dati aggiornati
#e Pozzo 4 ha andamenti non interpretabili

luco_filtered <- luco1 %>% 
  filter(Date >= "2017-09-01", well!="Pozzo_4" & well!="Podere_Casetta")


#mostro i grafici con i missing value del Pozzo 1 e Pozzo 3
(second_look <- ggplot(luco_filtered, aes(Date, abs(depth_to_gw.m), 
                                          color = well))+
    geom_line(size = .5)+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    ylab("Abs. depth to groundwater (m)")+
    xlab(""))

### scatter correlations ###

luco_cor <- luco_filtered %>%
  dplyr::select(-Date ) %>%
  spread(key=well, value = depth_to_gw.m) %>%
  cor(.,use = "complete.obs") %>%
  corrplot(.,method = "circle")

#a seguito dell'analisi del corrplot di rimuovere diverse features con alta correlazione

luco <- luco_filtered %>%
  dplyr::select(-Rainfall_Scorgiano, -Rainfall_Pentolina, -Rainfall_Simignano, -Rainfall_Siena_Poggio_al_Vento,-Rainfall_Monticiano_la_Pineta,-Rainfall_Sovicille,-Rainfall_Ponte_Orgia,-Rainfall_Monteroni_Arbia_Biena) %>%
  dplyr::select(-Temperature_Pentolina, -Temperature_Siena_Poggio_al_Vento, -Temperature_Monteroni_Arbia_Biena) %>%
  dplyr::select(-Volume_Pozzo_4) %>%
  spread(key=well, value = depth_to_gw.m)

## pozzo 1

#mostro l'andamento della target Pozzo 1
(luco_p1 <- ggplot(luco, aes(Date, Pozzo_1, group = 1))+
    geom_line(size = 0.5)+
    theme_classic())

## pozzo 3

#mostro l'andamento della target Pozzo 3
(luco_p3 <- ggplot(luco, aes(Date, Pozzo_3, group = 1))+
    geom_line(size = 0.5)+
    theme_classic())


## utilizzo imputeTS per riempire i NaN delle target

#Pozzo_1

ggplot_na_distribution(luco$Pozzo_1)+
  theme_classic()

statsNA(luco$Pozzo_1)

ggplot_na_intervals(luco$Pozzo_1)

ggplot_na_gapsize(luco$Pozzo_1)

#Pozzo_3

ggplot_na_distribution(luco$Pozzo_3)+
  theme_classic()

statsNA(luco$Pozzo_3)

ggplot_na_intervals(luco$Pozzo_3)

ggplot_na_gapsize(luco$Pozzo_3)

## uso na_ma da imputeTS


luco5 <- data.frame(luco, lapply(luco[,7:8], 
                                 function(x) na_ma(x, k=1)))

luco6 <-setnames(luco5, old = colnames(luco5[,9:10]),
                 new = c("imp1","imp3"))

luco7 <- luco6 %>% 
  gather(key="imp", value = "imputed_depth_to_gw.m",9:10) %>%
  mutate(Date = ymd(Date))

str(luco7)


## visualizzo il dataset con i valori "imputati" per le variabili target

(imp_vis <- ggplot(luco7, aes(Date, imputed_depth_to_gw.m, color = imp,
                              group = imp))+
    geom_line()+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y"))


statsNA(luco7$imputed_depth_to_gw.m) #non ci sono più Na


#### checking for outliers ####

## summary

luco8 <- luco7 %>% 
  spread(key = "imp", value = "imputed_depth_to_gw.m") 


## hist 

(hist_luco <- ggplot(luco7, aes(imputed_depth_to_gw.m))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(vars(imp)))  #distribuzioni similari


## boxplots

# target var 
(hist_target_luco <- ggplot(luco7,
                            aes(y = imputed_depth_to_gw.m,
                                color = imp))+
    geom_boxplot()+
    theme_classic()) #presenza di outlier per target 1


## extracting vals of potential outliers 

# for pozzo 1
out1_luco <- boxplot.stats(luco8$imp1)$out
out1_ind_luco <- which(luco8$imp1 %in% c(out1_luco))
out1_ind_luco # rows where outliers are found

upper_bound <- quantile(luco8$imp1, 0.975)
upper_bound # -10

upper_bound99 <- quantile(luco8$imp1, 0.99)
upper_bound99 # -9,0285

## verifico la presenza di outlier
# grubbs test
test1 <- grubbs.test(luco8$imp1)
test1 # -2.8 is an outlier (at 5% significance level)

## sostituisco con il valore q3  
luco8$imp1[luco8$imp1 >= -9.0285] <- -10.60
summary(luco8$imp1)

# per pozzo 3
out3_luco <- boxplot.stats(luco8$imp3) # -11,9
out3_luco
out3_ind_luco <- which(luco8$imp3 %in% c(out3_luco))
out3_ind_luco # line 945

# grubbs test 
test3 <- grubbs.test(luco8$imp3)
test3 # it is an outlier, 5% significance

# sostituisco? (non lo ritengo necessario)
summary(luco8$imp3)


####################

luco9 <- luco8 %>% 
  gather(key = "imp", value = "depth_to_gw.m", 9:10)

(luco_noout <- ggplot(luco9, aes(y=depth_to_gw.m,color = imp))+
    geom_boxplot()+
    theme_classic()) #ohhh, le due target ripulite dagli outlier


### checking outliers for feature variables 

luco10 <- luco9 %>% 
  gather(key = "temp_sensor", value = "temp.C", Temperature_Mensano) %>%
  gather(key = "rain_sensor", value = "rain.mm", Rainfall_Mensano, Rainfall_Montalcinello)



# rain 
(rain_box_luco <- ggplot(luco10, aes(y = rain.mm, color = rain_sensor))+
    geom_boxplot()+
    theme_classic())

# like in histogram, there's a long tail, which the plot recognises as outliers 


### let's check it statistically 

out_tempv_luco <- boxplot.stats(luco9$Temperature_Mensano)
out_tempv_luco

# confirmed no outliers for temp Mensano 

out_rainv_luco <- boxplot.stats(luco9$Rainfall_Mensano)$out
out_rainv_luco
# more than 392 outliers... 

out_rainm_luco <- boxplot.stats(luco9$Rainfall_Montalcinello)$out
out_rainm_luco
# same here 

## let's check where these "outliers" occur in time (rain)

# mensano
out_rainm_luco_ind <- which(luco9$Rainfall_Mensano %in% c(out_rainm_luco))
out_rainm_luco_ind

df_rainm_out <- luco9[out_rainm_luco_ind,]
df_rainm_out

#decido di lasciare i valori climatici senza rimuovere outlier 

# plotting it over time 
(plot_rainm_out <- ggplot(df_rainm_out, aes(Date, Rainfall_Mensano))+
    geom_point()+
    geom_line(data = luco9, aes(Date, Rainfall_Mensano, color = "red"))+
    theme_classic())

# lasso temporale - from 2017 through to 2020


# montalcinello
out_rainv_luco_ind <- which(luco9$Rainfall_Montalcinello %in% c(out_rainv_luco))
out_rainv_luco_ind
length(out_rainv_luco_ind)

df_rainv_luco <- luco9[out_rainv_luco_ind,]
View(df_rainv_luco)

# plotting over time 
(plot_rainv_luco <- ggplot(luco9, aes(Date, Rainfall_Mensano))+
    geom_line(color = "red")+
    geom_point(data = df_rainv_luco, aes(Date, Rainfall_Mensano))+
    theme_classic())

(plot_rainv_luco <- ggplot(df_rainv_luco, aes(Date, Rainfall_Mensano))+
    geom_point()+
    theme_classic())

#### lascio il dataset invariato


#### controllo per valori mancanti - rain and temp ####

# rain
statsNA(luco9$Rainfall_Mensano)

ggplot_na_distribution(luco9$Rainfall_Mensano)

statsNA(luco9$Rainfall_Montalcinello)

ggplot_na_distribution(luco9$Rainfall_Montalcinello)

(rain_luco <- ggplot(luco10, aes(Date, rain.mm, color = rain_sensor))+
    geom_line()+
    theme_classic())

# temp 
statsNA(luco9$Temperature_Mensano)

ggplot_na_distribution(luco9$Temperature_Mensano)

statsNA(luco9$Temperature_Mensano)

ggplot_na_distribution(luco9$Temperature_Mensano) #oleee, non ci sono valori mancanti!


## vis distrib data with imputed vars 

ggplot(luco9, aes(Date, Temperature_Mensano))+
  geom_line()+
  geom_line(data = luco9, aes(Date, Temperature_Mensano,
                              color = "red"))+
  theme_classic()

################################################################################
################################################################################
########################    FEATURE ENGINEERING             ####################
################################################################################
################################################################################


#creazione funzione stagione

add.seasons <- function(data) {
  seasons <- data %>% 
    mutate(Date = lubridate::as_date(Date),
           Year = as.factor(lubridate::year(Date)),
           Month = as.factor(lubridate::month(Date)),
           Day = as.factor(lubridate::day(Date)),
           Month_day = format(Date,format = "%m-%d")) %>% 
    mutate(Spring = factor(ifelse(Month_day >= "03-21" & Month_day < "06-21",
                                  1,0)),
           Summer = factor(ifelse(Month_day >="06-21" & Month_day < "09-21",
                                  1,0)),
           Autumn = factor(ifelse(Month_day >= "09-21" & Month_day < "12-21",
                                  1,0)),
           Winter = factor(ifelse(Month_day >= "12-21" & Month_day <= "12-31",
                                  1, 
                                  ifelse(Month_day >= "01-01" & Month_day < "03-21",
                                         1, 0)))) %>%
    dplyr::select(-Month_day)
  return(seasons)
}

# aggiunta features stagioni e neve

luco_featured <- add.seasons(luco9) %>%
  dplyr::select(-Pozzo_1,-Pozzo_3) %>%   #rimozione delle "vecchie" target con valori NA
  spread(key = imp, value = depth_to_gw.m) %>% 
  dplyr::mutate(snow.yes = as.factor(ifelse(Temperature_Mensano < 0 & Rainfall_Mensano >= 0, 1,0)),
         snow.no = as.factor(ifelse(Temperature_Mensano > 0, 1,0)))

str(luco_featured)

## 5 datasets with 4 levels of min rain changed to 0:


luco_rain_type1 <- luco_featured %>% 
  dplyr::mutate(rain1mensano = ifelse(Rainfall_Mensano <= 5, 0, Rainfall_Mensano),
         rain1montalcinello = ifelse(Rainfall_Montalcinello <= 5, 0, Rainfall_Montalcinello),
         lag1men = Lag(rain1mensano, +1),
         lag3men = Lag(rain1mensano,+3),
         lag5men = Lag(rain1mensano,+5),
         lag7men = Lag(rain1mensano,+7),
         lag9men = Lag(rain1mensano, +9),
         lag1mon = Lag(rain1montalcinello, +1),
         lag3mon = Lag(rain1montalcinello,+3),
         lag5mon = Lag(rain1montalcinello,+5),
         lag7mon = Lag(rain1montalcinello,+7),
         lag9mon = Lag(rain1montalcinello, +9))

luco_rain_type2 <- luco_featured %>% 
  dplyr::mutate(rain2mensano = ifelse(Rainfall_Mensano <= 10, 0, Rainfall_Mensano),
                rain2montalcinello = ifelse(Rainfall_Montalcinello <= 10, 0, Rainfall_Montalcinello),
                lag1men = Lag(rain2mensano, +1),
                lag3men = Lag(rain2mensano,+3),
                lag5men = Lag(rain2mensano,+5),
                lag7men = Lag(rain2mensano,+7),
                lag9men = Lag(rain2mensano, +9),
                lag1mon = Lag(rain2montalcinello, +1),
                lag3mon = Lag(rain2montalcinello,+3),
                lag5mon = Lag(rain2montalcinello,+5),
                lag7mon = Lag(rain2montalcinello,+7),
                lag9mon = Lag(rain2montalcinello, +9))

luco_rain_type3 <- luco_featured %>% 
  dplyr::mutate(rain3mensano = ifelse(Rainfall_Mensano <= 15, 0, Rainfall_Mensano),
                rain3montalcinello = ifelse(Rainfall_Montalcinello <= 15, 0, Rainfall_Montalcinello),
                lag1men = Lag(rain3mensano, +1),
                lag3men = Lag(rain3mensano,+3),
                lag5men = Lag(rain3mensano,+5),
                lag7men = Lag(rain3mensano,+7),
                lag9men = Lag(rain3mensano, +9),
                lag1mon = Lag(rain3montalcinello, +1),
                lag3mon = Lag(rain3montalcinello,+3),
                lag5mon = Lag(rain3montalcinello,+5),
                lag7mon = Lag(rain3montalcinello,+7),
                lag9mon = Lag(rain3montalcinello, +9))

luco_rain_type4 <- luco_featured %>% 
  dplyr::mutate(rain4mensano = ifelse(Rainfall_Mensano <= 30, 0, Rainfall_Mensano),
                rain4montalcinello = ifelse(Rainfall_Montalcinello <= 30, 0, Rainfall_Montalcinello),
                lag1men = Lag(rain4mensano, +1),
                lag3men = Lag(rain4mensano,+3),
                lag5men = Lag(rain4mensano,+5),
                lag7men = Lag(rain4mensano,+7),
                lag9men = Lag(rain4mensano, +9),
                lag1mon = Lag(rain4montalcinello, +1),
                lag3mon = Lag(rain4montalcinello,+3),
                lag5mon = Lag(rain4montalcinello,+5),
                lag7mon = Lag(rain4montalcinello,+7),
                lag9mon = Lag(rain4montalcinello, +9))


luco <- luco_featured

luco_base_val <- luco %>% 
  group_by(Year) %>% 
  summarise(mean_depth = mean(imp1))

str(luco)

#### divido il dataset tra train e test per ogni target

#POZZO_1

lucoRF1 <- luco %>%
  dplyr::select(-Volume_Pozzo_3, -Date, -Day, -Month, -Year, -imp3)

Train_Test <- sample(c("Train","Test"), nrow(lucoRF1), replace = T,
                     prob = c(0.75,0.25))
luco_train <- lucoRF1[Train_Test =="Train",]
luco_test <- lucoRF1[Train_Test == "Test",]

luco_train1 <- sample(1:nrow(lucoRF1),nrow(lucoRF1)/2)
luco_test1 <- luco[-luco_train1,"imp1"]

summary(luco_train)

#### random forest model ####
#Pozzo_1

rf_formula <- as.formula("imp1 ~.")
luco_rf <- randomForest(rf_formula, 
                        data = luco_train,
                        ntree = 200,
                        importance = T,
                        replace = T)
print(luco_rf)
# MSE = 0.0842
# % var expl = 80.7 %

plot(luco_rf)
# shows error change with increasing number of trees 
# Y - corresp MSE 
# X - tree number 

luco_rf_margin <- predict(luco_rf, newdata =luco[-luco_train1,])

mean((luco_rf_margin - luco_test1)^2) # MSE = 0.0596
sqrt(mean((luco_rf_margin - luco_test1)^2)) # RMSE = 0.2443


#var

importance(luco_rf)
varImpPlot(luco_rf)

#### 

#POZZO_3

lucoRF3 <- luco %>%
  dplyr::select(-Volume_Pozzo_1, -Date, -Day, -Month, -Year, -imp1)

Train_Test <- sample(c("Train","Test"), nrow(lucoRF3), replace = T,
                     prob = c(0.75,0.25))
luco_train <- lucoRF3[Train_Test =="Train",]
luco_test <- lucoRF3[Train_Test == "Test",]

luco_train3 <- sample(1:nrow(lucoRF3),nrow(lucoRF3)/2)
luco_test3 <- luco[-luco_train3,"imp3"]

summary(luco_train)

#### random forest model ####
#Pozzo_3

rf_formula <- as.formula("imp3 ~.")
luco_rf <- randomForest(rf_formula, 
                        data = luco_train,
                        ntree = 200,
                        importance = T,
                        replace = T)
print(luco_rf)
# MSE = 0.1596
# % var expl = 67.1 %

plot(luco_rf)
# shows error change with increasing number of trees 
# Y - corresp MSE 
# X - tree number 

luco_rf_margin <- predict(luco_rf, newdata =luco[-luco_train3,])

mean((luco_rf_margin - luco_test3)^2) # MSE = 0.1073
sqrt(mean((luco_rf_margin - luco_test3)^2)) # RMSE = 0.3275


#var

importance(luco_rf)
varImpPlot(luco_rf)


#### comparing with regression tree ####
#Pozzo_1

luco_tree <- tree(imp1 ~., data = lucoRF1, subset = luco_train1)
summary(luco_tree)


plot(luco_tree)
text(luco_tree, pretty = 0)

cv_luco <- cv.tree(luco_tree)
cv_luco

plot(cv_luco$size, cv_luco$dev, type = "b")
# could go down to 9 leaves

prune_luco <- prune.tree(luco_tree, best = 6)
summary(prune_luco)
cv.tree(luco_tree,,prune.tree)$dev

plot(prune_luco)
text(prune_luco, pretty = 0)

tree_luco_margin <- predict(luco_tree, newdata = luco[-luco_train1,])
mean((tree_luco_margin - luco_test1)^2) # MSE = 0.0830

#Pozzo_3

luco_tree <- tree(imp3 ~., data = lucoRF3, subset = luco_train3)
summary(luco_tree)


plot(luco_tree)
text(luco_tree, pretty = 0)

cv_luco <- cv.tree(luco_tree)
cv_luco

plot(cv_luco$size, cv_luco$dev, type = "b")
# could go down to 9 leaves

prune_luco <- prune.tree(luco_tree, best = 6)
summary(prune_luco)
cv.tree(luco_tree,,prune.tree)$dev

plot(prune_luco)
text(prune_luco, pretty = 0)

tree_luco_margin <- predict(luco_tree, newdata = luco[-luco_train3,])
mean((tree_luco_margin - luco_test3)^2) # MSE = 0.1511

################################################################################
################################################################################
######################### GRADIENT BOOST MACHINE  ##############################
################################################################################
################################################################################

## libraries 

library(rsample)
library(caret)
library(ggthemes)
library(scales)
library(wesanderson)
library(tidyverse)
library(gbm)
library(Metrics)
library(here)
library(MASS)
library(leaps)
library(purrr)



## prepping objects per target ##

meteo <- c("rain","temp")

pozzo1 <- luco %>% dplyr::select(-imp3, -Volume_Pozzo_3, -Date, -Year, -Month, -Day)
pozzo3 <- luco %>% dplyr::select(-imp1, -Volume_Pozzo_1, -Date, -Year, -Month, -Day)

#### computing stepwise regression for variable selection ####
# creating function
step.wisef <- function(x, DATA){
  set.seed(123)
  train.control <- trainControl(method = "cv", number = 10)
  step.model <- train(as.formula(paste(x,"~.")), data = DATA, 
                      method = "leapSeq", 
                      tuneGrid = data.frame(nvmax = 1:9),
                      trControl = train.control,
                      na.action = na.omit)
  return(step.model)
}

#### pozzo 1 ####

pozzo1_sw <- step.wisef("imp1", pozzo1)
pozzo1_sw$bestTune #7 var
pozzo1_sw$finalModel
coef(pozzo1_sw$finalModel, 7)

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
print(rmse_fit1) # RMSE 0.1586


### impact of different features on predicting depth to gw 

# summarise model 

pozzo1_effects <- tibble::as_tibble(gbm::summary.gbm(pozzo1_fit1,
                                                     plotit = F))
pozzo1_effects %>% utils::head()
 

# plot top 7 features
pozzo1_effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(7) %>%  # it's already only 7 vars
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


#### pozzo 3 ####

# stepwise

pozzo3_sw <- step.wisef("imp3",pozzo3)
pozzo3_sw$bestTune # 9
coef(pozzo3_sw$finalModel, 9)


# split 
set.seed(123)
p3.split <- initial_split(pozzo3, prop = .9)
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
print(p3_rmse) # 0.2528


# summarise model 

p3.effects <- tibble::as_tibble(gbm::summary.gbm(p3.fit1,
                                                 plotit = F))
p3.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 6 features
p3.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 6 vars
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


###############################################################################




