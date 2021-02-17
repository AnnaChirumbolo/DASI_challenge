
#### uploading libraries ####

library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(corrplot)
library(tidyverse)
library(imputeTS)
library(zoo)
library(data.table)
library(lubridate)
library(outliers)
library(tidyselect)
library(Hmisc)
library(caret)
library(h2o)
library(tidyverse)
library(randomForest)
library(caret)
library(tree)
library(e1071)
require(caTools)
library(tsibble)
library(lubridate)
library(gbm)
library(forecast)


#### uploading files ####

luco <- read.csv("data/Aquifer_Luco.csv")

#### data cleaning ####

#### luco ####

str(luco)

summary(luco)

luco_missing <- luco %>% 
  miss_var_summary()
print(luco_missing)
View(luco_missing)


luco1 <- luco %>% 
  gather(key = "well", value = "depth_to_gw.m", Depth_to_Groundwater_Podere_Casetta:Depth_to_Groundwater_Pozzo_4) %>%
  rename(Date = ï..Date) %>%
  mutate(Date = dmy(Date),
         well = gsub("Depth_to_Groundwater_","",well))

(first_look <- ggplot(luco1, aes(x =Date, y = abs(depth_to_gw.m), color = well))+
    geom_line(size = .5)+
    theme_classic())

### missing data clearly starting from 

## removing missing for depth to gw

min(luco1$Date[!is.na(luco1$depth_to_gw.m)])

luco_filtered <- luco1 %>% 
  filter(Date >= "2017-09-01", well!="Pozzo_4" & well!="Podere_Casetta")



(second_look <- ggplot(luco_filtered, aes(Date, abs(depth_to_gw.m), 
                                          color = well))+
    geom_line(size = .5)+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    ylab("Abs. depth to groundwater (m)")+
    xlab(""))

ggsave("img/luco_filtered_depth_to_groundwater.jpg", 
       dpi = 500, height = 5, width = 10)



### scatter correlations ###

luco_cor <- luco_filtered %>%
  dplyr::select(-Date ) %>%
  spread(key=well, value = depth_to_gw.m) %>%
  cor(.,use = "complete.obs") %>%
  corrplot(.,method = "circle")


##mi tengo due piogge (Mensano e Montalcinello), Podere è il Pozzo 2, e una sola temperatura (Mensano)

luco_finish <- luco_filtered %>%
  dplyr::select(-Volume_Pozzo_4,  -Temperature_Siena_Poggio_al_Vento, -Temperature_Pentolina, -Temperature_Monteroni_Arbia_Biena, -Rainfall_Simignano, -Rainfall_Siena_Poggio_al_Vento, -Rainfall_Monteroni_Arbia_Biena, -Rainfall_Monteroni_Arbia_Biena, -Rainfall_Monticiano_la_Pineta, -Rainfall_Pentolina, -Rainfall_Ponte_Orgia, -Rainfall_Scorgiano, -Rainfall_Sovicille) %>%
  spread(key = well, value = depth_to_gw.m)

(scatter_temp_Siena <- ggplot(luco_filtered, 
                              aes(x = Temperature_Siena_Poggio_al_Vento,
                                  y = abs(depth_to_gw.m),
                                  color = well))+
    geom_point(size = 1, alpha = 0.8)+
    theme_classic()+
    xlim(0,31))



# from the plot there doesnt seem to be any correlation.... 

(scatter_temp_Mensano <- ggplot(luco_filtered, 
                                aes(x = Temperature_Mensano,
                                    y = abs(depth_to_gw.m),
                                    color = well))+
    geom_point(size = 1, alpha = 0.8)+
    theme_classic()+
    xlim(0,31))

# from the plot there doesnt seem to be any correlation....

(scatter_temp_Pentolina <- ggplot(luco_filtered, 
                                  aes(x = Temperature_Pentolina,
                                      y = abs(depth_to_gw.m),
                                      color = well))+
    geom_point(size = 1, alpha = 0.8)+
    theme_classic()+
    xlim(0,31))

# from the plot there doesnt seem to be any correlation.... 


(scatter_temp_Monteroni <- ggplot(luco_filtered,
                                  aes(Temperature_Monteroni_Arbia_Biena, 
                                      abs(depth_to_gw.m),
                                      color = well))+
    geom_point(size = 1, alpha = 0.8)+
    theme_classic()+
    xlim(0,31))

# same for Monteroni ... 

## rain ##

min(luco_filtered$Rainfall_Simignano,na.rm = T)
max(luco_filtered$Rainfall_Simignano,na.rm = T)
min(luco_filtered$Rainfall_Siena_Poggio_al_Vento,na.rm = T)
max(luco_filtered$Rainfall_Siena_Poggio_al_Vento,na.rm = T)

(scatter_rain_Simignano <- ggplot(luco_filtered, 
                                  aes(Rainfall_Simignano,
                                      abs(depth_to_gw.m),
                                      color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))


(scatter_rain_Siena <- ggplot(luco_filtered,
                              aes(Rainfall_Siena_Poggio_al_Vento,
                                  abs(depth_to_gw.m),
                                  color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))




### saving plots ###

panelled_rain_scatter <- ggarrange(scatter_rain_Simignano,
                                   scatter_rain_Siena)
panelled_rain_scatter


ggsave("img/luco_corr_rain.png",
       dpi = 500, height = 10, width = 18)



panelled_temp_scatter <- ggarrange(scatter_temp_Siena,
                                   scatter_temp_Mensano,
                                   scatter_temp_Pentolina,
                                   scatter_temp_Monteroni)
panelled_temp_scatter

ggsave("img/luco_corr_temp.png",
       dpi = 500, height= 10, width = 18)


### saving luco .csv 

View(luco_filtered)

# saving 

write.csv(luco_filtered, "processed_data/luco_filtered.csv")
write.csv(luco_finish, "processed_data/luco_finish.csv")

################################################################################
############################# Fine 1 #############################################

luco <- luco_finish

luco1 <- luco %>% 
  gather(key = "well", value = "depth_to_gw.m", Pozzo_1:Pozzo_3) %>%
  mutate(Date = ymd (Date),
         well = gsub("Pozzo_","",well))

(first_look <- ggplot(luco1, aes(x =Date, y = abs(depth_to_gw.m), color = well))+
    geom_line(size = .5)+
    theme_classic())



# overall view of depth to gw changes by well type 
(depth_to_gw <- ggplot(luco1, aes(Date, depth_to_gw.m,
                                  color = well, group = well))+
    geom_line(size = 0.5)+
    theme_classic())


### vis well by well, to fill in gaps 

## pozzo 1


(luco_p1 <- ggplot(luco, aes(Date, Pozzo_1, group = 1))+
    geom_line(size = 0.5)+
    theme_classic())

## pozzo 3


(luco_p3 <- ggplot(luco, aes(Date, Pozzo_3, group = 1))+
    geom_line(size = 0.5)+
    theme_classic())

## if i wanted to count how many nas following in a row?

group.na <- function(x){
  group <- if_else(is.na(x), 1, 0)
  return(group)
}


# creating a function that counts sequence of nas in a row for each variable 
count.na <- function(x){
  group_na <- if_else(is.na(x), 1,0)
  grp <- with(rle(group_na), rep(seq_along(lengths), lengths))
  counter <- ave(grp,grp, FUN = seq_along)
  return(counter)
}

## imputeTS

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


## linear interpolation 

#luco4 <- data.frame(luco, lapply(luco[,8:9], na.approx))



#### FINAL ####

## using na.ma from imputeTS

luco5 <- data.frame(luco, lapply(luco[,7:8], 
                                 function(x) na_ma(x, k=1)))

luco6 <-setnames(luco5, old = colnames(luco5[,9:10]),
                 new = c("imp1","imp3"))

luco7 <- luco6 %>% 
  gather(key="imp", value = "imputed_depth_to_gw.m",9:10) %>%
  mutate(Date = ymd(Date))

str(luco7)


## vis with imputation 

(imp_vis <- ggplot(luco7, aes(Date, imputed_depth_to_gw.m, color = imp,
                              group = imp))+
    geom_line()+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y"))


statsNA(luco7$imputed_depth_to_gw.m)
## cleaned up - no more missing data 


#### checking for outliers ####

## summary

luco8 <- luco7 %>% 
  spread(key = "imp", value = "imputed_depth_to_gw.m") 

summary(luco8[,7:10])

## hist 

(hist_luco <- ggplot(luco7, aes(imputed_depth_to_gw.m))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(vars(imp)))


## boxplots

# target var 
(hist_target_luco <- ggplot(luco7,
                            aes(y = imputed_depth_to_gw.m,
                                color = imp))+
    geom_boxplot()+
    theme_classic())#

# saving
ggsave("img/hist_luco_target.jpg", dpi = 500, width = 10, height=7)

#### checking singularly 

(imp1_luco <- ggplot(luco7, aes(y = Pozzo_1))+
    geom_boxplot()+
    theme_classic())

(imp3_luco <- ggplot(luco7, aes(y = Pozzo_3))+
    geom_boxplot()+
    theme_classic())



## extracting vals of potential outliers 

# for pozzo 1
out1_luco <- boxplot.stats(luco8$imp1)$out
out1_ind_luco <- which(luco8$imp1 %in% c(out1_luco))
out1_ind_luco # rows where outliers are found

upper_bound <- quantile(luco8$imp1, 0.975)
upper_bound # -10

upper_bound99 <- quantile(luco8$imp1, 0.99)
upper_bound99 # -9,0285

## checking stats to verify it's an outlier
# grubbs test
test1 <- grubbs.test(luco8$imp1)
test1 # 112.5 is an outlier (at 5% significance level)

## substituting with q3 value 
luco8$imp1[luco8$imp1 >= -9.0285] <- -10.60
summary(luco8$imp1)

# for pozzo 3
out3_luco <- boxplot.stats(luco8$imp3) # -11,9
out3_luco
out3_ind_luco <- which(luco8$imp3 %in% c(out3_luco))
out3_ind_luco # line 945

# grubbs test 
test3 <- grubbs.test(luco8$imp3)
test3 # it is an outlier, 5% significance

# substituting (non necessario)
#luco8$imp3[luco8$imp3 >= -11.9] <- -11.9
summary(luco8$imp3)


(plot_check_out <- ggplot(luco8, aes(Date, imp1 ))+
    geom_point()+
    #geom_line(data = luco8, aes(Date, Rainfall_Mensano, color = "red"))+
    theme_classic())

####################

luco9 <- luco8 %>% 
  gather(key = "imp", value = "depth_to_gw.m", 9:10)

(luco_noout <- ggplot(luco9, aes(y=depth_to_gw.m,color = imp))+
    geom_boxplot()+
    theme_classic())

# saving 

ggsave("img/luco_boxplot.jpg", dpi = 500, width = 10, height = 7)



### keeping outliers for pozzi 1 and 3 - from most recent years, and there's 100+ according to the boxplot

### checking outliers for feature variables 

luco10 <- luco9 %>% 
  gather(key = "temp_sensor", value = "temp.C", Temperature_Mensano) %>%
  gather(key = "rain_sensor", value = "rain.mm", Rainfall_Mensano, Rainfall_Montalcinello)


## vis hist

# temp

(temp_hist_luco <- ggplot(luco10, aes(temp.C))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(vars(temp_sensor)))

# pretty similar distribution

# rain
(rain_hist_luco <- ggplot(luco10, aes(rain.mm))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(vars(rain_sensor)))

# pretty similar here as well


## vis boxplot

# temp 
(temp_box_luco <- ggplot(luco10, aes(y=temp.C, color = temp_sensor))+
    geom_boxplot()+
    theme_classic())

# no outliers 

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

# plotting it over time 
(plot_rainm_out <- ggplot(df_rainm_out, aes(Date, Rainfall_Mensano))+
    geom_point()+
    geom_line(data = luco9, aes(Date, Rainfall_Mensano, color = "red"))+
    theme_classic())

# it occurs throughout - from 2017 through to 2020


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

#### decision to leave them as such 


#### checking out for missing - rain and temp ####

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

ggplot_na_distribution(luco9$Temperature_Mensano)

#### filled in the data!!
statsNA(luco9$Temperature_Mensano) # still one na missing x9

## vis distrib data with imputed vars 

ggplot(luco9, aes(Date, Temperature_Mensano))+
  geom_line()+
  geom_line(data = luco9, aes(Date, Temperature_Mensano,
                              color = "red"))+
  theme_classic()

## perfettooooooo

################################################################################
#### looking at last feature: volume ####

# volume is of the water pumped OUT from the drinking water plant 
# thus removed from the aquifer

luco_vol <- luco %>% 
  dplyr::select(Date, Volume_Pozzo_1: Volume_Pozzo_3)  %>%
  gather(key = "pozzo", value = "volume.mc", -Date) %>%
  mutate(Date = ymd(Date))
str(luco_vol)

(vol_luco <- ggplot(luco_vol, aes(Date, volume.mc, color = pozzo,
                                 group = pozzo))+
    geom_line()+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y"))

# boxplots 

(vol_box_luco <- ggplot(luco_vol, aes(y = volume.mc, color = pozzo))+
    geom_boxplot()+
    theme_classic())

# hist 
(vol_hist_luco <- ggplot(luco_vol, aes(volume.mc))+
    geom_histogram()+
    facet_wrap(vars(pozzo))+
    theme_classic())

#######################################################################################################
## to use these data:
# need to remove the first years where there's total absence
# ohterwise how to predict it? impossible 
## or try and find data on it - but think it'll be pretty much impossible since there's no chance 
# if acea doesn't have it who would? 
## if i remove the first years i would be left with a very small dataset indeed
## might have to consider leaving these values aside and not use them for the model...? 
## otherwise i can't model just from the last few years, 
# if the target variable, of interest, is present from the years before 
## it would then be wiser to not consider volume data, and keep years where target data is available 
# and just use rain and temp as features 
#######################################################################################################

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





luco <- luco_featured %>%
  dplyr::select(-Pozzo_1,-Pozzo_3)

luco_base_val <- luco %>% 
  group_by(Year) %>% 
  summarise(mean_depth = mean(imp1))

str(luco)

summary(luco_base_val)

#### splitting dataset between train and test

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

mean((luco_rf_margin - luco_test1)^2) # MSE = 0.0635
sqrt(mean((luco_rf_margin - luco_test1)^2)) # RMSE = 0.2520


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

prune_luco <- prune.tree(luco_tree, best = 6)
summary(prune_luco)
cv.tree(luco_tree,,prune.tree)$dev

plot(prune_luco)
text(prune_luco, pretty = 0)

tree_luco_margin <- predict(luco_tree, newdata = luco[-luco_train1,])
mean((tree_luco_margin - luco_test1)^2) # MSE = 0.1237 (worse than RF MSE)

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

## reading files 

luco <- read.csv("processed_data/luco_featured.csv") %>%
  dplyr::select(-X, -Date, -Pozzo_1, -Pozzo_3, -Year, -Day, -Month) 
#spread(key = imp, value = depth_to_gw.m) # in regression date doesnt really matter 
str(luco)

## prepping objects per target ##

meteo <- c("rain","temp")

pozzo1 <- luco %>% dplyr::select(-imp3, -Volume_Pozzo_3)
pozzo3 <- luco %>% dplyr::select(-imp1, -Volume_Pozzo_1)

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

summary(pozzo1)
#### pozzo 1 ####

pozzo1_sw <- step.wisef("imp1", pozzo1)
pozzo1_sw$bestTune 
pozzo1_sw$finalModel
coef(pozzo1_sw$finalModel, 6)

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
print(rmse_fit1) # RMSE 0.1610
#(higher error than when keeping all variables)

#plot - rain mensano
gbm::plot.gbm(pozzo1_fit1, i.var = 1)
# plot - rain montalcinello
plot.gbm(pozzo1_fit1, i.var = 2)


## interactions of two features on the variable 

gbm::plot.gbm(pozzo1_fit1, i.var = c(1,3)) # temp-rain
plot.gbm(pozzo1_fit1, i.var = c(1,2)) # rain-rain
plot.gbm(pozzo1_fit1, i.var = c(2,3)) # temp-rain

### impact of different features on predicting depth to gw 

# summarise model 

pozzo1_effects <- tibble::as_tibble(gbm::summary.gbm(pozzo1_fit1,
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
  top_n(6) %>%  # it's already only 3 vars
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


#### XGBOOST ####

library(xgboost)
library(lubridate)
library(tibble)

View(pozzo1)
max(pozzo1$Date)
str(pozzo1)

pozzo1_ext <- luco_featured %>%
  dplyr::select(-imp3, -Pozzo_3) %>%
  dplyr::mutate(Date = as_date(Date)) %>%
  bind_rows(tibble(Date = seq(start = as_date("2020-07-31"),
                              from = as_date("2020-07-31"),
                              by = "day", length.out = 31),
                   imp1 = rep(NA, 31)))
tail(pozzo1_ext)

pozzo1_xgb <- luco_featured %>%
  dplyr::select(-imp3, -Pozzo_3) %>%
  dplyr::mutate(months = lubridate::month(Date),
         years = lubridate::year(Date),
         days = lubridate::day(Date))

str(pozzo1_xgb)

## split - train and prediction sets

p1.xgb.train <- pozzo1_xgb[1:nrow(luco_featured),]
p1.xgb.pred <- pozzo1_xgb[(nrow(luco_featured)+1):nrow(pozzo1_xgb),]

p1.x_train <- xgboost::xgb.DMatrix(as.matrix(p1.xgb.train %>% 
                                               dplyr::select(months, years)))
p1.x_pred <- xgboost::xgb.DMatrix(as.matrix(p1.xgb.pred %>% 
                                              dplyr::select(months, years)))
p1.y_train <- p1.xgb.train$imp1


p1.xgb_trcontrol <- caret::trainControl(
  method ="cv",
  number = 5,
  
)



