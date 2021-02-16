
#### uploading libraries ####

library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(corrplot)
library(tidyverse)
library(imputeTS)
library(ggplot2)
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


luco_featured <- add.seasons(luco9) %>%
  spread(key = imp, value = depth_to_gw.m) %>% 
  dplyr::mutate(snow.yes = as.factor(ifelse(Temperature_Mensano < 0 & Rainfall_Mensano >= 0, 1,0)),
         snow.no = as.factor(ifelse(Temperature_Mensano > 0, 1,0)))

str(luco_featured)


###
# autoTS
# regime shifts - script 
# livello medio trimestre per trimestre, o mese per mese 
# (non so se vale la pena) distinguere comportamento falda a seconda dell'anno (piovoso o siccitoso)
###
# filtro per livelli di pioggia - identificare livelli pioggia dura meno di un x giorni - cambio livello minimo a 0 
# con cinque livelli diversi di pioggia --> creo cinque time series diverse
# introduco un lag - traslo in avanti tutte le serie delle pioggie --> traslo a 4 gg in futuro
# disallineamento tra falda acquifera e la pioggia
# per cinque serie con livelli di pioggia minima creo altre cinque con il lag 
# confrontare i modelli 
# relazione monotona --> correl. spearman tra pioggia e acquifero
# sulla base di quella selezionerei un primo set di features 
# poi si runna il modello (autoTS/xgboost)
### 
# temperatura - va bene divsione per stagioni, neve, ricarica
# effetto maggiore su consumi

str(luco_featured)

# creating quarters, semesters and trimonthly data 

luco_months_pozzo1 <- luco_featured %>% 
  mutate(Y_m = as.Date(Date, format ="%Y-%m"),
                Semester = semester(Date, with_year = T),
                Quarters = quarter(Date, with_year = T),
                Trimonthly = as.factor(round_date(Y_m, unit = "3 months"))) %>% 
  # date written is first day of the period
  # dplyr::select(-Y_m) %>%
  group_by(Trimonthly) %>% 
  dplyr::mutate(Mean_depth_1.Tri = mean(imp1)) %>% 
  ungroup() %>% 
  group_by(Quarters) %>% 
  dplyr::mutate(Mean_depth_1.Quar = mean(imp1)) %>% 
  ungroup() %>% 
  group_by(Semester) %>% 
  dplyr::mutate(Mean_depth_1.Sem = mean(imp1)) %>% 
  ungroup() %>% 
  dplyr::mutate(lag1 = Lag(Rainfall_Mensano, +1),
         lag3 = Lag(Rainfall_Mensano,+3),
         lag5 = Lag(Rainfall_Mensano,+5),
         lag7 = Lag(Rainfall_Mensano,+7),
         lag9 = Lag(Rainfall_Mensano, +9))

luco_months_pozzo3 <- luco_featured %>% 
  dplyr::mutate(Y_m = as.Date(Date, format ="%Y-%m"),
                Semester = semester(Date, with_year = T),
                Quarters = quarter(Date, with_year = T),
                Trimonthly = as.factor(round_date(Y_m, unit = "3 months"))) %>% 
  # date written is first day of the period
  # dplyr::select(-Y_m) %>%
  group_by(Trimonthly) %>% 
  mutate(Mean_depth_1.Tri = mean(imp3)) %>% 
  ungroup() %>% 
  group_by(Quarters) %>% 
  mutate(Mean_depth_1.Quar = mean(imp3)) %>% 
  ungroup() %>% 
  group_by(Semester) %>% 
  mutate(Mean_depth_1.Sem = mean(imp3)) %>% 
  ungroup() %>% 
  mutate(lag1 = Lag(Rainfall_Mensano, +1),
         lag3 = Lag(Rainfall_Mensano,+3),
         lag5 = Lag(Rainfall_Mensano,+5),
         lag7 = Lag(Rainfall_Mensano,+7),
         lag9 = Lag(Rainfall_Mensano, +9)) 

# vis trimesters
ggplot(luco_months_pozzo1, aes(Trimonthly, Mean_depth_1.Tri, fill = Year))+
  geom_bar(stat = "identity")

# vis quarters 
ggplot(luco_months_pozzo1, aes(Quarters, Mean_depth_1.Quar, fill = Year))+
  geom_bar(stat = "identity")

# vis semesters
ggplot(luco_months_pozzo1, aes(Semester, Mean_depth_1.Sem, fill = Year))+
  geom_bar(stat = "identity")

write.csv(luco_months_pozzo1, "processed_data/luco_lag_pozzo1.csv")

####
### checking for rainfall ###
## changing mm levels 

#ggplot(canneto_rain, aes(y = Rainfall_Settefrati)) +
#geom_boxplot()

## 5 datasets with 5 levels of min rain changed to 0:

#5 per Mensano
luco_rain_mensano.5 <- luco %>% 
  mutate(rain1 = ifelse(Rainfall_Mensano <= 0.5, 0, Rainfall_Mensano),
         seq.rain.val = sequence(rle(as.character(rain1))$lengths))

luco_rain_mensano1.5 <- luco %>%  # whenever rain is lower than 1mm/day, = 0
  mutate(rain2 = ifelse(Rainfall_Mensano <= 1.5, 0, Rainfall_Mensano),
         seq.rain.val = sequence(rle(as.character(rain2))$lengths))

luco_rain_mensano3 <- luco %>% 
  mutate(rain3 = ifelse(Rainfall_Mensano <= 3,0,Rainfall_Mensano),
         seq.rain.val = sequence(rle(as.character(rain3))$lengths))

luco_rain_mensano5 <- luco %>% 
  mutate(rain4 = ifelse(Rainfall_Mensano <= 5, 0, Rainfall_Mensano),
         seq.rain.val = sequence(rle(as.character(rain4))$lengths))

luco_rain_mensano7 <- luco %>% 
  mutate(rain5 = ifelse(Rainfall_Mensano <=7, 0, Rainfall_Mensano),
         seq.rain.val = sequence(rle(as.character(rain5))$lengths))

#5 per Montalcinello
luco_rain_montalcinello.5 <- luco %>% 
  mutate(rain1 = ifelse(Rainfall_Montalcinello <= 0.5, 0, Rainfall_Montalcinello),
         seq.rain.val = sequence(rle(as.character(rain1))$lengths))

luco_rain_montalcinello1.5 <- luco %>%  # whenever rain is lower than 1mm/day, = 0
  mutate(rain2 = ifelse(Rainfall_Montalcinello <= 1.5, 0, Rainfall_Montalcinello),
         seq.rain.val = sequence(rle(as.character(rain2))$lengths))

luco_rain_montalcinello3 <- luco %>% 
  mutate(rain3 = ifelse(Rainfall_Montalcinello <= 3,0,Rainfall_Montalcinello),
         seq.rain.val = sequence(rle(as.character(rain3))$lengths))

luco_rain_montalcinello5 <- luco %>% 
  mutate(rain4 = ifelse(Rainfall_Montalcinello <= 5, 0, Rainfall_Montalcinello),
         seq.rain.val = sequence(rle(as.character(rain4))$lengths))

luco_rain_montalcinello7 <- luco %>% 
  mutate(rain5 = ifelse(Rainfall_Montalcinello <=7, 0, Rainfall_Montalcinello),
         seq.rain.val = sequence(rle(as.character(rain5))$lengths))


## creating 5 new datasets per dataset...
## ... or 5 new variables 

luco_rain0.5.lag <- luco_rain_mensano.5 %>% 
  mutate(lag1 = Lag(rain1, +1),
         lag3 = Lag(rain1,+3),
         lag5 = Lag(rain1,+5),
         lag7 = Lag(rain1,+7),
         lag9 = Lag(rain1, +9)) %>%
  write.csv(., "processed_data/luco_rain0.5.csv")

luco_rain1.5.lag <- luco_rain_mensano1.5 %>% 
  mutate(lag1 = Lag(rain2, +1),
         lag3 = Lag(rain2, +3),
         lag5 = Lag(rain2, +5),
         lag7 = Lag(rain2, +7),
         lag9 = Lag(rain2, +9))%>% 
  write.csv(., "processed_data/luco_rain1.5.csv")

luco_rain3.lag <- luco_rain_mensano3 %>% 
  mutate(lag1 = Lag(rain3, +1),
         lag3 = Lag(rain3, +3),
         lag5 = Lag(rain3, +5),
         lag7 = Lag(rain3, +7),
         lag9 = Lag(rain3, +9)) %>% 
  write.csv(., "processed_data/luco_rain3.csv")

luco_rain5.lag <- luco_rain_mensano5 %>% 
  mutate(lag1 = Lag(rain4, +1),
         lag3 = Lag(rain4, +3),
         lag5 = Lag(rain4, +5),
         lag7 = Lag(rain4, +7),
         lag9 = Lag(rain4, +9)) %>% 
  write.csv(., "processed_data/luco_rain5.csv")

luco_rain7.lag <- luco_rain_mensano7 %>% 
  mutate(lag1 = Lag(rain5, +1),
         lag3 = Lag(rain5, +3),
         lag5 = Lag(rain5, +5),
         lag7 = Lag(rain5, +7),
         lag9 = Lag(rain5, +9)) %>% 
  write.csv(., "processed_data/luco_rain7.csv")

write.csv(luco_featured, "processed_data/luco_featured.csv")

##passo poi a 03-luco.R per proseguire l'analisi


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



