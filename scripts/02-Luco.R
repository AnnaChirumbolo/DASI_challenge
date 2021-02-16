################################################################################
################################################################################
############ DATA PREP FOR MODELLING            ################################
################################################################################
################################################################################


#### libraries ####

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


###############
##############


#### LUCO AQUIFER #####

luco <- read.csv("processed_data/luco_finish.csv")


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

###
#with(mtcars, ave(mpg, cyl, FUN=mean))
# ave is baseR equivalent of group_by() %>% mutate()!!! YAS
####

#luco2 <- data.frame(luco, lapply(luco[,14:22], count.na))

#luco3 <- data.frame(luco2, lapply(luco2[,14:22],
#  function(x) if_else(is.na(x),1,0)))


## all this was to try and find a way to impute missing data

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

luco4 <- data.frame(luco, lapply(luco[,8:9], na.approx))



#### FINAL ####

## using na.ma from imputeTS

luco5 <- data.frame(luco, lapply(luco[,8:9], 
                                           function(x) na_ma(x, k=1)))

luco6 <-setnames(luco5, old = colnames(luco5[,10:11]),
                      new = c("imp1","imp3"))

luco7 <- luco6 %>% 
  gather(key="imp", value = "imputed_depth_to_gw.m",10:11) %>%
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

summary(luco8[,8:11])

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
    theme_classic())#+
#facet_wrap(vars(imp)))
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
out1_dog <- boxplot.stats(luco8$imp1)$out # 112.5 (repeated x4)
out1_ind_dog <- which(luco8$imp1 %in% c(out1_dog))
out1_ind_dog # rows where outliers are found

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
out3_dog <- boxplot.stats(luco8$imp3) # -11,9
out3_dog
out3_ind_dog <- which(luco8$imp3 %in% c(out3_dog))
out3_ind_dog # line 945

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
  gather(key = "imp", value = "depth_to_gw.m", 10:11)
View(luco9)

(luco_noout <- ggplot(luco9, aes(y=depth_to_gw.m,color = imp))+
    geom_boxplot()+
    theme_classic())

# saving 

ggsave("img/luco_boxplot.jpg", dpi = 500, width = 10, height = 7)



### keeping outliers for pozzi 1 and 3 - from most recent years, and there's 100+ according to the boxplot

### checking outliers for feature variables 

# luco9

str(luco9)


luco10 <- luco9 %>% 
  gather(key = "temp_sensor", value = "temp.C", Temperature_Mensano) %>%
  gather(key = "rain_sensor", value = "rain.mm", Rainfall_Mensano, Rainfall_Montalcinello)


## vis hist

# temp

(temp_hist_dog <- ggplot(luco10, aes(temp.C))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(vars(temp_sensor)))
# pretty similar distribution

# rain
(rain_hist_dog <- ggplot(luco10, aes(rain.mm))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(vars(rain_sensor)))
# pretty similar here as well


## vis boxplot

# temp 
(temp_box_dog <- ggplot(luco10, aes(y=temp.C, color = temp_sensor))+
    geom_boxplot()+
    theme_classic())
# no outliers 

# rain 
(rain_box_dog <- ggplot(luco10, aes(y = rain.mm, color = rain_sensor))+
    geom_boxplot()+
    theme_classic())
# like in histogram, there's a long tail, which the plot recognises as outliers 


### let's check it statistically 

out_tempv_dog <- boxplot.stats(luco9$Temperature_Mensano)
out_tempv_dog
# confirmed no outliers for temp Mensano 

out_rainv_dog <- boxplot.stats(luco9$Rainfall_Mensano)$out
out_rainv_dog
# more than 392 outliers... 

out_rainm_dog <- boxplot.stats(luco9$Rainfall_Montalcinello)$out
out_rainm_dog
# same here 

## let's check where these "outliers" occur in time (rain)

# mensano
out_rainm_dog_ind <- which(luco9$Rainfall_Mensano %in% c(out_rainm_dog))
out_rainm_dog_ind

df_rainm_out <- luco9[out_rainm_dog_ind,]
df_rainm_out

# plotting it over time 
(plot_rainm_out <- ggplot(df_rainm_out, aes(Date, Rainfall_Mensano))+
    geom_point()+
    geom_line(data = luco9, aes(Date, Rainfall_Mensano, color = "red"))+
    theme_classic())
# it occurs throughout - from 2017 through to 2020


# montalcinello
out_rainv_dog_ind <- which(luco9$Rainfall_Montalcinello %in% c(out_rainv_dog))
out_rainv_dog_ind
length(out_rainv_dog_ind)

df_rainv_dog <- luco9[out_rainv_dog_ind,]
View(df_rainv_dog)

# plotting over time 
(plot_rainv_dog <- ggplot(luco9, aes(Date, Rainfall_Mensano))+
    geom_line(color = "red")+
    geom_point(data = df_rainv_dog, aes(Date, Rainfall_Mensano))+
    theme_classic())

(plot_rainv_dog <- ggplot(df_rainv_dog, aes(Date, Rainfall_Mensano))+
    geom_point()+
    theme_classic())

#### decision to leave them as such 


#### checking out for missing - rain and temp ####

# rain
statsNA(luco9$Rainfall_Mensano)

ggplot_na_distribution(luco9$Rainfall_Mensano)

statsNA(luco9$Rainfall_Montalcinello)

ggplot_na_distribution(luco9$Rainfall_Montalcinello)

(rain_dog <- ggplot(luco10, aes(Date, rain.mm, color = rain_sensor))+
    geom_line()+
    theme_classic())

# temp 
statsNA(luco9$Temperature_Mensano)

ggplot_na_distribution(luco9$Temperature_Mensano)

statsNA(luco9$Temperature_Mensano)

ggplot_na_distribution(luco9$Temperature_Mensano)


#### FILLING GAPS WITH METEO ####

#### temperature Mensano

## non necessario in quanto non ci sono NA per le temperature
#read_plus <- function(flnm) {
 # read_csv(flnm) %>% 
  #  mutate(filename = flnm)
#}

#temp_dog_ls <- list.files(path = "./data/luco_3BMETEO/",
#                          pattern = "*.csv$", 
#                          full.names = T) %>%
#  map_df(~read_plus(.)) 

## manipulating temp data 
#temp_dog <- luco9$Temperature_Mensano %>% 
#  mutate(
#         Date = gsub(".csv", "", Date),
#         Date = gsub("([a-z])([[:digit:]])", "\\1 \\2", Date, perl = T)) %>%
#  separate(date, into = c("weekday", "day")) %>%
#  select(-weekday) %>%
#  unite(date_final, day,date, sep = " ") %>%
#  mutate(date_final = str_replace(date_final,"ago","08"),
#         date_final = str_replace(date_final, "gen", "01"),
#         date_final = str_replace(date_final, "feb", "02"),
#         date_final = str_replace(date_final, "mar", "03"),
#         date_final = str_replace(date_final, "apr", "04"),
#         date_final = str_replace(date_final, "mag", "05"),
#         date_final = str_replace(date_final, "giu", "06"),
#         date_final = str_replace(date_final, "lug", "07"),
#         date_final = str_replace(date_final, "sett", "09"),
#         date_final = str_replace(date_final, "ott", "10"),
#         date_final = str_replace(date_final, "nov","11"),
#         date_final = str_replace(date_final, "dec", "12"),
#         date_final = gsub(" ", "/", date_final),
#         date_final = dmy(date_final)) %>%
#  rename(Date = date_final) %>%
#  select(Date, tmin, tmax) %>%
#  mutate(Temperature_Mensano = rowMeans(subset(., select = c(tmin,tmax)),
#                                         na.rm = T)) %>%
#  select(-tmin, -tmax) %>%
#  arrange(Date)

#summary(temp_dog)


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
  select(Date, Volume_Pozzo_1: Volume_Pozzo_3)  %>%
  gather(key = "pozzo", value = "volume.mc", -Date) %>%
  mutate(Date = ymd(Date))
str(luco_vol)

(vol_dog <- ggplot(luco_vol, aes(Date, volume.mc, color = pozzo,
                                      group = pozzo))+
    geom_line()+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y"))

# boxplots 

(vol_box_dog <- ggplot(luco_vol, aes(y = volume.mc, color = pozzo))+
    geom_boxplot()+
    theme_classic())

# hist 
(vol_hist_dog <- ggplot(luco_vol, aes(volume.mc))+
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
    group_by(Year) %>%
    mutate(Spring = factor(ifelse(Month_day >= "03-21" & Month_day < "06-21",
                                  1,0)),
           Summer = factor(ifelse(Month_day >="06-21" & Month_day < "09-21",
                                  1,0)),
           Autumn = factor(ifelse(Month_day >= "09-21" & Month_day < "12-21",
                                  1,0)),
           Winter = factor(ifelse(Month_day >= "12-21" & Month_day < "03-21",
                                  1,0))) %>%
    select(-Month_day) %>% 
    ungroup()
  return(seasons)
}



#### luco ####


luco_featured <- add.seasons(luco9) %>%
  dplyr::select(-X) %>% 
  spread(key = imp, value = depth_to_gw.m) %>% 
  mutate(snow.yes = as.factor(ifelse(Temperature_Mensano <=0,1,0)),
         snow.no = as.factor(ifelse(Temperature_Mensano > 0, 1, 0)))

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


#### concentrarsi su canneto + aiuto per lago 

## re-reading the file 


str(luco)

# creating quarters, semesters and trimonthly data 

luco_months_pozzo1 <- luco_featured %>% 
  dplyr::mutate(Y_m = as.Date(Date, format ="%Y-%m"),
         Semester = semester(Date, with_year = T),
         Quarters = quarter(Date, with_year = T),
         Trimonthly = as.factor(round_date(Y_m, unit = "3 months"))) %>% 
  # date written is first day of the period
  # dplyr::select(-Y_m) %>%
  group_by(Trimonthly) %>% 
  mutate(Mean_depth_1.Tri = mean(imp1)) %>% 
  ungroup() %>% 
  group_by(Quarters) %>% 
  mutate(Mean_depth_1.Quar = mean(imp1)) %>% 
  ungroup() %>% 
  group_by(Semester) %>% 
  mutate(Mean_depth_1.Sem = mean(imp1)) %>% 
  ungroup() %>% 
  mutate(lag1 = Lag(Rainfall_Mensano, +1),
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
