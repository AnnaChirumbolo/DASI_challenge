################################
######### only canneto #########
################################

#### custom functions ####

## creating function to read multiple files at once 

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}


## Function to automate creation of seasons as categorical features 

add.seasons <- function(data) {
  seasons <- data %>% 
    mutate(Date = lubridate::as_date(Date),
           Month_day = format(Date,format = "%m-%d"),
           
           Spring = factor(ifelse(Month_day >= "03-21" & Month_day < "06-21",
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


## creating function for stepwise model selection

step.wisef <- function(x, DATA){
  set.seed(123)
  train.control <- trainControl(method = "cv", number = 10)
  step.model <- train(as.formula(paste(x,"~.")), data = DATA, 
                      method = "leapSeq", 
                      tuneGrid = data.frame(nvmax = 1:13), # in this particular case, max 13 features for the variable
                      trControl = train.control,
                      na.action = na.omit)
  return(step.model)
}


#### libraries ####

## installing packages
#install.packages('caret', dependencies = TRUE)
#install.packages("imputeTS")
#install.packages("varhandle")
#install.packages("autoTS")
#install.packages("h2o")
#install.packages("MLmetrics")
#install.packages("stats")
#install.packages("rsample")

## Here are all the libraries used for this project 

## need to divide them into section + brief comment on what they've been used for
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(imputeTS)
library(zoo)
library(data.table)
library(lubridate)
library(outliers)
library(tidyselect)
library(GGally)
library(naniar)
library(visdat)
library(forecast)
library(xts)
library(caTools)
library(reshape2)
library(Hmisc)
library(caret) 
library(randomForest)
library(tree)
library(tsibble)
library(gbm)
library(rsample)
library(scales)
library(Metrics)
library(here)
library(MASS)
library(leaps)
library(corrplot)
library(varhandle)
library(autoTS)
library(h2o)
library(MLmetrics)
library(stats)

#### reading file: madonna di canneto ####


canneto <- read.csv("data/Water_Spring_Madonna_di_Canneto.csv")

## First overall look
str(canneto)

summary(canneto)

#head(canneto)
#tail(canneto)

## Changing date value to "date" format
canneto <- canneto %>% 
  rename(Date = ï..Date) %>% 
  mutate(Date = dmy(Date))

## Checking for missing values 
canneto_missing <- canneto %>% 
  miss_var_summary()
print(canneto_missing) # flow rate data missing: more than 50% 

## First visualisation of target distribution over years

(first_look_canneto <- ggplot(canneto,
                              aes(Date ,Flow_Rate_Madonna_di_Canneto))+
    geom_line(size= 1, color = "blue", alpha = 0.7)+
    xlab("")+
    ylab("Flow Rate (L/s)\n")+
    theme_classic())
# big gap of data first years

ggsave("img/canneto/first_look_target_canneto.jpg",
       dpi = 500, width = 15, height = 8)

## Filtering out first years with no target data

# Which year to start from? 
#min(canneto$Date[!is.na(canneto$Flow_Rate_Madonna_di_Canneto)])        

# "2015-03-13", but there's another big gap before that, so removing until 2016-04-13

# Filtering out from 2015-03-13
canneto_filtered <- canneto %>% 
  filter(Date >= "2016-04-13") 

## How are these NAs distributed over time?
ggplot_na_distribution(canneto_filtered$Flow_Rate_Madonna_di_Canneto,
                       title = "Distribution of Missing Target Values (Madonna di Canneto)\n",
                       ylab = "Flow Rate (L/s)\n")
ggsave("img/canneto/na_dist_cannetof.jpg", dpi = 500, width = 15, height = 8)
?ggplot_na_distribution

## checking for missing data after filtering 
canneto_filtered_missing <- canneto_filtered %>% 
  miss_var_summary()
print(canneto_filtered_missing)
# there are missing data for all variables but Date

## manipulating rainfall data (3bmeteo)
rain_canneto <- meteo_canneto %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("../input/madonna-di-canneto-3bmeteo", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = str_replace(date_final,"ago","08"),
         date_final = str_replace(date_final, "gen", "01"),
         date_final = str_replace(date_final, "feb", "02"),
         date_final = str_replace(date_final, "mar", "03"),
         date_final = str_replace(date_final, "apr", "04"),
         date_final = str_replace(date_final, "mag", "05"),
         date_final = str_replace(date_final, "giu", "06"),
         date_final = str_replace(date_final, "lug", "07"),
         date_final = str_replace(date_final, "set", "09"),
         date_final = str_replace(date_final, "ott", "10"),
         date_final = str_replace(date_final, "nov","11"),
         date_final = str_replace(date_final, "dic", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>%
  rename(Date = date_final,
         Rainfall_Settefrati = prec) %>%
  dplyr::select(Date, Rainfall_Settefrati)

#summary(rain_canneto)

## manipulating temperature data (3bmeteo)

temp_canneto <- meteo_canneto %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("../input/madonna-di-canneto-3bmeteo", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = str_replace(date_final,"ago","08"),
         date_final = str_replace(date_final, "gen", "01"),
         date_final = str_replace(date_final, "feb", "02"),
         date_final = str_replace(date_final, "mar", "03"),
         date_final = str_replace(date_final, "apr", "04"),
         date_final = str_replace(date_final, "mag", "05"),
         date_final = str_replace(date_final, "giu", "06"),
         date_final = str_replace(date_final, "lug", "07"),
         date_final = str_replace(date_final, "set", "09"),
         date_final = str_replace(date_final, "ott", "10"),
         date_final = str_replace(date_final, "nov","11"),
         date_final = str_replace(date_final, "dic", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>%
  rename(Date = date_final) %>%
  dplyr::select(Date, tmin, tmax) %>%
  mutate(Temperature_Settefrati = rowMeans(subset(., select = c(tmin,tmax)),
                                           na.rm = T)) %>%
  dplyr::select(-tmin, -tmax)

#summary(temp_canneto)

## Filling dataset to model with new meteorological data 

# temp
canneto_nomissing$Temperature_Settefrati[is.na(canneto_nomissing$Temperature_Settefrati)] <- temp_canneto$Temperature_Settefrati[match(canneto_nomissing$Date[is.na(canneto_nomissing$Temperature_Settefrati)],
                                                                                                                                       temp_canneto$Date)]

# rain 
canneto_nomissing$Rainfall_Settefrati[is.na(canneto_nomissing$Rainfall_Settefrati)] <- rain_canneto$Rainfall_Settefrati[match(canneto_nomissing$Date[is.na(canneto_nomissing$Rainfall_Settefrati)],
                                                                                                                              rain_canneto$Date)]

#summary(canneto_nomissing)


## Checking visually that all NAs have gone 

# Rainfall 
ggplot_na_distribution(canneto_nomissing$Rainfall_Settefrati)

# Temperature 
ggplot_na_distribution(canneto_nomissing$Temperature_Settefrati)


### outliers ###


## Checking distributions 

# Feature 1: Rainfall 

(canneto_rain_box <- ggplot(canneto_filtered,
                            aes(y = Rainfall_Settefrati))+
    geom_boxplot()+
    theme_classic()) # few data outside top whisker

## Checking for outliers statistically 
outr_canneto <- boxplot.stats(canneto_nomissing$Rainfall_Settefrati)$out

# Grubbs test 
grubbs.test(canneto_nomissing$Rainfall_Settefrati)
# max value seems to be an outlier - statistically speaking

outr_canneto_ind <- which(canneto_nomissing$Rainfall_Settefrati %in% c(outr_canneto))

df_outr_canneto <- canneto_nomissing[outr_canneto_ind,]

## Let's visualise outliers over time

# plot
(outr_canneto_vis <- ggplot(df_outr_canneto, aes(Date, Rainfall_Settefrati))+
    geom_point()+
    theme_classic())
# spread out over time
# keeping them all 

# Feature 2: Temperature 

(canneto_temp_box <- ggplot(canneto_filtered,
                            aes(y = Temperature_Settefrati))+
    geom_boxplot()+
    theme_classic()) # no outliers here 

## Checking statistically 
outt_canneto <- boxplot.stats(canneto_nomissing$Temperature_Settefrati)
outt_canneto
# no out 

## Grubbs test 
testt_canneto <- grubbs.test(canneto_nomissing$Temperature_Settefrati)
testt_canneto # confirmed stats - no out 


#### feature engineering ####

## adding new features: seasons and presence/absence of snow

canneto_featured <- add.seasons(canneto_nomissing) %>%
  rename(fl_rate.Ls = imp_flow_rate) %>% 
  mutate(snow.yes = as.factor(ifelse(Temperature_Settefrati <=0 & Rainfall_Settefrati > 0, 1,0)),
         snow.no = as.factor(ifelse(Temperature_Settefrati >0,1,0))) 

str(canneto_featured)

### changing effect of rain on target, and lagging the effect of rain on the target ###

canneto_orig <- canneto_featured %>% 
  mutate(lag1 = Lag(Rainfall_Settefrati, +1),
         lag3 = Lag(Rainfall_Settefrati,+3),
         lag5 = Lag(Rainfall_Settefrati,+5),
         lag7 = Lag(Rainfall_Settefrati,+7),
         lag9 = Lag(Rainfall_Settefrati, +9)) 

canneto_orig1 <- canneto_orig %>% 
  dplyr::select(-Date)

## creating 5 new datasets with different min rainfall levels 
## and with new time lags (trying to represent true effect of rain over target)

canneto0.5 <- canneto_orig %>% 
  mutate(rain0.5 = ifelse(Rainfall_Settefrati <= 0.5, 0, 
                          Rainfall_Settefrati),
         lag1 = Lag(rain0.5, +1),
         lag3 = Lag(rain0.5,+3),
         lag5 = Lag(rain0.5,+5),
         lag7 = Lag(rain0.5,+7),
         lag9 = Lag(rain0.5, +9)) %>% 
  dplyr::select(-Rainfall_Settefrati)

canneto0.5_1 <- canneto0.5 %>%  dplyr::select(-Date)

canneto1.5 <- canneto_orig %>% 
  mutate(rain1.5 = ifelse(Rainfall_Settefrati <= 1.5, 0, 
                          Rainfall_Settefrati),
         lag1 = Lag(rain1.5, +1),
         lag3 = Lag(rain1.5, +3),
         lag5 = Lag(rain1.5, +5),
         lag7 = Lag(rain1.5, +7),
         lag9 = Lag(rain1.5, +9)) %>% 
  dplyr::select(-Rainfall_Settefrati)

canneto1.5_1 <- canneto1.5 %>%   dplyr::select(-Date)

canneto3 <- canneto_orig %>% 
  mutate(rain3 = ifelse(Rainfall_Settefrati <= 3,0,
                        Rainfall_Settefrati),
         lag1 = Lag(rain3, +1),
         lag3 = Lag(rain3, +3),
         lag5 = Lag(rain3, +5),
         lag7 = Lag(rain3, +7),
         lag9 = Lag(rain3, +9)) %>% 
  dplyr::select(-Rainfall_Settefrati)

canneto3_1 <- canneto3 %>%  dplyr::select(-Date)

canneto5 <- canneto_orig %>% 
  mutate(rain5 = ifelse(Rainfall_Settefrati <= 5, 0, 
                        Rainfall_Settefrati),
         lag1 = Lag(rain5, +1),
         lag3 = Lag(rain5, +3),
         lag5 = Lag(rain5, +5),
         lag7 = Lag(rain5, +7),
         lag9 = Lag(rain5, +9)) %>%
  dplyr::select(-Rainfall_Settefrati)

canneto5_1 <- canneto5 %>%   dplyr::select(-Date)

## checking for missing data after filtering 
canneto_filtered_missing <- canneto_nomissing %>% 
  miss_var_summary()
print(canneto_filtered_missing)
# there are missing data for all variables but Date

## manipulating rainfall data (3bmeteo)
rain_canneto <- meteo_canneto %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("../input/madonna-di-canneto-3bmeteo", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = str_replace(date_final,"ago","08"),
         date_final = str_replace(date_final, "gen", "01"),
         date_final = str_replace(date_final, "feb", "02"),
         date_final = str_replace(date_final, "mar", "03"),
         date_final = str_replace(date_final, "apr", "04"),
         date_final = str_replace(date_final, "mag", "05"),
         date_final = str_replace(date_final, "giu", "06"),
         date_final = str_replace(date_final, "lug", "07"),
         date_final = str_replace(date_final, "set", "09"),
         date_final = str_replace(date_final, "ott", "10"),
         date_final = str_replace(date_final, "nov","11"),
         date_final = str_replace(date_final, "dic", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>%
  rename(Date = date_final,
         Rainfall_Settefrati = prec) %>%
  dplyr::select(Date, Rainfall_Settefrati)

#summary(rain_canneto)

## manipulating temperature data (3bmeteo)

temp_canneto <- meteo_canneto %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("../input/madonna-di-canneto-3bmeteo", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = str_replace(date_final,"ago","08"),
         date_final = str_replace(date_final, "gen", "01"),
         date_final = str_replace(date_final, "feb", "02"),
         date_final = str_replace(date_final, "mar", "03"),
         date_final = str_replace(date_final, "apr", "04"),
         date_final = str_replace(date_final, "mag", "05"),
         date_final = str_replace(date_final, "giu", "06"),
         date_final = str_replace(date_final, "lug", "07"),
         date_final = str_replace(date_final, "set", "09"),
         date_final = str_replace(date_final, "ott", "10"),
         date_final = str_replace(date_final, "nov","11"),
         date_final = str_replace(date_final, "dic", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>%
  rename(Date = date_final) %>%
  dplyr::select(Date, tmin, tmax) %>%
  mutate(Temperature_Settefrati = rowMeans(subset(., select = c(tmin,tmax)),
                                           na.rm = T)) %>%
  dplyr::select(-tmin, -tmax)

#summary(temp_canneto)

## Filling dataset to model with new meteorological data 

# temp
canneto_nomissing$Temperature_Settefrati[is.na(canneto_nomissing$Temperature_Settefrati)] <- temp_canneto$Temperature_Settefrati[match(canneto_nomissing$Date[is.na(canneto_nomissing$Temperature_Settefrati)],
                                                                                                                                       temp_canneto$Date)]

# rain 
canneto_nomissing$Rainfall_Settefrati[is.na(canneto_nomissing$Rainfall_Settefrati)] <- rain_canneto$Rainfall_Settefrati[match(canneto_nomissing$Date[is.na(canneto_nomissing$Rainfall_Settefrati)],
                                                                                                                              rain_canneto$Date)]

#summary(canneto_nomissing)


## Checking visually that all NAs have gone 

# Rainfall 
ggplot_na_distribution(canneto_nomissing$Rainfall_Settefrati)

# Temperature 
ggplot_na_distribution(canneto_nomissing$Temperature_Settefrati)


### outliers ###


## Checking distributions 

# Feature 1: Rainfall 

(canneto_rain_box <- ggplot(canneto_filtered,
                            aes(y = Rainfall_Settefrati))+
    geom_boxplot()+
    theme_classic()) # few data outside top whisker

## Checking for outliers statistically 
outr_canneto <- boxplot.stats(canneto_nomissing$Rainfall_Settefrati)$out

# Grubbs test 
grubbs.test(canneto_nomissing$Rainfall_Settefrati)
# max value seems to be an outlier - statistically speaking

outr_canneto_ind <- which(canneto_nomissing$Rainfall_Settefrati %in% c(outr_canneto))

df_outr_canneto <- canneto_nomissing[outr_canneto_ind,]

## Let's visualise outliers over time

# plot
(outr_canneto_vis <- ggplot(df_outr_canneto, aes(Date, Rainfall_Settefrati))+
    geom_point()+
    theme_classic())
# spread out over time
# keeping them all 

# Feature 2: Temperature 

(canneto_temp_box <- ggplot(canneto_filtered,
                            aes(y = Temperature_Settefrati))+
    geom_boxplot()+
    theme_classic()) # no outliers here 

## Checking statistically 
outt_canneto <- boxplot.stats(canneto_nomissing$Temperature_Settefrati)
outt_canneto
# no out 

## Grubbs test 
testt_canneto <- grubbs.test(canneto_nomissing$Temperature_Settefrati)
testt_canneto # confirmed stats - no out 


#### feature engineering ####

## adding new features: seasons and presence/absence of snow

canneto_featured <- add.seasons(canneto_nomissing) %>%
  rename(fl_rate.Ls = imp_flow_rate) %>% 
  mutate(snow.yes = as.factor(ifelse(Temperature_Settefrati <=0 & Rainfall_Settefrati > 0, 1,0)),
         snow.no = as.factor(ifelse(Temperature_Settefrati >0,1,0))) 

str(canneto_featured)

### changing effect of rain on target, and lagging the effect of rain on the target ###

canneto_orig <- canneto_featured %>% 
  mutate(lag1 = Lag(Rainfall_Settefrati, +1),
         lag3 = Lag(Rainfall_Settefrati,+3),
         lag5 = Lag(Rainfall_Settefrati,+5),
         lag7 = Lag(Rainfall_Settefrati,+7),
         lag9 = Lag(Rainfall_Settefrati, +9)) 

canneto_orig1 <- canneto_orig %>% 
  dplyr::select(-Date)

## creating 5 new datasets with different min rainfall levels 
## and with new time lags (trying to represent true effect of rain over target)

canneto0.5 <- canneto_orig %>% 
  mutate(rain0.5 = ifelse(Rainfall_Settefrati <= 0.5, 0, 
                          Rainfall_Settefrati),
         lag1 = Lag(rain0.5, +1),
         lag3 = Lag(rain0.5,+3),
         lag5 = Lag(rain0.5,+5),
         lag7 = Lag(rain0.5,+7),
         lag9 = Lag(rain0.5, +9)) %>% 
  dplyr::select(-Rainfall_Settefrati)

canneto0.5_1 <- canneto0.5 %>%  dplyr::select(-Date)

canneto1.5 <- canneto_orig %>% 
  mutate(rain1.5 = ifelse(Rainfall_Settefrati <= 1.5, 0, 
                          Rainfall_Settefrati),
         lag1 = Lag(rain1.5, +1),
         lag3 = Lag(rain1.5, +3),
         lag5 = Lag(rain1.5, +5),
         lag7 = Lag(rain1.5, +7),
         lag9 = Lag(rain1.5, +9)) %>% 
  dplyr::select(-Rainfall_Settefrati)

canneto1.5_1 <- canneto1.5 %>%   dplyr::select(-Date)

canneto3 <- canneto_orig %>% 
  mutate(rain3 = ifelse(Rainfall_Settefrati <= 3,0,
                        Rainfall_Settefrati),
         lag1 = Lag(rain3, +1),
         lag3 = Lag(rain3, +3),
         lag5 = Lag(rain3, +5),
         lag7 = Lag(rain3, +7),
         lag9 = Lag(rain3, +9)) %>% 
  dplyr::select(-Rainfall_Settefrati)

canneto3_1 <- canneto3 %>%  dplyr::select(-Date)

canneto5 <- canneto_orig %>% 
  mutate(rain5 = ifelse(Rainfall_Settefrati <= 5, 0, 
                        Rainfall_Settefrati),
         lag1 = Lag(rain5, +1),
         lag3 = Lag(rain5, +3),
         lag5 = Lag(rain5, +5),
         lag7 = Lag(rain5, +7),
         lag9 = Lag(rain5, +9)) %>%
  dplyr::select(-Rainfall_Settefrati)

canneto5_1 <- canneto5 %>%   dplyr::select(-Date)

## checking for missing data after filtering 
canneto_filtered_missing <- canneto_nomissing %>% 
  miss_var_summary()
print(canneto_filtered_missing)
# there are missing data for all variables but Date

## manipulating rainfall data (3bmeteo)
rain_canneto <- meteo_canneto %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("../input/madonna-di-canneto-3bmeteo", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = str_replace(date_final,"ago","08"),
         date_final = str_replace(date_final, "gen", "01"),
         date_final = str_replace(date_final, "feb", "02"),
         date_final = str_replace(date_final, "mar", "03"),
         date_final = str_replace(date_final, "apr", "04"),
         date_final = str_replace(date_final, "mag", "05"),
         date_final = str_replace(date_final, "giu", "06"),
         date_final = str_replace(date_final, "lug", "07"),
         date_final = str_replace(date_final, "set", "09"),
         date_final = str_replace(date_final, "ott", "10"),
         date_final = str_replace(date_final, "nov","11"),
         date_final = str_replace(date_final, "dic", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>%
  rename(Date = date_final,
         Rainfall_Settefrati = prec) %>%
  dplyr::select(Date, Rainfall_Settefrati)

#summary(rain_canneto)

## manipulating temperature data (3bmeteo)

temp_canneto <- meteo_canneto %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("../input/madonna-di-canneto-3bmeteo", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = str_replace(date_final,"ago","08"),
         date_final = str_replace(date_final, "gen", "01"),
         date_final = str_replace(date_final, "feb", "02"),
         date_final = str_replace(date_final, "mar", "03"),
         date_final = str_replace(date_final, "apr", "04"),
         date_final = str_replace(date_final, "mag", "05"),
         date_final = str_replace(date_final, "giu", "06"),
         date_final = str_replace(date_final, "lug", "07"),
         date_final = str_replace(date_final, "set", "09"),
         date_final = str_replace(date_final, "ott", "10"),
         date_final = str_replace(date_final, "nov","11"),
         date_final = str_replace(date_final, "dic", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>%
  rename(Date = date_final) %>%
  dplyr::select(Date, tmin, tmax) %>%
  mutate(Temperature_Settefrati = rowMeans(subset(., select = c(tmin,tmax)),
                                           na.rm = T)) %>%
  dplyr::select(-tmin, -tmax)

#summary(temp_canneto)

## Filling dataset to model with new meteorological data 

# temp
canneto_nomissing$Temperature_Settefrati[is.na(canneto_nomissing$Temperature_Settefrati)] <- temp_canneto$Temperature_Settefrati[match(canneto_nomissing$Date[is.na(canneto_nomissing$Temperature_Settefrati)],
                                                                                                                                       temp_canneto$Date)]

# rain 
canneto_nomissing$Rainfall_Settefrati[is.na(canneto_nomissing$Rainfall_Settefrati)] <- rain_canneto$Rainfall_Settefrati[match(canneto_nomissing$Date[is.na(canneto_nomissing$Rainfall_Settefrati)],
                                                                                                                              rain_canneto$Date)]

#summary(canneto_nomissing)


## Checking visually that all NAs have gone 

# Rainfall 
ggplot_na_distribution(canneto_nomissing$Rainfall_Settefrati)

# Temperature 
ggplot_na_distribution(canneto_nomissing$Temperature_Settefrati)


### outliers ###


## Checking distributions 

# Feature 1: Rainfall 

(canneto_rain_box <- ggplot(canneto_filtered,
                            aes(y = Rainfall_Settefrati))+
    geom_boxplot()+
    theme_classic()) # few data outside top whisker

## Checking for outliers statistically 
outr_canneto <- boxplot.stats(canneto_nomissing$Rainfall_Settefrati)$out

# Grubbs test 
grubbs.test(canneto_nomissing$Rainfall_Settefrati)
# max value seems to be an outlier - statistically speaking

outr_canneto_ind <- which(canneto_nomissing$Rainfall_Settefrati %in% c(outr_canneto))

df_outr_canneto <- canneto_nomissing[outr_canneto_ind,]

## Let's visualise outliers over time

# plot
(outr_canneto_vis <- ggplot(df_outr_canneto, aes(Date, Rainfall_Settefrati))+
    geom_point()+
    theme_classic())
# spread out over time
# keeping them all 

# Feature 2: Temperature 

(canneto_temp_box <- ggplot(canneto_filtered,
                            aes(y = Temperature_Settefrati))+
    geom_boxplot()+
    theme_classic()) # no outliers here 

## Checking statistically 
outt_canneto <- boxplot.stats(canneto_nomissing$Temperature_Settefrati)
outt_canneto
# no out 

## Grubbs test 
testt_canneto <- grubbs.test(canneto_nomissing$Temperature_Settefrati)
testt_canneto # confirmed stats - no out 


#### feature engineering ####

## adding new features: seasons and presence/absence of snow

canneto_featured <- add.seasons(canneto_nomissing) %>%
  rename(fl_rate.Ls = imp_flow_rate) %>% 
  mutate(snow.yes = as.factor(ifelse(Temperature_Settefrati <=0 & Rainfall_Settefrati > 0, 1,0)),
         snow.no = as.factor(ifelse(Temperature_Settefrati >0,1,0))) 

str(canneto_featured)

### changing effect of rain on target, and lagging the effect of rain on the target ###

canneto_orig <- canneto_featured %>% 
  mutate(lag1 = Lag(Rainfall_Settefrati, +1),
         lag3 = Lag(Rainfall_Settefrati,+3),
         lag5 = Lag(Rainfall_Settefrati,+5),
         lag7 = Lag(Rainfall_Settefrati,+7),
         lag9 = Lag(Rainfall_Settefrati, +9)) 

canneto_orig1 <- canneto_orig %>% 
  dplyr::select(-Date)

## creating 5 new datasets with different min rainfall levels 
## and with new time lags (trying to represent true effect of rain over target)

canneto0.5 <- canneto_orig %>% 
  mutate(rain0.5 = ifelse(Rainfall_Settefrati <= 0.5, 0, 
                          Rainfall_Settefrati),
         lag1 = Lag(rain0.5, +1),
         lag3 = Lag(rain0.5,+3),
         lag5 = Lag(rain0.5,+5),
         lag7 = Lag(rain0.5,+7),
         lag9 = Lag(rain0.5, +9)) %>% 
  dplyr::select(-Rainfall_Settefrati)

canneto0.5_1 <- canneto0.5 %>%  dplyr::select(-Date)

canneto1.5 <- canneto_orig %>% 
  mutate(rain1.5 = ifelse(Rainfall_Settefrati <= 1.5, 0, 
                          Rainfall_Settefrati),
         lag1 = Lag(rain1.5, +1),
         lag3 = Lag(rain1.5, +3),
         lag5 = Lag(rain1.5, +5),
         lag7 = Lag(rain1.5, +7),
         lag9 = Lag(rain1.5, +9)) %>% 
  dplyr::select(-Rainfall_Settefrati)

canneto1.5_1 <- canneto1.5 %>%   dplyr::select(-Date)

canneto3 <- canneto_orig %>% 
  mutate(rain3 = ifelse(Rainfall_Settefrati <= 3,0,
                        Rainfall_Settefrati),
         lag1 = Lag(rain3, +1),
         lag3 = Lag(rain3, +3),
         lag5 = Lag(rain3, +5),
         lag7 = Lag(rain3, +7),
         lag9 = Lag(rain3, +9)) %>% 
  dplyr::select(-Rainfall_Settefrati)

canneto3_1 <- canneto3 %>%  dplyr::select(-Date)

canneto5 <- canneto_orig %>% 
  mutate(rain5 = ifelse(Rainfall_Settefrati <= 5, 0, 
                        Rainfall_Settefrati),
         lag1 = Lag(rain5, +1),
         lag3 = Lag(rain5, +3),
         lag5 = Lag(rain5, +5),
         lag7 = Lag(rain5, +7),
         lag9 = Lag(rain5, +9)) %>%
  dplyr::select(-Rainfall_Settefrati)

canneto5_1 <- canneto5 %>%   dplyr::select(-Date)


### Checking correlations between features with correlation matrix ### 

# first changing factors to numeric values ... 
canneto_orig1[,4:9] <- unfactor(canneto_orig1[,4:9])
canneto0.5_1[,3:8] <- unfactor(canneto0.5_1[,3:8])
canneto1.5_1[,3:8] <- unfactor(canneto1.5_1[,3:8])
canneto3_1[,3:8] <- unfactor(canneto3_1[,3:8])
canneto5_1[,3:8] <- unfactor(canneto5_1[,3:8])

canneto_orig.cor <- canneto_orig1 %>% 
  cor(., use = "complete.obs") %>%
  corrplot(., method = "circle",
           tl.col = "black",tl.srt = 35, 
           tl.cex = 0.72,
           main = "\nMadonna di Canneto - original dataset ")


canneto0.5_cor <- canneto0.5_1 %>% 
  cor(., use = "complete.obs") %>% 
  corrplot(., method = "circle",
           tl.col = "black",tl.srt = 35, 
           tl.cex = 0.72, 
           main = "\nMadonna di Canneto - 0.5mm")

canneto1.5_cor <- canneto1.5_1 %>% 
  cor(., use = "complete.obs") %>% 
  corrplot(., method = "circle",
           tl.col = "black",tl.srt = 35, 
           tl.cex = 0.72,
           main = "\nMadonna di Canneto - 1.5mm")

canneto3_cor <- canneto3_1 %>% 
  cor(., use = "complete.obs") %>% 
  corrplot(., method = "circle",
           tl.col = "black",tl.srt = 35, 
           tl.cex = 0.72,
           main = "\nMadonna di Canneto - 3mm")

canneto5_cor <- canneto5_1 %>% 
  cor(., use = "complete.obs") %>% 
  corrplot(., method = "circle",
           tl.col = "black",tl.srt = 35, 
           tl.cex = 0.72,
           main = "\nMadonna di Canneto - 5mm")


#####------------------------------------------------------------------------------------------------------------------------------------------####
### Running stepwise ###

# Original dataset
canneto_orig.sw <- step.wisef("fl_rate.Ls", canneto_orig)
canneto_orig.sw$bestTune ## 11 features
coef(canneto_orig.sw$finalModel,11)

# 2 
canneto0.5.sw <- step.wisef("fl_rate.Ls",canneto0.5)
canneto0.5.sw$bestTune ## 8 features
coef(canneto0.5.sw$finalModel,8)

# 3
canneto1.5.sw <- step.wisef("fl_rate.Ls",canneto1.5)
canneto1.5.sw$bestTune ## 8 features
coef(canneto1.5.sw$finalModel,8) 

# 4 
canneto3.sw <- step.wisef("fl_rate.Ls",canneto3)
canneto3.sw$bestTune ## 6 features 
coef(canneto3.sw$finalModel,6)

# 5 
canneto5.sw <- step.wisef("fl_rate.Ls",canneto5)
canneto5.sw$bestTune ## 8 features
coef(canneto5.sw$finalModel,8)


#####----------------------------------------------------------------------------------------------------------------------------------------#####
### Running autoML from h2o package ###

library(h2o)

h2o.init()
h2o.no_progress()  # Turn off progress bars for notebook readability

## canneto_orig ##

canneto_orig.h2o <- as.h2o(canneto_orig)
#h2o.describe(canneto_orig.h2o)
y <- "fl_rate.Ls"
x <- setdiff(names(canneto_orig.h2o), c(y))

aml <- h2o.automl(y = y, x = x,
                  training_frame = canneto_orig.h2o,
                  max_models = 10,
                  seed = 1)

lb <- aml@leaderboard
#print(lb)

# Get model ids for all models in the AutoML Leaderboard
model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]
# Get the "All Models" Stacked Ensemble model
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
# Get the Stacked Ensemble metalearner model
metalearner <- h2o.getModel(se@model$metalearner$name)

h2o.varimp_plot(metalearner) # best model - gbm


## canneto original ##

res <- getBestModel(
  canneto_orig$Date[1:600],
  canneto_orig$fl_rate.Ls[1:600],
  "day",
  complete = 0,
  n_test = 7, # prediction horizon (i.e. n. of days)
  graph = TRUE,
  algos = list("my.prophet", "my.sarima","my.tbats",
               "my.bats", "my.stlm", "my.stlf","my.stl",
               "my.shortterm"),
  bagged = "auto",
  metric.error = my.rmse)

res$best # TBATS

# getting predictions of best model
pred <- res %>%
  my.predictions()

# plotting predicted VS Actual values
dates_test <- as.Date(canneto_orig$Date, format = "%Y-%m-%d")[501:507]
values_test <- canneto_orig$fl_rate.Ls[501:507]

ggplot() +
  geom_line(data=pred[!is.na(pred$sarima) & pred$type == "mean",], aes(x=dates, 
                                                                       y=sarima), 
            color = "blue") +
  geom_line(aes(x = dates_test, y = values_test), color = "red") +
  xlab('Dates') +
  ylab('Flow Rate Madonna di Canneto') 

### GBM ###

## Canneto original dataset - no features removed, no changes to minimum rainfall level. 

# testing and training split

set.seed(123)
canneto_orig.split <- initial_split(canneto_orig, prop = .7)
canneto_orig.train <- training(canneto_orig.split)
canneto_orig.test <- testing(canneto_orig.split)

canneto_orig.fit <- gbm::gbm(fl_rate.Ls ~ .,
                             data = canneto_orig,
                             verbose = T, 
                             shrinkage = 0.01,
                             interaction.depth = 3, 
                             n.minobsinnode = 5,
                             n.trees = 1000,
                             cv.folds = 12)

canneto_orig.fit.perf <- gbm.perf(canneto_orig.fit, method = "cv")

# make predictions 

canneto_orig.fit.pred <- stats::predict(object = canneto_orig.fit,
                                        newdata = canneto_orig.test,
                                        n.trees = canneto_orig.fit.perf)
canneto_orig.fit.rmse <- Metrics::rmse(actual = canneto_orig.test$fl_rate.Ls,
                                       predicted = canneto_orig.fit.pred)
print(canneto_orig.fit.rmse) # 26.59


# summarise model 

canneto_orig.effects <- tibble::as_tibble(gbm::summary.gbm(canneto_orig.fit,
                                                           plotit = F))
canneto_orig.effects %>% utils::head() 

### comparing with values suggested by stepwise ###
canneto_orig_tm <- canneto_orig %>% 
  dplyr::select(Rainfall_Settefrati,Temperature_Settefrati,
                Spring,Summer,lag1,lag7,
                fl_rate.Ls, Autumn,snow.no)

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
print(canneto_orig_tm.fit.rmse) 
# 28.83 - larger error than without removing some features. 

### USING CANNETO ORIGINAL - KEEPING ALL THE FEATURES.


### plotting model fit - canneto original ###

## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

canneto_orig.test$predicted <- as.integer(predict(canneto_orig.fit,
                                                  newdata = canneto_orig.test,
                                                  n.trees = canneto_orig.fit.perf))

# plot predicted vs actual

ggplot(canneto_orig.test) +
  geom_point(aes(x = predicted,
                 y = fl_rate.Ls,
                 color = predicted - fl_rate.Ls),
             alpha = .7, size = 2) +
  theme_classic()


## plotting top 6 features by importance

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
  theme_classic()+
  scale_fill_discrete(name = "Relative Influence")


### Time series forecasting - SARIMA ### This could be considered too --> univariate TS analysis though (only target var and date)


canneto_ts.fit <- Arima(window(canneto_ts, end = c(2020,7)), order = c(2,0,0),
                        seasonal = list(order = c(2,0,0), period = 12))

tsdisplay(residuals(canneto_ts.fit))

plot(forecast(canneto_ts.fit, h = 12))
lines(canneto_ts.fit)

forecast_fit <- forecast(canneto_ts.fit)

RMSE(canneto_ts.fit$mean,validation)  # RMSE = 7 --> better model than gbm...? Less variables considered.
MAPE(canneto_ts.fit$mean,validation) * 100 
