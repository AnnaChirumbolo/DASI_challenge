################################################################################
################################################################################
#######       we can use this script to prep the data for modelling         ####
################################################################################
################################################################################


### libraries

#install.packages("outliers")

library(tidyverse)
library(imputeTS)
library(ggplot2)
library(zoo)
library(data.table)
library(lubridate)
library(outliers)

###############
##############


#### DOGANELLA AQUIFER #####

doganella <- read.csv("processed_data/DOGANELLA_filtered.csv")


doganella1 <- doganella %>% 
  select(-X) %>%
  gather(key = "pozzi", value = "depth_to_gw.m",
         14:22) %>% 
  mutate(depth_to_gw.m = abs(depth_to_gw.m))

doganella <- doganella1 %>% 
  spread(key = "pozzi", value = "depth_to_gw.m")



# overall view of depth to gw changes by well type 
(depth_to_gw <- ggplot(doganella1, aes(Date, depth_to_gw.m,
                                       color = pozzi, group = pozzi))+
    geom_line(size = 2)+
    theme_classic())


### vis well by well, to fill in gaps 

## pozzo 1


(doganella_p1 <- ggplot(doganella, aes(Date, Pozzo_1, group = 1))+
    geom_line(size = 1)+
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

#doganella2 <- data.frame(doganella, lapply(doganella[,14:22], count.na))

#doganella3 <- data.frame(doganella2, lapply(doganella2[,14:22],
                                          #  function(x) if_else(is.na(x),1,0)))
  

## all this was to try and find a way to impute missing data

## imputeTS

ggplot_na_distribution(doganella$Pozzo_1)+
  theme_classic()

statsNA(doganella$Pozzo_1)

ggplot_na_intervals(doganella$Pozzo_1)

ggplot_na_gapsize(doganella$Pozzo_1)


## linear interpolation 

doganella4 <- data.frame(doganella, lapply(doganella[,14:22], na.approx))



#### FINAL ####

## using na.ma from imputeTS

doganella5 <- data.frame(doganella, lapply(doganella[,14:22], 
                                                     function(x) na_ma(x, k=1)))

doganella6 <-setnames(doganella5, old = colnames(doganella5[,23:31]),
                      new = c("imp1","imp2",
                                     "imp3","imp4",
                                     "imp5","imp6",
                                     "imp7","imp8",
                                     "imp9"))


rm(doganella7)

doganella7 <- doganella6 %>% 
  gather(key="imp", value = "imputed_depth_to_gw.m",23:31) %>%
  mutate(Date = ymd(Date))
  
str(doganella7)


## vis with imputation 

(imp_vis <- ggplot(doganella7, aes(Date, imputed_depth_to_gw.m, color = imp,
                                   group = imp))+
    geom_line()+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y"))


statsNA(doganella7$imputed_depth_to_gw.m)
## cleaned up - no more missing data 


#### checking for outliers ####

## summary

doganella8 <- doganella7 %>% 
  spread(key = "imp", value = "imputed_depth_to_gw.m") 
  
summary(doganella8[,23:31])

## hist 

(hist_doganella <- ggplot(doganella7, aes(imputed_depth_to_gw.m))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(vars(imp)))
  
  
## boxplots

  # target var 
(hist_target_doganella <- ggplot(doganella7,
                                 aes(y = imputed_depth_to_gw.m,
                                     color = imp))+
    geom_boxplot()+
    theme_classic())#+
    #facet_wrap(vars(imp)))
# saving
ggsave("img/hist_doganella_target.jpg", dpi = 500, width = 10, height=7)

#### checking singularly 

(imp2_doganella <- ggplot(doganella7, aes(y = Pozzo_2))+
    geom_boxplot()+
    theme_classic())

(imp4_doganella <- ggplot(doganella7, aes(y = Pozzo_4))+
    geom_boxplot()+
    theme_classic())

(imp8_doganella <- ggplot(doganella7, aes(y = Pozzo_8))+
    geom_boxplot()+
    theme_classic())

## extracting vals of potential outliers 

  # for pozzo 2
out2_dog <- boxplot.stats(doganella8$imp2)$out # 112.5 (repeated x4)
out2_ind_dog <- which(doganella8$imp2 %in% c(out))
out2_ind_dog # rows where outliers are found

upper_bound <- quantile(doganella8$imp2, 0.975)
upper_bound # 101.49

upper_bound99 <- quantile(doganella8$imp2, 0.99)
upper_bound99 # 101.67

## checking stats to verify it's an outlier
# grubbs test
test2 <- grubbs.test(doganella8$imp2)
test2 # 112.5 is an outlier (at 5% significance level)

## substituting with q3 value 
doganella8$imp2[doganella8$imp2 == 112.5] <- 99.21
summary(doganella8$imp2)

# for pozzo 4
out4_dog <- boxplot.stats(doganella8$imp4) # 108.66
out4_dog
out4_ind_dog <- which(doganella8$imp4 %in% c(out4_dog))
out4_ind_dog # line 158

  # grubbs test 
test4 <- grubbs.test(doganella8$imp4)
test4 # it is an outlier, 5% significance

  # substituting 
doganella8$imp4[doganella8$imp4 == 108.66] <- 100.52
summary(doganella8$imp4)

  # for pozzo 8
out8_dog <- boxplot.stats(doganella8$imp8) # 107.5 (rep x2)
out8_dog

  # grubbs test 
test8 <- grubbs.test(doganella8$imp8)
test8 # it is an outlier 
  
  # subst 
doganella8$imp8[doganella8$imp8 == 107.5] <- 97.96
summary(doganella8$imp8)


## pozzo 6 e 9

out6 <- boxplot.stats(doganella8$imp6)$out
out6_ind <- which(doganella8$imp6 %in% c(out6))
out6_ind

data6 <- doganella8[out6_ind,]

(data6_vis <- ggplot(data6, aes(y = imp6))+
    geom_boxplot()+
    theme_classic())

(data6_vis <- ggplot(data6, aes(Date, imp6))+
    geom_point()+
    theme_classic())


boxplot.stats(doganella8$imp9)


out9 <- boxplot.stats(doganella8$imp9)$out
out9_ind <- which(doganella8$imp9 %in% c(out9))
out9_ind

data9 <- doganella8[out9_ind,]

(data9_vis <- ggplot(data9, aes(Date, imp9))+
    geom_point()+
    theme_classic())

####################

doganella9 <- doganella8 %>% 
  gather(key = "imp", value = "depth_to_gw.m", 23:31)
View(doganella9)

(doganella_noout <- ggplot(doganella9, aes(y=depth_to_gw.m,color = imp))+
    geom_boxplot()+
    theme_classic())

# saving 

ggsave("img/doganella_boxplot.jpg", dpi = 500, width = 10, height = 7)



### keeping outliers for pozzi 6 and 9 - from most recent years, and there's 100+ according to the boxplot

### checking outliers for feature variables 

# doganella9

str(doganella9)


doganella10 <- doganella9 %>% 
  gather(key = "temp_sensor", value = "temp.C", Temperature_Monteporzio, 
         Temperature_Velletri) %>%
  gather(key = "rain_sensor", value = "rain.mm", Rainfall_Monteporzio,
         Rainfall_Velletri)


## vis hist

# temp

(temp_hist_dog <- ggplot(doganella10, aes(temp.C))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(vars(temp_sensor)))
# pretty similar distribution

# rain
(rain_hist_dog <- ggplot(doganella10, aes(rain.mm))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(vars(rain_sensor)))
  # pretty similar here as well


## vis boxplot

# temp 
(temp_box_dog <- ggplot(doganella10, aes(y=temp.C, color = temp_sensor))+
    geom_boxplot()+
    theme_classic())
  # no outliers 

# rain 
(rain_box_dog <- ggplot(doganella10, aes(y = rain.mm, color = rain_sensor))+
    geom_boxplot()+
    theme_classic())
   # like in histogram, there's a long tail, which the plot recognises as outliers 


### let's check it statistically 

out_tempv_dog <- boxplot.stats(doganella9$Temperature_Velletri)
out_tempv_dog
  # confirmed no outliers for temp velletri 

out_tempm_dog <- boxplot.stats(doganella9$Temperature_Monteporzio)
out_tempm_dog
  # no outliers for temp monteporzio

out_rainv_dog <- boxplot.stats(doganella9$Rainfall_Velletri)$out
out_rainv_dog
  # more than 900 outliers... 

out_rainm_dog <- boxplot.stats(doganella9$Rainfall_Monteporzio)$out
out_rainm_dog
  # same here 

## let's check where these "outliers" occur in time (rain)

  # monteporzio
out_rainm_dog_ind <- which(doganella9$Rainfall_Monteporzio %in% c(out_rainm_dog))
out_rainm_dog_ind

df_rainm_out <- doganella9[out_rainm_dog_ind,]
df_rainm_out

  # plotting it over time 
(plot_rainm_out <- ggplot(df_rainm_out, aes(Date, Rainfall_Monteporzio))+
    geom_point()+
    geom_line(data = doganella9, aes(Date, Rainfall_Monteporzio, color = "red"))+
    theme_classic())
  # it occurs throughout - from 2012 through to 2020


  # velletri 
out_rainv_dog_ind <- which(doganella9$Rainfall_Velletri %in% c(out_rainv_dog))
out_rainv_dog_ind
length(out_rainv_dog_ind)

df_rainv_dog <- doganella9[out_rainv_dog_ind,]
View(df_rainv_dog)

  # plotting over time 
(plot_rainv_dog <- ggplot(doganella9, aes(Date, Rainfall_Velletri))+
    geom_line(color = "red")+
    geom_point(data = df_rainv_dog, aes(Date, Rainfall_Velletri))+
    theme_classic())

(plot_rainv_dog <- ggplot(df_rainv_dog, aes(Date, Rainfall_Velletri))+
    geom_point()+
    theme_classic())

#### decision to leave them as such 


#### checking out for missing - rain and temp ####

# rain
statsNA(doganella9$Rainfall_Monteporzio)

ggplot_na_distribution(doganella9$Rainfall_Monteporzio)

statsNA(doganella9$Rainfall_Velletri)

ggplot_na_distribution(doganalla9$Rainfall_Velletri)

(rain_dog <- ggplot(doganella10, aes(Date, rain.mm, color = rain_sensor))+
    geom_line()+
    theme_classic())

# temp 
statsNA(doganella9$Temperature_Monteporzio)

ggplot_na_distribution(doganella9$Temperature_Monteporzio)

statsNA(doganella9$Temperature_Velletri)

ggplot_na_distribution(doganella9$Temperature_Velletri)

#### filling in missing data - rain and temp - with na_ma (imputeTS) ####

doganella11 <- doganella9 %>% 
  mutate(imp_rain_velletri = na_ma(Rainfall_Velletri, k = 1),
         imp_rain_monteporzio = na_ma(Rainfall_Monteporzio, k = 1),
         imp_temp_velletri = na_ma(Temperature_Velletri, k = 1, maxgap = 40),
         imp_temp_monteporzio = na_ma(Temperature_Monteporzio, k = 1, maxgap = 40))

summary(doganella11$imp_rain_monteporzio)
summary(doganella11$imp_rain_velletri)
summary(doganella11$imp_temp_monteporzio) # longer gaps than 40 rows still na
summary(doganella11$imp_temp_velletri) # longer gaps than 40 rows still na

#### need to look for meteo data to fill in these gaps!!!
#########################################################

## vis distrib data with imputed vars 

doganella12 <- doganella11 %>% 
  select(Date, imp_rain_velletri, imp_rain_monteporzio, 
         imp_temp_monteporzio, imp_temp_velletri) %>% 
  gather(key = "temp", value = "temp.C", imp_temp_monteporzio, imp_temp_velletri) %>% 
  gather(key = "rain", value = "rain.mm", imp_rain_velletri, imp_rain_monteporzio)

(imp_temp_dog <- ggplot(doganella12, aes(Date, temp.C, color = temp))+
    geom_line()+
    theme_classic())# major gaps left 

  # comparing to dataset before imputation
(temp_dog <- ggplot(doganella10, aes(Date, temp.C, color = temp_sensor))+
    geom_line()+
    theme_classic())


(imp_rain_dog <- ggplot(doganella12, aes(Date, rain.mm, color = rain))+
    geom_line()+
    theme_classic())

  # comparing to dataset before imputation
(rain_dog <- ggplot(doganella10, aes(Date, rain.mm, color = rain_sensor))+
    geom_line()+
    theme_classic())


#### looking at last feature: volume ####

# volume is of the water pumped OUT from the drinking water plant 
# thus removed from the aquifer

doganella_vol <- doganella %>% 
  select(Date, Volume_Pozzo_1: Volume_Pozzo_9)  %>%
  gather(key = "pozzo", value = "volume.mc", -Date) %>%
  mutate(Date = ymd(Date))
str(doganella_vol)

(vol_dog <- ggplot(doganella_vol, aes(Date, volume.mc, color = pozzo,
                                      group = pozzo))+
    geom_line()+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y"))

# boxplots 

(vol_box_dog <- ggplot(doganella_vol, aes(y = volume.mc, color = pozzo))+
    geom_boxplot()+
    theme_classic())

# hist 
(vol_hist_dog <- ggplot(doganella_vol, aes(volume.mc))+
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

#### saving dataset as it is so far ####

write.csv(doganella11,"processed_data/DOGANELLA_to_model.csv")



#### LUPA WATER SPRING ####


lupa <- read.csv("processed_data/LUPA_filtered.csv")

str(lupa)

lupa1 <- lupa %>%
  select(-X, -Flow_Rate_Lupa) %>%
  mutate(Date = ymd(Date))

summary(lupa1)

statsNA(lupa1$abs.flow_rate)
ggplot_na_distribution(lupa1$abs.flow_rate)


(hist_rain_lupa <- ggplot(lupa1, aes(Rainfall_Terni))+
    geom_histogram()+
    theme_classic())


(box_rain_lupa <- ggplot(lupa1, aes(y = Rainfall_Terni))+
    geom_boxplot()+
    theme_classic())


#### dealing with missing first - target ####

lupa2 <- lupa1 %>% 
  mutate(imp_flow_rate = na_ma(abs.flow_rate, k = 1))

# vis 

(imp_lupa <- ggplot(lupa2, aes(Date, imp_flow_rate, group = 1))+
    geom_line()+
    theme_classic())
  # goodies 

#### dealing with outliers - target ####


# hist 

(hist_lupa <- ggplot(lupa2, aes(imp_flow_rate))+
   geom_histogram()+
   theme_classic())


# boxplot

(box_lupa <- ggplot(lupa2, aes(y = imp_flow_rate))+
    geom_boxplot()+
    theme_classic())


# statistically 

out_lupa <- boxplot.stats(lupa2$imp_flow_rate)$out
out_lupa

out_lupa_ind <- which(lupa2$imp_flow_rate %in% c(out_lupa))
out_lupa_ind

  # checking where these outliers occur in time 
df_out_lupa <- lupa2[out_lupa_ind,]
View(df_out_lupa)

  # plotting 

(vis_out_lupa <- ggplot(df_out_lupa, aes(Date, imp_flow_rate))+
    geom_point()+
    theme_classic()) # 2009 - outliers... is there a particular reason? 

# grubbs test 

test_lupa <- grubbs.test(lupa2$imp_flow_rate)
test_lupa
  # will just change value approx 0 


boxplot.stats(lupa2$imp_flow_rate) # lower quartile 86.26

lupa2$imp_flow_rate[lupa2$imp_flow_rate == 0] <- 86.26

summary(lupa2$imp_flow_rate)

## substituted lowest value = 0, to lower quartile value

## checking with boxplot 

(out_vis_lupa <- ggplot(lupa2, aes(y = imp_flow_rate))+
    geom_boxplot()+
    theme_classic())

  # all the upper values above whisket stay 

## vis distribution over time 
(noout_lupa <- ggplot(lupa2, aes(Date, imp_flow_rate))+
    geom_line()+
    theme_classic())


summary(lupa2)

## saving dataset 

lupa3 <- lupa2 %>% select(-abs.flow_rate) %>% 
  write.csv("processed_data//LUPA_to_model.csv")




















