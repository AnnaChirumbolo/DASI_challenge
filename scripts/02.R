################################################################################
################################################################################
############ DATA PREP FOR MODELLING            ################################
################################################################################
################################################################################


#### libraries ####

#install.packages("outliers")

library(tidyverse)
 library(imputeTS)
library(ggplot2)
library(zoo)
library(data.table)
library(lubridate)
library(outliers)
#install.packages("tidyselect")
library(tidyselect)

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


#### FILLING GAPS WITH METEO ####

#### temperature Velletri

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

temp_dog_ls <- list.files(path = "./data/DOGANELLA_3BMETEO/",
                          pattern = "*.csv$", 
                          full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating temp data 
temp_dog <- temp_dog_ls %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/DOGANELLA_3BMETEO/", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = str_replace(date_final,"ago","08"),
         date_final = str_replace(date_final, "gen", "01"),
         date_final = str_replace(date_final, "feb", "02"),
         date_final = str_replace(date_final, "mar", "03"),
         date_final = str_replace(date_final, "apr", "04"),
         date_final = str_replace(date_final, "mag", "05"),
         date_final = str_replace(date_final, "giu", "06"),
         date_final = str_replace(date_final, "lug", "07"),
         date_final = str_replace(date_final, "sett", "09"),
         date_final = str_replace(date_final, "ott", "10"),
         date_final = str_replace(date_final, "nov","11"),
         date_final = str_replace(date_final, "dec", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>%
  rename(Date = date_final) %>%
  select(Date, tmin, tmax) %>%
  mutate(Temperature_Velletri = rowMeans(subset(., select = c(tmin,tmax)),
                                         na.rm = T)) %>%
  select(-tmin, -tmax) %>%
  arrange(Date)

#summary(temp_dog)

  # visualising trend in newly added temperature 

(vis_temp_dog <- ggplot(temp_dog, aes(Date, Temperature_Velletri))+
    geom_line()+
    theme_classic())

#### temperature Monteporzio

temp_monte_dog_ls <- list.files(path = "./data/DOGANELLA_MONTEPORZIO_3BMETEO/",
                          pattern = "*.csv$", 
                          full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating temp data 
temp_monte_dog <- temp_monte_dog_ls %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/DOGANELLA_MONTEPORZIO_3BMETEO/", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = str_replace(date_final,"ago","08"),
         date_final = str_replace(date_final, "gen", "01"),
         date_final = str_replace(date_final, "feb", "02"),
         date_final = str_replace(date_final, "mar", "03"),
         date_final = str_replace(date_final, "apr", "04"),
         date_final = str_replace(date_final, "mag", "05"),
         date_final = str_replace(date_final, "giu", "06"),
         date_final = str_replace(date_final, "lug", "07"),
         date_final = str_replace(date_final, "sett", "09"),
         date_final = str_replace(date_final, "ott", "10"),
         date_final = str_replace(date_final, "nov","11"),
         date_final = str_replace(date_final, "dec", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>%
  rename(Date = date_final) %>%
  select(Date, tmin, tmax) %>%
  mutate(Temperature_Monteporzio = rowMeans(subset(., select = c(tmin,tmax)),
                                         na.rm = T)) %>%
  select(-tmin, -tmax)

summary(temp_monte_dog)
summary(temp_dog)

# vis of newly added temp data 

(vis_new_temp_monte <- ggplot(temp_monte_dog, aes(Date, Temperature_Monteporzio))+
    geom_line()+
    theme_classic())

#### adding new temp data to prev dataset ####

View(doganella10)

doganella_new <- doganella10 %>%
  spread(temp_sensor, temp.C) %>%
  spread(rain_sensor, rain.mm) 
  
doganella_new$Temperature_Velletri[is.na(doganella_new$Temperature_Velletri)] <- temp_dog$Temperature_Velletri[match(doganella_new$Date[is.na(doganella_new$Temperature_Velletri)],
                                                                                                                     temp_dog$Date)]
(filled_temp_velletri <- ggplot(doganella_new, aes(Date, Temperature_Velletri))+
    geom_line()+
    theme_classic())

doganella_new$Temperature_Monteporzio[is.na(doganella_new$Temperature_Monteporzio)] <- temp_monte_dog$Temperature_Monteporzio[match(doganella_new$Date[is.na(doganella_new$Temperature_Monteporzio)],
                                                                                                                                    temp_monte_dog$Date)]

(filled_temp_monteporzio <- ggplot(doganella_new, aes(Date, Temperature_Monteporzio))+
    geom_line()+
    theme_classic())

#### filled in the data!!
statsNA(doganella_new$Temperature_Monteporzio) # still one na missing x9
statsNA(doganella_new$Temperature_Velletri) # one row na missing x9

## imputing those vars 

doganella11 <- doganella_new %>% 
  mutate(imp_rain_velletri = na_ma(Rainfall_Velletri, k = 1), # rain
         imp_rain_monteporzio = na_ma(Rainfall_Monteporzio, k = 1)) # rain

summary(doganella11$imp_rain_monteporzio)
summary(doganella11$imp_rain_velletri)

statsNA(doganella11$Temperature_Monteporzio)
ggplot_na_distribution(doganella11$Temperature_Monteporzio)

View(doganella11[is.na(doganella11$Temperature_Monteporzio),])# one day 

doganella11$Temperature_Monteporzio <- na_ma(doganella11$Temperature_Monteporzio, k =1)

doganella11$Temperature_Monteporzio[doganella$Date == "2019-12-31"] # ok

statsNA(doganella11$Temperature_Monteporzio)

View(doganella11[is.na(doganella11$Temperature_Velletri),])# one day 

doganella11$Temperature_Velletri <- na_ma(doganella11$Temperature_Velletri, k = 1)
doganella11$Temperature_Velletri[doganella$Date == "2019-12-31"] # ok

## vis distrib data with imputed vars 

ggplot(doganella11, aes(Date, Temperature_Monteporzio))+
  geom_line()+
  geom_line(data = doganella11, aes(Date, Temperature_Velletri,
                                    color = "red"))+
  theme_classic()

ggplot(doganella11, aes(Date, Rainfall_Monteporzio))+
  geom_line()+
  geom_line(data = doganella11, aes(Date, Rainfall_Velletri,
                                    color = "red"))+
  theme_classic()

## perfettooooooo

################################################################################
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


###################################################################################
###################################################################################



#### MADONNA DI CANNETO WATER SPRING ####

# loading file 
canneto <- read.csv("processed_data/MADONNA_DI_CANNETO_filtered.csv")

str(canneto)

canneto1 <- canneto %>% 
  mutate(Date = ymd(Date),
         flow_rate = abs(Flow_Rate_Madonna_di_Canneto)) %>%
  select(-X, -Flow_Rate_Madonna_di_Canneto)


str(canneto1)

summary(canneto1)

#### checking for missing ####

statsNA(canneto1$flow_rate)

(vis_canneto <- ggplot(canneto1, aes(Date, flow_rate))+
    geom_line()+
    theme_classic())

ggplot_na_distribution(canneto1$flow_rate)


# rain 

statsNA(canneto1$Rainfall_Settefrati) # massive gap

(vis_rain_canneto <- ggplot(canneto1, aes(Date, Rainfall_Settefrati))+
    geom_line()+
    theme_classic()) # last years of missing data... damn it

ggplot_na_distribution(canneto1$Rainfall_Settefrati)
# yep... damn it 


# temp 

statsNA(canneto1$Temperature_Settefrati)

(vis_temp_canneto <- ggplot(canneto1, aes(Date, Temperature_Settefrati))+
    geom_line()+
    theme_classic()) # same thing for temperature 

ggplot_na_distribution(canneto1$Temperature_Settefrati)

#### FILLING IN WITH METEO DATA FROM 3B METEO ####

meteo_canneto <- list.files(path = "./data/MADONNA_DI_CANNETO_3BMETEO/",
                                pattern = "*.csv$", 
                                full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain data 
rain_canneto <- meteo_canneto %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/MADONNA_DI_CANNETO_3BMETEO/canneto", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  select(-weekday) %>%
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
  select(Date, Rainfall_Settefrati)

temp_canneto <- meteo_canneto %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/MADONNA_DI_CANNETO_3BMETEO/canneto", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  select(-weekday) %>%
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
  select(Date, tmin, tmax) %>%
  mutate(Temperature_Settefrati = rowMeans(subset(., select = c(tmin,tmax)),
                                            na.rm = T)) %>%
  select(-tmin, -tmax)

#### fixing missing data for target ####

canneto2 <- canneto1 %>% 
  mutate(imp_flow_rate = na_ma(flow_rate))
summary(canneto2)

(imp_canneto <- ggplot(canneto2, aes(Date, imp_flow_rate))+
    geom_line()+
    theme_classic()) # not ideal - particularly for massive gap of 100+ rows ...
# but it's the best option i guess



## filling dataset to model with new meteo data 

# temp
canneto2$Temperature_Settefrati[is.na(canneto2$Temperature_Settefrati)] <- temp_canneto$Temperature_Settefrati[match(canneto2$Date[is.na(canneto2$Temperature_Settefrati)],
                                                                                                                     temp_canneto$Date)]

# rain 
canneto2$Rainfall_Settefrati[is.na(canneto2$Rainfall_Settefrati)] <- rain_canneto$Rainfall_Settefrati[match(canneto2$Date[is.na(canneto2$Rainfall_Settefrati)],
                                                                                                            rain_canneto$Date)]


## checking all nas have been filled 

statsNA(canneto2$Rainfall_Settefrati) # no more nas in the timeseries!!!
statsNA(canneto2$Temperature_Settefrati) # no more nas in the timeseries!!!
# yaass

## checking visually 

ggplot_na_distribution(canneto2$Rainfall_Settefrati)
ggplot_na_distribution(canneto2$Temperature_Settefrati)
# double yaasss


#### outliers ####

# target

  # hist 
(hist_canneto <- ggplot(canneto2, aes(imp_flow_rate))+
   geom_histogram()+
   theme_classic())

  # boxplot
(box_canneto <- ggplot(canneto2, aes(y = imp_flow_rate))+
    geom_boxplot()+
    theme_classic())
  # no outliers visually 

  # stats
out_canneto <- boxplot.stats(canneto2$imp_flow_rate)
out_canneto
  # confirmed no out 

  # grubbs test 
test_canneto <- grubbs.test(canneto2$imp_flow_rate)
test_canneto # confirmed no out 

# rain 

  # hist 
(histr_canneto <- ggplot(canneto2, aes(Rainfall_Settefrati))+
    geom_histogram()+
    theme_classic())
  # a bit of tail 

  # boxplot 
(boxr_canneto <- ggplot(canneto2, aes(y = Rainfall_Settefrati))+
    geom_boxplot()+
    theme_classic())

  # stats 
outr_canneto <- boxplot.stats(canneto2$Rainfall_Settefrati)$out
outr_canneto

  # grubbs test 
outr_canneto_test <- grubbs.test(canneto2$Rainfall_Settefrati)
outr_canneto_test # max value seems to be an outlier - statistically speaking

  # let's visualise outliers over time 
outr_canneto_ind <- which(canneto2$Rainfall_Settefrati %in% c(outr_canneto))
outr_canneto_ind

df_outr_canneto <- canneto2[outr_canneto_ind,]
View(df_outr_canneto)

    # plot
(outr_canneto <- ggplot(df_outr_canneto, aes(Date, Rainfall_Settefrati))+
    geom_point()+
    theme_classic())
  # spread out over time
  # keeping them all 


# temp 

  # hist 
(histt_canneto <- ggplot(canneto2, aes(Temperature_Settefrati))+
    geom_histogram()+
    theme_classic())

  # boxplot
(boxt_canneto <- ggplot(canneto2, aes(y = Temperature_Settefrati))+
    geom_boxplot()+
    theme_classic())
  # no out 

  # checking statistically 
outt_canneto <- boxplot.stats(canneto2$Temperature_Settefrati)
outt_canneto
  # no out 

  # grubbs test 
testt_canneto <- grubbs.test(canneto2$Temperature_Settefrati)
testt_canneto # confirmed stats - no out 


#### saving (so far) ####

canneto3 <- canneto2 %>% 
  select(-flow_rate) %>%
  write.csv("processed_data/MADONNA_DI_CANNETO_to_model.csv")



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
           Winter = factor(ifelse(Month_day >= "12-21" & Month_day < "3-21",
                                  1,0))) %>%
    select(-Month_day) %>% 
    ungroup()
  return(seasons)
}

library(caret)


#### doganella ####

doganella <- read.csv("processed_data/DOGANELLA_to_model.csv")


doganella_featured <- add.seasons(doganella) %>%
  dplyr::select(-X,-Pozzo_1:-Pozzo_9,-Rainfall_Monteporzio,-Rainfall_Velletri) %>% 
  spread(key = imp, value=depth_to_gw.m) %>% 
  mutate(snow.yes = as.factor(ifelse(Temperature_Monteporzio <=0 | Temperature_Velletri <=0,1,0)),
         snow.no = as.factor(ifelse(Temperature_Monteporzio > 0 | Temperature_Velletri >0, 1, 0))) %>% 
  write.csv(., "processed_data/DOGANELLA_to_model.csv")
  
#### lupa ####

lupa <- read.csv("processed_data/LUPA_to_model.csv")

str(lupa)

lupa_featured <- add.seasons(lupa) %>%
  select(-X) %>% 
  rename(fl_rate.Ls = imp_flow_rate) %>% ## there's only rainfall as feature - can't assume when 0 = snow 
  write.csv(., "processed_data/LUPA_to_model.csv")

#### canneto ####

canneto <- read.csv("processed_data/MADONNA_DI_CANNETO_to_model.csv")

str(canneto)
canneto_featured <- add.seasons(canneto) %>%
  select(-X) %>%
  rename(fl_rate.Ls = imp_flow_rate) %>% 
  mutate(snow.yes = as.factor(ifelse(Temperature_Settefrati <=0, 1,0)),
         snow.no = as.factor(ifelse(Temperature_Settefrati >0,1,0))) %>% 
  write.csv(., "processed_data/MADONNA_DI_CANNETO_to_model.csv")

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
canneto <- read.csv("processed_data/MADONNA_DI_CANNETO_to_model.csv")

str(canneto)
  # summary when rain isn't 0 
summary(canneto$Rainfall_Settefrati[!canneto$Rainfall_Settefrati == 0])

ggplot(canneto,aes(Date, fl_rate.Ls))+
  geom_line() +
  geom_line(data = canneto, aes(Date, Rainfall_Settefrati, color = "red"))

ggplot(canneto,aes(Rainfall_Settefrati, fl_rate.Ls))+
  geom_point()


# creating quarters, semesters and trimonthly data 

canneto_months <- canneto %>% 
  mutate(Y_m = as.Date(Date, format ="%Y-%m"),
         Semester = semester(Date, with_year = T),
         Quarters = quarter(Date, with_year = T),
         Trimonthly = as.factor(round_date(Y_m, unit = "3 months"))) %>% 
                      # date written is first day of the period
  select(-Y_m) %>%
  group_by(Trimonthly) %>% 
  mutate(Fl_rate.Tri = mean(fl_rate.Ls)) %>% 
  ungroup() %>% 
  group_by(Quarters) %>% 
  mutate(Fl_rate.Quar = mean(fl_rate.Ls)) %>% 
  ungroup() %>% 
  group_by(Semester) %>% 
  mutate(Fl_rate.Sem = mean(fl_rate.Ls)) %>% 
  ungroup()

unique(canneto_months$Trimonthly)

min(canneto_months$Fl_rate.Tri)

# vis trimesters
ggplot(canneto_months, aes(Trimonthly, Fl_rate.Tri, fill = Year))+
  geom_bar(stat = "identity")

# vis quarters 
ggplot(canneto_months, aes(Quarters, Fl_rate.Quar, fill = Year))+
  geom_bar(stat = "identity")

# vis semesters
ggplot(canneto_months, aes(Semester, Fl_rate.Sem, fill = Year))+
  geom_bar(stat = "identity")

write.csv(canneto_months, "processed_data/MADONNA_DI_CANNETO_to_model.csv")

####
### checking for rainfall ###
## changing mm levels 

#ggplot(canneto_rain, aes(y = Rainfall_Settefrati)) +
#geom_boxplot()

## 5 datasets with 5 levels of min rain changed to 0:

canneto_rain.5 <- canneto_months %>% 
  mutate(rain1 = ifelse(Rainfall_Settefrati <= 0.5, 0, Rainfall_Settefrati),
         seq.rain.val = sequence(rle(as.character(rain1))$lengths))

canneto_rain1.5 <- canneto_months %>%  # whenever rain is lower than 1mm/day, = 0
  mutate(rain2 = ifelse(Rainfall_Settefrati <= 1.5, 0, Rainfall_Settefrati),
         seq.rain.val = sequence(rle(as.character(rain2))$lengths))

canneto_rain3 <- canneto_months %>% 
  mutate(rain3 = ifelse(Rainfall_Settefrati <= 3,0,Rainfall_Settefrati),
         seq.rain.val = sequence(rle(as.character(rain3))$lengths))

canneto_rain5 <- canneto_months %>% 
  mutate(rain4 = ifelse(Rainfall_Settefrati <= 5, 0, Rainfall_Settefrati),
         seq.rain.val = sequence(rle(as.character(rain4))$lengths))

canneto_rain7 <- canneto_months %>% 
  mutate(rain5 = ifelse(Rainfall_Settefrati <=7, 0, Rainfall_Settefrati),
         seq.rain.val = sequence(rle(as.character(rain5))$lengths))


## creating 5 new datasets per dataset...
## ... or 5 new variables 

#install.packages("Hmisc")
library(Hmisc)

canneto_rain.5.lag <- canneto_rain.5 %>% 
  mutate(lag1 = Lag(rain1, +1),
         lag3 = Lag(rain1,+3),
         lag5 = Lag(rain1,+5),
         lag7 = Lag(rain1,+7),
         lag9 = Lag(rain1, +9)) %>%
  write.csv(., "processed_data/canneto_rain0.5.csv")

canneto_rain1.5.lag <- canneto_rain1.5 %>% 
  mutate(lag1 = Lag(rain2, +1),
         lag3 = Lag(rain2, +3),
         lag5 = Lag(rain2, +5),
         lag7 = Lag(rain2, +7),
         lag9 = Lag(rain2, +9))%>% 
  write.csv(., "processed_data/canneto_rain1.5.csv")

canneto_rain3.lag <- canneto_rain3 %>% 
  mutate(lag1 = Lag(rain3, +1),
         lag3 = Lag(rain3, +3),
         lag5 = Lag(rain3, +5),
         lag7 = Lag(rain3, +7),
         lag9 = Lag(rain3, +9)) %>% 
  write.csv(., "processed_data/canneto_rain3.csv")

canneto_rain5.lag <- canneto_rain5 %>% 
  mutate(lag1 = Lag(rain4, +1),
         lag3 = Lag(rain4, +3),
         lag5 = Lag(rain4, +5),
         lag7 = Lag(rain4, +7),
         lag9 = Lag(rain4, +9)) %>% 
  write.csv(., "processed_data/canneto_rain5.csv")

canneto_rain7.lag <- canneto_rain7 %>% 
  mutate(lag1 = Lag(rain5, +1),
         lag3 = Lag(rain5, +3),
         lag5 = Lag(rain5, +5),
         lag7 = Lag(rain5, +7),
         lag9 = Lag(rain5, +9)) %>% 
  write.csv(., "processed_data/canneto_rain7.csv")


#### spearman correlations ####

## cannetorain1 ##

#lag1 
lag1 <- cor(canneto_rain1.lag$rain1,canneto_rain1.lag$fl_rate.Ls, method = "spearman")
lag2 <- cor(canneto_rain1.lag$lag1, canneto_rain1.lag$fl_rate.Ls, method = "spearman",
            use = "complete.obs")
lag3 <- cor(canneto_rain1.lag$lag3, canneto_rain1.lag$fl_rate.Ls, method = "spearman",
            use = "complete.obs")
lag4 <- cor(canneto_rain1.lag$lag5, canneto_rain1.lag$fl_rate.Ls, method = "spearman",
            use = "complete.obs")
lag5 <- cor(canneto_rain1.lag$lag7, canneto_rain1.lag$fl_rate.Ls, method = "spearman",
            use = "complete.obs")
lag6 <- cor(canneto_rain1.lag$lag9, canneto_rain1.lag$fl_rate.Ls, method = "spearman",
            use = "complete.obs")

