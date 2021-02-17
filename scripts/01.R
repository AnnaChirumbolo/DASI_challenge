################################################################################
################################################################################
###############   Anna Chirumbolo           ####################################
###############   Script prep of vars       ####################################
################################################################################
################################################################################


#### uploading libraries ####

library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)

#### uploading files ####

doganella <- read.csv("data/Aquifer_Doganella.csv")
lupa <- read.csv("data/Water_Spring_Lupa.csv")
canneto <- read.csv("data/Water_Spring_Madonna_di_Canneto.csv")



#### data cleaning ####

#### doganella ####

str(doganella)

summary(doganella)

doganella_missing <- doganella %>% 
  miss_var_summary()
print(doganella_missing)

View(doganella_missing)


doganella1 <- doganella %>% 
  gather(key = "well", value = "depth_to_gw.m", 4:12) %>%
  rename(Date = ï..Date) %>% 
  mutate(Date = dmy(Date),
         well = gsub("Depth_to_Groundwater_","",well))

(first_look <- ggplot(doganella1, aes(x =Date, y = abs(depth_to_gw.m), color = well))+
    geom_line(size = .5)+
    theme_classic())

### missing data clearly starting from 

## removing missing for depth to gw

doganella_filtered <- doganella1 %>% 
  filter(Date >= "2012-06-01")

(second_look <- ggplot(doganella_filtered, aes(Date, abs(depth_to_gw.m), 
                                               color = well))+
    geom_line(size = .5)+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    ylab("Abs. depth to groundwater (m)")+
    xlab(""))

ggsave("img/DOGANELLA_filtered_depth_to_groundwater.jpg", 
       dpi = 500, height = 5, width = 10)



### scatter correlations ###

## temp ##

min(doganella_filtered$Temperature_Monteporzio,na.rm = T)
max(doganella_filtered$Temperature_Monteporzio,na.rm = T)
min(doganella_filtered$Temperature_Velletri,na.rm = T)
max(doganella_filtered$Temperature_Velletri,na.rm = T)

(scatter_temp_velletri <- ggplot(doganella_filtered, 
                                 aes(x = Temperature_Velletri,
                                     y = abs(depth_to_gw.m),
                                     color = well))+
    geom_point(size = 1, alpha = 0.8)+
    theme_classic()+
    xlim(0,31))
    
  # from the plot there doesnt seem to be any correlation.... 


(scatter_temp_monteporzio <- ggplot(doganella_filtered,
                                    aes(Temperature_Monteporzio, 
                                        abs(depth_to_gw.m),
                                        color = well))+
    geom_point(size = 1, alpha = 0.8)+
    theme_classic()+
    xlim(0,31))
  # same for monteporzio... 

## rain ##

min(doganella_filtered$Rainfall_Monteporzio,na.rm = T)
max(doganella_filtered$Rainfall_Monteporzio,na.rm = T)
min(doganella_filtered$Rainfall_Velletri,na.rm = T)
max(doganella_filtered$Rainfall_Velletri,na.rm = T)

(scatter_rain_velletri <- ggplot(doganella_filtered, 
                                 aes(Rainfall_Velletri,
                                     abs(depth_to_gw.m),
                                     color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))


(scatter_rain_monteporzio <- ggplot(doganella_filtered,
                                    aes(Rainfall_Monteporzio,
                                        abs(depth_to_gw.m),
                                        color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))




### saving plots ###

panelled_rain_scatter <- ggarrange(scatter_rain_monteporzio,
                                   scatter_rain_velletri)
panelled_rain_scatter


ggsave("img/DOGANELLA_corr_rain.png",
       dpi = 500, height = 10, width = 18)



panelled_temp_scatter <- ggarrange(scatter_temp_monteporzio,
                                   scatter_temp_velletri)
panelled_temp_scatter

ggsave("img/DOGANELLA_corr_temp.png",
       dpi = 500, height= 10, width = 18)


### saving doganella .csv 

View(doganella_filtered)


# spreading values again 

doganella_filtered_save <- doganella_filtered %>% 
  spread(well,depth_to_gw.m)

# saving 

write.csv(doganella_filtered_save, "processed_data/DOGANELLA_filtered.csv")

################################################################################
################################################################################


#### lupa ####
  # water spring 


str(lupa)

summary(lupa)


lupa1 <- lupa %>% 
  rename(Date = ï..Date) %>% 
  mutate(Date = dmy(Date))

str(lupa1)

lupa_missing <- lupa1 %>% 
  miss_var_summary()

print(lupa_missing)
# only flow rate, target var has missing data, only 9% of total


(vis_missing_lupa <- ggplot(lupa1, aes(Date, Flow_Rate_Lupa))+
    geom_line(size = 1)+
    theme_classic())

## removing missing 

lupa_filtered <- lupa1 %>% 
  filter(Date >= "2009-02-19") %>%
  mutate(abs.flow_rate = abs(Flow_Rate_Lupa)) # changed flow to absolute value


(vis_lupa <- ggplot(lupa_filtered, 
                    aes(Date, abs.flow_rate))+
    geom_line(size = 1)+
    theme_classic())

hist(lupa_filtered$abs.flow_rate)


## other vis - scatter to see nature of odd line towards 0

(scatter_lupa <- ggplot(lupa_filtered, 
                        aes(Date, abs.flow_rate))+
    geom_point(size = 1)+
    theme_classic()) # there's only one value at 0 one one day

min(lupa_filtered$abs.flow_rate,na.rm = T)

# which day? 

lupa_filtered[lupa_filtered$abs.flow_rate == 0,] # on 26th august - 0.95 flow rate... 
# is it outlier?


## vis with boxplot 

(boxplot_lupa <- ggplot(lupa_filtered, 
                        aes(y = abs.flow_rate))+
    geom_boxplot()+
    theme_classic())

### yes definitely an outlier, as much as there's a bunch of them on the top 
### there's only the one 0.95 flow rate value 

### any specific reason? --> what happened on that day? 

#######################

## corr with rainfall

(scatter_lupa_rain <- ggplot(lupa_filtered, 
                             aes(Rainfall_Terni,
                                 abs.flow_rate))+
   geom_point(size = 1)+
   theme_classic())

  # there could be some sort of relationship... not linear though ... or is it? 



## boxplot rain 

(rain_boxplot <- ggplot(lupa_filtered,
                        aes(y = Rainfall_Terni))+
    geom_boxplot()+
    theme_classic())


## removing two outliers --> one in rainfall other in target


lupa_filtered[lupa_filtered$Rainfall_Terni > 60,] # 19th may 2020 (recent!)

    # to keep or not to keep ? 


###### 
# trial if i removed them 


lupa_trial_outliers <- lupa_filtered %>% 
  filter(!Rainfall_Terni > 60,
         !Date == "2009-08-26")

# re-doing rainfall boxplot 

(rain_boxplot1 <- ggplot(lupa_trial_outliers,
                         aes(y = Rainfall_Terni))+
    geom_boxplot()+
    theme_classic())

(lupa_boxplot2 <- ggplot(lupa_trial_outliers,
                         aes(y = abs.flow_rate))+
    geom_boxplot()+
    theme_classic())


    ## better, i think


## scatter rain-flow, no outliers

(scatter2 <- ggplot(lupa_trial_outliers, 
                    aes(Rainfall_Terni,
                        abs.flow_rate))+
    geom_point(size = 1)+
    theme_classic()) 


# saving with and without outliers .csv 

write.csv(lupa_filtered, "processed_data/LUPA_filtered.csv")

write.csv(lupa_trial_outliers, "processed_data/LUPA_filtered_noout.csv")


################################################################################
################################################################################


#### madonna di canneto ####

str(canneto)

canneto1 <- canneto %>% 
  rename(Date = ï..Date) %>% 
  mutate(Date = dmy(Date))

str(canneto1)

summary(canneto1)

canneto_missing <- canneto1 %>% 
  miss_var_summary()
print(canneto_missing)
# flow rate data missing 50% 


# removing first years of missing target data


# first vis

(first_look_canneto <- ggplot(canneto1,
                              aes(Date ,Flow_Rate_Madonna_di_Canneto))+
    geom_line(size= 1)+
    theme_classic())

## filtering

canneto_filtered <- canneto1 %>% 
  filter(Date >= "2015-03-13")

(second_look_canneto <- ggplot(canneto_filtered,
                               aes(Date, Flow_Rate_Madonna_di_Canneto))+
    geom_line(size = 1)+
    theme_classic())


## alright filtered first years without target value 


## checking data distributions, outliers etc.

(canneto_boxplot <- ggplot(canneto_filtered,
                           aes(y = Flow_Rate_Madonna_di_Canneto))+
    geom_boxplot()+
    theme_classic())

  ## perfetto nessun segno di outliers dal boxplot

(canneto_rain_box <- ggplot(canneto_filtered,
                            aes(y = Rainfall_Settefrati))+
    geom_boxplot()+
    theme_classic()) # hmm to check - potentially there could be --> there's a few data outside top whisker

(canneto_temp_box <- ggplot(canneto_filtered,
                            aes(y = Temperature_Settefrati))+
    geom_boxplot()+
    theme_classic()) # no outliers here 


### checking for correlations 

  # vis - scatters

(scatter_canneto_rain <- ggplot(canneto_filtered, 
                                aes(Rainfall_Settefrati, 
                                    Flow_Rate_Madonna_di_Canneto))+
    geom_point(size = 1)+
    theme_classic()) # there doesnt seem to be much of a linear relationship 


(scatter_canneto_temp <- ggplot(canneto_filtered,
                                aes(Temperature_Settefrati,
                                    Flow_Rate_Madonna_di_Canneto))+
    geom_point(size = 1)+
    theme_classic()) # there appear to be high flow rate even below 0°C



###############

summary(canneto_filtered)

canneto_filtered_missing <- canneto_filtered %>% 
  miss_var_summary()
print(canneto_filtered_missing)


canneto_filtered[is.na(canneto_filtered$Rainfall_Settefrati),]
# nas for rainfall and temperature match (same sensor)
# but not for flow rate (data recorded despite lack of meteo data)


### saving 

write.csv(canneto_filtered, "processed_data/MADONNA_DI_CANNETO_filtered.csv")



################################################################################
############################# Fine #############################################



