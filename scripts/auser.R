################################################################################
################################################################################
###############   Cinzia                    ####################################
###############   Script prep of vars       ####################################
################################################################################
################################################################################

#### inizializzo ####
getwd()
rm(list = ls(all=TRUE)) 


#### uploading libraries ####

install.packages("naniar")
library(naniar)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(tibble)
library(dplyr)

#### uploading files ####

auser <- read.csv("data/Aquifer_Auser.csv")




#### data cleaning ####

#### auser ####

str(auser)

summary(auser)

#Trasformo la colonna data in data
auser$Date<-as.Date(auser$Date, format = "%d/%m/%Y")

auser_missing <- auser %>% 
  miss_var_summary()
print(auser_missing)
View(auser_missing)

#visualizzo i missing
visdat::vis_dat(auser)


auser1 <- auser %>% 
  gather(key = "well", value = "depth_to_gw.m", 4:12) %>%
  rename(Date = ï..Date) %>% # mi da errore ¢¢¢¢
  mutate(Date = dmy(Date),
         well = gsub("Depth_to_Groundwater_","",well))

(first_look <- ggplot(auser1, aes(x =Date, y = abs(depth_to_gw.m), color = well))+
    geom_line(size = .5)+
    theme_classic())

### missing data clearly starting from 

## removing missing for depth to gw

auser_filtered <- auser %>%     # mi da errore ¢¢¢¢ con auser1
  filter(Date >= "2006-01-01")

visdat::vis_dat(auser_filtered)

(second_look <- ggplot(auser_filtered, aes(Date, abs(depth_to_gw.m), 
                                               color = well))+
    geom_line(size = .5)+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    ylab("Abs. depth to groundwater (m)")+
    xlab("")) # mi da errore ¢¢¢¢

ggsave("img/AUSER_filtered_depth_to_groundwater.jpg", 
       dpi = 500, height = 5, width = 10) # mi da errore ¢¢¢¢



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





################################################################################
############################# Fine #############################################