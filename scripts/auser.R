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



#### imposto tema ####
theme_21 <- theme(legend.position = "bottom", legend.direction = "horizontal", axis.text = element_text(size = 14), 
                  plot.caption = element_text(color = "gray25", face = "bold", size = 8), legend.text = element_text(size = 15), 
                  axis.title = element_text(size = 14.5, face = "bold", color = "gray25"), legend.title = element_text(size = 14), axis.line = element_line(size = 0.4), 
                  plot.title = element_text(size = 19), plot.subtitle = element_text(size = 14.5), strip.text = element_text(size = 14, face = "bold"))

core_col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

#### uploading files ####
auser <- read.csv("data/Aquifer_Auser.csv")

####Trasformo la colonna data in data####
auser$Date<-as.Date(auser$Date, format = "%d/%m/%Y")

#### N/A Visualization ###
visdat::vis_dat(auser)

str(auser) 
names(auser)
summary(auser) #per avere una visione del dataframe

#### data cleaning ####

#### auser ####


auser_missing <- auser %>% 
  miss_var_summary()
print(auser_missing)
View(auser_missing)


auser1 <- auser %>% 
 gather(key = "well", value = "depth_to_gw.m", Depth_to_Groundwater_LT2 : Depth_to_Groundwater_DIEC) %>%
 mutate(Date = ymd(Date),
        well = gsub("Depth_to_Groundwater_","",well))

(first_look <- ggplot(auser1, aes(x =Date, y = abs(depth_to_gw.m), color = well))+
  geom_line(size = .5)+
    theme_classic())

### missing data clearly starting from 

## removing missing for depth to gw
#### missing prima parte ####
#noto valori totalmente mancanti dei dati, dall'inizio del dataset
max(auser$Date[is.na(auser$Rainfall_Gallicano)]) # restituisce "2005-12-31"

#risulta il taglio da fare fino al giorno "2005-12-31"
auser_cut<- auser %>%     
  filter(Date >= "2006-01-01")
visdat::vis_dat(auser_cut)

auser_filtered <- auser1 %>%     
  filter(Date >= "2006-01-01")

visdat::vis_dat(auser_filtered)

(second_look <- ggplot(auser_filtered, aes(x=Date, y=abs(depth_to_gw.m), 
                                               color = well))+
    geom_line(size = .5)+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    ylab("Abs. depth to groundwater in m")+
    xlab("")) 

ggsave("img/AUSER_filtered_depth_to_groundwater.jpg", 
       dpi = 500, height = 5, width = 10)



### scatter correlations ###

## temp ##

min(auser_filtered$Temperature_Orentano,na.rm = T)
max(auser_filtered$Temperature_Orentano,na.rm = T)
min(auser_filtered$Temperature_Monte_Serra,na.rm = T)
max(auser_filtered$Temperature_Monte_Serra,na.rm = T)
min(auser_filtered$Temperature_Ponte_a_Moriano,na.rm = T)
max(auser_filtered$Temperature_Ponte_a_Moriano,na.rm = T)
min(auser_filtered$Temperature_Lucca_Orto_Botanico,na.rm = T)
max(auser_filtered$Temperature_Lucca_Orto_Botanico,na.rm = T)



(scatter_temp_orentano <- ggplot(auser_filtered, 
                                 aes(x = Temperature_Orentano,
                                     y = abs(depth_to_gw.m),
                                     color = well))+
    geom_point(size = 1, alpha = 0.8)+
    theme_classic()+
    xlim(0,31))

# from the plot there doesnt seem to be any correlation.... 


(scatter_temp_monteserra <- ggplot(auser_filtered,
                                    aes(Temperature_Monte_Serra, 
                                        abs(depth_to_gw.m),
                                        color = well))+
    geom_point(size = 1, alpha = 0.8)+
    theme_classic()+
    xlim(0,31))
# same for Monteserra... 
(scatter_temp_ponteamoriano <- ggplot(auser_filtered,
                                   aes(Temperature_Ponte_a_Moriano, 
                                       abs(depth_to_gw.m),
                                       color = well))+
    geom_point(size = 1, alpha = 0.8)+
    theme_classic()+
    xlim(0,31))

(scatter_temp_lucca <- ggplot(auser_filtered,
                                   aes(Temperature_Lucca_Orto_Botanico, 
                                       abs(depth_to_gw.m),
                                       color = well))+
    geom_point(size = 1, alpha = 0.8)+
    theme_classic()+
    xlim(0,31))





#### rain ####

min(auser_filtered$Rainfall_Gallicano,na.rm = T)
max(auser_filtered$Rainfall_Gallicano,na.rm = T)
min(auser_filtered$Rainfall_Pontetetto,na.rm = T)
max(auser_filtered$Rainfall_Pontetetto,na.rm = T)
min(auser_filtered$Rainfall_Monte_Serra,na.rm = T)
max(auser_filtered$Rainfall_Monte_Serra,na.rm = T)
min(auser_filtered$Rainfall_Orentano,na.rm = T)
max(auser_filtered$Rainfall_Orentano,na.rm = T)
min(auser_filtered$Rainfall_Borgo_a_Mozzano,na.rm = T)
max(auser_filtered$Rainfall_Borgo_a_Mozzano,na.rm = T)
min(auser_filtered$Rainfall_Calavorno,na.rm = T)
max(auser_filtered$Rainfall_Calavorno,na.rm = T)
min(auser_filtered$Rainfall_Piaggione,na.rm = T)
max(auser_filtered$Rainfall_Piaggione,na.rm = T)
min(auser_filtered$Rainfall_Croce_Arcana,na.rm = T)
max(auser_filtered$Rainfall_Croce_Arcana,na.rm = T)
min(auser_filtered$Rainfall_Tereglio_Coreglia_Antelminelli ,na.rm = T)
max(auser_filtered$Rainfall_Tereglio_Coreglia_Antelminelli,na.rm = T)
min(auser_filtered$Rainfall_Fabbriche_di_Vallico,na.rm = T)
max(auser_filtered$Rainfall_Fabbriche_di_Vallico,na.rm = T)

(scatter_rain_gallicano <- ggplot(auser_filtered, 
                                 aes(Rainfall_Gallicano,
                                     abs(depth_to_gw.m),
                                     color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))


(scatter_rain_pontetetto <- ggplot(auser_filtered,
                                    aes(Rainfall_Pontetetto,
                                        abs(depth_to_gw.m),
                                        color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))

(scatter_rain_Monte_Serra <- ggplot(auser_filtered,
                                   aes(Rainfall_Monte_Serra,
                                       abs(depth_to_gw.m),
                                       color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))

(scatter_rain_Orentano <- ggplot(auser_filtered,
                                   aes(Rainfall_Orentano,
                                       abs(depth_to_gw.m),
                                       color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))

(scatter_rain_Borgo_a_Mozzano <- ggplot(auser_filtered,
                                   aes(Rainfall_Borgo_a_Mozzano,
                                       abs(depth_to_gw.m),
                                       color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))

(scatter_rain_Calavorno <- ggplot(auser_filtered,
                                   aes(Rainfall_Calavorno,
                                       abs(depth_to_gw.m),
                                       color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))

(scatter_rain_Piaggione <- ggplot(auser_filtered,
                                   aes(Rainfall_Piaggione,
                                       abs(depth_to_gw.m),
                                       color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))

(scatter_rain_Croce_Arcana <- ggplot(auser_filtered,
                                   aes(Rainfall_Croce_Arcana,
                                       abs(depth_to_gw.m),
                                       color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))

(scatter_rain_Tereglio_Coreglia_Antelminelli <- ggplot(auser_filtered,
                                   aes(Rainfall_Tereglio_Coreglia_Antelminelli,
                                       abs(depth_to_gw.m),
                                       color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))

(scatter_rain_Fabbriche_di_Vallico <- ggplot(auser_filtered,
                                   aes(Rainfall_Fabbriche_di_Vallico,
                                       abs(depth_to_gw.m),
                                       color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))




#### saving plots ####

panelled_rain_scatter_auser <- ggarrange(scatter_rain_gallicano,
                                         scatter_rain_pontetetto,
                                         scatter_rain_Monte_Serra,
                                         scatter_rain_Orentano,
                                         scatter_rain_Borgo_a_Mozzano,
                                         scatter_rain_Calavorno,
                                         scatter_rain_Piaggione,
                                         scatter_rain_Croce_Arcana,
                                         scatter_rain_Tereglio_Coreglia_Antelminelli,
                                         scatter_rain_Fabbriche_di_Vallico
                                         )
panelled_rain_scatter_auser


ggsave("img/AUSER_corr_rain.png",
       dpi = 500, height = 10, width = 18)



panelled_temp_scatter_auser <- ggarrange(scatter_temp_orentano,
                                   scatter_temp_ponteamariano,
                                   scatter_temp_lucca,
                                   scatter_temp_monteserra
                                  
                                   )
panelled_temp_scatter_auser

ggsave("img/AUSER_corr_temp.png",
       dpi = 500, height= 10, width = 18)


### saving auser.csv 

View(auser_filtered)


# spreading values again 

auser_filtered_save <- auser_filtered %>% 
  spread(well,depth_to_gw.m)

# saving 

write.csv(auser_filtered_save, "processed_data/AUSER_filtered.csv")

################################################################################
################################################################################





################################################################################
############################# Fine #############################################