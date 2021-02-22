#### River Arno Analisi / CS ####
#### inizializzo il codice ####
getwd()
rm(list = ls(all=TRUE)) 

#### carico le librerie ####
#install.packages("recipes")
#install.packages("fastDummies")

library(fastDummies)
library(fastDummies)
#library(fastDummies)
#library(reshape)
library(reshape2)
library(stats)
#install.packages("GGally")
library(GGally)
install.packages("factoextra")
install.packages("tibble")
install.packages("imputeTS")
install.packages("zoo")
library(imputeTS)
library(class)
library(corrplot)
library(factoextra)
library("plotly")
library(FactoMineR)
library(gplots)
library("graphics")
library("ggpubr")
library("ggdendro")
library("maps")
library("NbClust")
library("fpc")
library("dbscan")
library(cluster)
library(clValid)
library(corrplot)
library(tidyverse)
library(ggplot2)
library(GGally)
library(naniar)
library(visdat)
library(forecast)
library(xts)
library(caTools)
#library(data.table)
library(dplyr)
#install.packages("metR")
#library(metR)
library(zoo)
library(ggthemes)
library(forecast)
library(tidyverse)
library(scales)
library(RColorBrewer)
library(reshape2)
library(lubridate)
library(viridis)
library(ggrepel)
library(corrplot)
library(caret)
library(boot)
library(class)
library(adabag)
library(randomForestSRC)
library(scales)
library(Metrics)
library(mgcv)






#### imposto tema ####
theme_21 <- theme(legend.position = "bottom", legend.direction = "horizontal", axis.text = element_text(size = 14), 
                  plot.caption = element_text(color = "gray25", face = "bold", size = 8), legend.text = element_text(size = 15), 
                  axis.title = element_text(size = 14.5, face = "bold", color = "gray25"), legend.title = element_text(size = 14), axis.line = element_line(size = 0.4), 
                  plot.title = element_text(size = 19), plot.subtitle = element_text(size = 14.5), strip.text = element_text(size = 14, face = "bold"))

core_col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))



##### leggo il dataset arno ####
River_Arno<-read.csv("data/River_Arno.csv")

#Trasformo la colonna data in data
River_Arno$Date<-as.Date(River_Arno$Date, format = "%d/%m/%Y")

### N/A Visualization
visdat::vis_dat(River_Arno)
ggsave("img/arno/01River_Arno_Inizio.jpg", dpi = 500, width = 10, height=7)
#01River_Arno_Inizio.jpg


str(River_Arno) 
names(River_Arno)
summary(River_Arno) #per avere una visione del dataframe

#### missing prima parte ####
#noto valori totalmente mancanti dei dati rainfa falla, dall'inizio del dataset
max(River_Arno$Date[is.na(River_Arno$Rainfall_Le_Croci )])
#risulta il taglio da fare fino al giorno "2003-12-31"

###elimino i valori mancanti degli anni fino al 2004 , 
# delete rows with NA in feature to forecast
cutdata <- as.Date("2004-01-01")
River_Arno_cut <- River_Arno[(River_Arno$Date > cutdata), ]
### N/A Visualization
visdat::vis_dat(River_Arno_cut)
ggsave("img/arno/02River_Arno_missing.jpg", dpi = 500, width = 10, height=7)
#02River_Arno_cut_missing.jpg

#### missing seconda parte ####
statsNA(River_Arno_cut$Rainfall_Le_Croci) #ok non ci sono missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Le_Croci)

statsNA(River_Arno_cut$Rainfall_Cavallina) #ok non ci sono missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Cavallina)

statsNA(River_Arno_cut$Rainfall_S_Agata) #ok non ci sono missing
ggplot_na_distribution(River_Arno_cut$Rainfall_S_Agata)

statsNA(River_Arno_cut$Rainfall_Mangona) #ok non ci sono missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Mangona)

statsNA(River_Arno_cut$Rainfall_S_Piero) #ok non ci sono missing
ggplot_na_distribution(River_Arno_cut$Rainfall_S_Piero)

statsNA(River_Arno_cut$Rainfall_Vernio)# richiede intervento 1743 mising
ggplot_na_distribution(River_Arno_cut$Rainfall_Vernio)
ggsave("img/arno/03Rainfall_Vernio_missing.jpg", dpi = 500, width = 10, height=7)
#03Rainfall_Vernio_missing.jpg

statsNA(River_Arno_cut$Rainfall_Stia) # richiede intervento 4742 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Stia)
ggsave("img/arno/04Rainfall_Stia_missing.jpg", dpi = 500, width = 10, height=7)
#04Rainfall_Stia_missing.jpg

statsNA(River_Arno_cut$Rainfall_Consuma) # richiede intervento 4743 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Consuma)
ggsave("img/arno/05Rainfall_Consuma_missing.jpg", dpi = 500, width = 10, height=7)
#05Rainfall_Consuma_missing.jpg

statsNA(River_Arno_cut$Rainfall_Incisa) # richiede intervento 1457 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Incisa)
ggsave("img/arno/06Rainfall_Incisa_missing.jpg", dpi = 500, width = 10, height=7)
#06Rainfall_Incisa_missing.jpg


statsNA(River_Arno_cut$Rainfall_Montevarchi) # richiede intervento 4378 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Montevarchi)
ggsave("img/arno/07Rainfall_Montevarchi_missing.jpg", dpi = 500, width = 10, height=7)
#07Rainfall_Montevarchi_missing.jpg

statsNA(River_Arno_cut$Rainfall_S_Savino) # richiede intervento 4743 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_S_Savino)
ggsave("img/arno/08Rainfall_S_Savino_missing.jpg", dpi = 500, width = 10, height=7)
#08Rainfall_S_Savino_missing



statsNA(River_Arno_cut$Rainfall_Laterina) # richiede intervento 4743 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Laterina)
ggsave("img/arno/09Rainfall_Laterina_missing.jpg", dpi = 500, width = 10, height=7)
#09Rainfall_Laterina_missing



statsNA(River_Arno_cut$Rainfall_Bibbiena) # richiede intervento 3648 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Bibbiena)
ggsave("img/arno/10Rainfall_Bibbiena_missing.jpg", dpi = 500, width = 10, height=7)
#10Rainfall_Bibbiena_missing


statsNA(River_Arno_cut$Rainfall_Camaldoli) # richiede intervento 4743 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Camaldoli)
ggsave("img/arno/11Rainfall_Camaldoli_missing.jpg", dpi = 500, width = 10, height=7)
#11Rainfall_Camaldoli_missing


statsNA(River_Arno_cut$Temperature_Firenze) # richiede intervento 1062 missing
ggplot_na_distribution(River_Arno_cut$Temperature_Firenze)
ggsave("img/arno/12Temperature_Firenze_missing.jpg", dpi = 500, width = 10, height=7)
#12Temperature_Firenze_missing

statsNA(River_Arno_cut$Hydrometry_Nave_di_Rosano)# ok
ggplot_na_distribution(River_Arno_cut$Hydrometry_Nave_di_Rosano)
ggsave("img/arno/13Hydrometry_Nave_di_Rosano_missing.jpg", dpi = 500, width = 10, height=7)
#13Hydrometry_Nave_di_Rosano_missing

# scarico i dati dal 2011 al 2020 su 3bmeteo


#### FILLING GAPS WITH METEO ####

#### temperature Firenze

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}
#temp_firenze<-0
temp_firenze_ls <- list.files(path = "./data/meteoFirenze/",
                          pattern = "*.csv$", 
                          full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating temp data 
temp_firenze <- temp_firenze_ls %>% 
  dplyr::rename(date1 = filename) %>% 
  dplyr::mutate(date1 = gsub("./data/meteoFirenze/", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  dplyr::mutate(date_final = stringr::str_replace(date_final,"ago","08"),
         date_final = stringr::str_replace(date_final, "gen", "01"),
         date_final = stringr::str_replace(date_final, "feb", "02"),
         date_final = stringr::str_replace(date_final, "mar", "03"),
         date_final = stringr::str_replace(date_final, "apr", "04"),
         date_final = stringr::str_replace(date_final, "mag", "05"),
         date_final = stringr::str_replace(date_final, "giu", "06"),
         date_final = stringr::str_replace(date_final, "lug", "07"),
         date_final = stringr::str_replace(date_final, "sett", "09"),
         date_final = stringr::str_replace(date_final, "ott", "10"),
         date_final = stringr::str_replace(date_final, "nov","11"),
         date_final = stringr::str_replace(date_final, "dec", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = lubridate::dmy(date_final)) %>% 
  dplyr::rename(Date = date_final) %>%
  dplyr::select(Date, tmin, tmax) %>%
  dplyr::mutate(Temperature_Firenze = rowMeans(subset(., select = c(tmin,tmax)),
                                         na.rm = T)) %>%
  dplyr::select(-tmin, -tmax) %>%
  arrange(Date)

summary(temp_firenze)

# visualizing trend in newly added temperature 

(vis_temp_firenze <- ggplot(temp_firenze, aes(Date, Temperature_Firenze))+
    geom_line()+
    theme_classic())
#non salvo il grfico in questo caso

#### Rain_fall Bibbiena aggiungo i dati scaricati da 3bmeteo dal 2011



rf_bibbiena_ls <- list.files(path = "./data/meteoBibbiena/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  Bibbiena
rf_bibbiena <- rf_bibbiena_ls %>% 
  dplyr::rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/meteoBibbiena/", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = stringr::str_replace(date_final,"ago","08"),
         date_final = stringr::str_replace(date_final, "gen", "01"),
         date_final = stringr::str_replace(date_final, "feb", "02"),
         date_final = stringr::str_replace(date_final, "mar", "03"),
         date_final = stringr::str_replace(date_final, "apr", "04"),
         date_final = stringr::str_replace(date_final, "mag", "05"),
         date_final = stringr::str_replace(date_final, "giu", "06"),
         date_final = stringr::str_replace(date_final, "lug", "07"),
         date_final = stringr::str_replace(date_final, "sett", "09"),
         date_final = stringr::str_replace(date_final, "ott", "10"),
         date_final = stringr::str_replace(date_final, "nov","11"),
         date_final = stringr::str_replace(date_final, "dec", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>% 
  dplyr::rename(Date = date_final) %>%
  dplyr::select(Date, prec) %>%
  arrange(Date)

summary(rf_bibbiena)

# visualising trend in newly added rainfall Bibbiena 

(vis_rf_bibbiena <- ggplot(rf_bibbiena, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall Camaldoli aggiungo i dati scaricati da 3bmeteo dal 2011



rf_camaldoli_ls <- list.files(path = "./data/meteoCamaldoli/",
                             pattern = "*.csv$", 
                             full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  Camaldoli
rf_camaldoli <- rf_camaldoli_ls %>% 
  dplyr::rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/meteoCamaldoli/", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = stringr::str_replace(date_final,"ago","08"),
         date_final = stringr::str_replace(date_final, "gen", "01"),
         date_final = stringr::str_replace(date_final, "feb", "02"),
         date_final = stringr::str_replace(date_final, "mar", "03"),
         date_final = stringr::str_replace(date_final, "apr", "04"),
         date_final = stringr::str_replace(date_final, "mag", "05"),
         date_final = stringr::str_replace(date_final, "giu", "06"),
         date_final = stringr::str_replace(date_final, "lug", "07"),
         date_final = stringr::str_replace(date_final, "sett", "09"),
         date_final = stringr::str_replace(date_final, "ott", "10"),
         date_final = stringr::str_replace(date_final, "nov","11"),
         date_final = stringr::str_replace(date_final, "dec", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>% 
  dplyr::rename(Date = date_final) %>%
  dplyr::select(Date, prec) %>%
  arrange(Date)

summary(rf_camaldoli)

# visualising trend in newly added rainfall Camaldoli 

(vis_rf_camaldoli <- ggplot(rf_camaldoli, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall Consuma aggiungo i dati scaricati da 3bmeteo dal 2011



rf_consuma_ls <- list.files(path = "./data/meteoConsuma/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  Consuma
rf_consuma <- rf_consuma_ls %>% 
  dplyr::rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/meteoConsuma/", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = stringr::str_replace(date_final,"ago","08"),
         date_final = stringr::str_replace(date_final, "gen", "01"),
         date_final = stringr::str_replace(date_final, "feb", "02"),
         date_final = stringr::str_replace(date_final, "mar", "03"),
         date_final = stringr::str_replace(date_final, "apr", "04"),
         date_final = stringr::str_replace(date_final, "mag", "05"),
         date_final = stringr::str_replace(date_final, "giu", "06"),
         date_final = stringr::str_replace(date_final, "lug", "07"),
         date_final = stringr::str_replace(date_final, "sett", "09"),
         date_final = stringr::str_replace(date_final, "ott", "10"),
         date_final = stringr::str_replace(date_final, "nov","11"),
         date_final = stringr::str_replace(date_final, "dec", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>% 
  dplyr::rename(Date = date_final) %>%
  dplyr::select(Date, prec) %>%
  arrange(Date)

summary(rf_consuma)

# visualising trend in newly added rainfall Consuma

(vis_rf_consuma <- ggplot(rf_consuma, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall Incisa aggiungo i dati scaricati da 3bmeteo dal 2016



rf_incisa_ls <- list.files(path = "./data/meteoIncisa/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data Incisa
rf_incisa <- rf_incisa_ls %>% 
  dplyr::rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/meteoIncisa/", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = stringr::str_replace(date_final,"ago","08"),
         date_final = stringr::str_replace(date_final, "gen", "01"),
         date_final = stringr::str_replace(date_final, "feb", "02"),
         date_final = stringr::str_replace(date_final, "mar", "03"),
         date_final = stringr::str_replace(date_final, "apr", "04"),
         date_final = stringr::str_replace(date_final, "mag", "05"),
         date_final = stringr::str_replace(date_final, "giu", "06"),
         date_final = stringr::str_replace(date_final, "lug", "07"),
         date_final = stringr::str_replace(date_final, "sett", "09"),
         date_final = stringr::str_replace(date_final, "ott", "10"),
         date_final = stringr::str_replace(date_final, "nov","11"),
         date_final = stringr::str_replace(date_final, "dec", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>% 
  dplyr::rename(Date = date_final) %>%
  dplyr::select(Date, prec) %>%
  arrange(Date)

summary(rf_incisa)

# visualising trend in newly added rainfall Incisa 

(vis_rf_incisa <- ggplot(rf_incisa, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall Laterina aggiungo i dati scaricati da 3bmeteo dal 2011



rf_laterina_ls <- list.files(path = "./data/meteoLaterina/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  LAterina
rf_laterina <- rf_laterina_ls %>% 
  dplyr::rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/meteoLaterina/", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr:: select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = stringr::str_replace(date_final,"ago","08"),
         date_final = stringr::str_replace(date_final, "gen", "01"),
         date_final = stringr::str_replace(date_final, "feb", "02"),
         date_final = stringr::str_replace(date_final, "mar", "03"),
         date_final = stringr::str_replace(date_final, "apr", "04"),
         date_final = stringr::str_replace(date_final, "mag", "05"),
         date_final = stringr::str_replace(date_final, "giu", "06"),
         date_final = stringr::str_replace(date_final, "lug", "07"),
         date_final = stringr::str_replace(date_final, "sett", "09"),
         date_final = stringr::str_replace(date_final, "ott", "10"),
         date_final = stringr::str_replace(date_final, "nov","11"),
         date_final = stringr::str_replace(date_final, "dec", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>% 
  dplyr::rename(Date = date_final) %>%
  dplyr::select(Date, prec) %>%
  arrange(Date)

summary(rf_laterina)

# visualising trend in newly added rainfall LAterina

(vis_rf_laterina <- ggplot(rf_camaldoli, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall Montevarchi aggiungo i dati scaricati da 3bmeteo dal 2011



rf_montevarchi_ls <- list.files(path = "./data/meteoMontevarchi/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  Montevarchi
rf_montevarchi <- rf_montevarchi_ls %>% 
  dplyr::rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/meteoMontevarchi/", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = stringr::str_replace(date_final,"ago","08"),
         date_final = stringr::str_replace(date_final, "gen", "01"),
         date_final = stringr::str_replace(date_final, "feb", "02"),
         date_final = stringr::str_replace(date_final, "mar", "03"),
         date_final = stringr::str_replace(date_final, "apr", "04"),
         date_final = stringr::str_replace(date_final, "mag", "05"),
         date_final = stringr::str_replace(date_final, "giu", "06"),
         date_final = stringr::str_replace(date_final, "lug", "07"),
         date_final = stringr::str_replace(date_final, "sett", "09"),
         date_final = stringr::str_replace(date_final, "ott", "10"),
         date_final = stringr::str_replace(date_final, "nov","11"),
         date_final = stringr::str_replace(date_final, "dec", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>% 
  dplyr::rename(Date = date_final) %>%
  dplyr::select(Date, prec) %>%
  arrange(Date)

summary(rf_montevarchi)

# visualising trend in newly added rainfall MOntevarchi

(vis_rf_montevarchi <- ggplot(rf_camaldoli, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall San Savino aggiungo i dati scaricati da 3bmeteo dal 2011



rf_ssavino_ls <- list.files(path = "./data/meteoSSavino/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  San Savino
rf_ssavino <- rf_ssavino_ls %>% 
  dplyr::rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/meteoSSavino/", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = stringr::str_replace(date_final,"ago","08"),
         date_final = stringr::str_replace(date_final, "gen", "01"),
         date_final = stringr::str_replace(date_final, "feb", "02"),
         date_final = stringr::str_replace(date_final, "mar", "03"),
         date_final = stringr::str_replace(date_final, "apr", "04"),
         date_final = stringr::str_replace(date_final, "mag", "05"),
         date_final = stringr::str_replace(date_final, "giu", "06"),
         date_final = stringr::str_replace(date_final, "lug", "07"),
         date_final = stringr::str_replace(date_final, "sett", "09"),
         date_final = stringr::str_replace(date_final, "ott", "10"),
         date_final = stringr::str_replace(date_final, "nov","11"),
         date_final = stringr::str_replace(date_final, "dec", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>% 
  dplyr::rename(Date = date_final) %>%
  dplyr::select(Date, prec) %>%
  arrange(Date)

summary(rf_ssavino)

# visualising trend in newly added rainfall San Savino

(vis_rf_ssavino<- ggplot(rf_ssavino, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall Stia aggiungo i dati scaricati da 3bmeteo dal 2011



rf_stia_ls <- list.files(path = "./data/meteoStia/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  Stia
rf_stia <- rf_stia_ls %>% 
  dplyr::rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/meteoStia/", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = stringr::str_replace(date_final,"ago","08"),
         date_final = stringr::str_replace(date_final, "gen", "01"),
         date_final = stringr::str_replace(date_final, "feb", "02"),
         date_final = stringr::str_replace(date_final, "mar", "03"),
         date_final = stringr::str_replace(date_final, "apr", "04"),
         date_final = stringr::str_replace(date_final, "mag", "05"),
         date_final = stringr::str_replace(date_final, "giu", "06"),
         date_final = stringr::str_replace(date_final, "lug", "07"),
         date_final = stringr::str_replace(date_final, "sett", "09"),
         date_final = stringr::str_replace(date_final, "ott", "10"),
         date_final = stringr::str_replace(date_final, "nov","11"),
         date_final = stringr::str_replace(date_final, "dec", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>% 
  dplyr:: rename(Date = date_final) %>%
  dplyr::select(Date, prec) %>%
  arrange(Date)

summary(rf_stia)

# visualising trend in newly added rainfall Stia

(vis_rf_stia <- ggplot(rf_stia, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall Vernio aggiungo i dati scaricati da 3bmeteo dal 2015

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

rf_vernio_ls <- list.files(path = "./data/meteoVernio/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  Vernio
rf_vernio <- rf_vernio_ls %>% 
  dplyr::rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/meteoVernio/", "", date1),
         date1 = gsub(".csv", "", date1),
         date1 = gsub("([a-z])([[:digit:]])", "\\1 \\2", date1, perl = T)) %>%
  separate(date, into = c("weekday", "day")) %>%
  dplyr::select(-weekday) %>%
  unite(date_final, day,date1, sep = " ") %>%
  mutate(date_final = stringr::str_replace(date_final,"ago","08"),
         date_final = stringr::str_replace(date_final, "gen", "01"),
         date_final = stringr::str_replace(date_final, "feb", "02"),
         date_final = stringr::str_replace(date_final, "mar", "03"),
         date_final = stringr::str_replace(date_final, "apr", "04"),
         date_final = stringr::str_replace(date_final, "mag", "05"),
         date_final = stringr::str_replace(date_final, "giu", "06"),
         date_final = stringr::str_replace(date_final, "lug", "07"),
         date_final = stringr::str_replace(date_final, "sett", "09"),
         date_final = stringr::str_replace(date_final, "ott", "10"),
         date_final = stringr::str_replace(date_final, "nov","11"),
         date_final = stringr::str_replace(date_final, "dec", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>% 
  dplyr::rename(Date = date_final) %>%
  dplyr:: select(Date, prec) %>%
  arrange(Date)

summary(rf_vernio)

# visualising trend in newly added rainfall Vernio

(vis_rf_vernio <- ggplot(rf_vernio, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### adding new data to prev dataset River_Arno_cut ####
River_Arno_cut$Temperature_Firenze[is.na(River_Arno_cut$Temperature_Firenze)] <- 
  temp_firenze$Temperature_Firenze[match(River_Arno_cut$Date[is.na(River_Arno_cut$Temperature_Firenze)],
   temp_firenze$Date)]

River_Arno_cut$Rainfall_Bibbiena[is.na(River_Arno_cut$Rainfall_Bibbiena)] <- 
  rf_bibbiena$prec[match(River_Arno_cut$Date[is.na(River_Arno_cut$Rainfall_Bibbiena)],
                                         rf_bibbiena$Date)]

River_Arno_cut$Rainfall_Camaldoli[is.na(River_Arno_cut$Rainfall_Camaldoli)] <- 
  rf_camaldoli$prec[match(River_Arno_cut$Date[is.na(River_Arno_cut$Rainfall_Camaldoli)],
                         rf_camaldoli$Date)]

River_Arno_cut$Rainfall_Consuma[is.na(River_Arno_cut$Rainfall_Consuma)] <- 
  rf_consuma$prec[match(River_Arno_cut$Date[is.na(River_Arno_cut$Rainfall_Consuma)],
                          rf_consuma$Date)]

River_Arno_cut$Rainfall_Incisa[is.na(River_Arno_cut$Rainfall_Incisa)] <- 
  rf_incisa$prec[match(River_Arno_cut$Date[is.na(River_Arno_cut$Rainfall_Incisa)],
                        rf_incisa$Date)]

River_Arno_cut$Rainfall_Laterina[is.na(River_Arno_cut$Rainfall_Laterina)] <- 
  rf_laterina$prec[match(River_Arno_cut$Date[is.na(River_Arno_cut$Rainfall_Laterina)],
                       rf_laterina$Date)]

River_Arno_cut$Rainfall_Montevarchi[is.na(River_Arno_cut$Rainfall_Montevarchi)] <- 
  rf_montevarchi$prec[match(River_Arno_cut$Date[is.na(River_Arno_cut$Rainfall_Montevarchi)],
                         rf_montevarchi$Date)]

River_Arno_cut$Rainfall_S_Savino[is.na(River_Arno_cut$Rainfall_S_Savino)] <- 
  rf_ssavino$prec[match(River_Arno_cut$Date[is.na(River_Arno_cut$Rainfall_S_Savino)],
                            rf_ssavino$Date)]

River_Arno_cut$Rainfall_Stia[is.na(River_Arno_cut$Rainfall_Stia)] <- 
  rf_stia$prec[match(River_Arno_cut$Date[is.na(River_Arno_cut$Rainfall_Stia)],
                            rf_stia$Date)]

River_Arno_cut$Rainfall_Vernio[is.na(River_Arno_cut$Rainfall_Vernio)] <- 
  rf_vernio$prec[match(River_Arno_cut$Date[is.na(River_Arno_cut$Rainfall_Vernio)],
                            rf_vernio$Date)]



####controllo ulteriori NA ####
### N/A Visualization
visdat::vis_dat(River_Arno_cut)
ggsave("img/arno/14River_Arno_cut_fewmissing.jpg", dpi = 500, width = 10, height=7)

# delete rows with NA  non posso recuperare i dati dal
# 05-07-2007 al 01-01-2011
#cutdata_1 <- as.Date("2007-07-05")
cutdata_2 <- as.Date("2011-01-01")
River_Arno_cut1 <- River_Arno_cut[(River_Arno_cut$Date > cutdata_2), ]

#River_Arno_cut1 e' il dataframe con una interruzione temporale dal
#2007 al 2011 e decido di prendere i dati solo dal 2011 in poi


summary(River_Arno_cut1)
statsNA(River_Arno_cut1$Hydrometry_Nave_di_Rosano)

max(River_Arno_cut1$Date[is.na(River_Arno_cut1$Hydrometry_Nave_di_Rosano )])
#ho un unico valore mancante nella variabile target Hydrometry_Nave_di_Rosano
#"2020-05-05"


####interpolo il valore mancante####
# interpolation
#River_Arno_cut1$Hydrometry_Nave_di_Rosano <-as.numeric(na.approx(River_Arno_cut1$Hydrometry_Nave_di_Rosano))
#
River_Arno_cut1$Hydrometry_Nave_di_Rosano <-as.numeric(na.interp(River_Arno_cut1$Hydrometry_Nave_di_Rosano))
visdat::vis_dat(River_Arno_cut1)
ggsave("img/arno/15River_Arno_cut_NOmissing.jpg", dpi = 500, width = 10, height=7)



#TARGET: Hydrometry_Nave_di_Rosano
#Il livello delle acque sotterranee rilevato dalla stazione idrometrica 
#è stagionale, più elevato durante novembre-maggio.  
#Ci sono dei picchi improvvisi verso lo zero, subito ripristinati
df <- River_Arno_cut1 %>% dplyr::select(Date, 
                            Hydrometry_Nave_di_Rosano) %>%
  pivot_longer(., cols = c(Hydrometry_Nave_di_Rosano),
               names_to = "Var", values_to = "Val", values_drop_na = FALSE)
df <- df[complete.cases(df), ]
ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() +
  ggtitle("River Arno: Hydrometry (meters)") +
  ylab("Hydrometry") +
  xlab("Date")
ggsave("img/arno/16Hydrometry_Nave_di_Rosano_target.jpg", dpi = 500, width = 10, height=7)

rm(df)
#Hydrometry_Nave_di_Rosano_target.jpg


#### Correlation Matrix ####
df <- River_Arno_cut1
df$Date <- NULL
ggcorr(df, label = TRUE, label_round = 2, hjust = 1, size = 4, 
       layout.exp = 4, label_size = 3)
ggsave("img/arno/17Correlation_matrix.jpg", dpi = 500, width = 10, height=7)

rm(df)
#plot 17Correlation_Matrix.jpg
#La matrice di correlazione mostra una serie di correlazioni 
#positive tra precipitazioni e correlazioni negative tra 
# la temperatura e precipitazioni.
#L'idromertia e' inversament correlata  con la temperatura. 
#Le precipitazioni possono essere suddivise in due categorie: 
#la prima per le prime 6 variabili che sono le zone, attraverso cui
#passa il principale affluente dell'arno, il fiume Sive.(localita':
#Le_Croci, CAvallina, S_Agata, Mangona, S_Piero, Vernio).
#La seconda categoria e' rappresetata dalle restanti 8 localita' 
#che si trovano lungo l'Arno a partire della fonte (localtia':
#Stia, CAmaldoli, Bibbiena, Laterina, S_Savino, Montevarchi, 
#Incisa, Consuma). 
#Le variabili sono fortemente correlate tra loro all'interno dei gruppi, 
#ma molto debolmente tra i gruppi diversi, quindi per il modello finale, 
#studieremo esempi di  gruppi diversi.



#### Rainfall analysis ####
# Rainfall analysis per le localita' lungo l'affluente 
# principale del fiume Arno, il Sieve
#quando piove nella stessa giornata, la quantita' di pioggia si somma
#nel fiume e sicuramente influenzera' il flusso
df<-River_Arno_cut1
df$Rainfall_mean_Sieve <- rowMeans(River_Arno_cut1[,c("Rainfall_Le_Croci", "Rainfall_Cavallina", 
                                                   "Rainfall_S_Agata", "Rainfall_Mangona",
                                                   "Rainfall_S_Piero","Rainfall_Vernio")])


# Rainfall analysis per le localita' dalla sorgente dell'arno
#stesso dicorso per la pioggia ceh si accumula nella stessa
#giornata dala sorgente dell'arno in giu'
df$Rainfall_mean_Sorgente <- rowMeans(River_Arno_cut1[,c("Rainfall_Camaldoli", "Rainfall_Bibbiena", 
                                                                   "Rainfall_Laterina", "Rainfall_S_Savino",
                                                                   "Rainfall_Montevarchi","Rainfall_Consuma",
                                                                   "Rainfall_Incisa", "Rainfall_Stia" )])


################### 

#ho preso i dati da 3B meteo, dal 2011 reali
#visualizzo le medie di pioggia divise in zone, lungo affluente Sieve
#e lungo l'Arno, dalla sorgente

df <- df %>% dplyr::select(Date,  Rainfall_mean_Sieve, Rainfall_mean_Sorgente) %>%
  pivot_longer(., cols = c( Rainfall_mean_Sieve, Rainfall_mean_Sorgente),
               names_to = "Var", values_to = "Val", values_drop_na = FALSE)
ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() + ggtitle("Rainfall (mm) - River Arno") +
  ylab("Rainfall") +   xlab("Date")
ggsave("img/arno/18mean_rainfall.jpg", dpi = 500, width = 10, height=7)
rm(df)
#18River_arno_mean_rainfall

#### visualizzo la pioggia per localita'####
River_Arno_cut1 %>%
  dplyr::select(Date, Rainfall_Le_Croci, Rainfall_Cavallina, Rainfall_S_Agata, Rainfall_Mangona, Rainfall_S_Piero,
         Rainfall_Vernio, Rainfall_Stia, Rainfall_Consuma, Rainfall_Incisa, Rainfall_Montevarchi,
         Rainfall_S_Savino, Rainfall_Laterina, Rainfall_Bibbiena, Rainfall_Camaldoli) %>%
  melt(., id.vars = "Date") %>%
  ggplot(., aes(Date, value, col = variable))+
  facet_wrap(variable~., ncol = 3)+
  geom_line(size = 0.4, alpha = 1)+
  scale_color_viridis_d(option = "inferno", begin = 0.15, end = 0.15)+
  scale_x_date(date_labels = "%Y", date_breaks = "3 years", limits = as.Date(c("2011-01-01", "2020-06-30")))+
  labs(x = "Date", y = "Quantity of rain-falling (in mm)", title = "Quantity of rain-falling depending on the region",
       subtitle = "explanatory variables on river Arno (from 01-2011)") + 
  theme_21+
  theme(legend.position = "none")
ggsave("img/arno/19rainfall_localita.jpg", dpi = 500, width = 10, height=7)
#abbiamo due gruppi (affluente Sieve e sorgente dell'arno) in cui le precipitazioni sono correlate tra loro, 
#ma non sono correlate tra i gruppi diversi.
#nella  maggior parte dei casi non superano i 100 mm  di pioggia 
#e a volte, in certe localita', raramente superano anche i 50 mm. 
#Decidiamo di scegliere due variabili la somma delle piogge tra sorgente e affluente Sieve
#
#Faro il confronto con il test, guardano la correlzione tra i gruppi pioggia
#Prendo una localita per gruppo per per un test:
#dal primo gruppo, dell'affluente Sieve, scegliamo le precipitazioni da "Le Croci" 
#e dal secondo gruppo, della sorgente dell'Arno, scelgo "Stia" per la posizione geografica centrale, e per
#alta correlazione tra le altre componenti del proprio gruppo.

#poi, per fare un terzo test di forecast, possiam prendere
# due diverse localita' com eesempio Cavallina (dal grupo dell'Affluente Sieve)
# e Bibbiena per il gruppo della sorgente dell'Arno.

#### Temperatura ####
# Temperature: the temperature mean is 16.61 °C
# Temperature analysis: ho riempito il dataset con le temperature
# reali prese da 3bmeteo dal 2011
mean(River_Arno_cut1$Temperature_Firenze)
df <- River_Arno_cut1 %>% dplyr::select(Date, Temperature_Firenze ) %>%
  pivot_longer(., cols = c(Temperature_Firenze ),
               names_to = "Var", values_to = "Val")
df <- df[complete.cases(df), ]
ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() + ggtitle("Temperature (°C) -  River Arno") +
  ylab("Temperature") + xlab("Date")
rm(df)
ggsave("img/arno/20temp.jpg", dpi = 500, width = 10, height=7)
#La temperatura della regione di Firenze è l'unica variabile 
#sulla temperatura del dataset Arno. 
#Ha una distribuzione stagionale e la maggior parte dei valori è 
#compresa tra 0 e 30 gradi, in linea per una regione del centro Italia. 





####inserisco la variabile stagioni####
##inserisco le stagioni####
River_Arno_cut1 <- River_Arno_cut1 %>%
  mutate(Season = case_when(month(Date) %in% c(3,4,5) ~ "Spring",                      
                            month(Date) %in% c(6,7,8) ~ "Summer",
                            month(Date) %in% c(9,10,11) ~ "Autumn",
                            month(Date) %in% c(1,2,12) ~ "Winter"))
River_Arno_cut1$Season<-factor(River_Arno_cut1$Season, 
                             levels=c("Winter","Spring", "Summer", "Autumn"))

# salvo il mio dataset River_Arno_cut1 ripulito con le stagioni:
write.csv(River_Arno_cut1,"processed_data/ARNO_to_model.csv")

####OUTLIERS####



###correlazione seconda tabella di visibilita' con il metodo di spearman 
#+ le stagioni
River_Arno_cut1 %>%
  dplyr::select(!c("Date", "Season")) %>%
  cor(., method = "spearman", use = "complete.obs") %>%
  corrplot(., method = "color", type = "upper", col = core_col(100),
           tl.col = "black",tl.srt = 35, diag = T, tl.cex = 0.72)
#ggsave("img/arno/17Correlation_matrix_spearman.jpg", dpi = 500, width = 10, height=7)
##Matrice di correlazione: anche in questa tabella,sono presenti due cluster di precipitazioni, 
#quelli sulla sorgente (Stia, Camaldoli, Bibbiena, Laterina, San Savino
# Montevarchi, Incisa, Consuma) e quelli lungo l'affluente principale
#il fiume Sieve che sono Vernio, S.Piero, Mangona, S_Agata, Cavallina, Le Croci


#analizzo la variabile target in base alla stagione:

River_Arno_cut1 %>%
  dplyr::select(Season, Hydrometry_Nave_di_Rosano) %>%
  melt(., id.vars = "Season") %>%
  ggplot(., aes(Season, value))+
  facet_wrap(variable~., ncol = 1, scales = "free_y")+
  geom_boxplot(outlier.size = 1.1, outlier.shape = 20, lwd = 0.5, fatten = 1.1, 
               alpha = 0.95, width = 0.75, col = "gray10", fill = "#f8fc9d")+
  scale_x_discrete(limit = c("Spring", "Summer", "Autumn", "Winter"))+
  labs(x = "Season", y = "Value", title = "The distribution of the explained variables by season",
       subtitle = "river Arno") + 
  theme_21
ggsave("img/arno/21target+seasons.jpg", dpi = 500, width = 10, height=7)
#valori superiori a 5, che rappresentano piogge intense,
#si vedono solo in autunno e in inverno.
#le stagioni spiegano poco la variabile target: la mediana e' leggerment
# piu' alta in primavera e in inverno.





### andamento della variabile target####
River_Arno_cut1%>%
  dplyr::select(Date, Hydrometry_Nave_di_Rosano) %>%
  melt(., id.vars = "Date") %>%
  ggplot(., aes(Date, value))+
  facet_wrap(variable~., ncol = 1, scales = "free_y")+
  geom_line(size = 1.6, alpha = 0.8, col = "gray65")+
  geom_smooth(method = "loess", color = "firebrick3", size = 2.2, formula = y ~ x, fill = "firebrick4", alpha = 0.32)+
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", limits = as.Date(c("2011-01-01", "2020-06-30")))+
  labs(x = "Date", y = "Value", title = "The distribution of the explained variables (along with the loess curve)",
       subtitle = "in river Arno from 01-2011") + 
  theme_21


ggsave("img/arno/16Hydrometry_Nave_di_Rosano_target+mean.jpg", dpi = 500, width = 10, height=7)
#16Hydrometry_Nave_di_Rosano_target+mean


ggsave("img/arno/16Hydrometry_Nave_di_Rosano_target+mean.jpg", dpi = 500, width = 10, height=7)
#16Hydrometry_Nave_di_Rosano_target+mean

# Livello idrometrico: indica l’altezza d’acqua del fiume rispetto a un riferimento fisso,
#denominato zero idrometrico (m). 
#La variabile target esplicita  il livello del fiume espresso in metri 
#misurato dalla stazione idrometrica "Nave_di_Rosano". 
#I valori di questa variabile sono compresi tra 0 e 7 metri.
#La maggioranza dei valori misurati è compresa tra 1 e 4 metri. 
#Dopo qualche stagione irregolare, ci sono diversi cali 
#fino a 0 nel 2013, 2014 e 2019, che tornano rapidamente alla normalità.



#boxplot
df<-River_Arno_cut1
(hist_df <- ggplot(df,
                  aes(y = Hydrometry_Nave_di_Rosano
                                     ))+
    geom_boxplot()+
    theme_classic())
ggsave("img/arno/22Hydrometry_Nave_di_Rosano_boxplot.jpg", dpi = 500, width = 10, height=7)

out_arno <- boxplot.stats(df$Hydrometry_Nave_di_Rosano)$out 
out_arno
out_ind_arno <- which(df$Hydrometry_Nave_di_Rosano %in% c(out_arno))
out_ind_arno # rows where outliers are found

upper_bound <- quantile(df$Hydrometry_Nave_di_Rosano, 0.975)
upper_bound # 3.10325

upper_bound99 <- quantile(df$Hydrometry_Nave_di_Rosano, 0.99)
upper_bound99 # 3.96

## checking stats to verify it's an outlier
# grubbs test
test <- grubbs.test(df$Hydrometry_Nave_di_Rosano)
test # data:  df$Hydrometry_Nave_di_Rosano
#G = 8.59604, U = 0.97868, p-value < 2.2e-16
#alternative hypothesis: highest value 6.75 is an outlier

## dopo l'analisi decido di mantenere gli outlieers della variabile target, non e' possibile sostituire
rm(df)



#### funzione season + lag ####

## adding new features: seasons and presence/absence of snow
#non ho la pioggia su firenze
#arno_featured <- add.seasons(River_Arno_cut1) %>%
#  mutate(snow.yes = as.factor(ifelse(Temperature_XXX <=0 & Rainfall_XXX > 0, 1,0)),
#         snow.no = as.factor(ifelse(Temperature_XXX >0,1,0))) 

#str(arno_featured)

### changing effect of rain on target, and lagging the effect 
#of rain on the target ###


#Le_Croci
arno_orig_LagLC <- River_Arno_cut1 %>% 
  mutate(lag1 = lag(Rainfall_Le_Croci, +1),
         lag3 = lag(Rainfall_Le_Croci,+3),
         lag5 = lag(Rainfall_Le_Croci,+5),
         lag7 = lag(Rainfall_Le_Croci,+7)) 

arno_orig_LagLC1 <- arno_orig_LagLC %>% 
  dplyr::select(-Date)

#Cavallina
arno_orig_LagCA <- River_Arno_cut1 %>% 
  mutate(lag1 = lag(Rainfall_Cavallina, +1),
         lag3 = lag(Rainfall_Cavallina,+3),
         lag5 = lag(Rainfall_Cavallina,+5),
         lag7 = lag(Rainfall_Cavallina,+7)) 

arno_orig_LagCA1 <- arno_orig_LagCA %>% 
  dplyr::select(-Date)

#Bibbiena
arno_orig_LagB <- River_Arno_cut1 %>% 
  mutate(lag1 = lag(Rainfall_Bibbiena, +1),
         lag3 = lag(Rainfall_Bibbiena,+3),
         lag5 = lag(Rainfall_Bibbiena,+5),
         lag7 = lag(Rainfall_Bibbiena,+7)) 

arno_orig_LagB1 <- arno_orig_LagB %>% 
  dplyr::select(-Date)

#Stia
arno_orig_LagS <- River_Arno_cut1 %>% 
  mutate(lag1 = lag(Rainfall_Stia, +1),
         lag3 = lag(Rainfall_Stia,+3),
         lag5 = lag(Rainfall_Stia,+5),
         lag7 = lag(Rainfall_Stia,+7)) 

arno_orig_LagS1 <- arno_orig_LagS %>% 
  dplyr::select(-Date)






#








#
## creating 5 new datasets  with different min rainfall levels 
## and with new time lags (trying to represent true effect of rain over target)
#####Le_Croci+lag####
arno0.5_LC <- arno_orig_LagLC %>% 
  mutate(rain0.5 = ifelse(Rainfall_Le_Croci <= 0.5, 0, 
                          Rainfall_Le_Croci),
         lag1 = lag(rain0.5, +1),
         lag3 = lag(rain0.5,+3),
         lag5 = lag(rain0.5,+5),
         lag7 = lag(rain0.5,+7))  %>% 
 
  dplyr::select(-Rainfall_Le_Croci)

arno0.5_LC_1 <- arno0.5_LC %>%  dplyr::select(-Date)

arno1.5_LC <- arno_orig_LagLC %>% 
  mutate(rain1.5 = ifelse(Rainfall_Le_Croci <= 1.5, 0, 
                          Rainfall_Le_Croci),
         lag1 = lag(rain1.5, +1),
         lag3 = lag(rain1.5, +3),
         lag5 = lag(rain1.5, +5),
         lag7 = lag(rain1.5, +7)
  ) %>% 
  dplyr::select(-Rainfall_Le_Croci)

arno1.5_LC_1 <- arno1.5_LC %>%   dplyr::select(-Date)

arno3_LC <- arno_orig_LagLC %>% 
  mutate(rain3 = ifelse(Rainfall_Le_Croci <= 3,0,
                        Rainfall_Le_Croci),
         lag1 = lag(rain3, +1),
         lag3 = lag(rain3, +3),
         lag5 = lag(rain3, +5),
         lag7 = lag(rain3, +7)
  ) %>% 
  dplyr::select(-Rainfall_Le_Croci)

arno3_LC_1 <- arno3_LC %>%  dplyr::select(-Date)

arno5_LC <- arno_orig_LagLC %>% 
  mutate(rain5 = ifelse(Rainfall_Le_Croci <= 5, 0, 
                        Rainfall_Le_Croci),
         lag1 = lag(rain5, +1),
         lag3 = lag(rain5, +3),
         lag5 = lag(rain5, +5),
         lag7 = lag(rain5, +7)) %>% 

  dplyr::select(-Rainfall_Le_Croci)

arno5_LC_1 <- arno5_LC %>%   dplyr::select(-Date)

## creating 5 new datasets per dataset...le_Croci 
## ... or 5 new variables 

#arno0.5_LC.lag <- arno0.5_LC  %>% 
 # dplyr::mutate(lag1 = lag(rain1, +1),
  #              lag3 = lag(rain1,+3),
  #              lag5 = lag(rain1,+5),
  #              lag7 = lag(rain1,+7)) %>%
  #write.csv(., "processed_data/arno0.5_LC+lag.csv")

#arno1.5_LC.lag <- arno1.5_LC %>% 
 # mutate(lag1 = lag(rain2, +1),
  #       lag3 = lag(rain2, +3),
   #      lag5 = lag(rain2, +5),
    #     lag7 = lag(rain2, +7))%>% 
  #write.csv(., "processed_data/bilancino_rain1_Mangona+lag.csv")

#bilancino_rain3_Mangona.lag <- bilancino_rain3_Mangona %>% 
 # dplyr::mutate(lag1 = lag(rain3, +1),
 #               lag3 = lag(rain3, +3),
 #               lag5 = lag(rain3, +5),
 #               lag7 = lag(rain3, +7)) %>% 
 # write.csv(., "processed_data/bilancino_rain1_Mangona+lag.csv")

#bilancino_rain5_Mangona.lag <- bilancino_rain5_Mangona %>% 
 # dplyr::mutate(lag1 = lag(rain4, +1),
  #              lag3 = lag(rain4, +3),
   #             lag5 = lag(rain4, +5),
   #             lag7 = lag(rain4, +7)) %>% 
 # write.csv(., "processed_data/bilancino_rain5_Mangona+lag.csv")




## creating 5 new datasets  with different min rainfall levels 








## creating 5 new datasets Sieve with different min rainfall levels 

## and with new time lags (trying to represent true effect of rain over target)
#Cavallina
arno0.5_CA <- arno_orig_LagCA %>% 
  mutate(rain0.5 = ifelse(Rainfall_Cavallina <= 0.5, 0, 
                          Rainfall_Cavallina),
         lag1 = lag(rain0.5, +1),
         lag3 = lag(rain0.5,+3),
         lag5 = lag(rain0.5,+5),
         lag7 = lag(rain0.5,+7)
         ) %>% 

  dplyr::select(-Rainfall_Cavallina)

arno0.5_CA_1 <- arno0.5_CA %>%  dplyr::select(-Date)

arno1.5_CA <- arno_orig_LagCA %>% 
  mutate(rain1.5 = ifelse(Rainfall_Cavallina <= 1.5, 0, 
                          Rainfall_Cavallina),
         lag1 = lag(rain1.5, +1),
         lag3 = lag(rain1.5, +3),
         lag5 = lag(rain1.5, +5),
         lag7 = lag(rain1.5, +7)
  ) %>% 
  dplyr::select(-Rainfall_Cavallina)

arno1.5_CA_1 <- arno1.5_CA %>%   dplyr::select(-Date)

arno3_CA <- arno_orig_LagCA %>% 
  mutate(rain3 = ifelse(Rainfall_Cavallina <= 3,0,
                        Rainfall_Cavallina),
         lag1 = lag(rain3, +1),
         lag3 = lag(rain3, +3),
         lag5 = lag(rain3, +5),
         lag7 = lag(rain3, +7)
  ) %>% 
  dplyr::select(-Rainfall_Cavallina)

arno3_CA_1 <- arno3_CA %>%  dplyr::select(-Date)

arno5_CA <- arno_orig_LagCA %>% 
  mutate(rain5 = ifelse(Rainfall_Cavallina <= 5, 0, 
                        Rainfall_Cavallina),
         lag1 = lag(rain5, +1),
         lag3 = lag(rain5, +3),
         lag5 = lag(rain5, +5),
         lag7 = lag(rain5, +7),
         lag9 = lag(rain5, +9)) %>%
  dplyr::select(-Rainfall_Cavallina)

arno5_CA_1 <- arno5_CA %>%   dplyr::select(-Date)

## creating 5 new datasets Sieve with different min rainfall levels 
## and with new time lags (trying to represent true effect of rain over target)
#Bibbiena
arno0.5_B <- arno_orig_LagB %>% 
  mutate(rain0.5 = ifelse(Rainfall_Bibbiena <= 0.5, 0, 
                          Rainfall_Bibbiena),
         lag1 = lag(rain0.5, +1),
         lag3 = lag(rain0.5,+3),
         lag5 = lag(rain0.5,+5),
         lag7 = lag(rain0.5,+7)) %>% 
  dplyr::select(-Rainfall_Bibbiena)

arno0.5_B_1 <- arno0.5_B %>%  dplyr::select(-Date)

arno1.5_B <- arno_orig_LagB %>% 
  mutate(rain1.5 = ifelse(Rainfall_Bibbiena <= 1.5, 0, 
                          Rainfall_Bibbiena),
         lag1 = lag(rain1.5, +1),
         lag3 = lag(rain1.5, +3),
         lag5 = lag(rain1.5, +5),
         lag7 = lag(rain1.5, +7)
  ) %>% 
  dplyr::select(-Rainfall_Bibbiena)

arno1.5_B_1 <- arno1.5_B %>%   dplyr::select(-Date)

arno3_B <- arno_orig_LagB %>% 
  mutate(rain3 = ifelse(Rainfall_Bibbiena <= 3,0,
                        Rainfall_Bibbiena),
         lag1 = lag(rain3, +1),
         lag3 = lag(rain3, +3),
         lag5 = lag(rain3, +5),
         lag7 = lag(rain3, +7)
  ) %>% 
  dplyr::select(-Rainfall_Bibbiena)

arno3_B_1 <- arno3_B %>%  dplyr::select(-Date)

arno5_B <- arno_orig_LagB %>% 
  mutate(rain5 = ifelse(Rainfall_Bibbiena <= 5, 0, 
                        Rainfall_Bibbiena),
         lag1 = lag(rain5, +1),
         lag3 = lag(rain5, +3),
         lag5 = lag(rain5, +5),
         lag7 = lag(rain5, +7)) %>%
  dplyr::select(-Rainfall_Bibbiena)

arno5_B_1 <- arno5_B %>%   dplyr::select(-Date)

## creating 5 new datasets Sieve with different min rainfall levels 
## and with new time lags (trying to represent true effect of rain over target)
#Stia
arno0.5_S <- arno_orig_LagS %>% 
  mutate(rain0.5 = ifelse(Rainfall_Stia <= 0.5, 0, 
                          Rainfall_Stia),
         lag1 = lag(rain0.5, +1),
         lag3 = lag(rain0.5,+3),
         lag5 = lag(rain0.5,+5),
         lag7 = lag(rain0.5,+7)) %>% 
  dplyr::select(-Rainfall_Stia)

arno0.5_S_1 <- arno0.5_S %>%  dplyr::select(-Date)

arno1.5_S <- arno_orig_LagS %>% 
  mutate(rain1.5 = ifelse(Rainfall_Stia <= 1.5, 0, 
                          Rainfall_Stia),
         lag1 = lag(rain1.5, +1),
         lag3 = lag(rain1.5, +3),
         lag5 = lag(rain1.5, +5),
         lag7 = lag(rain1.5, +7)
  ) %>% 
  dplyr::select(-Rainfall_Stia)

arno1.5_S_1 <- arno1.5_S %>%   dplyr::select(-Date)

arno3_S <- arno_orig_LagS %>% 
  mutate(rain3 = ifelse(Rainfall_Stia <= 3,0,
                        Rainfall_Stia),
         lag1 = lag(rain3, +1),
         lag3 = lag(rain3, +3),
         lag5 = lag(rain3, +5),
         lag7 = lag(rain3, +7)
  ) %>% 
  dplyr::select(-Rainfall_Stia)

arno3_S_1 <- arno3_S %>%  dplyr::select(-Date)

arno5_S <- arno_orig_LagS %>% 
  mutate(rain5 = ifelse(Rainfall_Stia <= 5, 0, 
                        Rainfall_Stia),
         lag1 = lag(rain5, +1),
         lag3 = lag(rain5, +3),
         lag5 = lag(rain5, +5),
         lag7 = lag(rain5, +7)) %>%
  dplyr::select(-Rainfall_Stia)

arno5_S_1 <- arno5_S %>%   dplyr::select(-Date)


#### nuova analisi Arno + LAG +####
#divisione dellle  zone delle rainfall in base alla distanza da Rosano (FI)
#
# Studiando la geografia e la disposizione idrogeologica delle localita"
# divido in gurppi le zone:
# Gruppo 1: Incisa(fiume arno), Consuma (fiume arno), Montevarchi (fiume arno),
#S. PIero (affluente SIeve)
# non imposto lag perche' hanno una distanza inferiore ai 50km dal 
#punto idrometrico Nave di ROsano
# Gruppo 2: Laterina (fiume arno),  S. Agata (affluente Sieve),
# Le Croci (affluente Sieve), Cavallina (affluente Sieve), Mangona inserisco un lag di un giorno
# perche' hanno una distanza fino a circa 100 km dal punto idrometrico
# Per le localita' di distanza superiore ai 100km: Stia, Camaldoli, Bibbiena, S. Avinro (per il fiume arno),
# e Vernio  (affluente Sieve) sinserisco un lag di 3 giorni

arno_LAG <- River_Arno_cut1 %>% 
  mutate(lag_Laterina = lag(Rainfall_Laterina, +1),
         lag_Cavallina = lag(Rainfall_Cavallina, +1),
         lag_Le_Croci = lag(Rainfall_Le_Croci, +1),
         lag_S_Agata = lag(Rainfall_S_Agata, +1),
         lag_Mangona = lag(Rainfall_Mangona, +1),
         lag_Stia = lag(Rainfall_Stia, +2),
         lag_Camaldoli = lag(Rainfall_Camaldoli, +2),
         lag_Bibbiena= lag(Rainfall_Bibbiena, +2),
         lag_S_Savino = lag(Rainfall_S_Savino, +2),
         lag_Vernio= lag(Rainfall_Vernio, +2),
         ) 


arno_LAG <- arno_LAG %>% 
  dplyr::select(-Date,-Rainfall_Laterina, -Rainfall_Cavallina, -Rainfall_Le_Croci,
               -Rainfall_S_Agata, -Rainfall_Mangona, -Rainfall_Stia,
               -Rainfall_Camaldoli, -Rainfall_Bibbiena, -Rainfall_S_Savino,
               -Rainfall_Vernio)

#rimane da studiare il random forest e gboost deii dataset


























#### RAndom Forest test ####




#### test 1 con localita Le_Croci e Stia ####

River_Arno_Season <- dummyVars(~Season, data = River_Arno_cut1, fullRank = F)
River_Arno_Season <- as.data.frame(predict(River_Arno_Season, newdata = River_Arno_cut1))

River_Arno_cut2 <- River_Arno_cut1 %>%
  dplyr::select(Hydrometry_Nave_di_Rosano, Temperature_Firenze, Rainfall_Le_Croci, Rainfall_Stia)

River_Arno_cut2 <- cbind(River_Arno_cut2, River_Arno_Season)

River_Arno_cut2 <- River_Arno_cut2[complete.cases(River_Arno_cut2),]

set.seed(2021)
rand_River_Arno <- sample(nrow(River_Arno_cut2), nrow(River_Arno_cut2)* 1/3, replace = F)
test_River_Arno <- River_Arno_cut2[rand_River_Arno,]
train_River_Arno <- River_Arno_cut2[-rand_River_Arno,]

cat("Number of rows in the training set:", nrow(train_River_Arno), "\n")
cat("Number of rows in the test set:", nrow(test_River_Arno))
#Ho diviso il dataset in training e tes, test 1156 obs, train 2312 obs

rf_River_Arno_Hydrometry_Nave_di_Rosano <- 
  rfsrc(Hydrometry_Nave_di_Rosano~
          Season.Autumn+Season.Spring+Season.Summer+Season.Winter+
          Temperature_Firenze+Rainfall_Le_Croci+Rainfall_Stia, 
        data = train_River_Arno, block.size = 1, importance = T, samptype = "swr", var.used = "all.trees", ntree = 200)

#procedo alla verifica del modello
plotrf1<-plot(rf_River_Arno_Hydrometry_Nave_di_Rosano, verbose = F)

#jpeg("img/arno/23RF_Stia_Croci1.jpg",  width = 5000, height=3500)
ggsave("img/arno/23RF_Stia_Croci.jpg",plot=plotrf1, dpi = 500, width = 10, height=7)
#non salva


#L'errore diminuisce molto rapidamente aggiungendo più alberi,
#la variabile che ha avuto piu' impatto e' la temperatura dell'aria
#di Firenze. La pioggia a Stia influisce abbastanza, alla sorgente dell'arno
# mentre lungo l'affluente
#dell'Arno, il fiume Sieve, la pioggia alla
#localita' Le Croci influisce molto poco
#(daconfrontare con alre scele di variabili)


#### rmse test 1 ####

pred_rf_River_Arno_Hydrometry_Nave_di_Rosano <- predict(rf_River_Arno_Hydrometry_Nave_di_Rosano, newdata = test_River_Arno)
pred_rf_River_Arno_Hydrometry_Nave_di_Rosano

RMSE_RF1<-cat("RMSE Test:", round(rmse(pred_rf_River_Arno_Hydrometry_Nave_di_Rosano$predicted, test_River_Arno$Hydrometry_Nave_di_Rosano),2))


#RMSE_RF1

#RMSE_RF1
#####RMSE Test: 0.48 le croci e Stia RF ####
#
#L'RMSE sul set di prova è 0,48.
#Si tratta di un risultato abbastanza buono che tiene conto 
#di un piccolo numero di variabili esplicative. 
#La varianza di questo modello è stata spiegata solo nel 34% dei casi


pred_River_Arno_Hydrometry_Nave_di_Rosano <- data.frame(pred = pred_rf_River_Arno_Hydrometry_Nave_di_Rosano$predicted, 
                                                        real = test_River_Arno$Hydrometry_Nave_di_Rosano)

pred_River_Arno_Hydrometry_Nave_di_Rosano$above <- ifelse(pred_River_Arno_Hydrometry_Nave_di_Rosano$pred>pred_River_Arno_Hydrometry_Nave_di_Rosano$real,
                                                          "Too high predict value", "Too low predict value")

ggplot(pred_River_Arno_Hydrometry_Nave_di_Rosano, aes(real, pred, fill = above))+
  geom_point(size = 4, shape = 21, alpha = 0.8)+ 
  scale_fill_viridis_d(option = "inferno", begin = 0.20, end = 0.90, name = "")+

  geom_abline(intercept = 0, slope = 1, col = "red1", size = 1)+
  labs(x = "Real values in the test set", y = "Predicted values in the test set", 
       title = "The predicted and true values on the test set", fill = "",
       subtitle = "Random forest model Hydrometry_Nave_di_Rosano variable in Arno_river") + 
  theme_classic()+
  theme(legend.position = "bottom", legend.direction = "vertical")
ggsave("img/arno/24Predictet_value_RF1.jpg", dpi = 500, width = 10, height=7)

      

# Possiamo osservare che la maggioranza dei valorisi trova vicino alla linea 
#di previsione e questo va bene dove ci sono errori#
#piccoli. Tuttavia ci sono dei valori con errore piu' grande



#### test 2 con localita Cavallina e Bibbiena ####
River_Arno_Season <- dummyVars(~Season, data = River_Arno_cut1, fullRank = F)
River_Arno_Season <- as.data.frame(predict(River_Arno_Season, newdata = River_Arno_cut1))

River_Arno_cut3 <- River_Arno_cut1 %>%
  dplyr::select(Hydrometry_Nave_di_Rosano, Temperature_Firenze, Rainfall_Cavallina, Rainfall_Bibbiena)

River_Arno_cut3 <- cbind(River_Arno_cut3, River_Arno_Season)

River_Arno_cut3 <- River_Arno_cut3[complete.cases(River_Arno_cut3),]

set.seed(2021)
rand_River_Arno3 <- sample(nrow(River_Arno_cut3), nrow(River_Arno_cut3)* 1/3, replace = F)
test_River_Arno3 <- River_Arno_cut3[rand_River_Arno,]
train_River_Arno3 <- River_Arno_cut3[-rand_River_Arno,]

cat("Number of rows in the training set:", nrow(train_River_Arno3), "\n")
cat("Number of rows in the test set:", nrow(test_River_Arno3))
#Ho diviso il dataset in training e tes, test 1156 obs, train 2312 obs

rf_River_Arno_Hydrometry_Nave_di_Rosano3 <- 
  rfsrc(Hydrometry_Nave_di_Rosano~Season.Autumn+Season.Spring+
          Season.Summer+Season.Winter+Temperature_Firenze+
          Rainfall_Cavallina+Rainfall_Bibbiena, 
        data = train_River_Arno3, block.size = 1, importance = T, samptype = "swr", var.used = "all.trees", ntree = 200)

#procedo alla verifica del modello
plot(rf_River_Arno_Hydrometry_Nave_di_Rosano3, verbose = F)

#ggsave("img/arno/25RF_Cavallina_Bibbiena.jpg",plot=plotrf1, dpi = 500, width = 10, height=7)
#non salva


#L'errore diminuisce molto rapidamente aggiungendo più alberi,
#la variabile che ha avuto piu' impatto e' la temperatura dell'aria
#di Firenze. La pioggia a Bibbiena influisce abbastanza, alla sorgente dell'arno
# mentre lungo l'affluente
#dell'Arno, il fiume Sieve, la pioggia alla
#localita' Cavallina influisce molto poco
#


#### rmse test 2 ####

pred_rf_River_Arno_Hydrometry_Nave_di_Rosano3 <- predict(rf_River_Arno_Hydrometry_Nave_di_Rosano3, newdata = test_River_Arno3)
pred_rf_River_Arno_Hydrometry_Nave_di_Rosano3

RMSE_RF2<-cat("RMSE Test:", round(rmse(pred_rf_River_Arno_Hydrometry_Nave_di_Rosano3$predicted, test_River_Arno3$Hydrometry_Nave_di_Rosano),2))
RMSE_RF2
#####RMSE Test : 0.48 Cavallina e Bibbiena RF ####
#ibrary(Metrics)
#rmse(testing$medv,predValues)
#
#L'RMSE sul set di prova è 0,48.
#Si tratta di un risultato abbastanza buono, migliore di prima, che tiene conto 
#di un piccolo numero di variabili esplicative. 
#La varianza di questo modello è stata spiegata solo nel 33% dei casi


pred_River_Arno_Hydrometry_Nave_di_Rosano3 <- data.frame(pred = pred_rf_River_Arno_Hydrometry_Nave_di_Rosano3$predicted, 
                                                        real = test_River_Arno3$Hydrometry_Nave_di_Rosano)

pred_River_Arno_Hydrometry_Nave_di_Rosano3$above <- ifelse(pred_River_Arno_Hydrometry_Nave_di_Rosano3$pred>pred_River_Arno_Hydrometry_Nave_di_Rosano3$real,
                                                          "Too high predict value", "Too low predict value")

ggplot(pred_River_Arno_Hydrometry_Nave_di_Rosano3, aes(real, pred, fill = above))+
  geom_point(size = 4, shape = 21, alpha = 0.8)+ 
  scale_fill_viridis_d(option = "inferno", begin = 0.20, end = 0.90, name = "")+
  geom_abline(intercept = 0, slope = 1, col = "red1", size = 1.25)+
  labs(x = "Real values in the test set", y = "Predicted values in the test set", 
       title = "The predicted and true values on the test set", fill = "",
       subtitle = "Random forest model for Hydrometry_Nave_di_Rosano variable in Arno_river") + 
  theme_21+
  theme(legend.position = "bottom", legend.direction = "vertical")


#### test con nuovi lag ####
River_Arno_Season <- dummyVars(~Season, data = arno_LAG, fullRank = F)
River_Arno_Season <- as.data.frame(predict(River_Arno_Season, newdata = arno_LAG))

arno_LAG2<- arno_LAG %>% 
  dplyr::select(-Season)

arno_LAG2 <- cbind(arno_LAG2, River_Arno_Season)

arno_LAG2 <- arno_LAG2[complete.cases(arno_LAG2),]

set.seed(2021)
rand_arno_LAG <- sample(nrow(arno_LAG2), nrow(arno_LAG2)* 1/3, replace = F)
test_arno_LAG <- arno_LAG2[rand_arno_LAG,]
train_arno_LAG <- arno_LAG2[-rand_arno_LAG,]

cat("Number of rows in the training set:", nrow(train_arno_LAG), "\n")
cat("Number of rows in the test set:", nrow(test_arno_LAG))
#Ho diviso il dataset in training e tes, test 1156 obs, train 2312 obs

rf_River_Arno_Hydrometry_Nave_di_Rosano_LAG <- 
  rfsrc(Hydrometry_Nave_di_Rosano~Temperature_Firenze+lag_Laterina+
          lag_S_Savino+Rainfall_Incisa+lag_Vernio+lag_Le_Croci+
          lag_Camaldoli+lag_Vernio+Rainfall_Montevarchi+
          lag_Mangona+lag_S_Agata+lag_Cavallina+lag_Stia+lag_Bibbiena+
          Rainfall_Consuma+Season.Autumn+Season.Spring+Season.Winter+
          Season.Summer, 
        data = train_arno_LAG, block.size = 1, importance = T, samptype = "swr", var.used = "all.trees", ntree = 300)

#procedo alla verifica del modello
#as.tibble(importance(rf_River_Arno_Hydrometry_Nave_di_Rosano_LAG))
#rank(importance(rf_River_Arno_Hydrometry_Nave_di_Rosano_LAG))
#varImpPlot(rf_River_Arno_Hydrometry_Nave_di_Rosano_LAG,  pch=8)

plot(rf_River_Arno_Hydrometry_Nave_di_Rosano_LAG,verbose = F ) 







#L'errore diminuisce molto rapidamente aggiungendo più alberi


#### rmse test LAG train_arno_LAG ####

pred_rf_River_Arno_Hydrometry_Nave_di_Rosano_LAG <- predict(rf_River_Arno_Hydrometry_Nave_di_Rosano_LAG, newdata = test_arno_LAG)
pred_rf_River_Arno_Hydrometry_Nave_di_Rosano_LAG

RMSE_RF_LAG<-cat("RMSE Test:", round(rmse(pred_rf_River_Arno_Hydrometry_Nave_di_Rosano_LAG$predicted, test_arno_LAG$Hydrometry_Nave_di_Rosano),2))
RMSE_RF_LAG
####RMSE Test: 0.37 con arno_LAG ####
#







#### Conclusioni ####
#conclusioni:
# si osservano rare ma ampie fluttuazione del flusso d'acqua, fino
#a zero, con rapida rislaita (dall'analisi dell idrometria).

#Le variabili delle precipitazioni si dividono in due gruppi, 
#non correlate tra loro ma internamente correlate.

#il gruppo che influisce di maggiormente riguarda la sorgente.

# La temperatura ha la maggiore influenza sul flusso d'acqua, tra le
#variabili studiate.

#La variabile target differisce leggermente tra le stagioni.

#Nonostante il contenuto numero di variabili si puo' 
#ottenere un buon modello.

#Il modello ha i maggiori problemi con valori fuori lo standard
#(forti aumenti e diminuzioni che sottostima e sovrastima di conseguenza).

#Il modello può essere migliorato quando vengono trovate più variabili
#esplicative o il periodo di raccolta è più lungo e più stabile.



#Sieve=c("Rainfall_Le_Croci", "Rainfall_Cavallina", 
#"Rainfall_S_Agata", "Rainfall_Mangona",
#"Rainfall_S_Piero","Rainfall_Vernio")]

#Sorgente=Sorgente <- c("Rainfall_Camaldoli", "Rainfall_Bibbiena", 
#"Rainfall_Laterina", "Rainfall_S_Savino",
#"Rainfall_Montevarchi","Rainfall_Consuma",
#"Rainfall_Incisa", "Rainfall_Stia" )


##### GRADIENT BOOST MACHINE  #####

## libraries 
install.packages("remotes") #per initial_split
install.packages(h2o)
library(h2o)
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

##
###anche qui faro vari test
#### test GB con LAG personalizzari ####

## reading files 
River_Arno_Season_Lag <- dummyVars(~Season, data = arno_LAG, fullRank = F)
River_Arno_Season_Lag <- as.data.frame(predict(River_Arno_Season_Lag, newdata = arno_LAG))

arno_LAG2<- arno_LAG %>% 
  dplyr::select(-Season)

arno_LAG2 <- cbind(arno_LAG2, River_Arno_Season_Lag)

arno_LAG2 <- arno_LAG2[complete.cases(arno_LAG2),]


#River_Arno_cut_gb2<- River_Arno_cut1 %>%
#3  dplyr::select( Rainfall_sum_Sieve, Rainfall_sum_Sorgente,  Temperature_Firenze,
#                 Hydrometry_Nave_di_Rosano) 


#River_Arno_cut_GB_LAG <- cbind(arno_LAG2, River_Arno_Season)

#River_Arno_cut_GB_LAG <- arno_LAG2[complete.cases(arno_LAG2),]


#spread(key = imp, value = depth_to_gw.m) # 
str(arno_LAG2)






#### computing stepwise regression for variable selection ####
# creating function
step.wisef <- function(x, DATA){
  set.seed(123)
  train.control <- trainControl(method = "cv", number = 10)
  step.model <- train(as.formula(paste(x,"~.")), data = DATA, 
                      method = "leapSeq", 
                      tuneGrid = data.frame(nvmax = 1:6),
                      trControl = train.control,
                      na.action = na.omit)
  return(step.model)
}


#### Hydrometry_Nave_diRosano target####

Hydrometry_Nave_di_Rosano_sw <- step.wisef("Hydrometry_Nave_di_Rosano", arno_LAG2)
Hydrometry_Nave_di_Rosano_sw$bestTune 
Hydrometry_Nave_di_Rosano_sw$finalModel
coef(Hydrometry_Nave_di_Rosano_sw$finalModel, 6)

### let's stick to three variables (?) ###
## question: model chooses the two temperatures even if they're highly correlated to one another...?
# why? 

#### testing and training split ####

set.seed(123)
Hydrometry_Nave_di_Rosano.split <- initial_split(arno_LAG2, prop = .7)
Hydrometry_Nave_di_Rosano.train <- training(Hydrometry_Nave_di_Rosano.split)
Hydrometry_Nave_di_Rosano.test <- testing(Hydrometry_Nave_di_Rosano.split)

Hydrometry_Nave_di_Rosano_fit1 <- gbm::gbm(Hydrometry_Nave_di_Rosano ~ .,
                                           data = arno_LAG2,
                                           verbose = T, 
                                           shrinkage = 0.01,
                                           interaction.depth = 3, 
                                           n.minobsinnode = 5,
                                           n.trees = 1000,
                                           cv.folds = 10)
perf_gbm1 <- gbm.perf(Hydrometry_Nave_di_Rosano_fit1, method = "cv")

## make predictions 

Hydrometry_Nave_di_Rosano_pred2 <- stats::predict(object = Hydrometry_Nave_di_Rosano_fit1,
                                                  newdata = Hydrometry_Nave_di_Rosano.test,
                                                  n.trees = perf_gbm1)
rmse_fit1 <- Metrics::rmse(actual = Hydrometry_Nave_di_Rosano.test$Hydrometry_Nave_di_Rosano,
                           predicted = Hydrometry_Nave_di_Rosano_pred2)
print(rmse_fit1) 
#### RMSE 0.3470 Test2 con LAG PERSONALIZZATI ####
#

#plot 
# summarise model 

Hydrometry_Nave_di_Rosano_effects <- tibble::as_tibble(gbm::summary.gbm(Hydrometry_Nave_di_Rosano_fit1,
                                                                        plotit = F))
Hydrometry_Nave_di_Rosano_effects %>% utils::head()
# plot top 6 features
Hydrometry_Nave_di_Rosano_effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # 
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")
#### il secondo classificato ####










#### testgb 2 con localita Le_Croci Cavallina  per il primo gruppo e Bibbiena Stia per il secondo gruppo ####

## reading files 
River_Arno_cut_gb2<- River_Arno_cut1 %>%
  dplyr::select( Rainfall_Cavallina,Rainfall_Le_Croci, Rainfall_Stia, Rainfall_Bibbiena, Temperature_Firenze,
                 Hydrometry_Nave_di_Rosano,
                 Season) 
#spread(key = imp, value = depth_to_gw.m) # in regression date doesnt really matter 
str(River_Arno_cut_gb2)

## prepping objects per target ##




#### computing stepwise regression for variable selection ####
# creating function
step.wisef <- function(x, DATA){
  set.seed(123)
  train.control <- trainControl(method = "cv", number = 10)
  step.model <- train(as.formula(paste(x,"~.")), data = DATA, 
                      method = "leapSeq", 
                      tuneGrid = data.frame(nvmax = 1:6),
                      trControl = train.control,
                      na.action = na.omit)
  return(step.model)
}


#### Hydrometry_Nave_diRosano target####

Hydrometry_Nave_di_Rosano_sw <- step.wisef("Hydrometry_Nave_di_Rosano", River_Arno_cut_gb2)
Hydrometry_Nave_di_Rosano_sw$bestTune 
Hydrometry_Nave_di_Rosano_sw$finalModel
coef(Hydrometry_Nave_di_Rosano_sw$finalModel, 5)

### let's stick to three variables (?) ###
## question: model chooses the two temperatures even if they're highly correlated to one another...?
# why? 

#### testing and training split ####

set.seed(123)
Hydrometry_Nave_di_Rosano.split <- initial_split(River_Arno_cut_gb2, prop = .7)
Hydrometry_Nave_di_Rosano.train <- training(Hydrometry_Nave_di_Rosano.split)
Hydrometry_Nave_di_Rosano.test <- testing(Hydrometry_Nave_di_Rosano.split)

Hydrometry_Nave_di_Rosano_fit1 <- gbm::gbm(Hydrometry_Nave_di_Rosano ~ .,
                        data = River_Arno_cut_gb2,
                        verbose = T, 
                        shrinkage = 0.01,
                        interaction.depth = 3, 
                        n.minobsinnode = 5,
                        n.trees = 5000,
                        cv.folds = 10)
perf_gbm1 <- gbm.perf(Hydrometry_Nave_di_Rosano_fit1, method = "cv")

## make predictions 

Hydrometry_Nave_di_Rosano_pred2 <- stats::predict(object = Hydrometry_Nave_di_Rosano_fit1,
                               newdata = Hydrometry_Nave_di_Rosano.test,
                               n.trees = perf_gbm1)
rmse_fit2 <- Metrics::rmse(actual = Hydrometry_Nave_di_Rosano.test$Hydrometry_Nave_di_Rosano,
                           predicted = Hydrometry_Nave_di_Rosano_pred2)
print(rmse_fit1) 
#### RMSE 0.3423 Test2 con Le_Croci, Cavallina, Bibbiena, Stia ####
#

#plot - RAinfall_CAvallina
gbm::plot.gbm(Hydrometry_Nave_di_Rosano_fit1, i.var = 1)
# plot - RAinfall_Le_Croci
plot.gbm(Hydrometry_Nave_di_Rosano_fit1, i.var = 2)
# plot - RAinfall_Stia
plot.gbm(Hydrometry_Nave_di_Rosano_fit1, i.var = 3)
# plot - RAinfall_Bibbiena
plot.gbm(Hydrometry_Nave_di_Rosano_fit1, i.var = 4)
# plot - Temperature_Firenze
plot.gbm(Hydrometry_Nave_di_Rosano_fit1, i.var = 5)


## interactions of two features on the variable 

gbm::plot.gbm(Hydrometry_Nave_di_Rosano_fit1, i.var = c(1,3)) # rain-rain
plot.gbm(Hydrometry_Nave_di_Rosano_fit1, i.var = c(1,2)) # rain-rain
plot.gbm(Hydrometry_Nave_di_Rosano_fit1, i.var = c(2,3)) # rain-rain
plot.gbm(Hydrometry_Nave_di_Rosano_fit1, i.var = c(2,4)) # rain-rain
plot.gbm(Hydrometry_Nave_di_Rosano_fit1, i.var = c(3,4)) # rain-rain
plot.gbm(Hydrometry_Nave_di_Rosano_fit1, i.var = c(3,5)) # temp-rain
plot.gbm(Hydrometry_Nave_di_Rosano_fit1, i.var = c(4,5)) # temp-rain
plot.gbm(Hydrometry_Nave_di_Rosano_fit1, i.var = c(1,5)) # temp-rain
plot.gbm(Hydrometry_Nave_di_Rosano_fit1, i.var = c(2,5)) # temp-rain




### impact of different features on predicting depth to gw 

# summarise model 

Hydrometry_Nave_di_Rosano_effects <- tibble::as_tibble(gbm::summary.gbm(Hydrometry_Nave_di_Rosano_fit1,
                                                     plotit = F))
Hydrometry_Nave_di_Rosano_effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 6 features
Hydrometry_Nave_di_Rosano_effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


######  confronto i dataset con i lag
####arno0.5_LC_1 rain LE CROCI ####
#%>%
#dplyr::select( Rainfall_Cavallina,Rainfall_Le_Croci, Rainfall_Stia, Rainfall_Bibbiena, Temperature_Firenze,
#               Hydrometry_Nave_di_Rosano,
#               Season, rain0.5) 
LC0 <- arno0.5_LC_1 %>%
  dplyr::select( Rainfall_Cavallina, Rainfall_Stia, Rainfall_Bibbiena, Temperature_Firenze,
                 Hydrometry_Nave_di_Rosano,
                 Season, rain0.5) 

LC0.split <- initial_split(LC0,prop =.7)

LC0.train <- training(LC0.split)
LC0.test <- testing(LC0.split)

LC0.fit <- gbm::gbm(Hydrometry_Nave_di_Rosano ~ .,
                   data = LC0,
                   verbose = T, 
                   shrinkage = 0.01,
                   interaction.depth = 3, 
                   n.minobsinnode = 5,
                   n.trees = 1000,
                   cv.folds = 12)

LC0.perf <- gbm.perf(LC0.fit, method = "cv")

## make predictions 

LC0.pred<- stats::predict(object = LC0.fit,
                         newdata = LC0.test,
                         n.trees = LC0.perf)
LC0.rmse <- Metrics::rmse(actual = LC0.test$Hydrometry_Nave_di_Rosano,
                         predicted = LC0.pred)
print(LC0.rmse) 
#### 0.4397 # RMSE LCO= Le croci rain0 ####

## rain 1 le croci 

LC1 <- arno1.5_LC %>%
  dplyr::select( Rainfall_Cavallina, Rainfall_Stia, Rainfall_Bibbiena, Temperature_Firenze,
                 Hydrometry_Nave_di_Rosano,
                 Season, rain1.5)
str(LC1)

LC1.split <- initial_split(LC1,prop =.7)

LC1.train <- training(LC1.split)
LC1.test <- testing(LC1.split)

LC1.fit <- gbm::gbm(Hydrometry_Nave_di_Rosano ~ .,
                         data = LC1,
                         verbose = T, 
                         shrinkage = 0.01,
                         interaction.depth = 3, 
                         n.minobsinnode = 5,
                         n.trees = 600,
                         cv.folds = 12)

LC1.perf <- gbm.perf(LC1.fit, method = "cv")

## make predictions 

LC1.pred<- stats::predict(object = LC1.fit,
                               newdata = LC1.test,
                               n.trees = LC1.perf)
LC1.rmse <- Metrics::rmse(actual = LC1.test$Hydrometry_Nave_di_Rosano,
                               predicted = LC1.pred)
print(LC1.rmse) 
#### 0.4702  RMSE LC1###  

## rain 3 le croci 

#LC3 <- arno3_LC %>%
#  dplyr::select( Rainfall_Cavallina, Rainfall_Stia, Rainfall_Bibbiena, Temperature_Firenze,
#                 Hydrometry_Nave_di_Rosano,
#                 Season, rain3)

## rain 5 le croci 

LC5 <- arno5_LC %>%
  dplyr::select( Rainfall_Cavallina, Rainfall_Stia, Rainfall_Bibbiena, Temperature_Firenze,
                 Hydrometry_Nave_di_Rosano,
                 Season, rain5)

LC5.split <- initial_split(LC5,prop =.7)

LC5.train <- training(LC5.split)
LC5.test <- testing(LC5.split)

LC5.fit <- gbm::gbm(Hydrometry_Nave_di_Rosano ~ .,
                         data = LC5,
                         verbose = T, 
                         shrinkage = 0.01,
                         interaction.depth = 3, 
                         n.minobsinnode = 5,
                         n.trees = 600,
                         cv.folds = 12)

LC5.perf <- gbm.perf(LC5.fit, method = "cv")

## make predictions 

LC5.pred<- stats::predict(object = LC5.fit,
                               newdata = LC5.test,
                               n.trees = LC5.perf)
LC5.rmse <- Metrics::rmse(actual = LC5.test$Hydrometry_Nave_di_Rosano,
                               predicted = LC5.pred)
print(LC5.rmse) 
#### 0.4775 RMSE LE CROCI RAIN5 ####




### rain 0 Bibbiena 
bibbiena0 <- arno0.5_B %>%
  dplyr::select( Rainfall_Cavallina, Rainfall_Stia, Rainfall_Le_Croci, Temperature_Firenze,
                 Hydrometry_Nave_di_Rosano,
                 Season, rain0.5)

bibbiena0.split <- initial_split(bibbiena0,prop =.7)

bibbiena0.train <- training(bibbiena0.split)
bibbiena0.test <- testing(bibbiena0.split)

bibbiena0.fit <- gbm::gbm(Hydrometry_Nave_di_Rosano ~ .,
                   data = bibbiena0,
                   verbose = T, 
                   shrinkage = 0.01,
                   interaction.depth = 3, 
                   n.minobsinnode = 5,
                   n.trees = 600,
                   cv.folds = 12)

bibbiena0.perf <- gbm.perf(bibbiena0.fit, method = "cv")

## make predictions 

bibbiena0.pred<- stats::predict(object = bibbiena0.fit,
                         newdata = bibbiena0.test,
                         n.trees = bibbiena0.perf)
bibbiena0.rmse <- Metrics::rmse(actual = bibbiena0.test$Hydrometry_Nave_di_Rosano,
                         predicted = bibbiena0.pred)
print(bibbiena0.rmse
)
##### 0.4699 Bibbiena0 ####

### Bibbiena1 rain 1 
bibbiena1 <- arno1.5_B %>%
  dplyr::select( Rainfall_Cavallina, Rainfall_Stia, Rainfall_Le_Croci, Temperature_Firenze,
                 Hydrometry_Nave_di_Rosano,
                 Season, rain1.5)

bibbiena1.split <- initial_split(bibbiena1,prop =.7)

bibbiena1.train <- training(bibbiena1.split)
bibbiena1.test <- testing(bibbiena1.split)

bibbiena1.fit <- gbm::gbm(Hydrometry_Nave_di_Rosano ~ .,
                          data = bibbiena1,
                          verbose = T, 
                          shrinkage = 0.01,
                          interaction.depth = 3, 
                          n.minobsinnode = 5,
                          n.trees = 600,
                          cv.folds = 12)

bibbiena1.perf <- gbm.perf(bibbiena1.fit, method = "cv")

## make predictions 

bibbiena1.pred<- stats::predict(object = bibbiena1.fit,
                                newdata = bibbiena1.test,
                                n.trees = bibbiena1.perf)
bibbiena1.rmse <- Metrics::rmse(actual = bibbiena1.test$Hydrometry_Nave_di_Rosano,
                                predicted = bibbiena1.pred)
print(bibbiena1.rmse
)
##### 0.4919 Bibbiena1 ####

### Bibbiena3 rain 1

### Bibbiena5 rain 1

bibbiena5 <- arno5_B %>%
  dplyr::select( Rainfall_Cavallina, Rainfall_Stia, Rainfall_Le_Croci, Temperature_Firenze,
                 Hydrometry_Nave_di_Rosano,
                 Season, rain5)

bibbiena5.split <- initial_split(bibbiena5,prop =.7)

bibbiena5.train <- training(bibbiena5.split)
bibbiena5.test <- testing(bibbiena5.split)

bibbiena5.fit <- gbm::gbm(Hydrometry_Nave_di_Rosano ~ .,
                          data = bibbiena5,
                          verbose = T, 
                          shrinkage = 0.01,
                          interaction.depth = 3, 
                          n.minobsinnode = 5,
                          n.trees = 600,
                          cv.folds = 12)

bibbiena5.perf <- gbm.perf(bibbiena0.fit, method = "cv")

## make predictions 

bibbiena5.pred<- stats::predict(object = bibbiena5.fit,
                                newdata = bibbiena5.test,
                                n.trees = bibbiena5.perf)
bibbiena5.rmse <- Metrics::rmse(actual = bibbiena5.test$Hydrometry_Nave_di_Rosano,
                                predicted = bibbiena5.pred)
print(bibbiena5.rmse
)
##### 0.4417 Bibbiena5 #### 

### rain 0 Cavallina 
CA0 <- arno0.5_CA %>%
  dplyr::select( Rainfall_Bibbiena, Rainfall_Stia, Rainfall_Le_Croci, Temperature_Firenze,
                 Hydrometry_Nave_di_Rosano,
                 Season, rain0.5)

CA0.split <- initial_split(CA0,prop =.7)

CA0.train <- training(CA0.split)
CA0.test <- testing(CA0.split)

CA0.fit <- gbm::gbm(Hydrometry_Nave_di_Rosano ~ .,
                          data = CA0,
                          verbose = T, 
                          shrinkage = 0.01,
                          interaction.depth = 3, 
                          n.minobsinnode = 5,
                          n.trees = 600,
                          cv.folds = 12)

CA0.perf <- gbm.perf(CA0.fit, method = "cv")

## make predictions 

CA0.pred<- stats::predict(object = CA0.fit,
                                newdata = CA0.test,
                                n.trees = CA0.perf)
CA0.rmse <- Metrics::rmse(actual = CA0.test$Hydrometry_Nave_di_Rosano,
                                predicted = CA0.pred)
print(CA0.rmse
)
##### 0.4575 Cavallina0 ####

### rain 1 Cavallina 
CA1 <- arno1.5_CA %>%
  dplyr::select( Rainfall_Bibbiena, Rainfall_Stia, Rainfall_Le_Croci, Temperature_Firenze,
                 Hydrometry_Nave_di_Rosano,
                 Season, rain1.5)

CA1.split <- initial_split(CA1,prop =.7)

CA1.train <- training(CA1.split)
CA1.test <- testing(CA1.split)

CA1.fit <- gbm::gbm(Hydrometry_Nave_di_Rosano ~ .,
                    data = CA1,
                    verbose = T, 
                    shrinkage = 0.01,
                    interaction.depth = 3, 
                    n.minobsinnode = 5,
                    n.trees = 600,
                    cv.folds = 12)

CA1.perf <- gbm.perf(CA1.fit, method = "cv")

## make predictions 

CA1.pred<- stats::predict(object = CA1.fit,
                          newdata = CA1.test,
                          n.trees = CA1.perf)
CA1.rmse <- Metrics::rmse(actual = CA1.test$Hydrometry_Nave_di_Rosano,
                          predicted = CA1.pred)
print(CA1.rmse
)
##### 0.4486 Cavallina1 ####





### rain 5 Cavallina 
CA5 <- arno5_CA %>%
  dplyr::select( Rainfall_Bibbiena, Rainfall_Stia, Rainfall_Le_Croci, Temperature_Firenze,
                 Hydrometry_Nave_di_Rosano,
                 Season, rain5)

CA5.split <- initial_split(CA5,prop =.7)

CA5.train <- training(CA5.split)
CA5.test <- testing(CA5.split)

CA5.fit <- gbm::gbm(Hydrometry_Nave_di_Rosano ~ .,
                    data = CA5,
                    verbose = T, 
                    shrinkage = 0.01,
                    interaction.depth = 3, 
                    n.minobsinnode = 5,
                    n.trees = 600,
                    cv.folds = 12)

CA5.perf <- gbm.perf(CA5.fit, method = "cv")

## make predictions 

CA5.pred<- stats::predict(object = CA5.fit,
                          newdata = CA5.test,
                          n.trees = CA5.perf)
CA5.rmse <- Metrics::rmse(actual = CA5.test$Hydrometry_Nave_di_Rosano,
                          predicted = CA5.pred)
print(CA5.rmse
)
##### 0.4511 Cavallina5 ####

### rain 0 Stia 
ST0 <- arno0.5_S %>%
  dplyr::select( Rainfall_Bibbiena, Rainfall_Cavallina, Rainfall_Le_Croci, Temperature_Firenze,
                 Hydrometry_Nave_di_Rosano,
                 Season, rain0.5)

ST0.split <- initial_split(ST0,prop =.7)

ST0.train <- training(ST0.split)
ST0.test <- testing(ST0.split)

ST0.fit <- gbm::gbm(Hydrometry_Nave_di_Rosano ~ .,
                    data = ST0,
                    verbose = T, 
                    shrinkage = 0.01,
                    interaction.depth = 3, 
                    n.minobsinnode = 5,
                    n.trees = 600,
                    cv.folds = 12)

ST0.perf <- gbm.perf(ST0.fit, method = "cv")

## make predictions 

ST0.pred<- stats::predict(object = ST0.fit,
                          newdata = ST0.test,
                          n.trees = ST0.perf)
ST0.rmse <- Metrics::rmse(actual = ST0.test$Hydrometry_Nave_di_Rosano,
                          predicted = ST0.pred)
print(ST0.rmse
)
##### 0.4529 Stia0 ####





### rain 1 Stia 
ST1 <- arno1.5_S %>%
  dplyr::select( Rainfall_Bibbiena, Rainfall_Cavallina, Rainfall_Le_Croci, Temperature_Firenze,
                 Hydrometry_Nave_di_Rosano,
                 Season, rain1.5)

ST1.split <- initial_split(ST1,prop =.7)

ST1.train <- training(ST1.split)
ST1.test <- testing(ST1.split)

ST1.fit <- gbm::gbm(Hydrometry_Nave_di_Rosano ~ .,
                    data = ST1,
                    verbose = T, 
                    shrinkage = 0.01,
                    interaction.depth = 3, 
                    n.minobsinnode = 5,
                    n.trees = 600,
                    cv.folds = 12)

ST1.perf <- gbm.perf(ST1.fit, method = "cv")

## make predictions 

ST1.pred<- stats::predict(object = ST1.fit,
                          newdata = ST1.test,
                          n.trees = ST1.perf)
ST1.rmse <- Metrics::rmse(actual = ST1.test$Hydrometry_Nave_di_Rosano,
                          predicted = ST1.pred)
print(ST1.rmse
)
##### 0.4674 Stia1 ####


### rain 5 Stia 
ST5 <- arno5_S %>%
  dplyr::select( Rainfall_Bibbiena, Rainfall_Cavallina, Rainfall_Le_Croci, Temperature_Firenze,
                 Hydrometry_Nave_di_Rosano,
                 Season, rain5)

ST5.split <- initial_split(ST5,prop =.7)

ST5.train <- training(ST5.split)
ST5.test <- testing(ST5.split)

ST5.fit <- gbm::gbm(Hydrometry_Nave_di_Rosano ~ .,
                    data = ST5,
                    verbose = T, 
                    shrinkage = 0.01,
                    interaction.depth = 3, 
                    n.minobsinnode = 5,
                    n.trees = 600,
                    cv.folds = 12)

ST5.perf <- gbm.perf(ST5.fit, method = "cv")

## make predictions 

ST5.pred<- stats::predict(object = ST5.fit,
                          newdata = ST5.test,
                          n.trees = ST5.perf)
ST5.rmse <- Metrics::rmse(actual = ST5.test$Hydrometry_Nave_di_Rosano,
                          predicted = ST5.pred)
print(ST5.rmse
)
##### 0.4539 Stia5 ####

#RMSE Test: 0.48 per randomforest river arno

  ##River arno RMSE CONFRONTO RMSE gboost
  #RMSE 0.3423 Test2 con Le_Croci, Cavallina, Bibbiena, Stia THE BEST river arno
  
  #0.4397 # RMSE LCO= Le croci rain0 secondo classificato gboost
  #### 0.4702  RMSE LC1###  LE CROCI RAIN 1
  #### 0.4775 RMSE LE CROCI RAIN5 ####

##### 0.4699 Bibbiena0 ####
##### 0.4919 Bibbiena1 ####
##### 0.4417 Bibbiena5 #### 

##### 0.4575 Cavallina0 ####
##### 0.4486 Cavallina1 ####
##### 0.4511 Cavallina5 ####

##### 0.4529 Stia0 ####
##### 0.4674 Stia1 ####
##### 0.4539 Stia5 ####

#### PLOT  plot rmse LC0 ####
#plot - RAinfall_CAvallina
gbm::plot.gbm(LC0.fit, i.var = 1)
# plot - RAinfall_Stia
plot.gbm(LC0.fit, i.var = 2)
# plot - RAinfall_Bibbiena
plot.gbm(LC0.fit, i.var = 3)
# plot - Temperature_Firenze
plot.gbm(LC0.fit, i.var = 4)



## interactions of two features on the variable 

gbm::plot.gbm(LC0.fit, i.var = c(1,3)) # rain-rain
plot.gbm(LC0.fit, i.var = c(1,2)) # rain-rain
plot.gbm(LC0.fit, i.var = c(2,3)) # rain-rain

plot.gbm(LC0.fit, i.var = c(3,4)) # temp-rain
plot.gbm(LC0.fit, i.var = c(3,5)) # season-rain
plot.gbm(LC0.fit, i.var = c(4,5)) # temp-season
plot.gbm(LC0.fit, i.var = c(1,5)) # season-rain
plot.gbm(LC0.fit, i.var = c(2,5)) # season-rain




### impact of different features on predicting depth to gw 

# summarise model 

LC0.fit_effects <- tibble::as_tibble(gbm::summary.gbm(LC0.fit,
                                                                        plotit = F))
LC0.fit_effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 6 features
LC0.fit_effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")
#rain0.5 e' il lag per Rainfall_Le_Croci


