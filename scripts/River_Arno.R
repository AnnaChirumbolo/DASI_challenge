#### River Arno Analisi / Cinzia Schiavuta ####
#### inizializzo il codice ####
getwd()
rm(list = ls(all=TRUE)) 

#### carico le librerie ####
#install.packages("recipes")
#install.packages("fastDummies")
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
library(data.table)
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

statsNA(River_Arno_cut$Rainfall_Stia) # richiede intervento 4742 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Stia)

statsNA(River_Arno_cut$Rainfall_Consuma) # richiede intervento 4743 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Consuma)

statsNA(River_Arno_cut$Rainfall_Incisa) # richiede intervento 1457 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Incisa)

statsNA(River_Arno_cut$Rainfall_Montevarchi) # richiede intervento 4378 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Montevarchi)

statsNA(River_Arno_cut$Rainfall_S_Savino) # richiede intervento 4743 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_S_Savino)

statsNA(River_Arno_cut$Rainfall_Laterina) # richiede intervento 4743 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Laterina)

statsNA(River_Arno_cut$Rainfall_Bibbiena) # richiede intervento 3648 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Bibbiena)

statsNA(River_Arno_cut$Rainfall_Camaldoli) # richiede intervento 4743 missing
ggplot_na_distribution(River_Arno_cut$Rainfall_Camaldoli)

statsNA(River_Arno_cut$Temperature_Firenze) # richiede intervento 1062 missing
ggplot_na_distribution(River_Arno_cut$Temperature_Firenze)

statsNA(River_Arno_cut$Hydrometry_Nave_di_Rosano)# ok
ggplot_na_distribution(River_Arno_cut$Hydrometry_Nave_di_Rosano)

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
         date_final = dmy(date_final)) %>% 
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
  select(-weekday) %>%
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
  select(Date, prec) %>%
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
  select(-weekday) %>%
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
  select(Date, prec) %>%
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
  select(-weekday) %>%
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
  select(Date, prec) %>%
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
  select(-weekday) %>%
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
  select(Date, prec) %>%
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
  select(-weekday) %>%
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
  select(Date, prec) %>%
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
  select(-weekday) %>%
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
  select(Date, prec) %>%
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
  select(-weekday) %>%
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
  select(Date, prec) %>%
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
  select(-weekday) %>%
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
  select(Date, prec) %>%
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
  select(-weekday) %>%
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
  select(Date, prec) %>%
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
# delete rows with NA  non posso recuperare i dati dal
# 05-07-2007 al 01-01-2011
cutdata_1 <- as.Date("2007-07-05")
cutdata_2 <- as.Date("2011-01-01")
River_Arno_cut1 <- River_Arno_cut[(River_Arno_cut$Date > cutdata_2), ]
#River_Arno_cut1 e' il dataframe con una interruzione temporale dal
#2007 al 2011 e decido di prendere i dati solo dal 2011 in poi

visdat::vis_dat(River_Arno_cut1)

summary(River_Arno_cut1)
statsNA(River_Arno_cut1$Hydrometry_Nave_di_Rosano)



max(River_Arno_cut1$Date[is.na(River_Arno_cut1$Hydrometry_Nave_di_Rosano )])
#ho un unico valore mancante nella variabile target Hydrometry_Nave_di_Rosano
#"2020-05-05"


####interpolo il valore mancante####
# interpolation
River_Arno_cut1$Hydrometry_Nave_di_Rosano <-as.numeric(na.approx(River_Arno_cut1$Hydrometry_Nave_di_Rosano))

#oppure
River_Arno_cut1$Hydrometry_Nave_di_Rosano <-as.numeric(na.interp(River_Arno_cut1$Hydrometry_Nave_di_Rosano))



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
rm(df)



#### Correlation Matrix ####
df <- River_Arno_cut1
df$Date <- NULL
ggcorr(df, label = TRUE, label_round = 2, hjust = 1, size = 4, 
       layout.exp = 4, label_size = 3)
rm(df)
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
River_Arno_cut1$Rainfall_mean_Sieve <- rowMeans(River_Arno_cut1[,c("Rainfall_Le_Croci", "Rainfall_Cavallina", 
                                                   "Rainfall_S_Agata", "Rainfall_Mangona",
                                                   "Rainfall_S_Piero","Rainfall_Vernio")])


# Rainfall analysis per le localita' dalla sorgente dell'arno
River_Arno_cut1$Rainfall_mean_Sorgente <- rowMeans(River_Arno_cut1[,c("Rainfall_Camaldoli", "Rainfall_Bibbiena", 
                                                                   "Rainfall_Laterina", "Rainfall_S_Savino",
                                                                   "Rainfall_Montevarchi","Rainfall_Consuma",
                                                                   "Rainfall_Incisa", "Rainfall_Stia" )])
df <- River_Arno_cut1
##rm(df)
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
rm(df)

#### visualizzo la pioggia per localita'####
River_Arno_cut1 %>%
  select(Date, Rainfall_Le_Croci, Rainfall_Cavallina, Rainfall_S_Agata, Rainfall_Mangona, Rainfall_S_Piero,
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
#abbiamo due gruppi (affluente Sieve e sorgente dell'arno) in cui le precipitazioni sono correlate tra loro, 
#ma non sono correlate tra i gruppi diversi.
#nella  maggior parte dei casi non superano i 100 mm  di pioggia 
#e a volte, in certe localita', raramente superano anche i 50 mm. 
#Decidiamo di scegliere due variabili, una per ogni gruppo: 
#dal primo gruppo, dell'affluente Sieve, scegliamo le precipitazioni da "Le Croci" 
#e dal secondo gruppo, della sorgente dell'Arno, scelgo "Stia" per la posizione geografica centrale, e per
#alta correlazione tra le altre componenti del proprio gruppo.

#poi, per fare un secondo test di forecast, possiam prendere
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

###correlazione seconda tabella di visibilita' con il metodo di spearman 
#+ le stagioni
River_Arno_cut1 %>%
  select(!c("Date", "Season")) %>%
  cor(., method = "spearman", use = "complete.obs") %>%
  corrplot(., method = "color", type = "upper", col = core_col(100),
           tl.col = "black",tl.srt = 35, diag = T, tl.cex = 0.72)
##Matrice di correlazione: anche in questa tabella,sono presenti due cluster di precipitazioni, 
#quelli sulla sorgente (Stia, Camaldoli, Bibbiena, Laterina, San Savino
# Montevarchi, Incisa, Consuma) e quelli lungo l'affluente principale
#il fiume Sieve che sono Vernio, S.Piero, Mangona, S_Agata, Cavallina, Le Croci


#analizzo la variabile target in base alla stagione:

River_Arno_cut1 %>%
  select(Season, Hydrometry_Nave_di_Rosano) %>%
  melt(., id.vars = "Season") %>%
  ggplot(., aes(Season, value))+
  facet_wrap(variable~., ncol = 1, scales = "free_y")+
  geom_boxplot(outlier.size = 1.1, outlier.shape = 20, lwd = 0.5, fatten = 1.1, 
               alpha = 0.95, width = 0.75, col = "gray10", fill = "#f8fc9d")+
  scale_x_discrete(limit = c("Spring", "Summer", "Autumn", "Winter"))+
  labs(x = "Season", y = "Value", title = "The distribution of the explained variables by season",
       subtitle = "river Arno") + 
  theme_21
#valori superiori a 5, che rappresentano piogge intense,
#si vedono solo in autunno e in inverno.
#le stagioni spiegano poco la variabile target: la mediana e' leggerment
# piu' alta in primavera e in inverno.





### andamento della variabile target####
River_Arno_cut1%>%
  select(Date, Hydrometry_Nave_di_Rosano) %>%
  melt(., id.vars = "Date") %>%
  ggplot(., aes(Date, value))+
  facet_wrap(variable~., ncol = 1, scales = "free_y")+
  geom_line(size = 1.6, alpha = 0.8, col = "gray65")+
  geom_smooth(method = "loess", color = "firebrick3", size = 2.2, formula = y ~ x, fill = "firebrick4", alpha = 0.32)+
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", limits = as.Date(c("2011-01-01", "2020-06-30")))+
  labs(x = "Date", y = "Value", title = "The distribution of the explained variables (along with the loess curve)",
       subtitle = "in river Arno (from 01-2011)") + 
  theme_21
#La variabile target esplicita  il livello del fiume espresso in metri 
#misurato dalla stazione idrometrica "Nave_di_Rosano". 
#I valori di questa variabile sono compresi tra 0 e 7 metri.
#La maggioranza dei valori misurati è compresa tra 1 e 4 metri. 
#Dopo qualche stagione irregolare, ci sono diversi cali 
#fino a 0 nel 2013, 2014 e 2019, che tornano rapidamente alla normalità.


#### RAndom Forest test ####
#### test 1 con localita Le_Croci e Stia ####

River_Arno_Season <- dummyVars(~Season, data = River_Arno_cut1, fullRank = F)
River_Arno_Season <- as.data.frame(predict(River_Arno_Season, newdata = River_Arno_cut1))

River_Arno_cut2 <- River_Arno_cut1 %>%
  select(Hydrometry_Nave_di_Rosano, Temperature_Firenze, Rainfall_Le_Croci, Rainfall_Stia)

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
  rfsrc(Hydrometry_Nave_di_Rosano~Season.Autumn+Season.Spring+Season.Summer+Season.Winter+Temperature_Firenze+
                                                   Rainfall_Le_Croci+Rainfall_Stia, 
        data = train_River_Arno, block.size = 1, importance = T, samptype = "swr", var.used = "all.trees", ntree = 200)

#procedo alla verifica del modello
plot(rf_River_Arno_Hydrometry_Nave_di_Rosano, verbose = F)

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

cat("RMSE Test:", round(rmse(pred_rf_River_Arno_Hydrometry_Nave_di_Rosano$predicted, test_River_Arno$Hydrometry_Nave_di_Rosano),2))
#RMSE Test: 0.48
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
  geom_abline(intercept = 0, slope = 1, col = "red1", size = 1.25)+
  labs(x = "Real values in the test set", y = "Predicted values in the test set", 
       title = "The predicted and true values on the test set", fill = "",
       subtitle = "Random forest model for Hydrometry_Nave_di_Rosano variable in Arno_river") + 
  theme_21+
  theme(legend.position = "bottom", legend.direction = "vertical")
# Possiamo osservare che la maggioranza dei valorisi trova vicino alla linea 
#di previsione e questo va bene dove ci sono errori#
#piccoli. Tuttavia ci sono dei valori con errore piu' grande

#### test 2 con localita Cavallina e Bibbiena ####
River_Arno_Season <- dummyVars(~Season, data = River_Arno_cut1, fullRank = F)
River_Arno_Season <- as.data.frame(predict(River_Arno_Season, newdata = River_Arno_cut1))

River_Arno_cut3 <- River_Arno_cut1 %>%
  select(Hydrometry_Nave_di_Rosano, Temperature_Firenze, Rainfall_Cavallina, Rainfall_Bibbiena)

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
  rfsrc(Hydrometry_Nave_di_Rosano~Season.Autumn+Season.Spring+Season.Summer+Season.Winter+Temperature_Firenze+
          Rainfall_Cavallina+Rainfall_Bibbiena, 
        data = train_River_Arno3, block.size = 1, importance = T, samptype = "swr", var.used = "all.trees", ntree = 200)

#procedo alla verifica del modello
plot(rf_River_Arno_Hydrometry_Nave_di_Rosano3, verbose = F)

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

cat("RMSE Test:", round(rmse(pred_rf_River_Arno_Hydrometry_Nave_di_Rosano3$predicted, test_River_Arno3$Hydrometry_Nave_di_Rosano),2))
#RMSE Test: 0.23
#
#L'RMSE sul set di prova è 0,23.
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