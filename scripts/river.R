#### River Arno Analisi / Cinzia Schiavuta ####
#### inizializzo il codice ####
getwd()
rm(list = ls(all=TRUE)) 

#### carico le librerie ####
library(reshape)
library(stats)
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
install.packages("metR")
library(metR)



##### leggo il dataset arno ####
River_Arno<-read.csv("data/River_Arno.csv")

#Trasformo la colonna data in data
River_Arno$Date<-as.Date(River_Arno$Date, format = "%d/%m/%Y")

### N/A Visualization
vis_dat(River_Arno)

str(River_Arno) 
names(River_Arno)
summary(River_Arno) #per avere una visione del dataframe

#### missing prima parte ####

###elimino i valori mancanti degli anni fino al 2004 , 
# delete rows with NA in feature to forecast
cutdata <- as.Date("2004-01-01")
River_Arno_cut <- River_Arno[(River_Arno$Date > cutdata), ]
### N/A Visualization
vis_dat(River_Arno_cut)

#### missing seconda parte ####
statsNA(River_Arno_cut$Rainfall_Le_Croci) #ok non ci sono missing
statsNA(River_Arno_cut$Rainfall_Cavallina) #ok non ci sono missing
statsNA(River_Arno_cut$Rainfall_S_Agata) #ok non ci sono missing
statsNA(River_Arno_cut$Rainfall_Mangona) #ok non ci sono missing
statsNA(River_Arno_cut$Rainfall_S_Piero) #ok non ci sono missing
statsNA(River_Arno_cut$Rainfall_Vernio) # richiede intervento 1743 mising
statsNA(River_Arno_cut$Rainfall_Stia) # richiede intervento 4742 missing
statsNA(River_Arno_cut$Rainfall_Consuma) # richiede intervento 4743 missing
statsNA(River_Arno_cut$Rainfall_Incisa) # richiede intervento 1457 missing
statsNA(River_Arno_cut$Rainfall_Montevarchi) # richiede intervento 4378 missing
statsNA(River_Arno_cut$Rainfall_S_Savino) # richiede intervento 4743 missing
statsNA(River_Arno_cut$Rainfall_Laterina) # richiede intervento 4743 missing
statsNA(River_Arno_cut$Rainfall_Bibbiena) # richiede intervento 3648 missing
statsNA(River_Arno_cut$Rainfall_Camaldoli) # richiede intervento 4743 missing
statsNA(River_Arno_cut$Temperature_Firenze) # richiede intervento 1062 missing
statsNA(River_Arno_cut$Hydrometry_Nave_di_Rosano)# ok
# scarico i dati dal 2011 al 2020 su 3bmeteo


#### FILLING GAPS WITH METEO ####

#### temperature Firenze

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}
temp_firenze<-0
temp_firenze_ls <- list.files(path = "./data/meteoFirenze/",
                          pattern = "*.csv$", 
                          full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating temp data 
temp_firenze <- temp_firenze_ls %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/meteoFirenze/", "", date1),
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
  rename(Date = date_final) %>%
  dplyr::select(Date, tmin, tmax) %>%
  mutate(Temperature_Firenze = rowMeans(subset(., select = c(tmin,tmax)),
                                         na.rm = T)) %>%
  dplyr::select(-tmin, -tmax) %>%
  arrange(Date)

summary(temp_firenze)

# visualizing trend in newly added temperature 

(vis_temp_firenze <- ggplot(temp_firenze, aes(Date, Temperature_Firenze))+
    geom_line()+
    theme_classic())

#### Rain_fall Bibbiena aggiungo i dati scaricati da 3bmeteo dal 2011

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

rf_bibbiena_ls <- list.files(path = "./data/meteoBibbiena/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  Bibbiena
rf_bibbiena <- rf_bibbiena_ls %>% 
  rename(date1 = filename) %>% 
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
  rename(Date = date_final) %>%
  select(Date, prec) %>%
  arrange(Date)

summary(rf_bibbiena)

# visualising trend in newly added rainfall Bibbiena 

(vis_rf_bibbiena <- ggplot(rf_bibbiena, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall Camaldoli aggiungo i dati scaricati da 3bmeteo dal 2011

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

rf_camaldoli_ls <- list.files(path = "./data/meteoCamaldoli/",
                             pattern = "*.csv$", 
                             full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  Camaldoli
rf_camaldoli <- rf_camaldoli_ls %>% 
  rename(date1 = filename) %>% 
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
  rename(Date = date_final) %>%
  select(Date, prec) %>%
  arrange(Date)

summary(rf_camaldoli)

# visualising trend in newly added rainfall Camaldoli 

(vis_rf_camaldoli <- ggplot(rf_camaldoli, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall Consuma aggiungo i dati scaricati da 3bmeteo dal 2011

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

rf_consuma_ls <- list.files(path = "./data/meteoConsuma/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  Consuma
rf_consuma <- rf_consuma_ls %>% 
  rename(date1 = filename) %>% 
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
  rename(Date = date_final) %>%
  select(Date, prec) %>%
  arrange(Date)

summary(rf_consuma)

# visualising trend in newly added rainfall Consuma

(vis_rf_consuma <- ggplot(rf_consuma, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall Incisa aggiungo i dati scaricati da 3bmeteo dal 2016

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

rf_incisa_ls <- list.files(path = "./data/meteoIncisa/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data Incisa
rf_incisa <- rf_incisa_ls %>% 
  rename(date1 = filename) %>% 
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
  rename(Date = date_final) %>%
  select(Date, prec) %>%
  arrange(Date)

summary(rf_incisa)

# visualising trend in newly added rainfall Incisa 

(vis_rf_incisa <- ggplot(rf_incisa, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall Laterina aggiungo i dati scaricati da 3bmeteo dal 2011

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

rf_laterina_ls <- list.files(path = "./data/meteoLaterina/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  LAterina
rf_laterina <- rf_laterina_ls %>% 
  rename(date1 = filename) %>% 
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
  rename(Date = date_final) %>%
  select(Date, prec) %>%
  arrange(Date)

summary(rf_laterina)

# visualising trend in newly added rainfall LAterina

(vis_rf_laterina <- ggplot(rf_camaldoli, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall Montevarchi aggiungo i dati scaricati da 3bmeteo dal 2011

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

rf_montevarchi_ls <- list.files(path = "./data/meteoMontevarchi/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  Montevarchi
rf_montevarchi <- rf_montevarchi_ls %>% 
  rename(date1 = filename) %>% 
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
  rename(Date = date_final) %>%
  select(Date, prec) %>%
  arrange(Date)

summary(rf_montevarchi)

# visualising trend in newly added rainfall MOntevarchi

(vis_rf_montevarchi <- ggplot(rf_camaldoli, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall San Savino aggiungo i dati scaricati da 3bmeteo dal 2011

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

rf_ssavino_ls <- list.files(path = "./data/meteoSSavino/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  San Savino
rf_ssavino <- rf_ssavino_ls %>% 
  rename(date1 = filename) %>% 
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
  rename(Date = date_final) %>%
  select(Date, prec) %>%
  arrange(Date)

summary(rf_ssavino)

# visualising trend in newly added rainfall San Savino

(vis_rf_ssavino<- ggplot(rf_ssavino, aes(Date, prec))+
    geom_line()+
    theme_classic())

#### Rain_fall Stia aggiungo i dati scaricati da 3bmeteo dal 2011

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

rf_stia_ls <- list.files(path = "./data/meteoStia/",
                              pattern = "*.csv$", 
                              full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating rain_fall data  Stia
rf_stia <- rf_stia_ls %>% 
  rename(date1 = filename) %>% 
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
  rename(Date = date_final) %>%
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
  rename(date1 = filename) %>% 
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
  rename(Date = date_final) %>%
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
vis_dat(River_Arno_cut1)
# delete rows with NA  non posso recuperare i dati dal
# 05-07-2007 al 01-01-2011
cutdata_1 <- as.Date("2007-07-05")
cutdata_2 <- as.Date("2011-01-01")
River_Arno_cut1 <- River_Arno_cut[(River_Arno_cut$Date > cutdata_2), ]
#River_Arno_cut1 e' il dataframe con una interruzione temporale dal
#2007 al 2011 e decido di prendere i dati solo dal 2011 in poi



summary(River_Arno_cut1)
statsNA(River_Arno_cut1$Rainfall_Stia)



max(River_Arno_cut1$Date[is.na(River_Arno_cut1$Hydrometry_Nave_di_Rosano )])
#ho un unico valore mancante nella variabile target Hydrometry_Nave_di_Rosano


##interpolo il valore mancante
# interpolation
River_Arno_cut1$Hydrometry_Nave_di_Rosano <-as.numeric(na.interp(River_Arno_cut1$Hydrometry_Nave_di_Rosano))

#TARGET: Hydrometry_Nave_di_Rosano
#Il livello delle acque sotterranee rilevato dalla stazione idrometrica 
#è stagionale, più elevato durante novembre-maggio
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

##Matrice di correlazione: sono presenti due cluster di precipitazioni, 
#quelli sulla sorgente e quelli lungo l'affluente principale
#il fiume Sieve che sono Vernio, S.Piero, MAngona, S_Agata Cavallina, Le Croci


# Correlation Matrix
df <- River_Arno_cut1
df$Date <- NULL
ggcorr(df, label = TRUE, label_round = 2, hjust = 1, size = 4, 
       layout.exp = 4, label_size = 3)
rm(df)


#### Rainfall analysis ####
# Rainfall analysis per le localita' lungo l'affluente Sieve
River_Arno_cut1$Rainfall_mean_Sieve <- rowMeans(River_Arno_cut1[,c("Rainfall_Le_Croci", "Rainfall_Cavallina", 
                                                   "Rainfall_S_Agata", "Rainfall_Mangona",
                                                   "Rainfall_S_Piero","Rainfall_Vernio")])


# Rainfall analysis per le localita' dalla sorgente dell'arno
River_Arno_cut1$Rainfall_mean_Sorgente <- rowMeans(River_Arno_cut1[,c("Rainfall_Camaldoli", "Rainfall_Bibbiena", 
                                                                   "Rainfall_Laterina", "Rainfall_S_Savino",
                                                                   "Rainfall_Montevarchi","Rainfall_Consuma",
                                                                   "Rainfall_Incisa", "Rainfall_Stia" )])
df <- River_Arno_cut1
rm(df)
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


# Temperature: the temperature mean is 16.61 °C
# Temperature analysis: ho riempito il dataset con le temperature
#reali prese da 3bmeteo dal 2011
mean(River_Arno_cut1$Temperature_Firenze)
df <- River_Arno_cut1 %>% dplyr::select(Date, Temperature_Firenze ) %>%
  pivot_longer(., cols = c(Temperature_Firenze ),
               names_to = "Var", values_to = "Val")
df <- df[complete.cases(df), ]
ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() + ggtitle("Temperature (°C) -  River Arno") +
  ylab("Temperature") + xlab("Date")
rm(df)






####inserisco la variabile stagioni####
##inserisco le stagioni####
River_Arno_cut1 <- River_Arno_cut1 %>%
  mutate(Season = case_when(month(Date) %in% c(3,4,5) ~ "Spring",                      
                            month(Date) %in% c(6,7,8) ~ "Summer",
                            month(Date) %in% c(9,10,11) ~ "Autumn",
                            month(Date) %in% c(1,2,12) ~ "Winter"))
River_Arno_cut1$Season<-factor(River_Arno_cut1$Season, 
                             levels=c("Winter","Spring", "Summer", "Autumn"))


 

#season(1, lang = "en")
#season(as.Date("2017-01-01"))

#### imposto tema ####
theme_21 <- theme(legend.position = "bottom", legend.direction = "horizontal", axis.text = element_text(size = 14), 
                  plot.caption = element_text(color = "gray25", face = "bold", size = 8), legend.text = element_text(size = 15), 
                  axis.title = element_text(size = 14.5, face = "bold", color = "gray25"), legend.title = element_text(size = 14), axis.line = element_line(size = 0.4), 
                  plot.title = element_text(size = 19), plot.subtitle = element_text(size = 14.5), strip.text = element_text(size = 14, face = "bold"))

core_col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

River_Arno_cut1%>%
  select(Date, Hydrometry_Nave_di_Rosano) %>%
  melt(., id.vars = "Date") %>%
  ggplot(., aes(Date, value))+
  facet_wrap(variable~., ncol = 1, scales = "free_y")+
  geom_line(size = 1.6, alpha = 0.8, col = "gray65")+
  geom_smooth(method = "loess", color = "firebrick3", size = 2.2, formula = y ~ x, fill = "firebrick4", alpha = 0.32)+
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", limits = as.Date(c("2004-01-01", "2020-06-30")))+
  labs(x = "Date", y = "Value", title = "The distribution of the explained variables (along with the loess curve)",
       subtitle = "in river Arno (from 01-2004)") + 
  theme_21
