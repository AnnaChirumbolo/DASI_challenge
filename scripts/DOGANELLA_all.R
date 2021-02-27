#### doganella ####

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
                      tuneGrid = data.frame(nvmax = 1:9), # in this particular case, max 13 features for the variable
                      trControl = train.control,
                      na.action = na.omit)
  return(step.model)
}

getbestmodel.modif <- function (dates, values, freq, complete = 0, n_test = NA, graph = TRUE, 
                                algos = list("my.prophet", "my.ets", "my.sarima", 
                                             "my.tbats", "my.bats", "my.stlm", "my.shortterm"), 
                                bagged = "auto", metric.error = my.rmse) 
{
  . <- NULL
  freq.num <- getFrequency(freq)
  if (is.na(n_test)) 
    n_test <- freq.num[1]
  if (bagged == "auto") 
    algos <- list("my.prophet", "my.ets", "my.sarima", 
                  "my.tbats", "my.bats", "my.stlm", 
                  "my.shortterm")
  df <- complete.ts(dates, values, freq, complete = 0)
  fin <- max(df$dates[1:(length(df$dates) - n_test)])
  df_filter <- dplyr::filter(df, dates <= fin)
  full.TS <- prepare.ts(df$dates, df$val, freq, complete)
  filtered.TS <- prepare.ts(df_filter$dates, df_filter$val, 
                            freq, complete)
  train <- my.predictions(prepedTS = filtered.TS, algos = algos, 
                          n_pred = n_test) %>% dplyr::select(-.data$actual.value) %>% 
    dplyr::full_join(df, by = "dates") %>% dplyr::rename(actual.value = .data$val)
  errors <- dplyr::filter(train, .data$type == "mean") %>% 
    dplyr::summarise_if(is.numeric, list(~metric.error(., 
                                                       .data$actual.value))) %>% dplyr::select(-.data$actual.value)
  best <- names(errors)[apply(errors, which.min, MARGIN = 1)] %>% 
    paste("my", ., sep = ".")
  ddd <- dplyr::filter(train, .data$type %in% c(NA, "mean")) %>% 
    dplyr::select(-.data$type) %>% tidyr::gather(key = "algo", 
                                                 value = "val", -.data$dates)
  gg <- ggplot2::ggplot(ddd, ggplot2::aes(.data$dates, .data$val, 
                                          color = .data$algo)) + 
    ggplot2::geom_line(size = 1) + 
    ggplot2::theme_minimal() +
    ggplot2::xlab("")+
    ggplot2::ylab("Flow Rate (L/s)")
  if (graph == TRUE) {
    print(gg)
  }
  return(list(prepedTS = full.TS, best = best, train.errors = errors, 
              res.train = train, algos = algos, graph.train = gg))
}

mean.rain <- function(DF) {
  DF %>% group_by(weekly) %>% 
    mutate(mean.rain = mean(Rainfall_Velletri)) %>%
    arrange(weekly)
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
#install.packages("tsDyn")

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
library(tsDyn)
library(RColorBrewer)

#### reading dataset #### 

doganella <- read.csv("data/Aquifer_Doganella.csv")
str(doganella)

doganella1 <- doganella %>% 
  gather(key = "well", value = "depth_to_gw.m", 4:12) %>%
  rename(Date = ï..Date) %>% 
  mutate(Date = dmy(Date),
         well = gsub("Depth_to_Groundwater_","",well),
         well = gsub("_"," ",well))

(first_look <- ggplot(doganella1, aes(x =Date, y = abs(depth_to_gw.m), 
                                      color = well))+
    geom_line(size = .5)+
    ylab("Depth to groundwater (m)\n")+
    xlab("")+
    ggtitle("Distribution of target variables: Doganella\n")+
    scale_color_discrete(name = "")+
    theme_classic())

ggsave("img/doganella/first_look.jpg", first_look, 
       dpi = 500, width = 8, height = 6)

### missing ###

min(doganella1$Date[!is.na(doganella1$Volume_Pozzo_1)])

## removing missing for depth to gw

doganella_filtered <- doganella1 %>% 
  filter(Date >= "2016-10-07")

pal <- brewer.pal(n = 9, name = "Paired")

(second_look <- ggplot(doganella_filtered, aes(Date, abs(depth_to_gw.m), 
                                               color = well))+
    geom_line(size = .5)+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    ylab("Abs. depth to groundwater (m)")+
    scale_color_manual(values = pal,name="" )+
    ggtitle("Distribution of target variables: Doganella\n")+
    xlab(""))

ggsave("img/doganella/filtered_target.jpg", 
       dpi = 500, height = 6, width = 8)

### where the remaining missing? ###

doganella_missing <- doganella_filtered %>% 
  miss_var_summary()
print(doganella_missing)

# vis missing for all targets #

x_label <- doganella_filtered$Date

#### imputing missing data ####

doganella_filtered1 <- doganella_filtered %>% 
  spread(key = well, value = depth_to_gw.m)

doganella_filled <- data.frame(doganella_filtered1, lapply(doganella_filtered1[,c(4:11,14:22)], 
                                           function(x) na_ma(x, k=1)))

doganella_filled <-setnames(doganella_filled, 
                            old = colnames(doganella_filled[,31:39]),
                      new = c("1","2",
                              "3","4",
                              "5","6",
                              "7","8",
                              "9"))

doganella7 <- doganella_filled %>% 
  gather(key="imp", value = "target",31:39) %>%
  mutate(Date = ymd(Date))


## vis with imputation 
(imp_vis <- ggplot(doganella7, aes(Date, target, color = imp,
                                        group = imp))+
    geom_line()+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    scale_color_manual(name ="",values =pal)+
    ylab("Depth to groundwater (m)\n")+
    xlab("")+
    ggtitle("Imputed target variables\nDistribution over time: Doganella\n"))

ggsave("img/doganella/imp_vis.jpg", imp_vis, dpi = 500,
       width = 8, height = 6)



#### imputing volumes ####

doganella_vols <- doganella7[,1:30]
doganella_vols <-setnames(doganella_vols, 
                       old = colnames(doganella_vols[,23:30]),
                       new = c("1","2",
                               "3","4",
                               "5","6",
                               "7","8")) %>% 
  gather(key = "imp.vols", value = "volumes",23:30)

(imp_vis_vols <- ggplot(doganella_vols, aes(Date, volumes, color = imp.vols,
                                        group = imp.vols))+
   geom_line()+
   theme_classic()+
   scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
   scale_color_manual(name = "",values =pal)+
   ylab("Volume (m3)\n")+
  scale_y_continuous(limits = c(0,9000),expand = c(0,0))+
   xlab("")+
   ggtitle("Imputed volumes\nDistribution over time: Doganella\n"))

ggsave("img/doganella/imp_volumes.jpg", imp_vis_vols, dpi = 500,
       width = 8, height = 6)


#### checking for outliers #### 

doganella_spread <- doganella7 %>% 
  mutate(target = abs(target)) %>% 
  spread(key = "imp", value = "target") %>% 
  dplyr::select(Date, contains("temp"),
                contains("rain"), 23:39)

## boxplots

# target var 
(box_target_doganella <- ggplot(doganella7,
                                 aes(y = abs(target),
                                     color = imp))+
    geom_boxplot()+
    scale_color_manual(name = "",values = pal)+
    ylab("Depth to groundwater (m)\n")+
    ggtitle("Boxplot target variables: Doganella\n")+
    theme_classic())

# saving
ggsave("img/doganella/boxplot_target.jpg", box_target_doganella,
       dpi = 500, width = 10, height=7)

## extracting vals of potential outliers 

# for pozzo 2
out2_dog <- boxplot.stats(doganella_spread$`2`)$out # 112.5 (repeated x4)
out2_dog
out2_ind_dog <- which(doganella_spread$`2` %in% c(out2_dog))
out2_ind_dog # rows where outliers are found

upper_bound <- quantile(doganella_spread$`2`, 0.975)
upper_bound # 101.65

## checking stats to verify it's an outlier
# grubbs test
test2 <- grubbs.test(doganella_spread$`2`)
test2 # 112.5 is an outlier (at 5% significance level)

## substituting with q3 value 
doganella_spread$`2`[doganella_spread$`2` == 112.5] <- 101.65

# for pozzo 8
out8_dog <- boxplot.stats(doganella_spread$`8`) # 107.5 (rep x2)
out8_dog

test8 <- grubbs.test(doganella_spread$`8`)
test8 # it is an outlier 

# subst 
doganella_spread$`8`[doganella_spread$`8` == 107.5] <- 97.96

#### checking out for missing - rain and temp ####

# rain
statsNA(doganella_spread$Rainfall_Monteporzio)

ggplot_na_distribution(doganella_spread$Rainfall_Monteporzio)

statsNA(doganella_spread$Rainfall_Velletri)

ggplot_na_distribution(doganella_spread$Rainfall_Velletri)

# temp 
statsNA(doganella_spread$Temperature_Monteporzio)

ggplot_na_distribution(doganella_spread$Temperature_Monteporzio)

statsNA(doganella_spread$Temperature_Velletri)

ggplot_na_distribution(doganella_spread$Temperature_Velletri)

#### FILLING GAPS WITH METEO ####

#### temperature Velletri

temp_dog_ls <- list.files(path = "./data/DOGANELLA_3BMETEO_Velletri/", 
                          pattern = "*.csv$", 
                          full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating temp data 
meteo_dog <- temp_dog_ls %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/DOGANELLA_3BMETEO_Velletri/", "", date1),
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
         date_final = str_replace(date_final, "sett", "09"),
         date_final = str_replace(date_final, "ott", "10"),
         date_final = str_replace(date_final, "nov","11"),
         date_final = str_replace(date_final, "dec", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>%
  rename(Date = date_final) 

### temperature ### 

temp_dog <- meteo_dog %>%
  dplyr::select(Date, tmin, tmax) %>%
  mutate(Temperature_Velletri = rowMeans(subset(., select = c(tmin,tmax)),
                                         na.rm = T)) %>%
  dplyr::select(-tmin, -tmax) %>%
  arrange(Date) %>% 
  filter(Date >= "2016-10-07")

### rainfall ###

rain_dog <- meteo_dog %>% 
  dplyr::select(Date, prec) %>% 
  mutate(Rainfall_Velletri = rowMeans(subset(., select = prec),na.rm=T)) %>%
  dplyr::select(-prec) %>% 
  arrange(Date) %>% 
  filter(Date >= "2016-10-07")

#### temperature Monteporzio

temp_monte_dog_ls <- list.files(path = "./data/DOGANELLA_3BMETEO_Monteporzio/",
                                pattern = "*.csv$", 
                                full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating temp data 
meteo_monte_dog <- temp_monte_dog_ls %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/DOGANELLA_3BMETEO_Monteporzio/", "", date1),
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
         date_final = str_replace(date_final, "sett", "09"),
         date_final = str_replace(date_final, "ott", "10"),
         date_final = str_replace(date_final, "nov","11"),
         date_final = str_replace(date_final, "dec", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>%
  rename(Date = date_final) 

temp_monte_dog <- meteo_monte_dog %>%
  dplyr::select(Date, tmin, tmax) %>%
  mutate(Temperature_Monteporzio = rowMeans(subset(., select = c(tmin,tmax)),
                                            na.rm = T)) %>%
  dplyr::select(-tmin, -tmax) %>% 
  filter(Date >= "2016-10-07")

### rainfall

rain_monte_dog <- meteo_monte_dog %>% 
  dplyr::select(Date, prec) %>% 
  mutate(Rainfall_Monteporzio = rowMeans(subset(.,select=prec),na.rm = T)) %>% 
  dplyr::select(-prec) %>% 
  arrange(Date) %>% 
  filter(Date >= "2016-10-07")


#### adding rainfall data ####


doganella_spread$Rainfall_Velletri[is.na(doganella_spread$Rainfall_Velletri)] <- rain_dog$Rainfall_Velletri[match(doganella_spread$Date[is.na(doganella_spread$Rainfall_Velletri)],
                                                                                                                           rain_dog$Date)]


doganella_spread$Rainfall_Monteporzio[is.na(doganella_spread$Rainfall_Monteporzio)] <- rain_monte_dog$Rainfall_Monteporzio[match(doganella_spread$Date[is.na(doganella_spread$Rainfall_Monteporzio)],
                                                                                                                                          rain_monte_dog$Date)]

#### adding new temp data to prev dataset ####

doganella_spread$Temperature_Velletri[is.na(doganella_spread$Temperature_Velletri)] <- temp_dog$Temperature_Velletri[match(doganella_spread$Date[is.na(doganella_spread$Temperature_Velletri)],
                                                                                                                     temp_dog$Date)]


doganella_spread$Temperature_Monteporzio[is.na(doganella_spread$Temperature_Monteporzio)] <- temp_monte_dog$Temperature_Monteporzio[match(doganella_spread$Date[is.na(doganella_spread$Temperature_Monteporzio)],
                                                                                                                                    temp_monte_dog$Date)]

#### filled in the data
statsNA(doganella_spread$Temperature_Monteporzio) # still one na missing x9
statsNA(doganella_spread$Temperature_Velletri) # one row na missing x9

statsNA(doganella_spread$Rainfall_Monteporzio)
statsNA(doganella_spread$Rainfall_Velletri)

## imputing those vars 

doganella_tm <- doganella_spread %>% 
  mutate(Temperature_Monteporzio = na_ma(Temperature_Monteporzio, k = 1), # temp
         Temperature_Velletri = na_ma(Temperature_Velletri, k = 1),
         Rainfall_Monteporzio = na_ma(Rainfall_Monteporzio,k=1),
         Rainfall_Velletri = na_ma(Rainfall_Velletri, k=1)) 

doganella_meteo_plot <- doganella_tm %>% 
  gather(key ="rain",value = "mm", Rainfall_Monteporzio,Rainfall_Velletri) %>% 
  gather(key = "temp", value = "C", Temperature_Monteporzio,Temperature_Velletri) %>% 
  mutate(temp = gsub("Temperature_","",temp),
         rain = gsub("Rainfall_","",rain))

## vis distrib data with imputed vars 

## temperature ##
pal.temp <- brewer.pal(3,"Spectral")

temp <- ggplot(doganella_meteo_plot, aes(Date, C, group = temp, color = temp))+
  geom_line()+
  scale_color_manual(name ="Location\n",values =pal.temp )+
  scale_y_continuous(limits = c(0,31),expand = c(0,0))+
  ylab("Temperature (°C)\n")+
  xlab("")+
  labs(title = "Distribution of meteorological variables (after enrichment): Doganella\n",
       subtitle = "Temperature (°C)\n")+
  theme_classic()
temp

## rainfall ## 
rain <- ggplot(doganella_meteo_plot, aes(Date, mm, group = rain, color = rain))+
  geom_line()+
  scale_color_manual(name = "",values = pal)+
  scale_y_continuous(limits = c(0,131),expand = c(0,0))+
  xlab("")+
  ylab("Rainfall (mm)\n")+
  labs(subtitle = "Rainfall (mm)\n")+
  theme_classic()
rain

panel_meteo <- gridExtra::arrangeGrob(temp, rain,
                                      nrow = 2)
panel_meteo

ggsave("img/doganella/panel_meteo.jpg",panel_meteo,
       dpi = 500, width = 10, height = 9)


#### to model #### 

str(doganella_tm) 

doganella_tm <- doganella_tm %>% 
  gather(key = "welln",value ="volume",6:13) %>% 
  mutate(welln = gsub(".1","",welln),
         welln = gsub("_"," ",welln)) %>% 
  spread(welln, volume) 

doganella_featured <- add.seasons(doganella_tm) %>% 
  dplyr::select(-Rainfall_Monteporzio,-Temperature_Monteporzio) %>% # highly correlated with other sensor
  mutate(snow.yes = as.factor(ifelse(Temperature_Velletri <= 0 & Rainfall_Velletri > 0, 1,0)),
         snow.no = as.factor(ifelse(Temperature_Velletri,1,0)),
         weekly = lubridate::week(Date))

str(doganella_featured)

## prepping objects per target ##

meteo <- c("rain","temp")

pozzo1 <- doganella_featured %>% dplyr::select(., contains("1"), 
                                               contains(meteo), 21:27,
                                               `Volume Pozzo`)

pozzo2 <- doganella_featured %>% dplyr::select(., contains("2"), 
                                               contains(meteo), 21:27)

pozzo3 <- doganella_featured %>% dplyr::select(., contains("3"), 
                                               contains(meteo), 21:27) 

pozzo4 <- doganella_featured %>% dplyr::select(., contains("4"), 
                                               contains(meteo), 21:27)

pozzo5 <- doganella_featured%>% dplyr::select(., contains("5"), 
                                              contains(meteo), 21:27)

pozzo6 <- doganella_featured %>% dplyr::select(., contains("6"), 
                                         contains(meteo), 21:27)

pozzo7 <- doganella_featured %>% dplyr::select(., contains("7"), 
                                               contains(meteo), 21:27)

pozzo8 <- doganella_featured %>% dplyr::select(., contains("8"), 
                                               contains(meteo), 21:27)

pozzo9 <- doganella_featured %>% dplyr::select(., contains("9"), 
                                               contains(meteo), 21:27)

### creating a list 

list.pozzi <- list(pozzo1 = pozzo1,pozzo2 = pozzo2, 
                   pozzo3 = pozzo3, pozzo4 = pozzo4, 
                   pozzo5 = pozzo5, pozzo6 = pozzo6, 
                   pozzo7 = pozzo7, pozzo8 = pozzo8,
                   pozzo9 = pozzo9)

### adding other features - rainfall thresholds and lags 

list.pozzi1 <- map(list.pozzi, mean.rain)

## stats ##

boxplot(list.pozzi1$pozzo1$mean.rain)$stats
##stats:
# min - 0
# q1 - 1.44
# median - 2.83
# q3 - 4.72 
# max - 9.06

list.pozzi0.5 <- lapply(list.pozzi, function(x) cbind(x, 
                                                      rain0.5 = ifelse(x$Rainfall_Velletri <= 0.5,
                                                                       0, x$Rainfall_Velletri)))
list.pozzi1 <- lapply(list.pozzi, function(x) cbind(x, 
                                                    rain1 = ifelse(x$Rainfall_Velletri <= 1,
                                                                   0, x$Rainfall_Velletri)))
  
list.pozzi1.5 <- lapply(list.pozzi, function(x) cbind(x, 
                                                      rain1.5 = ifelse(x$Rainfall_Velletri <=1.5,
                                                                       0, x$Rainfall_Velletri)))

list.pozzi2.83 <- lapply(list.pozzi, function(x) cbind(x, 
                                                       rain2.83 = ifelse(x$Rainfall_Velletri <= 2.83,
                                                                         0, x$Rainfall_Velletri)))

#### choosing optima lag ####

pozzi.ts <- lapply(list.pozzi, function(x) ts(x, start = c(2016,2),
                                              frequency = 7))
pozzi.ts0.5 <- lapply(list.pozzi0.5, function(x) ts(x, start = c(2016, 2),
                                                    frequency = 7))
pozzi.ts1 <- lapply(list.pozzi1, function(x) ts(x, start = c(2016,2),
                                                frequency = 7))
pozzi.ts1.5 <- lapply(list.pozzi1.5, function(x) ts(x, start = c(2016,2),
                                                    frequency = 7))
pozzi.ts2.83 <- lapply(list.pozzi2.83, function(x) ts(x, start = c(2016,2),
                                                      frequency = 7))

lag.selectts <- lapply(pozzi.ts, function(x) lags.select(x, lag.max = 10))

lag.selectts0.5 <- lapply(pozzi.ts0.5, function(x) lags.select(x, lag.max = 10))

lag.selectts1 <- lapply(pozzi.ts1, function(x) lags.select(x, lag.max = 10))

lag.selectts1.5 <- lapply(pozzi.ts1.5, function(x) lags.select(x, lag.max = 10))

lags.selectts2.83 <- lapply(pozzi.ts2.83, function(x) lags.select(x, lag.max = 10))


#### adding lags ####

# ts - no threshold 

print(lag.selectts)

list.pozzi$pozzo1 <- cbind(list.pozzi$pozzo1, lag4 = Lag(list.pozzi$pozzo1$Rainfall_Velletri,+4))
list.pozzi$pozzo2 <- cbind(list.pozzi$pozzo2, lag4 = Lag(list.pozzi$pozzo2$Rainfall_Velletri,+4))
list.pozzi$pozzo3 <- cbind(list.pozzi$pozzo3, lag6 = Lag(list.pozzi$pozzo3$Rainfall_Velletri,+6))
list.pozzi$pozzo4 <- cbind(list.pozzi$pozzo4, lag1 = Lag(list.pozzi$pozzo4$Rainfall_Velletri,+1))
list.pozzi$pozzo5 <- cbind(list.pozzi$pozzo5, lag10 = Lag(list.pozzi$pozzo5$Rainfall_Velletri, +10))
list.pozzi$pozzo6 <- cbind(list.pozzi$pozzo6, lag4 = Lag(list.pozzi$pozzo6$Rainfall_Velletri, +4))
list.pozzi$pozzo7 <- cbind(list.pozzi$pozzo7, lag5 = Lag(list.pozzi$pozzo7$Rainfall_Velletri, +5))
list.pozzi$pozzo8 <- cbind(list.pozzi$pozzo8, lag5= Lag(list.pozzi$pozzo8$Rainfall_Velletri, +5))
list.pozzi$pozzo9 <- cbind(list.pozzi$pozzo9, lag1 = Lag(list.pozzi$pozzo9$Rainfall_Velletri, +1))

# ts - 0.5

print(lag.selectts0.5)

list.pozzi0.5$pozzo1 <- cbind(list.pozzi0.5$pozzo1, lag10 = Lag(list.pozzi0.5$pozzo1$rain0.5,+10))
list.pozzi0.5$pozzo2 <- cbind(list.pozzi0.5$pozzo2, lag9 = Lag(list.pozzi0.5$pozzo2$rain0.5,+9))
list.pozzi0.5$pozzo3 <- cbind(list.pozzi0.5$pozzo3, lag3 = Lag(list.pozzi0.5$pozzo3$rain0.5,+3))
list.pozzi0.5$pozzo4 <- cbind(list.pozzi0.5$pozzo4, lag2 = Lag(list.pozzi0.5$pozzo4$rain0.5, +2))
list.pozzi0.5$pozzo5 <- cbind(list.pozzi0.5$pozzo5, lag5 = Lag(list.pozzi0.5$pozzo5$rain0.5,+5))
list.pozzi0.5$pozzo6 <- cbind(list.pozzi0.5$pozzo6, lag7 = Lag(list.pozzi0.5$pozzo6$rain0.5,+7))
list.pozzi0.5$pozzo7 <- cbind(list.pozzi0.5$pozzo7, lag1 = Lag(list.pozzi0.5$pozzo7$rain0.5, +1))
list.pozzi0.5$pozzo8 <- cbind(list.pozzi0.5$pozzo8, lag9 = Lag(list.pozzi0.5$pozzo8$rain0.5,+9))
list.pozzi0.5$pozzo9 <- cbind(list.pozzi0.5$pozzo9, lag5 = Lag(list.pozzi0.5$pozzo9$rain0.5,+5))

# ts - 1

print(lag.selectts1)

list.pozzi1$pozzo1 <- cbind(list.pozzi1$pozzo1, lag3 = Lag(list.pozzi1$pozzo1$rain1, +3))
list.pozzi1$pozzo2 <- cbind(list.pozzi1$pozzo2, lag3 = Lag(list.pozzi1$pozzo2$rain1, +3))
list.pozzi1$pozzo3 <- cbind(list.pozzi1$pozzo3, lag9 = Lag(list.pozzi1$pozzo3$rain1,+9))
list.pozzi1$pozzo4 <- cbind(list.pozzi1$pozzo4, lag6 = Lag(list.pozzi1$pozzo4$rain1,+6))
list.pozzi1$pozzo5 <- cbind(list.pozzi1$pozzo5, lag8 = Lag(list.pozzi1$pozzo5$rain1,+8))
list.pozzi1$pozzo6 <- cbind(list.pozzi1$pozzo6, lag9 = Lag(list.pozzi1$pozzo6$rain1,+9))
list.pozzi1$pozzo7 <- cbind(list.pozzi1$pozzo7, lag5 = Lag(list.pozzi1$pozzo7$rain1,+5))
list.pozzi1$pozzo8 <- cbind(list.pozzi1$pozzo8, lag10 = Lag(list.pozzi1$pozzo8$rain1,+10))
list.pozzi1$pozzo9 <- cbind(list.pozzi1$pozzo9, lag2 = Lag(list.pozzi1$pozzo9$rain1,+2))

# ts - 1.5 

print(lag.selectts1.5)

list.pozzi1.5$pozzo1 <- cbind(list.pozzi1.5$pozzo1, lag7 = Lag(list.pozzi1.5$pozzo1$rain1.5,+7))
list.pozzi1.5$pozzo2 <- cbind(list.pozzi1.5$pozzo2, lag2 = Lag(list.pozzi1.5$pozzo2$rain1.5,+2))
list.pozzi1.5$pozzo3 <- cbind(list.pozzi1.5$pozzo3, lag1 = Lag(list.pozzi1.5$pozzo3$rain1.5,+1))
list.pozzi1.5$pozzo4 <- cbind(list.pozzi1.5$pozzo4, lag5 = Lag(list.pozzi1.5$pozzo4$rain1.5,+5))
list.pozzi1.5$pozzo5 <- cbind(list.pozzi1.5$pozzo5, lag6 = Lag(list.pozzi1.5$pozzo5$rain1.5,+6))
list.pozzi1.5$pozzo6 <- cbind(list.pozzi1.5$pozzo6, lag1 = Lag(list.pozzi1.5$pozzo6$rain1.5,+1))
list.pozzi1.5$pozzo7 <- cbind(list.pozzi1.5$pozzo7, lag2 = Lag(list.pozzi1.5$pozzo7$rain1.5,+2))
list.pozzi1.5$pozzo8 <- cbind(list.pozzi1.5$pozzo8, lag9 = Lag(list.pozzi1.5$pozzo8$rain1.5,+9))
list.pozzi1.5$pozzo9 <- cbind(list.pozzi1.5$pozzo9, lag4 = Lag(list.pozzi1.5$pozzo9$rain1.5,+4))

# ts - 2.83 

print(lags.selectts2.83)

list.pozzi2.83$pozzo1 <- cbind(list.pozzi2.83$pozzo1, lag5 = Lag(list.pozzi2.83$pozzo1$rain2.83,+5))
list.pozzi2.83$pozzo2 <- cbind(list.pozzi2.83$pozzo2, lag5 = Lag(list.pozzi2.83$pozzo2$rain2.83,+5))
list.pozzi2.83$pozzo3 <- cbind(list.pozzi2.83$pozzo3, lag2 = Lag(list.pozzi2.83$pozzo3$rain2.83,+2))
list.pozzi2.83$pozzo4 <- cbind(list.pozzi2.83$pozzo4, lag10 = Lag(list.pozzi2.83$pozzo4$rain2.83,+10))
list.pozzi2.83$pozzo5 <- cbind(list.pozzi2.83$pozzo5, lag2 = Lag(list.pozzi2.83$pozzo5$rain2.83,+2))
list.pozzi2.83$pozzo6 <- cbind(list.pozzi2.83$pozzo6, lag1 = Lag(list.pozzi2.83$pozzo6$rain2.83,+1))
list.pozzi2.83$pozzo7 <- cbind(list.pozzi2.83$pozzo7, lag1 = Lag(list.pozzi2.83$pozzo7$rain2.83,+1))
list.pozzi2.83$pozzo8 <- cbind(list.pozzi2.83$pozzo8, lag6 = Lag(list.pozzi2.83$pozzo8$rain2.83,+6))
list.pozzi2.83$pozzo9 <- cbind(list.pozzi2.83$pozzo9, lag2 = Lag(list.pozzi2.83$pozzo9$rain2.83,+2))

## removing unnecessary variables (weekly and rainfall_velletri for those with changed thresholds)

list.pozzi.all <- list(pozzi = list.pozzi,
                       pozzi0.5 = list.pozzi0.5,
                       pozzi1 = list.pozzi1,
                       pozzi1.5 = list.pozzi1.5,
                       pozzi2.83 = list.pozzi2.83)

list.pozzi.all <- lapply(list.pozzi.all, function(x) lapply(x, function(y) {y["weekly"] <- NULL; y}))

list.pozzi.all[2:5] <- lapply(list.pozzi.all[2:5], function(x) lapply(x, function(y) {y["Rainfall_Velletri"]<-NULL;y}))

#### separating by pozzo ####

list.pozzo1 <- unlist(lapply(list.pozzi.all, function(x) x["pozzo1"]),recursive = F)
list.pozzo2 <- unlist(lapply(list.pozzi.all, function(x) x["pozzo2"]),recursive=F)
list.pozzo3 <- unlist(lapply(list.pozzi.all, function(x) x["pozzo3"]),recursive = F)
list.pozzo4 <- unlist(lapply(list.pozzi.all, function(x) x["pozzo4"]),recursive=F)
list.pozzo5 <- unlist(lapply(list.pozzi.all, function(x) x["pozzo5"]),recursive = F)
list.pozzo6 <- unlist(lapply(list.pozzi.all, function(x) x["pozzo6"]),recursive = F)
list.pozzo7 <- unlist(lapply(list.pozzi.all, function(x) x["pozzo7"]),recursive = F)
list.pozzo8 <- unlist(lapply(list.pozzi.all, function(x) x["pozzo8"]),recursive = F)
list.pozzo9 <- unlist(lapply(list.pozzi.all, function(x) x["pozzo9"]),recursive = F)

#### AUTOML ####

h2o.init()
h2o.no_progress()

## pozzo 1 as trial ##

pozzo1.h2o <- lapply(list.pozzo1, function(x) as.h2o(x))

y <-"1"
#x <- lapply(pozzo1.h2o, function(x) setdiff(names(x), c(y)))

pozzo1.aml <- lapply(pozzo1.h2o, function(z) h2o.automl(y = y, x = setdiff(names(z),c(y)),
                                                        training_frame = z,
                                                        max_models = 10, 
                                                        seed = 1))

pozzo1.lb <- lapply(pozzo1.aml, function(x) x@leaderboard)
lapply(pozzo1.lb, print)

# Get model ids for all models in the AutoML Leaderboard
pozzo1.model_ids <- lapply(pozzo1.aml, function(x) as.data.frame(x@leaderboard$model_id)[,1])
# Get the "All Models" Stacked Ensemble model
pozzo1.se <- lapply(pozzo1.model_ids, function(x) h2o.getModel(grep("StackedEnsemble_AllModels", 
                                                                    x, value = TRUE)[1]))
# Get the Stacked Ensemble metalearner model
pozzo1.metalearner <- lapply(pozzo1.se,function(x) h2o.getModel(x@model$metalearner$name))

lapply(pozzo1.metalearner,function(x) h2o.varimp_plot(x))
## best model: gbm


#### pozzo 1 #### 

# test + train split 

set.seed(123)

list.pozzo1.split <- lapply(list.pozzo1, function(x) initial_split(x, prop=.7))
list.pozzo1.train <- lapply(list.pozzo1.split, function(x) training(x))
list.pozzo1.test <- lapply(list.pozzo1.split, function(x) testing(x))

list.pozzo1.fit <- lapply(list.pozzo1, function(x) gbm::gbm(`1` ~ .,
                                                            data = x, 
                                                            verbose = T,
                                                            shrinkage = 0.01,
                                                            interaction.depth = 3,
                                                            n.minobsinnode = 5,
                                                            n.trees = 5000,
                                                            cv.folds = 10))

list.pozzo1.perf <- lapply(list.pozzo1.fit, function(x) gbm.perf(x,method="cv"))

## make predictions 

# original

pozzo1_pred <- stats::predict(object = list.pozzo1.fit[[1]],
                               newdata = list.pozzo1.test[[1]],
                               n.trees = list.pozzo1.perf[[1]])

rmse_fit1 <- Metrics::rmse(actual = list.pozzo1.test$pozzi.pozzo1$`1`,
                           predicted = pozzo1_pred)
print(rmse_fit1) # 3.56

# 0.5

pozzo1pred0.5 <- stats::predict(object = list.pozzo1.fit[[2]],
                                newdata = list.pozzo1.test[[2]],
                                n.trees = list.pozzo1.perf[[2]])

rmsefit0.5 <- Metrics::rmse(actual = list.pozzo1.test[[2]]$`1`,
                            predicted = pozzo1pred0.5)
print(rmsefit0.5) # 2.94

# 1

pozzo1pred1 <- predict(object = list.pozzo1.fit[[3]],
                       newdata = list.pozzo1.test[[3]],
                       n.trees = list.pozzo1.perf[[3]])
rmsefit1 <- rmse(actual = list.pozzo1.test[[2]]$`1`,
                 predicted = pozzo1pred1)
print(rmsefit1) # 5.11

# 1.5 

pozzo1pred1.5 <- predict(object = list.pozzo1.fit[[4]],
                         newdata = list.pozzo1.test[[4]],
                         n.trees = list.pozzo1.perf[[4]])
rmsefit1.5 <- rmse(actual = list.pozzo1.test[[4]]$`1`,
                   predicted = pozzo1pred1.5)
print(rmsefit1.5) # 3.05

# 2.83 

pozzo1pred2.83 <- predict(object = list.pozzo1.fit[[5]],
                          newdata = list.pozzo1.test[[5]],
                          n.trees = list.pozzo1.perf[[5]])
rmse2.83 <- rmse(actual = list.pozzo1.test[[5]]$`1`,
                 predicted = pozzo1pred2.83)
print(rmse2.83) # 3.80

#### pozzo 2 #### 

# test + train split 

set.seed(123)

list.pozzo2.split <- lapply(list.pozzo2, function(x) initial_split(x, prop=.7))
list.pozzo2.train <- lapply(list.pozzo2.split, function(x) training(x))
list.pozzo2.test <- lapply(list.pozzo2.split, function(x) testing(x))

list.pozzo2.fit <- lapply(list.pozzo2, function(x) gbm::gbm(`2` ~ .,
                                                            data = x, 
                                                            verbose = T,
                                                            shrinkage = 0.01,
                                                            interaction.depth = 3,
                                                            n.minobsinnode = 5,
                                                            n.trees = 5000,
                                                            cv.folds = 10))

list.pozzo2.perf <- lapply(list.pozzo2.fit, function(x) gbm.perf(x,method="cv"))

## make predictions 

pozzo2_pred <- stats::predict(object = list.pozzo2.fit[[1]],
                              newdata = list.pozzo2.test[[1]],
                              n.trees = list.pozzo2.perf[[1]])

rmse_fit2 <- Metrics::rmse(actual = list.pozzo2.test$pozzi.pozzo2$`2`,
                           predicted = pozzo2_pred)
print(rmse_fit2) # 1.12

# 0.5

pozzo2pred0.5 <- stats::predict(object = list.pozzo2.fit[[2]],
                                newdata = list.pozzo2.test[[2]],
                                n.trees = list.pozzo2.perf[[2]])

rmse2fit0.5 <- Metrics::rmse(actual = list.pozzo2.test[[2]]$`2`,
                            predicted = pozzo2pred0.5)
print(rmse2fit0.5) # 1.04

# 1

pozzo2pred1 <- predict(object = list.pozzo2.fit[[3]],
                       newdata = list.pozzo2.test[[3]],
                       n.trees = list.pozzo2.perf[[3]])
rmse2fit1 <- rmse(actual = list.pozzo2.test[[2]]$`2`,
                 predicted = pozzo2pred1)
print(rmse2fit1) # 1.30

# 1.5 

pozzo2pred1.5 <- predict(object = list.pozzo2.fit[[4]],
                         newdata = list.pozzo2.test[[4]],
                         n.trees = list.pozzo2.perf[[4]])
rmse2fit1.5 <- rmse(actual = list.pozzo2.test[[4]]$`2`,
                   predicted = pozzo2pred1.5)
print(rmse2fit1.5) # 1.06

# 2.83 

pozzo2pred2.83 <- predict(object = list.pozzo2.fit[[5]],
                          newdata = list.pozzo2.test[[5]],
                          n.trees = list.pozzo2.perf[[5]])
rmse2fit3.83 <- rmse(actual = list.pozzo2.test[[5]]$`2`,
                     predicted = pozzo2pred2.83)
print(rmse2fit3.83) # 1.036

#### pozzo 3 ####

# test + train split 

set.seed(123)

list.pozzo3.split <- lapply(list.pozzo3, function(x) initial_split(x, prop=.7))
list.pozzo3.train <- lapply(list.pozzo3.split, function(x) training(x))
list.pozzo3.test <- lapply(list.pozzo3.split, function(x) testing(x))

list.pozzo3.fit <- lapply(list.pozzo3, function(x) gbm::gbm(`3` ~ .,
                                                            data = x, 
                                                            verbose = T,
                                                            shrinkage = 0.01,
                                                            interaction.depth = 3,
                                                            n.minobsinnode = 5,
                                                            n.trees = 5000,
                                                            cv.folds = 10))

list.pozzo3.perf <- lapply(list.pozzo3.fit, function(x) gbm.perf(x,method="cv"))

## make predictions 

pozzo3_pred <- stats::predict(object = list.pozzo3.fit[[1]],
                              newdata = list.pozzo3.test[[1]],
                              n.trees = list.pozzo3.perf[[1]])

rmse_fit3 <- Metrics::rmse(actual = list.pozzo3.test[[1]]$`3`,
                           predicted = pozzo3_pred)
print(rmse_fit3) # 1.68

# 0.5

pozzo3pred0.5 <- stats::predict(object = list.pozzo3.fit[[2]],
                                newdata = list.pozzo3.test[[2]],
                                n.trees = list.pozzo3.perf[[2]])

rmse3fit0.5 <- Metrics::rmse(actual = list.pozzo3.test[[2]]$`3`,
                             predicted = pozzo3pred0.5)
print(rmse3fit0.5) # 1.67

# 1

pozzo3pred1 <- predict(object = list.pozzo3.fit[[3]],
                       newdata = list.pozzo3.test[[3]],
                       n.trees = list.pozzo3.perf[[3]])
rmse3fit1 <- rmse(actual = list.pozzo3.test[[2]]$`3`,
                  predicted = pozzo3pred1)
print(rmse3fit1) # 2.38

# 1.5 

pozzo3pred1.5 <- predict(object = list.pozzo3.fit[[4]],
                         newdata = list.pozzo3.test[[4]],
                         n.trees = list.pozzo3.perf[[4]])
rmse3fit1.5 <- rmse(actual = list.pozzo3.test[[4]]$`3`,
                    predicted = pozzo3pred1.5)
print(rmse3fit1.5) # 1.59

# 2.83 

pozzo3pred2.83 <- predict(object = list.pozzo3.fit[[5]],
                          newdata = list.pozzo3.test[[5]],
                          n.trees = list.pozzo3.perf[[5]])
rmse3fit3.83 <- rmse(actual = list.pozzo3.test[[5]]$`3`,
                     predicted = pozzo3pred2.83)
print(rmse3fit3.83) # 1.66

#### pozzo 4 ####

# test + train split 

set.seed(123)

list.pozzo4.split <- lapply(list.pozzo4, function(x) initial_split(x, prop=.7))
list.pozzo4.train <- lapply(list.pozzo4.split, function(x) training(x))
list.pozzo4.test <- lapply(list.pozzo4.split, function(x) testing(x))

list.pozzo4.fit <- lapply(list.pozzo4, function(x) gbm::gbm(`4` ~ .,
                                                            data = x, 
                                                            verbose = T,
                                                            shrinkage = 0.01,
                                                            interaction.depth = 3,
                                                            n.minobsinnode = 5,
                                                            n.trees = 5000,
                                                            cv.folds = 10))

list.pozzo4.perf <- lapply(list.pozzo4.fit, function(x) gbm.perf(x,method="cv"))

## make predictions 

pozzo4_pred <- stats::predict(object = list.pozzo4.fit[[1]],
                              newdata = list.pozzo4.test[[1]],
                              n.trees = list.pozzo4.perf[[1]])

rmse_fit4 <- Metrics::rmse(actual = list.pozzo4.test[[1]]$`4`,
                           predicted = pozzo4_pred)
print(rmse_fit4) # 0.52

# 0.5

pozzo4pred0.5 <- stats::predict(object = list.pozzo4.fit[[2]],
                                newdata = list.pozzo4.test[[2]],
                                n.trees = list.pozzo4.perf[[2]])

rmse4fit0.5 <- Metrics::rmse(actual = list.pozzo4.test[[2]]$`4`,
                             predicted = pozzo4pred0.5)
print(rmse4fit0.5) # 0.68

# 1

pozzo4pred1 <- predict(object = list.pozzo4.fit[[3]],
                       newdata = list.pozzo4.test[[3]],
                       n.trees = list.pozzo4.perf[[3]])
rmse4fit1 <- rmse(actual = list.pozzo4.test[[2]]$`4`,
                  predicted = pozzo4pred1)
print(rmse4fit1) # 0.82

# 1.5 

pozzo4pred1.5 <- predict(object = list.pozzo4.fit[[4]],
                         newdata = list.pozzo4.test[[4]],
                         n.trees = list.pozzo4.perf[[4]])
rmse4fit1.5 <- rmse(actual = list.pozzo4.test[[4]]$`4`,
                    predicted = pozzo4pred1.5)
print(rmse4fit1.5) # 0.62

# 2.83 

pozzo4pred2.83 <- predict(object = list.pozzo4.fit[[5]],
                          newdata = list.pozzo4.test[[5]],
                          n.trees = list.pozzo4.perf[[5]])
rmse4fit3.83 <- rmse(actual = list.pozzo4.test[[5]]$`4`,
                     predicted = pozzo4pred2.83)
print(rmse4fit3.83) # 0.66

#### pozzo 5 ####

# test + train split 

set.seed(123)

list.pozzo5.split <- lapply(list.pozzo5, function(x) initial_split(x, prop=.7))
list.pozzo5.train <- lapply(list.pozzo5.split, function(x) training(x))
list.pozzo5.test <- lapply(list.pozzo5.split, function(x) testing(x))

list.pozzo5.fit <- lapply(list.pozzo5, function(x) gbm::gbm(`5` ~ .,
                                                            data = x, 
                                                            verbose = T,
                                                            shrinkage = 0.01,
                                                            interaction.depth = 3,
                                                            n.minobsinnode = 5,
                                                            n.trees = 5000,
                                                            cv.folds = 10))

list.pozzo5.perf <- lapply(list.pozzo5.fit, function(x) gbm.perf(x,method="cv"))

## make predictions 

pozzo5_pred <- stats::predict(object = list.pozzo5.fit[[1]],
                              newdata = list.pozzo5.test[[1]],
                              n.trees = list.pozzo5.perf[[1]])

rmse_fit5 <- Metrics::rmse(actual = list.pozzo5.test[[1]]$`5`,
                           predicted = pozzo5_pred)
print(rmse_fit5) # 1.12

# 0.5

pozzo5pred0.5 <- stats::predict(object = list.pozzo5.fit[[2]],
                                newdata = list.pozzo5.test[[2]],
                                n.trees = list.pozzo5.perf[[2]])

rmse5fit0.5 <- Metrics::rmse(actual = list.pozzo5.test[[2]]$`5`,
                             predicted = pozzo5pred0.5)
print(rmse5fit0.5) # 1.29

# 1

pozzo5pred1 <- predict(object = list.pozzo5.fit[[3]],
                       newdata = list.pozzo5.test[[3]],
                       n.trees = list.pozzo5.perf[[3]])
rmse5fit1 <- rmse(actual = list.pozzo5.test[[2]]$`5`,
                  predicted = pozzo5pred1)
print(rmse5fit1) # 1.52

# 1.5 

pozzo5pred1.5 <- predict(object = list.pozzo5.fit[[4]],
                         newdata = list.pozzo5.test[[4]],
                         n.trees = list.pozzo5.perf[[4]])
rmse5fit1.5 <- rmse(actual = list.pozzo5.test[[4]]$`5`,
                    predicted = pozzo5pred1.5)
print(rmse5fit1.5) # 1.16

# 2.83 

pozzo5pred2.83 <- predict(object = list.pozzo5.fit[[5]],
                          newdata = list.pozzo5.test[[5]],
                          n.trees = list.pozzo5.perf[[5]])
rmse5fit3.83 <- rmse(actual = list.pozzo5.test[[5]]$`5`,
                     predicted = pozzo5pred2.83)
print(rmse5fit3.83) # 1.17

#### pozzo 6 ####

# test + train split 

set.seed(123)

list.pozzo6.split <- lapply(list.pozzo6, function(x) initial_split(x, prop=.7))
list.pozzo6.train <- lapply(list.pozzo6.split, function(x) training(x))
list.pozzo6.test <- lapply(list.pozzo6.split, function(x) testing(x))

list.pozzo6.fit <- lapply(list.pozzo6, function(x) gbm::gbm(`6` ~ .,
                                                            data = x, 
                                                            verbose = T,
                                                            shrinkage = 0.01,
                                                            interaction.depth = 3,
                                                            n.minobsinnode = 5,
                                                            n.trees = 5000,
                                                            cv.folds = 10))

list.pozzo6.perf <- lapply(list.pozzo6.fit, function(x) gbm.perf(x,method="cv"))

## make predictions 

pozzo6_pred <- stats::predict(object = list.pozzo6.fit[[1]],
                              newdata = list.pozzo6.test[[1]],
                              n.trees = list.pozzo6.perf[[1]])

rmse_fit6 <- Metrics::rmse(actual = list.pozzo6.test[[1]]$`6`,
                           predicted = pozzo6_pred)
print(rmse_fit6) # 1.07

# 0.5

pozzo6pred0.5 <- stats::predict(object = list.pozzo6.fit[[2]],
                                newdata = list.pozzo6.test[[2]],
                                n.trees = list.pozzo6.perf[[2]])

rmse6fit0.5 <- Metrics::rmse(actual = list.pozzo6.test[[2]]$`6`,
                             predicted = pozzo6pred0.5)
print(rmse6fit0.5) # 1.00

# 1

pozzo6pred1 <- predict(object = list.pozzo6.fit[[3]],
                       newdata = list.pozzo6.test[[3]],
                       n.trees = list.pozzo6.perf[[3]])
rmse6fit1 <- rmse(actual = list.pozzo6.test[[2]]$`6`,
                  predicted = pozzo6pred1)
print(rmse6fit1) # 1.30

# 1.5 

pozzo6pred1.5 <- predict(object = list.pozzo6.fit[[4]],
                         newdata = list.pozzo6.test[[4]],
                         n.trees = list.pozzo6.perf[[4]])
rmse6fit1.5 <- rmse(actual = list.pozzo6.test[[4]]$`6`,
                    predicted = pozzo6pred1.5)
print(rmse6fit1.5) # 1.06

# 2.83 

pozzo6pred2.83 <- predict(object = list.pozzo6.fit[[5]],
                          newdata = list.pozzo6.test[[5]],
                          n.trees = list.pozzo6.perf[[5]])
rmse6fit3.83 <- rmse(actual = list.pozzo6.test[[5]]$`6`,
                     predicted = pozzo6pred2.83)
print(rmse6fit3.83) # 1.03

#### pozzo 7 ####

# test + train split 

set.seed(123)

list.pozzo7.split <- lapply(list.pozzo7, function(x) initial_split(x, prop=.7))
list.pozzo7.train <- lapply(list.pozzo7.split, function(x) training(x))
list.pozzo7.test <- lapply(list.pozzo7.split, function(x) testing(x))

list.pozzo7.fit <- lapply(list.pozzo7, function(x) gbm::gbm(`7` ~ .,
                                                            data = x, 
                                                            verbose = T,
                                                            shrinkage = 0.01,
                                                            interaction.depth = 3,
                                                            n.minobsinnode = 5,
                                                            n.trees = 5000,
                                                            cv.folds = 10))

list.pozzo7.perf <- lapply(list.pozzo7.fit, function(x) gbm.perf(x,method="cv"))

## make predictions 

pozzo7_pred <- stats::predict(object = list.pozzo7.fit[[1]],
                              newdata = list.pozzo7.test[[1]],
                              n.trees = list.pozzo7.perf[[1]])

rmse_fit7 <- Metrics::rmse(actual = list.pozzo7.test[[1]]$`7`,
                           predicted = pozzo7_pred)
print(rmse_fit7) # 0.72

# 0.5

pozzo7pred0.5 <- stats::predict(object = list.pozzo7.fit[[2]],
                                newdata = list.pozzo7.test[[2]],
                                n.trees = list.pozzo7.perf[[2]])

rmse7fit0.5 <- Metrics::rmse(actual = list.pozzo7.test[[2]]$`7`,
                             predicted = pozzo7pred0.5)
print(rmse7fit0.5) # 0.73

# 1

pozzo7pred1 <- predict(object = list.pozzo7.fit[[3]],
                       newdata = list.pozzo7.test[[3]],
                       n.trees = list.pozzo7.perf[[3]])
rmse7fit1 <- rmse(actual = list.pozzo7.test[[2]]$`7`,
                  predicted = pozzo7pred1)
print(rmse7fit1) # 1.13

# 1.5 

pozzo7pred1.5 <- predict(object = list.pozzo7.fit[[4]],
                         newdata = list.pozzo7.test[[4]],
                         n.trees = list.pozzo7.perf[[4]])
rmse7fit1.5 <- rmse(actual = list.pozzo7.test[[4]]$`7`,
                    predicted = pozzo7pred1.5)
print(rmse7fit1.5) # 0.71

# 2.83 

pozzo7pred2.83 <- predict(object = list.pozzo7.fit[[5]],
                          newdata = list.pozzo7.test[[5]],
                          n.trees = list.pozzo7.perf[[5]])
rmse7fit3.83 <- rmse(actual = list.pozzo7.test[[5]]$`7`,
                     predicted = pozzo7pred2.83)
print(rmse7fit3.83) # 0.76

#### pozzo 8 ####

# test + train split 

set.seed(123)

list.pozzo8.split <- lapply(list.pozzo8, function(x) initial_split(x, prop=.7))
list.pozzo8.train <- lapply(list.pozzo8.split, function(x) training(x))
list.pozzo8.test <- lapply(list.pozzo8.split, function(x) testing(x))

list.pozzo8.fit <- lapply(list.pozzo8, function(x) gbm::gbm(`8` ~ .,
                                                            data = x, 
                                                            verbose = T,
                                                            shrinkage = 0.01,
                                                            interaction.depth = 3,
                                                            n.minobsinnode = 5,
                                                            n.trees = 5000,
                                                            cv.folds = 10))

list.pozzo8.perf <- lapply(list.pozzo8.fit, function(x) gbm.perf(x,method="cv"))

## make predictions 

pozzo8_pred <- stats::predict(object = list.pozzo8.fit[[1]],
                              newdata = list.pozzo8.test[[1]],
                              n.trees = list.pozzo8.perf[[1]])

rmse_fit8 <- Metrics::rmse(actual = list.pozzo8.test[[1]]$`8`,
                           predicted = pozzo8_pred)
print(rmse_fit8) # 0.862

# 0.5

pozzo8pred0.5 <- stats::predict(object = list.pozzo8.fit[[2]],
                                newdata = list.pozzo8.test[[2]],
                                n.trees = list.pozzo8.perf[[2]])

rmse8fit0.5 <- Metrics::rmse(actual = list.pozzo8.test[[2]]$`8`,
                             predicted = pozzo8pred0.5)
print(rmse8fit0.5) # 0.863

# 1

pozzo8pred1 <- predict(object = list.pozzo8.fit[[3]],
                       newdata = list.pozzo8.test[[3]],
                       n.trees = list.pozzo8.perf[[3]])
rmse8fit1 <- rmse(actual = list.pozzo8.test[[2]]$`8`,
                  predicted = pozzo8pred1)
print(rmse8fit1) # 1.02

# 1.5 

pozzo8pred1.5 <- predict(object = list.pozzo8.fit[[4]],
                         newdata = list.pozzo8.test[[4]],
                         n.trees = list.pozzo8.perf[[4]])
rmse8fit1.5 <- rmse(actual = list.pozzo8.test[[4]]$`8`,
                    predicted = pozzo8pred1.5)
print(rmse8fit1.5) # 0.77

# 2.83 

pozzo8pred2.83 <- predict(object = list.pozzo8.fit[[5]],
                          newdata = list.pozzo8.test[[5]],
                          n.trees = list.pozzo8.perf[[5]])
rmse8fit3.83 <- rmse(actual = list.pozzo8.test[[5]]$`8`,
                     predicted = pozzo8pred2.83)
print(rmse8fit3.83) # 0.82

#### pozzo 9 ####

# test + train split 

set.seed(123)

list.pozzo9.split <- lapply(list.pozzo9, function(x) initial_split(x, prop=.7))
list.pozzo9.train <- lapply(list.pozzo9.split, function(x) training(x))
list.pozzo9.test <- lapply(list.pozzo9.split, function(x) testing(x))

list.pozzo9.fit <- lapply(list.pozzo9, function(x) gbm::gbm(`9` ~ .,
                                                            data = x, 
                                                            verbose = T,
                                                            shrinkage = 0.01,
                                                            interaction.depth = 3,
                                                            n.minobsinnode = 5,
                                                            n.trees = 5000,
                                                            cv.folds = 10))

list.pozzo9.perf <- lapply(list.pozzo9.fit, function(x) gbm.perf(x,method="cv"))

## make predictions 

pozzo9_pred <- stats::predict(object = list.pozzo9.fit[[1]],
                              newdata = list.pozzo9.test[[1]],
                              n.trees = list.pozzo9.perf[[1]])

rmse_fit9 <- Metrics::rmse(actual = list.pozzo9.test[[1]]$`9`,
                           predicted = pozzo9_pred)
print(rmse_fit9) # 2.68

# 0.5

pozzo9pred0.5 <- stats::predict(object = list.pozzo9.fit[[2]],
                                newdata = list.pozzo9.test[[2]],
                                n.trees = list.pozzo9.perf[[2]])

rmse9fit0.5 <- Metrics::rmse(actual = list.pozzo9.test[[2]]$`9`,
                             predicted = pozzo9pred0.5)
print(rmse9fit0.5) # 2.73

# 1

pozzo9pred1 <- predict(object = list.pozzo9.fit[[3]],
                       newdata = list.pozzo9.test[[3]],
                       n.trees = list.pozzo9.perf[[3]])
rmse9fit1 <- rmse(actual = list.pozzo9.test[[2]]$`9`,
                  predicted = pozzo9pred1)
print(rmse9fit1) # 2.98

# 1.5 

pozzo9pred1.5 <- predict(object = list.pozzo9.fit[[4]],
                         newdata = list.pozzo9.test[[4]],
                         n.trees = list.pozzo9.perf[[4]])
rmse9fit1.5 <- rmse(actual = list.pozzo9.test[[4]]$`9`,
                    predicted = pozzo9pred1.5)
print(rmse9fit1.5) # 2.79

# 2.83 

pozzo9pred2.83 <- predict(object = list.pozzo9.fit[[5]],
                          newdata = list.pozzo9.test[[5]],
                          n.trees = list.pozzo9.perf[[5]])
rmse9fit3.83 <- rmse(actual = list.pozzo9.test[[5]]$`9`,
                     predicted = pozzo9pred2.83)
print(rmse9fit3.83) # 2.69


#### choosing best models and plotting them ####

#### pozzo1 - best model: 0.5 mm ####

pozzo1pred0.5 <- stats::predict(object = list.pozzo1.fit[[2]],
                                newdata = list.pozzo1.test[[2]],
                                n.trees = list.pozzo1.perf[[2]])

rmsefit0.5 <- Metrics::rmse(actual = list.pozzo1.test[[2]]$`1`,
                            predicted = pozzo1pred0.5)
print(rmsefit0.5) # 2.94

# summarise model 

pozzo10.5.effects <- tibble::as_tibble(gbm::summary.gbm(list.pozzo1.fit[[2]],
                                                               plotit = F))
pozzo10.5.effects %>% utils::head() 

## plotting pred vs actual 

list.pozzo1.test[[2]]$predicted <- as.integer(predict(list.pozzo1.fit[[2]],
                                                      newdata = list.pozzo1.test[[2]],
                                                      n.trees = list.pozzo1.perf[[2]]))

reg <- lm(predicted ~ `1`, data = list.pozzo1.test[[2]])
reg

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)
coeff

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),
             "\nr.squared = ",r.sq)

# plot
(gbm_actualvspred <- ggplot(list.pozzo1.test[[2]]) +
    geom_point(aes(x = predicted,
                   y = `1`,
                   color = predicted - `1`),
               alpha = .7, size = 2) +
    geom_abline(intercept = 8.33,slope = 0.78, 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 50, y = 40, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (GBM): Pozzo 1 in Doganella\n",
         subtitle = "Minimum rainfall threshold at 0.5 mm\n")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())

ggsave("img/doganella/1_act_vs_pred.jpg",gbm_actualvspred,
       dpi = 500, width = 10,height=7)

# plotting 6 best vars by rel importance 

plot_rel.infl <- pozzo10.5.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  xlab("Features")+
  ylab("Relative Influence")+
  labs(title = "Relative influence of features on target variable (GBM):\nPozzo 1 in Doganella\n",
       subtitle = "Minimum rainfall threshold at 0.5 mm\n")+
  scale_color_brewer(palette = "Dark2") +
  theme_classic()+
  scale_fill_continuous(name = "Relative Influence")
plot_rel.infl

ggsave("img/doganella/1_rel_infl.gbm.jpg",plot_rel.infl,
       dpi = 500, width = 8, height = 6)

#### pozzo 2 - best model: 2.83 mm ####

pozzo2pred2.83 <- predict(object = list.pozzo2.fit[[5]],
                          newdata = list.pozzo2.test[[5]],
                          n.trees = list.pozzo2.perf[[5]])
rmse2fit3.83 <- rmse(actual = list.pozzo2.test[[5]]$`2`,
                     predicted = pozzo2pred2.83)
print(rmse2fit3.83) # 1.04

# summarise model 

pozzo22.83.effects <- tibble::as_tibble(gbm::summary.gbm(list.pozzo2.fit[[5]],
                                                        plotit = F))
pozzo22.83.effects %>% utils::head() 

## plotting pred vs actual 

list.pozzo2.test[[5]]$predicted <- as.integer(predict(list.pozzo2.fit[[5]],
                                                      newdata = list.pozzo2.test[[5]],
                                                      n.trees = list.pozzo2.perf[[5]]))

reg <- lm(predicted ~ `2`, data = list.pozzo2.test[[5]])
reg

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)
coeff

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),
             "\nr.squared = ",r.sq)

# plot
(gbm_actualvspred <- ggplot(list.pozzo2.test[[5]]) +
    geom_point(aes(x = predicted,
                   y = `2`,
                   color = predicted - `2`),
               alpha = .7, size = 2) +
    geom_abline(intercept = 14.48,slope = 0.85, 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 99, y = 93, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (GBM): Pozzo 2 in Doganella\n",
         subtitle = "Minimum rainfall threshold at 2.83 mm\n")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())

ggsave("img/doganella/2_act_vs_pred.jpg",gbm_actualvspred,
       dpi = 500, width = 10,height=7)

# plotting 6 best vars by rel importance 

plot_rel.infl <- pozzo22.83.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  xlab("Features")+
  ylab("Relative Influence")+
  labs(title = "Relative influence of features on target variable (GBM):\nPozzo 2 in Doganella\n",
       subtitle = "Minimum rainfall threshold at 2.83 mm\n")+
  scale_color_brewer(palette = "Dark2") +
  theme_classic()+
  scale_fill_continuous(name = "Relative Influence")
plot_rel.infl

ggsave("img/doganella/2_rel_infl.gbm.jpg",plot_rel.infl,
       dpi = 500, width = 8, height = 6)

#### pozzo 3 - best model: 1.5 mm ####

# 1.5 

pozzo3pred1.5 <- predict(object = list.pozzo3.fit[[4]],
                         newdata = list.pozzo3.test[[4]],
                         n.trees = list.pozzo3.perf[[4]])
rmse3fit1.5 <- rmse(actual = list.pozzo3.test[[4]]$`3`,
                    predicted = pozzo3pred1.5)
print(rmse3fit1.5) # 1.59

# summarise model 

pozzo31.5.effects <- tibble::as_tibble(gbm::summary.gbm(list.pozzo3.fit[[4]],
                                                         plotit = F))
pozzo31.5.effects %>% utils::head() 

## plotting pred vs actual 

list.pozzo3.test[[4]]$predicted <- as.integer(predict(list.pozzo3.fit[[4]],
                                                      newdata = list.pozzo3.test[[4]],
                                                      n.trees = list.pozzo3.perf[[4]]))

reg <- lm(predicted ~ `3`, data = list.pozzo3.test[[4]])
reg

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)
coeff

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),
             "\nr.squared = ",r.sq)

# plot
(gbm_actualvspred <- ggplot(list.pozzo3.test[[4]]) +
    geom_point(aes(x = predicted,
                   y = `3`,
                   color = predicted - `3`),
               alpha = .7, size = 2) +
    geom_abline(intercept = 34.21,slope = 0.70, 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 115, y = 107, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (GBM): Pozzo 2 in Doganella\n",
         subtitle = "Minimum rainfall threshold at 1.5 mm\n")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())

ggsave("img/doganella/3_act_vs_pred.jpg",gbm_actualvspred,
       dpi = 500, width = 10,height=7)

# plotting 6 best vars by rel importance 

plot_rel.infl <- pozzo31.5.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  xlab("Features")+
  ylab("Relative Influence")+
  labs(title = "Relative influence of features on target variable (GBM):\nPozzo 3 in Doganella\n",
       subtitle = "Minimum rainfall threshold at 1.5 mm\n")+
  scale_color_brewer(palette = "Dark2") +
  theme_classic()+
  scale_fill_continuous(name = "Relative Influence")
plot_rel.infl

ggsave("img/doganella/3_rel_infl.gbm.jpg",plot_rel.infl,
       dpi = 500, width = 8, height = 6)

#### pozzo 4 - best model: no threshold changed ####

pozzo4_pred <- stats::predict(object = list.pozzo4.fit[[1]],
                              newdata = list.pozzo4.test[[1]],
                              n.trees = list.pozzo4.perf[[1]])

rmse_fit4 <- Metrics::rmse(actual = list.pozzo4.test[[1]]$`4`,
                           predicted = pozzo4_pred)
print(rmse_fit4) # 0.52

# summarise model 

pozzo4.effects <- tibble::as_tibble(gbm::summary.gbm(list.pozzo4.fit[[1]],
                                                        plotit = F))
pozzo4.effects %>% utils::head() 

## plotting pred vs actual 

list.pozzo4.test[[1]]$predicted <- as.integer(predict(list.pozzo4.fit[[1]],
                                                      newdata = list.pozzo4.test[[1]],
                                                      n.trees = list.pozzo4.perf[[1]]))

reg <- lm(predicted ~ `4`, data = list.pozzo4.test[[1]])
reg

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)
coeff

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),
             "\nr.squared = ",r.sq)

# plot
(gbm_actualvspred <- ggplot(list.pozzo4.test[[1]]) +
    geom_point(aes(x = predicted,
                   y = `4`,
                   color = predicted - `4`),
               alpha = .7, size = 2) +
    geom_abline(intercept = 13.82,slope = 0.86, 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 101.5, y = 99, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (GBM): Pozzo 4 in Doganella\n",
         subtitle = "No minimum threshold\n")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())

ggsave("img/doganella/4_act_vs_pred.jpg",gbm_actualvspred,
       dpi = 500, width = 10,height=7)

# plotting 6 best vars by rel importance 

plot_rel.infl <- pozzo4.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  xlab("Features")+
  ylab("Relative Influence")+
  labs(title = "Relative influence of features on target variable (GBM):\nPozzo 4 in Doganella\n",
       subtitle = "No minimum threshold\n")+
  scale_color_brewer(palette = "Dark2") +
  theme_classic()+
  scale_fill_continuous(name = "Relative Influence")
plot_rel.infl

ggsave("img/doganella/4_rel_infl.gbm.jpg",plot_rel.infl,
       dpi = 500, width = 8, height = 6)

#### pozzo 5 - best model: no threshold changed ####

pozzo5_pred <- stats::predict(object = list.pozzo5.fit[[1]],
                              newdata = list.pozzo5.test[[1]],
                              n.trees = list.pozzo5.perf[[1]])

rmse_fit5 <- Metrics::rmse(actual = list.pozzo5.test[[1]]$`5`,
                           predicted = pozzo5_pred)
print(rmse_fit5) # 1.12

# summarise model 

pozzo5.effects <- tibble::as_tibble(gbm::summary.gbm(list.pozzo5.fit[[1]],
                                                     plotit = F))
pozzo5.effects %>% utils::head() 

## plotting pred vs actual 

list.pozzo5.test[[1]]$predicted <- as.integer(predict(list.pozzo5.fit[[1]],
                                                      newdata = list.pozzo5.test[[1]],
                                                      n.trees = list.pozzo5.perf[[1]]))

reg <- lm(predicted ~ `5`, data = list.pozzo5.test[[1]])
reg

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),
             "\nr.squared = ",r.sq)

# plot
(gbm_actualvspred <- ggplot(list.pozzo5.test[[1]]) +
    geom_point(aes(x = predicted,
                   y = `5`,
                   color = predicted - `5`),
               alpha = .7, size = 2) +
    geom_abline(intercept = 27.05,slope = 0.75, 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 105, y = 107.5, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (GBM): Pozzo 5 in Doganella\n",
         subtitle = "No minimum threshold\n")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())

ggsave("img/doganella/5_act_vs_pred.jpg",gbm_actualvspred,
       dpi = 500, width = 10,height=7)

# plotting 6 best vars by rel importance 

plot_rel.infl <- pozzo5.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  xlab("Features")+
  ylab("Relative Influence")+
  labs(title = "Relative influence of features on target variable (GBM):\nPozzo 5 in Doganella\n",
       subtitle = "No minimum threshold\n")+
  scale_color_brewer(palette = "Dark2") +
  theme_classic()+
  scale_fill_continuous(name = "Relative Influence")
plot_rel.infl

ggsave("img/doganella/5_rel_infl.gbm.jpg",plot_rel.infl,
       dpi = 500, width = 8, height = 6)

#### pozzo 6 - best model: 0.5 mm ####

# 0.5

pozzo6pred0.5 <- stats::predict(object = list.pozzo6.fit[[2]],
                                newdata = list.pozzo6.test[[2]],
                                n.trees = list.pozzo6.perf[[2]])

rmse6fit0.5 <- Metrics::rmse(actual = list.pozzo6.test[[2]]$`6`,
                             predicted = pozzo6pred0.5)
print(rmse6fit0.5) # 1.00

# summarise model 

pozzo60.5.effects <- tibble::as_tibble(gbm::summary.gbm(list.pozzo6.fit[[2]],
                                                     plotit = F))
pozzo60.5.effects %>% utils::head() 

## plotting pred vs actual 

list.pozzo6.test[[2]]$predicted <- as.integer(predict(list.pozzo6.fit[[2]],
                                                      newdata = list.pozzo6.test[[2]],
                                                      n.trees = list.pozzo6.perf[[2]]))

reg <- lm(predicted ~ `6`, data = list.pozzo6.test[[2]])
reg

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)
coeff
eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),
             "\nr.squared = ",r.sq)

# plot
(gbm_actualvspred <- ggplot(list.pozzo6.test[[2]]) +
    geom_point(aes(x = predicted,
                   y = `6`,
                   color = predicted - `6`),
               alpha = .7, size = 2) +
    geom_abline(intercept = 28.69, slope = 0.69, 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 97, y = 92.5, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (GBM): Pozzo 6 in Doganella\n",
         subtitle = "Minimum rainfall threshold at 0.5 mm\n")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())

ggsave("img/doganella/6_act_vs_pred.jpg",gbm_actualvspred,
       dpi = 500, width = 10,height=7)

# plotting 6 best vars by rel importance 

plot_rel.infl <- pozzo60.5.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  xlab("Features")+
  ylab("Relative Influence")+
  labs(title = "Relative influence of features on target variable (GBM):\nPozzo 6 in Doganella\n",
       subtitle = "Minimum rainfall threshold at 0.5 mm\n")+
  scale_color_brewer(palette = "Dark2") +
  theme_classic()+
  scale_fill_continuous(name = "Relative Influence")
plot_rel.infl

ggsave("img/doganella/6_rel_infl.gbm.jpg",plot_rel.infl,
       dpi = 500, width = 8, height = 6)

#### pozzo 7 - best model: ####

# 1.5 

pozzo7pred1.5 <- predict(object = list.pozzo7.fit[[4]],
                         newdata = list.pozzo7.test[[4]],
                         n.trees = list.pozzo7.perf[[4]])
rmse7fit1.5 <- rmse(actual = list.pozzo7.test[[4]]$`7`,
                    predicted = pozzo7pred1.5)
print(rmse7fit1.5) # 0.71

# summarise model 

pozzo71.5.effects <- tibble::as_tibble(gbm::summary.gbm(list.pozzo7.fit[[4]],
                                                        plotit = F))
pozzo71.5.effects %>% utils::head() 

## plotting pred vs actual 

list.pozzo7.test[[4]]$predicted <- as.integer(predict(list.pozzo7.fit[[4]],
                                                      newdata = list.pozzo7.test[[4]],
                                                      n.trees = list.pozzo7.perf[[4]]))

reg <- lm(predicted ~ `7`, data = list.pozzo7.test[[4]])
reg

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),
             "\nr.squared = ",r.sq)

# plot
(gbm_actualvspred <- ggplot(list.pozzo7.test[[4]]) +
    geom_point(aes(x = predicted,
                   y = `7`,
                   color = predicted - `7`),
               alpha = .7, size = 2) +
    geom_abline(intercept = 54.10, slope = 0.43, 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 94, y = 96, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (GBM): Pozzo 7 in Doganella\n",
         subtitle = "Minimum rainfall threshold at 1.5 mm\n")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())

ggsave("img/doganella/7_act_vs_pred.jpg",gbm_actualvspred,
       dpi = 500, width = 10,height=7)

# plotting 6 best vars by rel importance 

plot_rel.infl <- pozzo71.5.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  xlab("Features")+
  ylab("Relative Influence")+
  labs(title = "Relative influence of features on target variable (GBM):\nPozzo 7 in Doganella\n", 
       subtitle = "Minimum rainfall threshold at 1.5 mm\n")+
  scale_color_brewer(palette = "Dark2") +
  theme_classic()+
  scale_fill_continuous(name = "Relative Influence")
plot_rel.infl

ggsave("img/doganella/7_rel_infl.gbm.jpg",plot_rel.infl,
       dpi = 500, width = 8, height = 6)

#### pozzo 8 - best model: 1.5 mm ####

pozzo8pred1.5 <- predict(object = list.pozzo8.fit[[4]],
                         newdata = list.pozzo8.test[[4]],
                         n.trees = list.pozzo8.perf[[4]])
rmse8fit1.5 <- rmse(actual = list.pozzo8.test[[4]]$`8`,
                    predicted = pozzo8pred1.5)
print(rmse8fit1.5) # 0.77

# summarise model 

pozzo81.5.effects <- tibble::as_tibble(gbm::summary.gbm(list.pozzo8.fit[[4]],
                                                        plotit = F))
pozzo81.5.effects %>% utils::head() 

## plotting pred vs actual 

list.pozzo8.test[[4]]$predicted <- as.integer(predict(list.pozzo8.fit[[4]],
                                                      newdata = list.pozzo8.test[[4]],
                                                      n.trees = list.pozzo8.perf[[4]]))

reg <- lm(predicted ~ `8`, data = list.pozzo8.test[[4]])
reg

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),
             "\nr.squared = ",r.sq)

# plot
(gbm_actualvspred <- ggplot(list.pozzo8.test[[4]]) +
    geom_point(aes(x = predicted,
                   y = `8`,
                   color = predicted - `8`),
               alpha = .7, size = 2) +
    geom_abline(intercept = 23.03, slope = 0.76, 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 95, y = 102, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (GBM): Pozzo 8 in Doganella\n", 
         subtitle = "Minimum rainfall threshold at 1.5 mm\n")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())

ggsave("img/doganella/8_act_vs_pred.jpg",gbm_actualvspred,
       dpi = 500, width = 10,height=7)

# plotting 6 best vars by rel importance 

plot_rel.infl <- pozzo81.5.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  xlab("Features")+
  ylab("Relative Influence")+
  labs(title = "Relative influence of features on target variable (GBM):\nPozzo 8 in Doganella\n",
       subtitle = "Minimum rainfall threshold at 1.5 mm\n")+
  scale_color_brewer(palette = "Dark2") +
  theme_classic()+
  scale_fill_continuous(name = "Relative Influence")
plot_rel.infl

ggsave("img/doganella/8_rel_infl.gbm.jpg",plot_rel.infl,
       dpi = 500, width = 8, height = 6)

#### pozzo 9 - best model: no threshold changed ####

pozzo9_pred <- stats::predict(object = list.pozzo9.fit[[1]],
                              newdata = list.pozzo9.test[[1]],
                              n.trees = list.pozzo9.perf[[1]])

rmse_fit9 <- Metrics::rmse(actual = list.pozzo9.test[[1]]$`9`,
                           predicted = pozzo9_pred)
print(rmse_fit9) # 2.68

# summarise model 

pozzo9.effects <- tibble::as_tibble(gbm::summary.gbm(list.pozzo9.fit[[1]],
                                                        plotit = F))
pozzo9.effects %>% utils::head() 

## plotting pred vs actual 

list.pozzo9.test[[1]]$predicted <- as.integer(predict(list.pozzo9.fit[[1]],
                                                      newdata = list.pozzo9.test[[1]],
                                                      n.trees = list.pozzo9.perf[[1]]))

reg <- lm(predicted ~ `9`, data = list.pozzo9.test[[1]])
reg

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),
             "\nr.squared = ",r.sq)

# plot
(gbm_actualvspred <- ggplot(list.pozzo9.test[[1]]) +
    geom_point(aes(x = predicted,
                   y = `9`,
                   color = predicted - `9`),
               alpha = .7, size = 2) +
    geom_abline(intercept = 30.10, slope = 0.68, 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 95, y = 100, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (GBM): Pozzo 9 in Doganella\n",
         subtitle = "No minimum threshold\n")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())

ggsave("img/doganella/9_act_vs_pred.jpg",gbm_actualvspred,
       dpi = 500, width = 10,height=7)

# plotting 6 best vars by rel importance 

plot_rel.infl <- pozzo9.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  xlab("Features")+
  ylab("Relative Influence")+
  labs(title = "Relative influence of features on target variable (GBM):\nPozzo 9 in Doganella\n", 
       subtitle = "No minimum threshold\n")+
  scale_color_brewer(palette = "Dark2") +
  theme_classic()+
  scale_fill_continuous(name = "Relative Influence")
plot_rel.infl

ggsave("img/doganella/9_rel_infl.gbm.jpg",plot_rel.infl,
       dpi = 500, width = 8, height = 6)

