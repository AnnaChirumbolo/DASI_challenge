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

ggplot_na_distribution(abs(doganella_filtered$depth_to_gw.m),
                       title = "Distribution of Missing Target Values (Madonna di Canneto)\n",
                       ylab = "Depth to groundwater (m)\n",
                       theme = ggplot2::theme_classic(),
                       x_axis_labels = x_label,
                       xlab="")

#### imputing missing data ####

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
(box_target_doganella <- ggplot(doganella8,
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

mean.rain <- function(DF) {
  DF %>% group_by(weekly) %>% 
    mutate(mean.rain = mean(Rainfall_Velletri)) %>%
    arrange(weekly)
}

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

#### NEED TO DO AUTOTS AND AUTOML ####


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


pozzo1_pred1 <- stats::predict(object = pozzo1_fit1,
                               newdata = pozzo1.test,
                               n.trees = perf_gbm1)
rmse_fit1 <- Metrics::rmse(actual = pozzo1.test$imp1,
                           predicted = pozzo1_pred1)
print(rmse_fit1) # 9.313319
#(higher error than when keeping all variables)

#plot - rain velletri
gbm::plot.gbm(pozzo1_fit1, i.var = 1)
# plot - temp monteporzio
plot.gbm(pozzo1_fit1, i.var = 2)
# plot - temp velletri
plot.gbm(pozzo1_fit1, i.var = 3)

## interactions of two features on the variable 

gbm::plot.gbm(pozzo1_fit1, i.var = c(1,3)) # vol-rain
plot.gbm(pozzo1_fit1, i.var = c(1,2)) # vol-temp
plot.gbm(pozzo1_fit1, i.var = c(2,3)) # temp-rain

### impact of different features on predicting depth to gw 

# summarise model 

pozzo1_effects <- tibble::as_tibble(gbm::summary.gbm(pozzo1_fit1,
                                                     plotit = F))
pozzo1_effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
pozzo1_effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

pozzo1.test$predicted <- as.integer(predict(pozzo1_fit1,
                                            newdata = pozzo1.test,
                                            n.trees = perf_gbm1))

# plot predicted vs actual

ggplot(pozzo1.test) +
  geom_point(aes(x = predicted,
                 y = imp1,
                 color = predicted - imp1),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()

#### pozzo 2 #### 

## stepwise 

pozzo2_sw <- step.wisef("imp2", pozzo2)
pozzo2_sw$bestTune # 5
coef(pozzo2_sw$finalModel, 3)

## train and test  ##### how to automate this??? 
#gmb.f(pozzo2)...

# split 
set.seed(123)
p2.split <- initial_split(pozzo2, prop = .7)
p2.train <- training(p2.split)
p2.test <- testing(p2.split)

p2.fit1 <- gbm(imp2 ~ .,
               data = pozzo2,
               verbose = T, 
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 10)

p2.fit1_perf <- gbm.perf(p2.fit1, method = "cv")
p2.fit1_perf

## make predictions 

p2_pred1 <- stats::predict(object = p2.fit1,
                           newdata = p2.test,
                           n.trees = p2.fit1_perf)
p2_rmse <- Metrics::rmse(actual = p2.test$imp2,
                         predicted = p2_pred1)
print(p2_rmse) # 0.98

gbm::plot.gbm(p2.fit1, i.var = 1)

plot.gbm(p2.fit1, i.var = 2)

plot.gbm(p2.fit1, i.var = 3)

## interactions of two features on the variable 

gbm::plot.gbm(p2.fit1, i.var = c(1,3))
plot.gbm(p2.fit1, i.var = c(1,2))
plot.gbm(p2.fit1, i.var = c(2,3))

### impact of different features on predicting depth to gw 

# summarise model 

p2.effects <- tibble::as_tibble(gbm::summary.gbm(p2.fit1,
                                                 plotit = F))
p2.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
p2.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p2.test$predicted <- as.integer(predict(p2.fit1,
                                        newdata = p2.test,
                                        n.trees = p2.fit1_perf))

# plot predicted vs actual

ggplot(p2.test) +
  geom_point(aes(x = predicted,
                 y = imp2,
                 color = predicted - imp2),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()


#### pozzo 3 ####

# stepwise

pozzo3_sw <- step.wisef("imp3",pozzo3)
pozzo3_sw$bestTune # 5
coef(pozzo3_sw$finalModel, 4)


# split 
set.seed(123)
p3.split <- initial_split(pozzo3, prop = .7)
p3.train <- training(p3.split)
p3.test <- testing(p3.split)

p3.fit1 <- gbm(imp3 ~ .,
               data = pozzo3,
               verbose = T, 
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 10)

p3.fit1_perf <- gbm.perf(p3.fit1, method = "cv")

## make predictions 

p3_pred1 <- stats::predict(object = p3.fit1,
                           newdata = p3.test,
                           n.trees = p3.fit1_perf)
p3_rmse <- Metrics::rmse(actual = p3.test$imp3,
                         predicted = p3_pred1)
print(p3_rmse) # 1.89

gbm::plot.gbm(p3.fit1, i.var = 1)

plot.gbm(p3.fit1, i.var = 2)

plot.gbm(p3.fit1, i.var = 3)

# ecc...

## interactions of two features on the variable 

gbm::plot.gbm(p3.fit1, i.var = c(1,3))
plot.gbm(p3.fit1, i.var = c(1,2))
plot.gbm(p3.fit1, i.var = c(2,3))

### impact of different features on predicting depth to gw 

# summarise model 

p3.effects <- tibble::as_tibble(gbm::summary.gbm(p3.fit1,
                                                 plotit = F))
p3.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
p3.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p3.test$predicted <- as.integer(predict(p3.fit1,
                                        newdata = p3.test,
                                        n.trees = p3.fit1_perf))

# plot predicted vs actual

ggplot(p3.test) +
  geom_point(aes(x = predicted,
                 y = imp3,
                 color = predicted - imp3),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()



#### pozzo 4 ####

# stepwise
pozzo4_sw <- step.wisef("imp4", pozzo4)
pozzo4_sw$bestTune # 5
coef(pozzo4_sw$finalModel, 3)

# split, train and test
set.seed(123)
p4.split <- initial_split(pozzo4, prop = .7)
p4.train <- training(p4.split)
p4.test <- testing(p4.split)

p4.fit1 <- gbm(imp4 ~ .,
               data = pozzo4,
               verbose = T, 
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 10)

p4.fit1_perf <- gbm.perf(p4.fit1, method = "cv")

## make predictions 

p4_pred1 <- stats::predict(object = p4.fit1,
                           newdata = p4.test,
                           n.trees = p4.fit1_perf)
p4_rmse <- Metrics::rmse(actual = p4.test$imp4,
                         predicted = p4_pred1)
print(p4_rmse) # 0.82

gbm::plot.gbm(p4.fit1, i.var = 1)

plot.gbm(p4.fit1, i.var = 2)

plot.gbm(p4.fit1, i.var = 3)

# ecc...

## interactions of two features on the variable 

gbm::plot.gbm(p4.fit1, i.var = c(1,3))
plot.gbm(p4.fit1, i.var = c(1,2))
plot.gbm(p4.fit1, i.var = c(2,3))

### impact of different features on predicting depth to gw 

# summarise model 

p4.effects <- tibble::as_tibble(gbm::summary.gbm(p4.fit1,
                                                 plotit = F))
p4.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
p4.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p4.test$predicted <- as.integer(predict(p4.fit1,
                                        newdata = p4.test,
                                        n.trees = p4.fit1_perf))

# plot predicted vs actual

ggplot(p4.test) +
  geom_point(aes(x = predicted,
                 y = imp4,
                 color = predicted - imp4),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()



#### pozzo 5 ####

#stepwise
pozzo5_sw <- step.wisef("imp5", pozzo5)
pozzo5_sw$bestTune # 5
coef(pozzo5_sw$finalModel, 3)

# split 
set.seed(123)
p5.split <- initial_split(pozzo5, prop = .7)
p5.train <- training(p5.split)
p5.test <- testing(p5.split)

p5.fit1 <- gbm(imp5 ~ .,
               data = pozzo5,
               verbose = T, 
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 10)

p5.fit1_perf <- gbm.perf(p5.fit1, method = "cv")

## make predictions 

p5_pred1 <- stats::predict(object = p5.fit1,
                           newdata = p5.test,
                           n.trees = p5.fit1_perf)
p5_rmse <- Metrics::rmse(actual = p5.test$imp5,
                         predicted = p5_pred1)
print(p5_rmse) # 2.36

gbm::plot.gbm(p5.fit1, i.var = 1)

# ecc...

## interactions of two features on the variable 

gbm::plot.gbm(p5.fit1, i.var = c(1,3))
plot.gbm(p5.fit1, i.var = c(1,2))
plot.gbm(p5.fit1, i.var = c(2,3))

### impact of different features on predicting depth to gw 

# summarise model 

p5.effects <- tibble::as_tibble(gbm::summary.gbm(p5.fit1,
                                                 plotit = F))
p5.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
p5.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p5.test$predicted <- as.integer(predict(p5.fit1,
                                        newdata = p5.test,
                                        n.trees = p5.fit1_perf))

# plot predicted vs actual

ggplot(p5.test) +
  geom_point(aes(x = predicted,
                 y = imp5,
                 color = predicted - imp5),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()


#### pozzo 6 ####

#stepwise
pozzo6_sw <- step.wisef("imp6", pozzo6)
pozzo6_sw$bestTune # 5
coef(pozzo6_sw$finalModel, 2)

# split 
set.seed(123)
p6.split <- initial_split(pozzo6,prop = .7)
p6.train <- training(p6.split)
p6.test <- testing(p6.split)

p6.fit1 <- gbm(imp6 ~ .,
               data = pozzo6,
               verbose = T, 
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 10)

p6.fit1_perf <- gbm.perf(p6.fit1, method = "cv")

## make predictions 

p6_pred1 <- stats::predict(object = p6.fit1,
                           newdata = p6.test,
                           n.trees = p6.fit1_perf)
p6_rmse <- Metrics::rmse(actual = p6.test$imp6,
                         predicted = p6_pred1)
print(p6_rmse) # 0.93

gbm::plot.gbm(p6.fit1, i.var = 1)

plot.gbm(p6.fit1, i.var = 2)

plot.gbm(p6.fit1, i.var = 3)

# ecc...

## interactions of two features on the variable 

gbm::plot.gbm(p6.fit1, i.var = c(1,3))
plot.gbm(p6.fit1, i.var = c(1,2))
plot.gbm(p6.fit1, i.var = c(2,3))

### impact of different features on predicting depth to gw 

# summarise model 

p6.effects <- tibble::as_tibble(gbm::summary.gbm(p6.fit1,
                                                 plotit = F))
p6.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
p6.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p6.test$predicted <- as.integer(predict(p6.fit1,
                                        newdata = p6.test,
                                        n.trees = p6.fit1_perf))

# plot predicted vs actual

ggplot(p6.test) +
  geom_point(aes(x = predicted,
                 y = imp6,
                 color = predicted - imp6),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()


#### pozzo 7 ####

#stepwise
pozzo7_sw <- step.wisef("imp7", pozzo7)
pozzo7_sw$bestTune # 5
coef(pozzo7_sw$finalModel, 3)

# split 
set.seed(123)
p7.split <- initial_split(pozzo7, prop = .7)
p7.train <- training(p7.split)
p7.test <- testing(p7.split)

p7.fit1 <- gbm(imp7 ~ .,
               data = pozzo7,
               verbose = T, 
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 10)

p7.fit1_perf <- gbm.perf(p7.fit1, method = "cv")

## make predictions 

p7_pred1 <- stats::predict(object = p7.fit1,
                           newdata = p7.test,
                           n.trees = p7.fit1_perf)
p7_rmse <- Metrics::rmse(actual = p7.test$imp7,
                         predicted = p3_pred1)
print(p7_rmse) # 16.32

gbm::plot.gbm(p7.fit1, i.var = 1)

plot.gbm(p7.fit1, i.var = 2)

plot.gbm(p7.fit1, i.var = 3)

# ecc...

## interactions of two features on the variable 

gbm::plot.gbm(p7.fit1, i.var = c(1,3))
plot.gbm(p7.fit1, i.var = c(1,2))
plot.gbm(p7.fit1, i.var = c(2,3))

### impact of different features on predicting depth to gw 

# summarise model 

p7.effects <- tibble::as_tibble(gbm::summary.gbm(p7.fit1,
                                                 plotit = F))
p7.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
p7.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p7.test$predicted <- as.integer(predict(p7.fit1,
                                        newdata = p7.test,
                                        n.trees = p7.fit1_perf))

# plot predicted vs actual

ggplot(p7.test) +
  geom_point(aes(x = predicted,
                 y = imp7,
                 color = predicted - imp7),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()


#### pozzo 8 #### 

#stepwise
pozzo8_sw <- step.wisef("imp8", pozzo8)
pozzo8_sw$bestTune # 5
coef(pozzo8_sw$finalModel, 3)

# split 
set.seed(123)
p8.split <- initial_split(pozzo8, prop = .7)
p8.train <- training(p8.split)
p8.test <- testing(p8.split)

p8.fit1 <- gbm(imp8 ~ .,
               data = pozzo8,
               verbose = T, 
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 10)

p8.fit1_perf <- gbm.perf(p8.fit1, method = "cv")

## make predictions 

p8_pred1 <- stats::predict(object = p8.fit1,
                           newdata = p8.test,
                           n.trees = p8.fit1_perf)
p8_rmse <- Metrics::rmse(actual = p8.test$imp8,
                         predicted = p8_pred1)
print(p8_rmse) # 0.87

gbm::plot.gbm(p8.fit1, i.var = 1)

plot.gbm(p8.fit1, i.var = 2)

plot.gbm(p8.fit1, i.var = 3)

# ecc...

## interactions of two features on the variable 

gbm::plot.gbm(p8.fit1, i.var = c(1,3))
plot.gbm(p8.fit1, i.var = c(1,2))
plot.gbm(p8.fit1, i.var = c(2,3))

### impact of different features on predicting depth to gw 

# summarise model 

p8.effects <- tibble::as_tibble(gbm::summary.gbm(p8.fit1,
                                                 plotit = F))
p8.effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 3 features
p8.effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(3) %>%  # it's already only 3 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")


## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p8.test$predicted <- as.integer(predict(p8.fit1,
                                        newdata = p8.test,
                                        n.trees = p8.fit1_perf))

# plot predicted vs actual

ggplot(p8.test) +
  geom_point(aes(x = predicted,
                 y = imp8,
                 color = predicted - imp8),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()


#### pozzo 9 #### 

#stepwise
pozzo9_sw <- step.wisef("imp9", pozzo9)
pozzo9_sw$bestTune # 1!
coef(pozzo9_sw$finalModel, 1)
# others "subscript out of bounds", as best model is already 1?

# split 
set.seed(123)
p9.split <- initial_split(pozzo9, prop = .7)
p9.train <- training(p9.split)
p9.test <- testing(p9.split)

p9.fit1 <- gbm(imp9 ~ imp_rain_velletri, # in this case just one var...
               data = pozzo9,
               verbose = 1,
               shrinkage = 0.01,
               interaction.depth = 3, 
               n.minobsinnode = 5,
               n.trees = 5000,
               cv.folds = 1) # can't go larger or i get error:
# >1 nodes produced errors; first error: incorrect number of dimensions

p9.fit1_perf <- gbm.perf(p9.fit1)

## make predictions 

p9_pred1 <- stats::predict(object = p9.fit1,
                           newdata = p9.test,
                           n.trees = p9.fit1_perf)
p9_rmse <- Metrics::rmse(actual = p9.test$imp9,
                         predicted = p9_pred1)
print(p9_rmse) # 3.88

gbm::plot.gbm(p9.fit1, i.var = 1) # only the one var

## interactions of two features on the variable not possible

### impact of different features on predicting depth to gw 

# summarise model 

p9.effects <- tibble::as_tibble(gbm::summary.gbm(p9.fit1,
                                                 plotit = F))
p9.effects %>% utils::head() # 100 ... of course it's the only one 
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

p9.test$predicted <- as.integer(predict(p9.fit1,
                                        newdata = p9.test,
                                        n.trees = p9.fit1_perf))

# plot predicted vs actual

ggplot(p9.test) +
  geom_point(aes(x = predicted,
                 y = imp9,
                 color = predicted - imp9),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()




