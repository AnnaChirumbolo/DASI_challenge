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
                      tuneGrid = data.frame(nvmax = 1:10), # in this particular case, max 13 features for the variable
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
         Rainfall_Velletri = na_ma(Rainfall_Velletri, k=1)) %>% 
  gather(key ="rain",value = "mm", Rainfall_Monteporzio,Rainfall_Velletri) %>% 
  gather(key = "temp", value = "C", Temperature_Monteporzio,Temperature_Velletri) %>% 
  mutate(temp = gsub("Temperature_","",temp),
         rain = gsub("Rainfall_","",rain))

## vis distrib data with imputed vars 

## temperature ##
pal.temp <- brewer.pal(3,"Spectral")

temp <- ggplot(doganella_tm, aes(Date, C, group = temp, color = temp))+
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
rain <- ggplot(doganella_tm, aes(Date, mm, group = rain, color = rain))+
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