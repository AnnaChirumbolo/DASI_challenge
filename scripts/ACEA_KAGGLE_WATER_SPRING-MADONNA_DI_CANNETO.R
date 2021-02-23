################################
######### only canneto #########
################################

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

#### reading file: madonna di canneto ####


canneto <- read.csv("data/Water_Spring_Madonna_di_Canneto.csv")

## First overall look
str(canneto)

summary(canneto)

#head(canneto)
#tail(canneto)

## Changing date value to "date" format
canneto <- canneto %>% 
  rename(Date = ï..Date) %>% 
  mutate(Date = dmy(Date))

## Checking for missing values 
canneto_missing <- canneto %>% 
  miss_var_summary()
print(canneto_missing) # flow rate data missing: more than 50% 

## First visualisation of target distribution over years

(first_look_canneto <- ggplot(canneto,
                              aes(Date ,Flow_Rate_Madonna_di_Canneto))+
    geom_line(size= 1, color = "steelblue2", alpha = 0.7)+
    xlab("")+
    ggtitle("Distribution flow rate (L/s) over time in Madonna di Canneto\n")+
    scale_y_continuous(limits = c(0,300),expand = c(0,0))+
    ylab("Flow Rate (L/s)\n")+
    theme_classic())
# big gap of data first years

ggsave("img/canneto/first_look_target_canneto.jpg",
       dpi = 500, width = 10, height = 6)

## Filtering out first years with no target data

# Which year to start from? 
#min(canneto$Date[!is.na(canneto$Flow_Rate_Madonna_di_Canneto)])        

# "2015-03-13", but there's another big gap before that, so removing until 2016-04-13

# Filtering out from 2015-03-13
canneto_filtered <- canneto %>% 
  filter(Date >= "2016-04-13") 

canneto_ts <- ts(canneto_filtered$Flow_Rate_Madonna_di_Canneto, start = c(2016,1),frequency = 7)

x_label <- canneto_filtered$Date

## How are these NAs distributed over time?
ggplot_na_distribution(canneto_ts,
                       title = "Distribution of Missing Target Values (Madonna di Canneto)\n",
                       ylab = "Flow Rate (L/s)\n",
                       theme = ggplot2::theme_classic(),
                       x_axis_labels = x_label,
                       xlab="")
ggsave("img/canneto/na_dist_cannetof.jpg", dpi = 500, width = 10, height = 6)

?ggplot_na_distribution

## checking for missing data after filtering 
canneto_filtered_missing <- canneto_filtered %>% 
  miss_var_summary()
print(canneto_filtered_missing)
# there are missing data for all variables but Date


## outliers for target ##

canneto_filtered <- canneto_filtered %>% 
  mutate(imp_flow_rate = na_ma(Flow_Rate_Madonna_di_Canneto)) %>% 
  dplyr::select(-Flow_Rate_Madonna_di_Canneto)

# boxplot
(box_canneto <- ggplot(canneto_filtered, aes(y = imp_flow_rate))+
    geom_boxplot()+
    theme_classic())
# no outliers visually 

#### features: meteo ####

meteo_canneto <- list.files(path = "./data/MADONNA_DI_CANNETO_3BMETEO/",
                            pattern = "*.csv$", 
                            full.names = T) %>%
  map_df(~read_plus(.)) 


## manipulating rainfall data (3bmeteo)
rain_canneto <- meteo_canneto %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/MADONNA_DI_CANNETO_3BMETEO/", "", date1),
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
         date_final = str_replace(date_final, "set", "09"),
         date_final = str_replace(date_final, "ott", "10"),
         date_final = str_replace(date_final, "nov","11"),
         date_final = str_replace(date_final, "dic", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>%
  rename(Date = date_final,
         Rainfall_Settefrati = prec) %>%
  dplyr::select(Date, Rainfall_Settefrati)

#summary(rain_canneto)

## manipulating temperature data (3bmeteo)

temp_canneto <- meteo_canneto %>% 
  rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/MADONNA_DI_CANNETO_3BMETEO/", "", date1),
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
         date_final = str_replace(date_final, "set", "09"),
         date_final = str_replace(date_final, "ott", "10"),
         date_final = str_replace(date_final, "nov","11"),
         date_final = str_replace(date_final, "dic", "12"),
         date_final = gsub(" ", "/", date_final),
         date_final = dmy(date_final)) %>%
  rename(Date = date_final) %>%
  dplyr::select(Date, tmin, tmax) %>%
  mutate(Temperature_Settefrati = rowMeans(subset(., select = c(tmin,tmax)),
                                           na.rm = T)) %>%
  dplyr::select(-tmin, -tmax)

#summary(temp_canneto)

## Filling dataset to model with new meteorological data 

# temp
canneto_filtered$Temperature_Settefrati[is.na(canneto_filtered$Temperature_Settefrati)] <- temp_canneto$Temperature_Settefrati[match(canneto_filtered$Date[is.na(canneto_filtered$Temperature_Settefrati)],
                                                                                                                                       temp_canneto$Date)]

# rain 
canneto_filtered$Rainfall_Settefrati[is.na(canneto_filtered$Rainfall_Settefrati)] <- rain_canneto$Rainfall_Settefrati[match(canneto_filtered$Date[is.na(canneto_filtered$Rainfall_Settefrati)],
                                                                                                                              rain_canneto$Date)]

#summary(canneto_filtered)


## Checking visually that all NAs have gone 

# Rainfall 
rain_filled <- ggplot_na_distribution(canneto_filtered$Rainfall_Settefrati,
                       title = "",
                       subtitle = "",
                       ylab="Rainfall (mm)\n",
                       theme= ggplot2::theme_classic(),
                       xlab="",
                       x_axis_labels = x_label)
rain_filled

# Temperature 
temp_filled <- ggplot_na_distribution(canneto_filtered$Temperature_Settefrati,
                       title = "",
                       subtitle = "",
                       ylab="Temperature (°C)\n",
                       theme =ggplot2::theme_classic(),
                       xlab="",
                       x_axis_labels = x_label)
temp_filled

meteo <- gridExtra::arrangeGrob(rain_filled,temp_filled)
ggsave("img/canneto/panel_meteo_canneto.jpg", meteo, dpi = 500,
       width=10, height=9)


### outliers ###


## Checking distributions 

# Feature 1: Rainfall 

(canneto_rain_box <- ggplot(canneto_filtered,
                            aes(y = Rainfall_Settefrati))+
    geom_boxplot()+
    theme_classic()) # few data outside top whisker

## Checking for outliers statistically 
outr_canneto <- boxplot.stats(canneto_filtered$Rainfall_Settefrati)$out

# Grubbs test 
grubbs.test(canneto_filtered$Rainfall_Settefrati)
# max value seems to be an outlier - statistically speaking

outr_canneto_ind <- which(canneto_filtered$Rainfall_Settefrati %in% c(outr_canneto))

df_outr_canneto <- canneto_filtered[outr_canneto_ind,]

## Let's visualise outliers over time

# plot
(outr_canneto_vis <- ggplot(df_outr_canneto, aes(Date, Rainfall_Settefrati))+
    geom_point()+
    theme_classic())
# spread out over time
# keeping them all 

# Feature 2: Temperature 

(canneto_temp_box <- ggplot(canneto_filtered,
                            aes(y = Temperature_Settefrati))+
    geom_boxplot()+
    theme_classic()) # no outliers here 

## Checking statistically 
outt_canneto <- boxplot.stats(canneto_filtered$Temperature_Settefrati)
outt_canneto
# no out 

## Grubbs test 
testt_canneto <- grubbs.test(canneto_filtered$Temperature_Settefrati)
testt_canneto # confirmed stats - no out 

#### feature engineering ####

## adding new features: seasons and presence/absence of snow

canneto_featured <- add.seasons(canneto_filtered) %>%
  rename(fl_rate.Ls = imp_flow_rate) %>% 
  mutate(snow.yes = as.factor(ifelse(Temperature_Settefrati <=0 & 
                                       Rainfall_Settefrati > 0, 1,0)),
         snow.no = as.factor(ifelse(Temperature_Settefrati >0,1,0)),
         week = cut(Date, "week")) %>% 
  group_by(week) %>% 
  mutate(mean_rain = mean(Rainfall_Settefrati)) %>% 
  ungroup()

str(canneto_featured)

summary(canneto_featured$mean_rain)

#### choosing best lag length ####

canneto_ts <- ts(canneto_featured, start = c(2016,1),
                 frequency = 7)
View(canneto_ts)

canneto_lag <- lags.select(canneto_ts, lag.max = 10)

print(canneto_lag)
summary(canneto_lag)

# lag 8 - best lag 


canneto_featured <- canneto_featured %>% 
  mutate(lag8 = Lag(Rainfall_Settefrati, +8)) %>% 
  dplyr::select(-week)

canneto.sw <- canneto_featured %>% 
  dplyr::select(-mean_rain)

canneto_featured1 <- canneto_featured %>% 
  dplyr::select(-Date)

## creating new timeframes with different min rainfall levels 
## and with new time lags (trying to represent true effect of rain over target)

canneto0.8 <- canneto_featured %>% 
  mutate(rain0.8 = ifelse(mean_rain <= 0.8, 0, 
                          Rainfall_Settefrati),
         lag8 = Lag(rain0.8, +8)) %>% 
  dplyr::select(-Rainfall_Settefrati,-mean_rain)

canneto0.8_1 <- canneto0.8 %>%  dplyr::select(-Date)

canneto1.5 <- canneto_featured %>% 
  mutate(rain1.5 = ifelse(mean_rain <= 1.5, 0, 
                          Rainfall_Settefrati),
         lag8 = Lag(rain1.5, +8)) %>% 
  dplyr::select(-Rainfall_Settefrati,-mean_rain)

canneto1.5_1 <- canneto1.5 %>%   dplyr::select(-Date)

canneto2.8 <- canneto_featured %>% 
  mutate(rain2.8 = ifelse(mean_rain <= 2.8,0,
                        Rainfall_Settefrati),
         lag8 = Lag(rain2.8, +8)) %>% 
  dplyr::select(-Rainfall_Settefrati,-mean_rain)

canneto2.8_1 <- canneto2.8 %>%  dplyr::select(-Date)

canneto4.4 <- canneto_featured %>% 
  mutate(rain4.4 = ifelse(mean_rain <= 4.4, 0, 
                        Rainfall_Settefrati),
         lag8 = Lag(rain4.4, +8)) %>%
  dplyr::select(-Rainfall_Settefrati,-mean_rain)

canneto4.4_1 <- canneto4.4 %>%   dplyr::select(-Date)

### Checking correlations between features with correlation matrix ### 

# first changing factors to numeric values ... 
canneto_featured1[,4:9] <- unfactor(canneto_featured1[,4:9])
canneto0.8_1[,3:8] <- unfactor(canneto0.8_1[,3:8])
canneto1.5_1[,3:8] <- unfactor(canneto1.5_1[,3:8])
canneto2.8_1[,3:8] <- unfactor(canneto2.8_1[,3:8])
canneto4.4_1[,3:8] <- unfactor(canneto4.4_1[,3:8])

par(mfrow = c(2,2))

str(canneto_featured1)
canneto_featured.cor <- canneto_featured1 %>% 
  cor(., use = "complete.obs") %>%
  corrplot(., method = "circle",
           tl.col = "black",tl.srt = 35, 
           tl.cex = 0.72,
           main = "\nMadonna di Canneto - original dataset ")


canneto0.8_cor <- canneto0.8_1 %>% 
  cor(., use = "complete.obs") %>% 
  corrplot(., method = "circle",
           tl.col = "black",tl.srt = 35, 
           tl.cex = 0.72, 
           main = "\nMadonna di Canneto - 0.5mm")

canneto1.5_cor <- canneto1.5_1 %>% 
  cor(., use = "complete.obs") %>% 
  corrplot(., method = "circle",
           tl.col = "black",tl.srt = 35, 
           tl.cex = 0.72,
           main = "\nMadonna di Canneto - 1.5mm")

canneto2.8_cor <- canneto2.8_1 %>% 
  cor(., use = "complete.obs") %>% 
  corrplot(., method = "circle",
           tl.col = "black",tl.srt = 35, 
           tl.cex = 0.72,
           main = "\nMadonna di Canneto - 3mm")

canneto4.4_cor <- canneto4.4_1 %>% 
  cor(., use = "complete.obs") %>% 
  corrplot(., method = "circle",
           tl.col = "black",tl.srt = 35, 
           tl.cex = 0.72,
           main = "\nMadonna di Canneto - 5mm")

par(mfrow =c (1,1))

#### visualise the time lags ####

rain_boxplot <- ggplot(canneto_featured, aes(y = Rainfall_Settefrati))+
    geom_boxplot(color = "steelblue")+
    xlab("")+
    ylab("Rainfall (mm)")+
    ggtitle("Boxplot rainfall variable: Madonna di Canneto\n")+
    theme_classic()

vis_rain <- ggplot(canneto_featured, aes(Date, Rainfall_Settefrati))+
    geom_line(color = "steelblue")+
    scale_y_continuous(limits = c(0,141),expand = c(0,0))+
    ylab("")+
    xlab("")+
    geom_hline(aes(yintercept = 0.8,linetype = "0.8"),color = "darkred",size=.5)+
    geom_hline(aes(yintercept = 1.5, linetype = "1.5"),color = "darkgreen",size=.5)+
    geom_hline(aes(yintercept=2.8, linetype = "2.8"),color = "darkorange",size=.5)+
    geom_hline(aes(yintercept = 4.4, linetype = "4.4"),color ="brown",size=.5)+
    scale_linetype_manual(name = "Rainfall (mm)", 
                          values = c(1,2,3,4),
                          guide = guide_legend(override.aes = list(color = c("darkred",
                                                                             "darkgreen",
                                                                             "darkorange",
                                                                             "brown"))))+
  theme_classic()+
  theme(legend.position = c(0.9,0.9))+
  labs(title = "Distribution of rainfall over time: Madonna di Canneto\n",
       subtitle = "Lines show set minimum weekly rainfall thresholds\n")


## panel

#install.packages("cowplot")
library(cowplot)

panelled_rain <- plot_grid(rain_boxplot,
                           vis_rain,
                           align = "h",
                           nrow = 1,ncol =2,
                           rel_widths = c(1/4,1/2))

panelled_rain

ggsave("img/canneto/panelled_rain.jpg",panelled_rain,
       dpi = 500, width =8, height = 5)


#####------------------------------------------------------------------------------------------------------------------------------------------####
### Running stepwise ###

# Original dataset
canneto_featured.sw <- step.wisef("fl_rate.Ls", canneto.sw)
canneto_featured.sw$bestTune ## 11 features
coef(canneto_featured.sw$finalModel,7)

# 2 
canneto0.8.sw <- step.wisef("fl_rate.Ls",canneto0.8)
canneto0.8.sw$bestTune ## 8 features
coef(canneto0.8.sw$finalModel,7)

# 3
canneto1.5.sw <- step.wisef("fl_rate.Ls",canneto1.5)
canneto1.5.sw$bestTune ## 8 features
coef(canneto1.5.sw$finalModel,9) 

# 4 
canneto2.8.sw <- step.wisef("fl_rate.Ls",canneto2.8)
canneto2.8.sw$bestTune ## 6 features 
coef(canneto2.8.sw$finalModel,7)

# 5 
canneto4.4.sw <- step.wisef("fl_rate.Ls",canneto4.4)
canneto4.4.sw$bestTune ## 8 features
coef(canneto4.4.sw$finalModel,7)


#####----------------------------------------------------------------------------------------------------------------------------------------#####
### Running autoML from h2o package ###

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-zeno/2/R")

# Finally, let's load H2O and start up an H2O cluster
library(h2o)

h2o.init()

h2o.no_progress()  # Turn off progress bars for notebook readability



## canneto_featured ##

canneto_featured.h2o <- as.h2o(canneto_featured)
#h2o.describe(canneto_featured.h2o)
y <- "fl_rate.Ls"
x <- setdiff(names(canneto_featured.h2o), c(y))

aml <- h2o.automl(y = y, x = x,
                  training_frame = canneto_featured.h2o,
                  max_models = 10,
                  seed = 1)

lb <- aml@leaderboard
print(lb)

# Get model ids for all models in the AutoML Leaderboard
model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]
# Get the "All Models" Stacked Ensemble model
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
# Get the Stacked Ensemble metalearner model
metalearner <- h2o.getModel(se@model$metalearner$name)

h2o.varimp_plot(metalearner) # best model - gbm

## canneto original ##

res <- getbestmodel.modif(
  canneto_featured$Date[1:600],
  canneto_featured$fl_rate.Ls[1:600],
  "day",
  complete = 0,
  n_test = 7, # prediction horizon (i.e. n. of days)
  graph = T,
  algos = list("my.prophet", "my.sarima","my.tbats",
               "my.bats", "my.stlm", "my.stlf","my.stl",
               "my.shortterm"),
  bagged = "auto",
  metric.error = my.rmse)

res$best # TBATS

print(res$graph.train)

?getBestModel

# getting predictions of best model
pred <- res %>%
  my.predictions()

# plotting predicted VS Actual values
dates_test <- as.Date(canneto_featured$Date, format = "%Y-%m-%d")[501:507]
values_test <- canneto_featured$fl_rate.Ls[501:507]

(tbats <- ggplot() +
  geom_line(data=pred[!is.na(pred$tbats) & pred$type == "mean",], aes(x=dates, 
                                                                       y=tbats), 
            color = "blue") +
  geom_line(aes(x = dates_test, y = values_test), color = "red") +
  xlab('') +
  ylab('Flow rate (L/s)') +
  ggtitle("Predicted (TBATS) vs Actual values: Madonna di Canneto")+
  theme_classic())

ggsave("img/canneto/tbats.jpg", tbats, 
       dpi=500, width=8,height=5)

### GBM ###

## Canneto original dataset - no features removed, no changes to minimum rainfall level. 

# testing and training split

par(mar=c(5,5,5,5))

str(canneto_featured1)
canneto_featured2 <- canneto_featured1 %>% dplyr::select(-mean_rain)

set.seed(123)
canneto_featured.split <- initial_split(canneto_featured2, prop = .7)
canneto_featured.train <- training(canneto_featured.split)
canneto_featured.test <- testing(canneto_featured.split)

canneto_featured.fit <- gbm::gbm(fl_rate.Ls ~ .,
                             data = canneto_featured2, # removed date
                             verbose = T, 
                             shrinkage = 0.01,
                             interaction.depth = 3, 
                             n.minobsinnode = 5,
                             n.trees = 1500,
                             cv.folds = 12)

canneto_featured.fit.perf <- gbm.perf(canneto_featured.fit, method = "cv")

# make predictions 

canneto_featured.fit.pred <- stats::predict(object = canneto_featured.fit,
                                        newdata = canneto_featured.test,
                                        n.trees = canneto_featured.fit.perf)
canneto_featured.fit.rmse <- Metrics::rmse(actual = canneto_featured.test$fl_rate.Ls,
                                       predicted = canneto_featured.fit.pred)

print(canneto_featured.fit.rmse) # 26.05


# summarise model 

canneto_featured.effects <- tibble::as_tibble(gbm::summary.gbm(canneto_featured.fit,
                                                           plotit = F))
canneto_featured.effects %>% utils::head() 


## plotting pred vs actual 

canneto_featured.test$predicted <- as.integer(predict(canneto_featured.fit,
                                                      newdata = canneto_featured.test,
                                                      n.trees = canneto_featured.fit.perf))

str(canneto_featured.test)

reg <- lm(predicted ~ fl_rate.Ls, data = canneto_featured.test)
reg

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),"\n",r.sq)

# plot
(gbm_actualvspred <- ggplot(canneto_featured.test) +
  geom_point(aes(x = predicted,
                 y = fl_rate.Ls,
                 color = predicted - fl_rate.Ls),
             alpha = .7, size = 2) +
  geom_abline(intercept = 196,slope = 0.3, 
              color = "darkred", linetype ="dashed")+
  geom_text(x = 240, y = 260, label = eq, color = "darkred")+
  ggtitle("Predicted vs Actual values (GBM): Madonna di Canneto\n")+
  ylab("Actual\n")+
  xlab("\nPredicted")+
  scale_color_continuous(name = "Difference\npredicted - actual")+
  theme_classic())

ggsave("img/canneto/gbm_act_pred.jpg",gbm_actualvspred,
       dpi = 500, width = 10,height=7)

# plotting 6 best vars by rel importance 

plot_rel.infl <- canneto_featured.effects %>% 
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
  ggtitle("Relative influence of features on target variable (GBM): Madonna di Canneto\n")+
  scale_color_brewer(palette = "Dark2") +
  theme_classic()+
  scale_fill_continuous(name = "Relative Influence")

ggsave("img/canneto/rel_infl.gbm.jpg",plot_rel.infl,
       dpi = 500, width = 8, height = 6)


### comparing with values suggested by stepwise ###
canneto_featured_tm <- canneto_featured %>% 
  dplyr::select(Rainfall_Settefrati,Temperature_Settefrati,
                Spring,Winter,lag8,
                fl_rate.Ls, Autumn,snow.no)

set.seed(123)
canneto_featured_tm.split <- initial_split(canneto_featured_tm, prop = .7)
canneto_featured_tm.train <- training(canneto_featured_tm.split)
canneto_featured_tm.test <- testing(canneto_featured_tm.split)

canneto_featured_tm.fit <- gbm::gbm(fl_rate.Ls ~ .,
                                data = canneto_featured_tm,
                                verbose = T, 
                                shrinkage = 0.01,
                                interaction.depth = 3, 
                                n.minobsinnode = 5,
                                n.trees = 2000,
                                cv.folds = 12)

canneto_featured_tm.fit.perf <- gbm.perf(canneto_featured_tm.fit, method = "cv")

## make predictions 

canneto_featured_tm.fit.pred <- stats::predict(object = canneto_featured_tm.fit,
                                           newdata = canneto_featured_tm.test,
                                           n.trees = canneto_featured_tm.fit.perf)
canneto_featured_tm.fit.rmse <- Metrics::rmse(actual = canneto_featured_tm.test$fl_rate.Ls,
                                          predicted = canneto_featured_tm.fit.pred)
print(canneto_featured_tm.fit.rmse) ## 25.97 - slightly smaller than with all the features.

# summarise model 

canneto_featured.effects <- tibble::as_tibble(gbm::summary.gbm(canneto_featured_tm.fit,
                                                               plotit = F))
canneto_featured.effects %>% utils::head() 


## plotting pred vs actual 

canneto_featured_tm.test$predicted <- as.integer(predict(canneto_featured_tm.fit,
                                                      newdata = canneto_featured_tm.test,
                                                      n.trees = canneto_featured_tm.fit.perf))
str(canneto_featured_tm.test)

reg <- lm(predicted ~ fl_rate.Ls, data = canneto_featured_tm.test)
reg
summary(reg)

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),"\n",r.sq)

# plot
(gbm_actualvspred <- ggplot(canneto_featured_tm.test) +
    geom_point(aes(x = predicted,
                   y = fl_rate.Ls,
                   color = predicted - fl_rate.Ls),
               alpha = .7, size = 2) +
    geom_abline(intercept = 196,slope = 0.3, 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 240, y = 260, label = eq, color = "darkred")+
    ggtitle("Predicted vs Actual values (GBM): Madonna di Canneto\n")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())

ggsave("img/canneto/gbm_act_pred.jpg",gbm_actualvspred,
       dpi = 500, width = 10,height=7)

# plotting 6 best vars by rel importance 

plot_rel.infl <- canneto_featured.effects %>% 
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
  ggtitle("Relative influence of features on target variable (GBM): Madonna di Canneto\n")+
  scale_color_brewer(palette = "Dark2") +
  theme_classic()+
  scale_fill_continuous(name = "Relative Influence")

ggsave("img/canneto/rel_infl.gbm.jpg",plot_rel.infl,
       dpi = 500, width = 8, height = 6)




### Time series forecasting - SARIMA ### This could be considered too --> univariate TS analysis though (only target var and date)


canneto_ts.fit <- Arima(window(canneto_ts, end = c(2020,7)), order = c(2,0,0),
                        seasonal = list(order = c(2,0,0), period = 12))

tsdisplay(residuals(canneto_ts.fit))

plot(forecast(canneto_ts.fit, h = 12))
lines(canneto_ts.fit)

forecast_fit <- forecast(canneto_ts.fit)

RMSE(canneto_ts.fit$mean,validation)  # RMSE = 7 --> better model than gbm...? Less variables considered.
MAPE(canneto_ts.fit$mean,validation) * 100 
