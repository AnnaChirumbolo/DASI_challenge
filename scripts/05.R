################################################################################
################################################################################
#######################      REGRESSION MODELS        ##########################
################################################################################
################################################################################


## libraries 

library(tidyverse)
library(tsibble)
library(randomForest)
library(forecast)
library(ggplot2)

#### opening files ####
# aquifer and other water spring (madonna di canneto)

doganella <- read.csv("processed_data/DOGANELLA_to_model.csv") %>% 
  select(-X, -Rainfall_Velletri,-Rainfall_Monteporzio,-Temperature_Velletri,
         -Temperature_Monteporzio,-Pozzo_1:-Pozzo_9) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  spread(key = imp, value = depth_to_gw.m)

canneto <- read.csv("processed_data/MADONNA_DI_CANNETO_to_model.csv") %>% 
  select(-X) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

str(doganella)
str(canneto)


#### doganella ####

#### pozzo 1 ####

# to ts format 
pozzo1_ts <- ts(doganella$imp1, frequency = 12)

plot.ts(pozzo1_ts)
  # seems like there's an increasing, perhaps non-linear trend over time
  # strong seasonal patterns? 

# decomposition ts 

pozzo1_decomp <- decompose(pozzo1_ts)

plot(pozzo1_decomp, yax.flip = T)


# differencing to remove trend or seasonal effects 

pozzo1_d2 <- diff(pozzo1_ts, differences = 2)

plot(pozzo1_d2)


# removing seasonality 

pozzo1_d12 <- diff(pozzo1_d2, lag = 12)

plot(pozzo1_d12)



### time delay embedding ###

lag_order <- 6
horizon <- 12


pozzo1_mbd <- embed(pozzo1_d12, lag_order + 1)
View(pozzo1_mbd)

y_train <- pozzo1_mbd[,1]
x_train <- pozzo1_mbd[,-1]

y_test <- window(pozzo1_ts)
x_test <- pozzo1_mbd[nrow(pozzo1_mbd),c(1:lag_order)]


forecast_rf <- numeric(horizon)

for (i in 1:horizon){
  set.seed(2019)
  
  fit_rf <- randomForest(x_train, y_train)
  
  forecast_rf[i] <- predict(fit_rf, x_test)
  
  y_train <- y_train[-1]
  
  x_train <- x_train[-nrow(x_train), ]
}



#######################################


 ## try again 

doganella1<- read_delim("processed_data/DOGANELLA_to_model.csv",
                        delim = ",") %>%
  select(-X1, -Rainfall_Velletri,-Rainfall_Monteporzio,-Temperature_Velletri,
         -Temperature_Monteporzio,-Pozzo_1:-Pozzo_9) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  spread(key = imp, value = depth_to_gw.m)  %>%
  as_tsibble(index = "Date")

View(doganella1)

class(doganella1)
# it's a tibble data.frame class

# to ts 

doganella_ts <- ts(doganella1$imp1, frequency = 12, start = c(2012,6))
class(doganella_ts)

View(doganella_ts)

# implicit missing 
has_gaps(doganella1)
#explicit missing
colSums(is.na(doganella1))

## kind of time series

plot_org <- doganella1 %>% 
  ggplot(aes(Date, imp1)) +
  geom_line()+
  theme_classic()
plot_org


### differencing 
# classical ts models require data to be stationary
# stationarity - mean and variance are finite and do not change 
# over time 

# doesnt look like it from the plot 

# differencing: to make non-stationary data stationary
# differencing removes changes in the level of a series and 
# with it, the trend 
# that's what we need for random forest 


## diff()

doganella1_d2 <- diff(doganella_ts, differences = 2)
plot(doganella1_d2)
plot_org






