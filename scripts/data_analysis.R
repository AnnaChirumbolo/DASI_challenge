##################################################
####### first analyses of datasets        ########
####### from coding club tutorial         ########
####### on time series analysis           ########
##################################################

## starting with libraries 

library(tidyverse)
library(lubridate)
library(ggplot2)
library(xts)

##########
#### aquifers ####

#### doganella ####

# reading file 

doganella <- read.csv("data/Aquifer_Doganella.csv")

# how many obs? how many cols? 

dim(doganella) # 6026 # cols: 22

# var types? 

str(doganella)
# date is character --> convert it to date

summary(doganella)
# depth to gw are all negative vals (i guess in terms of surface being 0, everything below it has minus sign)
# 9 wells 
# volume "pozzo 5.6"?
# 2 rainfall sensors 
# 2 temp sensors 


doganella$ï..Date <- dmy(doganella$ï..Date)

doganella <- doganella %>% rename(date = ï..Date)


# analysing correlation between 2 rainfall variables 
plot(doganella$Rainfall_Monteporzio ~ doganella$Rainfall_Velletri)
lm_rain <- lm(Rainfall_Monteporzio ~ Rainfall_Velletri, data = doganella)
summary(lm_rain)
# R2 = 0.68

# 2 temp vars 
plot(doganella$Temperature_Monteporzio ~ doganella$Temperature_Velletri)
  ## just by looking at it, it appears to be quite better correlated than ppt info is

lm_temp <- lm(Temperature_Monteporzio ~ Temperature_Velletri, data = doganella)
summary(lm_temp) 
# R2 = 0.98


########

doganella_xts <- xts(doganella[,-1],order.by = doganella[,1])

plot(doganella_xts)

doganella_filtered <- doganella %>% 
  filter(date >= "2012-06-01") %>% 
  select(-c(Volume_Pozzo_1:Volume_Pozzo_9))

doganella_xts <- doganella_filtered
doganella_xts <- xts(doganella_xts[,-1],order.by = doganella_xts[,1])

doganella_xts <- abs(doganella_xts)


# plot depth to groundwater 

plot(doganella_xts, main = "ts temperature, rainfall and depth to groundwater")


########### water spring ###############

#### lupa ####

lupa <- read.csv("data/Water_Spring_Lupa.csv")

dim(lupa)

str(lupa)

lupa$ï..Date <- dmy(lupa$ï..Date)
lupa <- lupa %>% rename(date = ï..Date) %>% 
  filter(date > "2009-02-18")
  

lupa_xts <- xts(lupa[,-1],order.by = lupa[,1])
plot(lupa_xts)


lupa_xts$Flow_Rate_Lupa <- abs(lupa_xts$Flow_Rate_Lupa)

plot(lupa_xts)




#### madonna di canneto ####

madonna <- read.csv("data/Water_Spring_Madonna_di_Canneto.csv")

dim(madonna)


str(madonna)

madonna$ï..Date <- dmy(madonna$ï..Date)
madonna <- madonna %>% rename(date = ï..Date) %>% 
  filter(!is.na(date),
         date > "2015-03-12")

madonna_xts <- xts(madonna[,-1],order.by = madonna[,1])

plot(madonna_xts)

head(madonna_xts)


################################
# visualising time series data #
################################

# doganella --> rewriting so as to have all depth values on same column 

# data prep

doganella1 <- doganella_filtered %>% 
  gather(key = "well", value = "depth_to_groundwater.m",
         -date,-Rainfall_Velletri,-Rainfall_Monteporzio,
         -Temperature_Velletri,-Temperature_Monteporzio) %>%
  mutate(well = gsub("Depth_to_Groundwater_","", well))


# plotting 

(ts_doganella <- ggplot(doganella1, aes(x = date, y = abs(depth_to_groundwater.m), 
                                        color = well))+
   geom_line(size = 2)+
   scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
   theme_classic()+
    xlab("")+
    ylab("abs. depth to groundwater (m)\n"))


# plotting the sum of well values?

doganella2 <- doganella1 %>% 
  group_by(date) %>% 
  mutate(sum_depths.m = sum(depth_to_groundwater.m))


  # plotting 

(ts_doganella_sum <- ggplot(doganella2, aes(x = date, y =abs(sum_depths.m)))+
    geom_line()+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
    theme_classic())


#############################################################


# statistical analysis - decomposition 

(decomp_1 <- ggplot(doganella2, aes(x = date, y = abs(sum_depths.m)))+
   geom_line()+
   scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
   theme_classic())

(decomp2 <- ggplot(doganella2, aes(x = date, y = abs(sum_depths.m)))+
    geom_line()+
    geom_smooth(method = "loess", se = T, span = 0.6)+
    theme_classic())
# span sets n of points used to plot each local regression in the curve - the smaller 
# the number the more points are used and more closely the curve will fit the original data


# is there seasonality? 

# extracting month and year in separate columns 
doganella2$year <- format(doganella2$date, format = "%Y")
doganella2$month_n <- format(doganella2$date, format = "%m")

# creating color palette 
year_pal <- colortools::sequential(color = "darkturquoise", percentage = 7, what = "value")

str(doganella2)

# plot

(doganella_seasonal <- ggplot(doganella2, aes(x = month_n, 
                                              y = abs(sum_depths.m), 
                                              group = year))+
    geom_line(aes(color= year), size = 2)+
    theme_classic()+
    scale_color_manual(values = year_pal))

### plot doesnt make sense? 


# 
library(stats)
install.packages("imputeTS")
library(imputeTS)
library(forecast)

# to ts class
doganella2_ts <- ts(doganella2$sum_depths.m, start = 2012, end = 2020, freq = 12,
                    )
class(doganella2_ts)


### filling missing data 
  # option using interpolation
doganella2_ts_filled_nas <- na_interpolation(doganella2_ts)
doganella2_ts_filled_nas

# decompose using stl
doganella2_stl <- stl(doganella2_ts_filled_nas, s.window = "period")
class(doganella2_stl)
doganella2_stl

# plots 

plot(doganella2_stl)

monthplot(doganella2_ts_filled_nas, choice = "seasonal")

seasonplot(doganella2_ts_filled_nas)

### it doesn't look seasonal at all! 


### forecasting ###
# ETS models 

doganella_model <- window(x = doganella2_ts_filled_nas,
                                  start = c(2012), end = c(2016))
doganella_test <- window(x = doganella2_ts_filled_nas,
                                 start = c(2018))



# creating model objects of each type of ets model 

abs_doganella_model <- abs(doganella_model)
abs_doganella_test <- abs(doganella_test)

doganella_ets_auto <- ets(abs_doganella_model)
doganella_ets_mmm <- ets(abs_doganella_model, model = "MMM")
doganella_ets_zzz <- ets(abs_doganella_model, model = "ZZZ")
doganella_ets_mmm_damped <- ets(abs_doganella_model, model = "MMM",
                                damped = T)

# creating forecast objects from the model objects 
doganella_ets_fc <- forecast(doganella_ets_auto, h = 60) # 60 time period long (aka 1 month)
doganella_ets_mmm_fc <- forecast(doganella_ets_mmm, h = 60)
doganella_ets_zzz_fc <- forecast(doganella_ets_zzz, h = 60)
doganella_ets_mmm_damped_fc <- forecast(doganella_ets_mmm_damped, h = 60)

# convert forecast to df
doganella_ets_fc_df <- cbind("Month" = rownames(as.data.frame(doganella_ets_fc)),
                             as.data.frame(doganella_ets_fc))
names(doganella_ets_fc_df) <- gsub(" ", "_", names(doganella_ets_fc_df))
doganella_ets_fc_df$Date <- as.Date(paste("01-", doganella_ets_fc_df$Month, sep = ""),
                                    format = "%d-%b %Y")
doganella_ets_fc_df$Model <- rep("ets")

doganella_ets_mmm_fc_df <- cbind("Month" = rownames(as.data.frame(doganella_ets_mmm_fc)),
                                 as.data.frame(doganella_ets_mmm_fc))
names(doganella_ets_mmm_fc_df) <- gsub(" ","_", names(doganella_ets_mmm_fc_df))
doganella_ets_mmm_fc_df$Date <- as.Date(paste("01-",doganella_ets_mmm_fc_df$Month,
                                              sep = ""), 
                                        format = "%d-%b %Y")
doganella_ets_mmm_fc_df$Model <- rep("ets_mmm")

doganella_ets_zzz_fc_df <- cbind("Month" = rownames(as.data.frame(doganella_ets_zzz_fc)),
                                 as.data.frame(doganella_ets_zzz_fc))
names(doganella_ets_zzz_fc_df) <- gsub(" ", "_", names(doganella_ets_zzz_fc_df))
doganella_ets_zzz_fc_df$Date <- as.Date(paste("01-",doganella_ets_zzz_fc_df$Month, 
                                              sep = ""),
                                        format = "%d-%b %Y")
doganella_ets_zzz_fc_df$Model <- rep("ets_zzz")

doganella_ets_mmm_damped_fc_df <- cbind("Month"= rownames(as.data.frame(doganella_ets_mmm_damped_fc)),
                                        as.data.frame(doganella_ets_mmm_damped_fc))
names(doganella_ets_mmm_damped_fc_df) <- gsub(" ", "_", names(doganella_ets_mmm_damped_fc_df))
doganella_ets_mmm_damped_fc_df$Date <- as.Date(paste("01-",doganella_ets_mmm_damped_fc_df$Month,
                                                     sep = ""),
                                               format = "%d-%b %Y")
doganella_ets_mmm_damped_fc_df$Model <- rep("ets_mmm_damped")



## combining into one df

doganella_fc_all <- rbind(doganella_ets_fc_df, doganella_ets_mmm_damped_fc_df,
                      doganella_ets_zzz_fc_df, doganella_ets_mmm_damped_fc_df)



###### plotting with all four forecasts 

(doganella_fc_plot <- ggplot()+
    geom_line(data = doganella2, aes(x = date, y = abs(sum_depths.m)))+
    geom_line(data = doganella_fc_all, aes(x = Date, y = Point_Forecast,
                                           colour = Model),
              size = 1)+
    theme_classic())


# it's not looking good 



### checking accuracy 
accuracy(doganella_ets_fc, abs_doganella_test)
accuracy(doganella_ets_mmm_fc, abs_doganella_test)
accuracy(doganella_ets_zzz_fc, abs_doganella_test)
accuracy(doganella_ets_mmm_damped_fc, abs_doganella_test)

## ets_mmm 

## extracting values from forecast 

doganella_ets_mmm_fc_df %>%
  filter(Month == "Jan 2021") %>% 
  select(Month, Point_Forecast)

#      Month        Point_Forecast
#      Jan 2021        813.312


