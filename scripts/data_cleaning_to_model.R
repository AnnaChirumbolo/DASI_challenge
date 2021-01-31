################################################################################
################################################################################
#######       we can use this script to prep the data for modelling         ####
################################################################################
################################################################################


### libraries

#install.packages("outliers")

library(tidyverse)
library(imputeTS)
library(ggplot2)
library(zoo)
library(data.table)
library(lubridate)
library(outliers)

###############
##############


#### doganella #####

doganella <- read.csv("processed_data/DOGANELLA_filtered.csv")


doganella1 <- doganella %>% 
  select(-X) %>%
  gather(key = "pozzi", value = "depth_to_gw.m",
         14:22) %>% 
  mutate(depth_to_gw.m = abs(depth_to_gw.m))

doganella <- doganella1 %>% 
  spread(key = "pozzi", value = "depth_to_gw.m")



# overall view of depth to gw changes by well type 
(depth_to_gw <- ggplot(doganella1, aes(Date, depth_to_gw.m,
                                       color = pozzi, group = pozzi))+
    geom_line(size = 2)+
    theme_classic())


### vis well by well, to fill in gaps 

## pozzo 1


(doganella_p1 <- ggplot(doganella, aes(Date, Pozzo_1, group = 1))+
    geom_line(size = 1)+
    theme_classic())

## if i wanted to count how many nas following in a row?

group.na <- function(x){
  group <- if_else(is.na(x), 1, 0)
  return(group)
}


# creating a function that counts sequence of nas in a row for each variable 
count.na <- function(x){
  group_na <- if_else(is.na(x), 1,0)
  grp <- with(rle(group_na), rep(seq_along(lengths), lengths))
  counter <- ave(grp,grp, FUN = seq_along)
  return(counter)
}

###
#with(mtcars, ave(mpg, cyl, FUN=mean))
# ave is baseR equivalent of group_by() %>% mutate()!!! YAS
####

#doganella2 <- data.frame(doganella, lapply(doganella[,14:22], count.na))

#doganella3 <- data.frame(doganella2, lapply(doganella2[,14:22],
                                          #  function(x) if_else(is.na(x),1,0)))
  

## all this was to try and find a way to impute missing data

## imputeTS

ggplot_na_distribution(doganella$Pozzo_1)+
  theme_classic()

statsNA(doganella$Pozzo_1)

ggplot_na_intervals(doganella$Pozzo_1)

ggplot_na_gapsize(doganella$Pozzo_1)


## linear interpolation 

doganella4 <- data.frame(doganella, lapply(doganella[,14:22], na.approx))



#### FINAL ####

## using na.ma from imputeTS

doganella5 <- data.frame(doganella, lapply(doganella[,14:22], 
                                                     function(x) na_ma(x, k=1)))

doganella6 <-setnames(doganella5, old = colnames(doganella5[,23:31]),
                      new = c("imp1","imp2",
                                     "imp3","imp4",
                                     "imp5","imp6",
                                     "imp7","imp8",
                                     "imp9"))


rm(doganella7)

doganella7 <- doganella6 %>% 
  gather(key="imp", value = "imputed_depth_to_gw.m",23:31) %>%
  mutate(Date = ymd(Date))
  
str(doganella7)


## vis with imputation 

(imp_vis <- ggplot(doganella7, aes(Date, imputed_depth_to_gw.m, color = imp,
                                   group = imp))+
    geom_line()+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y"))


statsNA(doganella7$imputed_depth_to_gw.m)
## cleaned up - no more missing data 


#### checking for outliers ####

## summary

doganella8 <- doganella7 %>% 
  spread(key = "imp", value = "imputed_depth_to_gw.m") 
  
summary(doganella8[,23:31])

## hist 

(hist_doganella <- ggplot(doganella7, aes(imputed_depth_to_gw.m))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(vars(imp)))
  
  
## boxplots

  # target var 
(hist_target_doganella <- ggplot(doganella7,
                                 aes(y = imputed_depth_to_gw.m,
                                     color = imp))+
    geom_boxplot()+
    theme_classic())#+
    #facet_wrap(vars(imp)))
# saving
ggsave("img/hist_doganella_target.jpg", dpi = 500, width = 10, height=7)

#### checking singularly 

(imp2_doganella <- ggplot(doganella7, aes(y = Pozzo_2))+
    geom_boxplot()+
    theme_classic())

(imp4_doganella <- ggplot(doganella7, aes(y = Pozzo_4))+
    geom_boxplot()+
    theme_classic())

(imp8_doganella <- ggplot(doganella7, aes(y = Pozzo_8))+
    geom_boxplot()+
    theme_classic())

## extracting vals of potential outliers 

  # for pozzo 2
out2_dog <- boxplot.stats(doganella8$imp2)$out # 112.5 (repeated x4)
out2_ind_dog <- which(doganella8$imp2 %in% c(out))
out2_ind_dog # rows where outliers are found

upper_bound <- quantile(doganella8$imp2, 0.975)
upper_bound # 101.49

upper_bound99 <- quantile(doganella8$imp2, 0.99)
upper_bound99 # 101.67

## checking stats to verify it's an outlier
# grubbs test
test2 <- grubbs.test(doganella8$imp2)
test2 # 112.5 is an outlier (at 5% significance level)

## substituting with q3 value 
doganella8$imp2[doganella8$imp2 == 112.5] <- 99.21
summary(doganella8$imp2)

# for pozzo 4
out4_dog <- boxplot.stats(doganella8$imp4) # 108.66
out4_dog
out4_ind_dog <- which(doganella8$imp4 %in% c(out4_dog))
out4_ind_dog # line 158

  # grubbs test 
test4 <- grubbs.test(doganella8$imp4)
test4 # it is an outlier, 5% significance

  # substituting 
doganella8$imp4[doganella8$imp4 == 108.66] <- 100.52
summary(doganella8$imp4)

  # for pozzo 8
out8_dog <- boxplot.stats(doganella8$imp8) # 107.5 (rep x2)
out8_dog

  # grubbs test 
test8 <- grubbs.test(doganella8$imp8)
test8 # it is an outlier 
  
  # subst 
doganella8$imp8[doganella8$imp8 == 107.5] <- 97.96
summary(doganella8$imp8)


## pozzo 6 e 9

out6 <- boxplot.stats(doganella8$imp6)$out
out6_ind <- which(doganella8$imp6 %in% c(out6))
out6_ind

data6 <- doganella8[out6_ind,]

(data6_vis <- ggplot(data6, aes(y = imp6))+
    geom_boxplot()+
    theme_classic())

(data6_vis <- ggplot(data6, aes(Date, imp6))+
    geom_point()+
    theme_classic())


boxplot.stats(doganella8$imp9)


out9 <- boxplot.stats(doganella8$imp9)$out
out9_ind <- which(doganella8$imp9 %in% c(out9))
out9_ind

data9 <- doganella8[out9_ind,]

(data9_vis <- ggplot(data9, aes(Date, imp9))+
    geom_point()+
    theme_classic())

####################

doganella9 <- doganella8 %>% 
  gather(key = "imp", value = "depth_to_gw.m", 23:31)
View(doganella9)

(doganella_noout <- ggplot(doganella9, aes(y=depth_to_gw.m,color = imp))+
    geom_boxplot()+
    theme_classic())




