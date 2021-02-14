################################################################################
################################################################################
############ DATA PREP FOR MODELLING            ################################
################################################################################
################################################################################


#### libraries ####

#install.packages("outliers")

library(tidyverse)
library(imputeTS)
library(ggplot2)
library(zoo)
library(data.table)
library(lubridate)
library(outliers)
#install.packages("tidyselect")
library(tidyselect)

library(GGally)
library(dplyr)
library(naniar)
library(visdat)
library(forecast)
library(xts)
library(caTools)



###############
##############
#### imposto tema ####
theme_21 <- theme(legend.position = "bottom", legend.direction = "horizontal", axis.text = element_text(size = 14), 
                  plot.caption = element_text(color = "gray25", face = "bold", size = 8), legend.text = element_text(size = 15), 
                  axis.title = element_text(size = 14.5, face = "bold", color = "gray25"), legend.title = element_text(size = 14), axis.line = element_line(size = 0.4), 
                  plot.title = element_text(size = 19), plot.subtitle = element_text(size = 14.5), strip.text = element_text(size = 14, face = "bold"))

core_col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))



#### AUSER AQUIFER #####
auser_original<-read.csv("data/Aquifer_Auser.csv")
####Trasformo la colonna data in data####
auser_original$Date<-as.Date(auser_original$Date, format = "%d/%m/%Y")
#dal primo file 01-auser
#noto valori totalmente mancanti dei dati, dall'inizio del dataset
#max(auser_original$Date[is.na(auser_original$Rainfall_Gallicano)]) # restituisce "2005-12-31"
#risulta il taglio da fare fino al giorno "2005-12-31"
#auser_cut<- auser_original %>%     
#  filter(Date >= "2006-01-01")
#visdat::vis_dat(auser_cut)



#### osservo le variabili target ####

# Target feature ridondante gia' fatto nel file 01-auser
df <- auser_cut %>% select(Date, 
                               Depth_to_Groundwater_LT2,Depth_to_Groundwater_SAL,
                               Depth_to_Groundwater_PAG, Depth_to_Groundwater_DIEC,
                               Depth_to_Groundwater_CoS) %>%
  pivot_longer(., cols = c(Depth_to_Groundwater_LT2,Depth_to_Groundwater_SAL,
                           Depth_to_Groundwater_PAG, Depth_to_Groundwater_DIEC,
                           Depth_to_Groundwater_CoS),
               names_to = "Var", values_to = "Val", values_drop_na = FALSE)
df <- df[complete.cases(df), ]
ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() +   ggtitle("Aquifer Auser: Depth_to_Groundwater meters") +
  ylab("Depth_to_Groundwater") + xlab("Date")
rm(df)

#osservo che il pozzo LT2 ha un comportamento anomalo  improvviso
# da marzo 2020

#continuano ad esserci missing non recuperabili, soprattutto su
#Depth_to_Groundwater_DIEC , CoS, LT2 fino a maggio 2011
#decido di prendere i valori da giugno 2011 in poi
#auser_cut<- auser_cut %>%     
#  filter(Date >= "2011-06-01")
#visdat::vis_dat(auser_cut)
#######   #########










#leggo il dato gia' filtrato
auser <- read.csv("processed_data/AUSER_filtered.csv")
#Trasformo la colonna data in data
auser$Date<-as.Date(auser$Date, format = "%Y-%m-%d")

auser1 <- auser %>% 
  select(-X) %>%
  gather(key = "pozzi", value = "depth_to_gw.m",
         CoS:SAL) %>% 
  mutate(depth_to_gw.m = abs(depth_to_gw.m))

auser <- auser1 %>% 
  spread(key = "pozzi", value = "depth_to_gw.m")

# overall view of depth to gw changes by well type 
(depth_to_gw <- ggplot(auser1, aes(Date, depth_to_gw.m,
                                       color = pozzi, group = pozzi))+
    geom_line(size = 1)+
    theme_classic())


### vis well by well, to fill in gaps 

## pozzo 1 = CoS

(auser_p1 <- ggplot(auser, aes(Date, CoS, group = 1))+
    geom_line(size = 1)+
    theme_classic())

## pozzo 2 = DIEC

(auser_p2 <- ggplot(auser, aes(Date, DIEC, group = 2))+
    geom_line(size = 1)+
    theme_classic())

## pozzo 3 = LT2

(auser_p3 <- ggplot(auser, aes(Date, LT2, group = 3))+
    geom_line(size = 1)+
    theme_classic())

## pozzo 4 = PAG

(auser_p4 <- ggplot(auser, aes(Date, PAG, group = 4))+
    geom_line(size = 1)+
    theme_classic())

## pozzo 5 = SAL

(auser_p5 <- ggplot(auser, aes(Date, SAL, group = 5))+
    geom_line(size = 1)+
    theme_classic())






####FUNCTION if i wanted to count how many nas following in a row?####

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

ggplot_na_distribution(auser$CoS)+
  theme_classic()
ggplot_na_distribution(auser$DIEC)+
  theme_classic()
ggplot_na_distribution(auser$LT2)+
  theme_classic()
ggplot_na_distribution(auser$PAG)+
  theme_classic()
ggplot_na_distribution(auser$SAL)+
  theme_classic()

statsNA(auser$CoS)
statsNA(auser$DIEC)
statsNA(auser$LT2)
statsNA(auser$PAG)
statsNA(auser$SAL)


ggplot_na_intervals(auser$CoS)
ggplot_na_intervals(auser$DIEC)
ggplot_na_intervals(auser$LT2)
ggplot_na_intervals(auser$PAG)
ggplot_na_intervals(auser$SAL)

ggplot_na_gapsize(auser$CoS)
ggplot_na_gapsize(auser$DIEC)
ggplot_na_gapsize(auser$LT2)
ggplot_na_gapsize(auser$PAG)
ggplot_na_gapsize(auser$SAL)


#### linear interpolation ####

auser4 <- auser

auser4$CoS <-as.numeric(na.interp(auser4$CoS))
auser4$DIEC <-as.numeric(na.interp(auser4$DIEC))
auser4$LT2 <-as.numeric(na.interp(auser4$LT2))
auser4$PAG <-as.numeric(na.interp(auser4$PAG))
auser4$SAL <-as.numeric(na.interp(auser4$SAL))


#### FINAL ####

## using na.ma from imputeTS

auser5 <- data.frame(auser, lapply(auser[,23:27], 
                                           function(x) na_ma(x, k=1)))

auser6 <-setnames(auser5, old = colnames(auser5[,28:32]),
                      new = c("imp1","imp2",
                              "imp3","imp4",
                              "imp5"))


#rm(auser7)

auser7 <- auser6 %>% 
  gather(key="imp", value = "imputed_depth_to_gw.m",28:32) %>%
  mutate(Date = ymd(Date))

str(auser7)

#imp1=CoS
#imp2=DIEC
#imp3=LT2
#imp4=PAG
#imp5=SAL
## vis with imputation 

(imp_vis <- ggplot(auser7, aes(Date, imputed_depth_to_gw.m, color = imp,
                                   group = imp))+
    geom_line()+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y"))


statsNA(auser7$imputed_depth_to_gw.m)
## cleaned up - no more missing data 


#### checking for outliers ####

## summary

auser8 <- auser7 %>% 
  spread(key = "imp", value = "imputed_depth_to_gw.m") 

summary(auser8[,28:32])

## hist 

(hist_auser <- ggplot(auser7, aes(imputed_depth_to_gw.m))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(vars(imp)))


## boxplots

# target var 
(hist_target_auser <- ggplot(auser7,
                                 aes(y = imputed_depth_to_gw.m,
                                     color = imp))+
    geom_boxplot()+
    theme_classic())#+
#facet_wrap(vars(imp)))
# saving
ggsave("img/hist_auser_target.jpg", dpi = 500, width = 10, height=7)

#### checking singularly 

(imp1_auser <- ggplot(auser8, aes(y = imp1))+
    geom_boxplot()+
    theme_classic())
#(imp1_auser <- ggplot(auser7, aes(y = CoS))+
#    geom_boxplot()+
#    theme_classic())

#(imp2_auser <- ggplot(auser7, aes(y = DIEC))+
#   geom_boxplot()+
#    theme_classic())
(imp2_auser <- ggplot(auser8, aes(y = imp2))+
    geom_boxplot()+
    theme_classic())

#(imp3_auser <- ggplot(auser7, aes(y = LT2))+
#    geom_boxplot()+
#    theme_classic())
(imp3_auser <- ggplot(auser8, aes(y = imp3))+
    geom_boxplot()+
    theme_classic())

#(imp4_auser <- ggplot(auser7, aes(y = PAG))+
#    geom_boxplot()+
#    theme_classic())
(imp4_auser <- ggplot(auser8, aes(y = imp4))+
    geom_boxplot()+
    theme_classic())

#(imp5_auser <- ggplot(auser7, aes(y = SAL))+
#    geom_boxplot()+
#    theme_classic())
(imp5_auser <- ggplot(auser8, aes(y = imp5))+
    geom_boxplot()+
    theme_classic())

## extracting vals of potential outliers 

# for pozzo 1 CoS
out1_auser <- boxplot.stats(auser8$imp1)$out # 0 (repeated x4)
out1_auser
out1_ind_auser <- which(auser8$imp1 %in% c(out1_auser))
out1_ind_auser # rows where outliers are found

under_bound <- quantile(auser8$imp1, 0.025)
under_bound # 4.32

under_bound99 <- quantile(auser8$imp1, 0.01)
under_bound99 # 4.22 

## checking stats to verify it's an outlier
# grubbs test
test1 <- grubbs.test(auser8$imp1)
test1 # 0 is an outlier (at 5% significance level)

## substituting with q1 value 
auser8$imp1[auser8$imp1 == 0] <- 4.32
summary(auser8$imp1)

# for pozzo 2 DIEC ok non ho outliers
out2_auser <- boxplot.stats(auser8$imp2)$out # non ci sono valore critici
out2_auser

# for pozzo 3 LT2
out3_auser <- boxplot.stats(auser8$imp3)$out # 0 (repeated x2)
out3_auser
out3_ind_auser <- which(auser8$imp3 %in% c(out3_auser))
out3_ind_auser # rows where outliers are found

under_bound <- quantile(auser8$imp3, 0.025)
under_bound # 11.85

under_bound99 <- quantile(auser8$imp3, 0.01)
under_bound99 # 11.78 

## checking stats to verify it's an outlier
# grubbs test
test3 <- grubbs.test(auser8$imp3)
test3 # 0 is an outlier 

## substituting with q1 value 
auser8$imp3[auser8$imp3 == 0] <- 11.85
summary(auser8$imp3)


# for pozzo 4 PAG
out4_auser <- boxplot.stats(auser8$imp4)$out # non ci sono valore critici
out4_auser


# for pozzo 5 SAL
out5_auser <- boxplot.stats(auser8$imp5) # 
out5_auser
out5_ind_auser <- which(auser8$imp5 %in% c(out5_auser))
out5_ind_auser # 

under_bound_5 <- quantile(auser8$imp5, 0.025)
under_bound_5 # 4.28

# grubbs test 
test5 <- grubbs.test(auser8$imp5)
test5 # 0 it is an outlier, 8% significance

# substituting 
auser8$imp5[auser8$imp5 == 0] <- 4.28
summary(auser8$imp5)


boxplot.stats(auser8$imp5)

out5 <- boxplot.stats(auser8$imp5)$out
out5 
out5_ind <- which(auser8$imp6 %in% c(out5))
out5_ind

data5 <- auser8[out5_ind,]




####################

auser9 <- auser8 %>% 
  gather(key = "imp", value = "depth_to_gw.m", 28:32)
View(auser9)

(auser_noout <- ggplot(auser9, aes(y=depth_to_gw.m,color = imp))+
    geom_boxplot()+
    theme_classic())

# saving 

ggsave("img/auser_boxplot.jpg", dpi = 500, width = 10, height = 7)



### keeping outliers for pozzo 5 - 


### checking outliers for feature variables 

# auser9

str(auser9)


auser10 <- auser9 %>% 
  gather(key = "temp_sensor", value = "temp.C", Temperature_Monte_Serra, 
         Temperature_Orentano, Temperature_Ponte_a_Moriano, Temperature_Lucca_Orto_Botanico) %>%
  gather(key = "rain_sensor", value = "rain.mm", Rainfall_Monte_Serra,
         Rainfall_Orentano, Rainfall_Gallicano, Rainfall_Borgo_a_Mozzano, 
         Rainfall_Fabbriche_di_Vallico, Rainfall_Piaggione, Rainfall_Calavorno,
        Rainfall_Croce_Arcana, Rainfall_Tereglio_Coreglia_Antelminelli)


## vis hist

# temp

(temp_hist_auser <- ggplot(auser10, aes(temp.C))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(vars(temp_sensor)))
# pretty similar distribution

# rain
(rain_hist_dog <- ggplot(auser10, aes(rain.mm))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(vars(rain_sensor)))
# pretty similar here as well


## vis boxplot

# temp 
(temp_box_auser <- ggplot(auser10, aes(y=temp.C, color = temp_sensor))+
    geom_boxplot()+
    theme_classic())
# no outliers 

# rain 
(rain_box_auser <- ggplot(auser10, aes(y = rain.mm, color = rain_sensor))+
    geom_boxplot()+
    theme_classic())
# like in histogram, there's a long tail, 
#which the plot recognises as outliers 


### let's check it statistically 

out_tempor_auser <- boxplot.stats(auser9$Temperature_Orentano)
out_tempv_auser
# confirmed no outliers for temp Orentano 

out_tempms_auser <- boxplot.stats(auser9$Temperature_Monte_Serra)
out_tempms_auser
# no outliers for temp Temperature_Monte_Serra

out_tempam_auser <- boxplot.stats(auser9$Temperature_Ponte_a_Moriano)
out_tempam_auser
# no outliers for temp Ponte_a_Moriano

out_templu_auser <- boxplot.stats(auser9$Temperature_Lucca_Orto_Botanico)
out_templu_auser
# no outliers for temp Lucca_Orto_Botanico




out_rainb_auser <- boxplot.stats(auser9$Rainfall_Borgo_a_Mozzano)$out
out_rainb_auser
# more than 900 outliers... 


out_rainca_auser <- boxplot.stats(auser9$Rainfall_Calavorno)$out
out_rainca_auser
# same here 

out_raincr_auser <- boxplot.stats(auser9$Rainfall_Croce_Arcana)$out
out_raincr_auser
# same here 

out_rainf_auser <- boxplot.stats(auser9$Rainfall_Fabbriche_di_Vallico)$out
out_rainf_auser
# same here 

out_rainca_auser <- boxplot.stats(auser9$Rainfall_Calavorno)$out
out_rainca_auser
# same here 

out_raing_auser <- boxplot.stats(auser9$Rainfall_Gallicano)$out
out_raing_auser
# same here 

out_raino_auser <- boxplot.stats(auser9$Rainfall_Orentano)$out
out_raino_auser
# same here 

out_rainms_auser <- boxplot.stats(auser9$Rainfall_Monte_Serra)$out
out_rainms_auser
# same here 

out_rainp_auser <- boxplot.stats(auser9$Rainfall_Piaggione)$out
out_rainp_auser
# same here 

out_raint_auser <- boxplot.stats(auser9$Rainfall_Tereglio_Coreglia_Antelminelli)$out
out_raint_auser
# same here 


## let's check where these "outliers" occur in time (rain)

# Borgo a Mozzano
out_rainb_auser_ind <- which(auser9$Rainfall_Borgo_a_Mozzano %in% c(out_rainb_auser))
out_rainb_auser_ind

df_rainb_out <- auser9[out_rainb_auser_ind,]
df_rainb_out

# plotting it over time 
(plot_rainb_out <- ggplot(df_rainb_out, aes(Date, Rainfall_Borgo_a_Mozzano))+
    geom_point()+
    geom_line(data = auser9, aes(Date, Rainfall_Borgo_a_Mozzano, color = "red"))+
    theme_classic())
# it occurs throughout - from 2012 through to 2020


#### rainfall Calavorno ####
out_rainc_auser_ind <- which(auser9$Rainfall_Calavorno %in% c(out_rainca_auser))
out_rainc_auser_ind
length(out_rainc_auser_ind)

df_rainc_auser <- auser9[out_rainc_auser_ind,]
View(df_rainc_auser)

# plotting over time 
(plot_rainc_auser <- ggplot(auser9, aes(Date, Rainfall_Calavorno))+
    geom_line(color = "red")+
    geom_point(data = df_rainc_auser, aes(Date, Rainfall_Calavorno))+
    theme_classic())

#(plot_rainc_auser <- ggplot(df_rainc_auser, aes(Date, Rainfall_Calavorno))+
#    geom_point()+
#    theme_classic())

#### rainfall Croce_Arcana ####
out_raincr_auser_ind <- which(auser9$Rainfall_Croce_Arcana %in% c(out_raincr_auser))
out_raincr_auser_ind
length(out_raincr_auser_ind)

df_raincr_auser <- auser9[out_raincr_auser_ind,]
View(df_raincr_auser)

# plotting over time 
(plot_raincr_auser <- ggplot(auser9, aes(Date, Rainfall_Croce_Arcana))+
    geom_line(color = "red")+
    geom_point(data = df_rainc_auser, aes(Date, Rainfall_Croce_Arcana))+
    theme_classic())

#### rainfall Fabbriche_di_Vallico ####
out_rainf_auser_ind <- which(auser9$Rainfall_Fabbriche_di_Vallico %in% c(out_rainf_auser))
out_rainf_auser_ind
length(out_rainf_auser_ind)

df_rainf_auser <- auser9[out_rainf_auser_ind,]
View(df_rainf_auser)

# plotting over time 
(plot_rainf_auser <- ggplot(auser9, aes(Date, Rainfall_Fabbriche_di_Vallico))+
    geom_line(color = "red")+
    geom_point(data = df_rainc_auser, aes(Date, Rainfall_Fabbriche_di_Vallico))+
    theme_classic())

#### rainfall Gallicano ####
out_raing_auser_ind <- which(auser9$Rainfall_Gallicano %in% c(out_raing_auser))
out_raing_auser_ind
length(out_raing_auser_ind)

df_raing_auser <- auser9[out_raing_auser_ind,]
View(df_raing_auser)

# plotting over time 
(plot_raing_auser <- ggplot(auser9, aes(Date, Rainfall_Gallicano))+
    geom_line(color = "red")+
    geom_point(data = df_raing_auser, aes(Date, Rainfall_Gallicano))+
    theme_classic())


#### rainfall Monte_Serra ####
out_rainms_auser_ind <- which(auser9$Rainfall_Monte_Serra %in% c(out_rainms_auser))
out_rainms_auser_ind
length(out_rainms_auser_ind)

df_rainms_auser <- auser9[out_rainms_auser_ind,]
View(df_rainms_auser)

# plotting over time 
(plot_rainms_auser <- ggplot(auser9, aes(Date, Rainfall_Monte_Serra))+
    geom_line(color = "red")+
    geom_point(data = df_rainms_auser, aes(Date, Rainfall_Monte_Serra))+
    theme_classic())

#### rainfall Orentano ####
out_raino_auser_ind <- which(auser9$Rainfall_Orentano %in% c(out_raino_auser))
out_raino_auser_ind
length(out_raino_auser_ind)

df_raino_auser <- auser9[out_raino_auser_ind,]
View(df_raino_auser)

# plotting over time 
(plot_raino_auser <- ggplot(auser9, aes(Date, Rainfall_Orentano))+
    geom_line(color = "red")+
    geom_point(data = df_rainms_auser, aes(Date, Rainfall_Orentano))+
    theme_classic())

#### rainfall Piaggione ####
out_rainp_auser_ind <- which(auser9$Rainfall_Piaggione %in% c(out_rainp_auser))
out_rainp_auser_ind
length(out_rainp_auser_ind)

df_rainp_auser <- auser9[out_rainp_auser_ind,]
View(df_rainp_auser)

# plotting over time 
(plot_rainp_auser <- ggplot(auser9, aes(Date, Rainfall_Piaggione))+
    geom_line(color = "red")+
    geom_point(data = df_rainp_auser, aes(Date, Rainfall_Piaggione))+
    theme_classic())

#### rainfall Tereglio_Coreglia_Antelminelli ####
out_raint_auser_ind <- which(auser9$Rainfall_Tereglio_Coreglia_Antelminelli %in% c(out_raint_auser))
out_raint_auser_ind
length(out_raint_auser_ind)

df_raint_auser <- auser9[out_raint_auser_ind,]
View(df_raint_auser)

# plotting over time 
(plot_rainms_auser <- ggplot(auser9, aes(Date, Rainfall_Tereglio_Coreglia_Antelminelli))+
    geom_line(color = "red")+
    geom_point(data = df_rainms_auser, aes(Date, Rainfall_Tereglio_Coreglia_Antelminelli))+
    theme_classic())



#### decision to leave them as such 


#### checking out for missing - rain and temp ####

# rain
statsNA(auser9$Rainfall_Borgo_a_Mozzano) #no missing
ggplot_na_distribution(auser9$Rainfall_Borgo_a_Mozzano)

statsNA(auser9$Rainfall_Calavorno) #no missing
ggplot_na_distribution(auser9$Rainfall_Calavorno)

statsNA(auser9$Rainfall_Croce_Arcana) #no missing
ggplot_na_distribution(auser9$Rainfall_Croce_Arcana)

statsNA(auser9$Rainfall_Fabbriche_di_Vallico) #no missing
ggplot_na_distribution(auser9$Rainfall_Fabbriche_di_Vallico)

statsNA(auser9$Rainfall_Gallicano) #no missing
ggplot_na_distribution(auser9$Rainfall_Gallicano)

statsNA(auser9$Rainfall_Monte_Serra) # 30 missing
ggplot_na_distribution(auser9$Rainfall_Monte_Serra)

statsNA(auser9$Rainfall_Orentano) #no missing
ggplot_na_distribution(auser9$Rainfall_Orentano)

statsNA(auser9$Rainfall_Piaggione) # 1825 missing
ggplot_na_distribution(auser9$Rainfall_Piaggione)

statsNA(auser9$Rainfall_Tereglio_Coreglia_Antelminelli) # no missing
ggplot_na_distribution(auser9$Rainfall_Tereglio_Coreglia_Antelminelli)


(rain_auser <- ggplot(auser10, aes(Date, rain.mm, color = rain_sensor))+
    geom_line()+
    theme_classic())

# temp 
statsNA(auser9$Temperature_Orentano) #no missing
ggplot_na_distribution(auser9$Temperature_Orentano)

statsNA(auser9$Temperature_Monte_Serra)# no missing
ggplot_na_distribution(auser9$Temperature_Monte_Serra)

statsNA(auser9$Temperature_Ponte_a_Moriano)# no missing
ggplot_na_distribution(auser9$Temperature_Ponte_a_Moriano)

statsNA(auser9$Temperature_Lucca_Orto_Botanico)# no missing
ggplot_na_distribution(auser9$Temperature_Lucca_Orto_Botanico)



#### FILLING GAPS WITH METEO ####

####  Rainfall_Monte_Serra



####  Rainfall_Piaggione





####
#### Correlation Matrix ####
df <- auser
df$Date <- NULL
ggcorr(df, label = TRUE, label_round = 2, hjust = 1, size = 4, layout.exp = 4, label_size = 3)
rm(df)