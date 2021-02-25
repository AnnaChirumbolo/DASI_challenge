################################################################################
################################################################################
###############                             ####################################
###############   Script prep of vars       ####################################
################################################################################
################################################################################

#### inizializzo ####
getwd()
rm(list = ls(all=TRUE)) 


#### uploading libraries ####

install.packages("naniar")
library(naniar)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(tibble)
library(dplyr)



#### imposto tema ####
theme_21 <- theme(legend.position = "bottom", legend.direction = "horizontal", axis.text = element_text(size = 14), 
                  plot.caption = element_text(color = "gray25", face = "bold", size = 8), legend.text = element_text(size = 15), 
                  axis.title = element_text(size = 14.5, face = "bold", color = "gray25"), legend.title = element_text(size = 14), axis.line = element_line(size = 0.4), 
                  plot.title = element_text(size = 19), plot.subtitle = element_text(size = 14.5), strip.text = element_text(size = 14, face = "bold"))

core_col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

#### uploading files ####
auser <- read.csv("data/Aquifer_Auser.csv")

####Trasformo la colonna data in data####
auser$Date<-as.Date(auser$Date, format = "%d/%m/%Y")

#### N/A Visualization ###
visdat::vis_dat(auser)
ggsave("img/auser/01Auser.jpg",
       dpi = 500, width = 10, height=7)

str(auser) 
names(auser)
summary(auser) #per avere una visione del dataframe

#### data cleaning ####

#### auser ####


auser_missing <- auser %>% 
  miss_var_summary()
print(auser_missing)
View(auser_missing)

#Target Sal, CoS, LT2
auser1 <- auser %>% 
  gather(key = "well", value = "depth_to_gw.m", Depth_to_Groundwater_LT2 : Depth_to_Groundwater_DIEC) %>%
  mutate(Date = ymd(Date),
         well = gsub("Depth_to_Groundwater_","",well))

(first_look <- ggplot(auser1, aes(x =Date, y = abs(depth_to_gw.m), color = well))+
    geom_line(size = .5)+
    theme_classic())
ggsave("img/auser/03Auser.jpg",
       dpi = 500, width = 10, height=7)
### missing data clearly starting from 

## removing missing for depth to gw
#### missing prima parte ####
#noto valori totalmente mancanti dei dati, dall'inizio del dataset
max(auser$Date[is.na(auser$Rainfall_Gallicano)]) # restituisce "2005-12-31"

#risulta il taglio da fare fino al giorno "2005-12-31"
auser_cut<- auser %>%     
  filter(Date >= "2006-01-01")
visdat::vis_dat(auser_cut)
ggsave("img/auser/02Auser.jpg",
       dpi = 500, width = 10, height=7)
#continuano ad eserci missing non recuperabili, soprattutto su
#Depth_to_Groundwater_DIEC , CoS, LT2 (fino a maggio 2011)


#1 plot con tutte le 5 variabili
auser_filtered <- auser1 %>% 
  filter(Date >= "2006-01-01")


(second_look <- ggplot(auser_filtered, aes(x=Date, y=abs(depth_to_gw.m), 
                                           color = well))+
    geom_line(size = .5)+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    ylab("Abs. depth to groundwater in m")+
    xlab("")) 

ggsave("img/auser/04AUSER_filtered_depth_to_groundwater.jpg", 
       dpi = 500, height = 7, width = 10)



#2 osservo solo le variabilitarget


auser_filtered1 <- auser1 %>% 
  filter(well==c("LT2", "CoS", "SAL")) %>%
  filter(Date >= "2006-01-01")


(second_look <- ggplot(auser_filtered1, aes(x=Date, y=abs(depth_to_gw.m), 
                                           color = well))+
    geom_line(size = .5)+
    theme_classic()+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    ylab("Abs. depth to groundwater in m")+
    xlab("")) 

ggsave("img/auser/05AUSER_target_depth_to_groundwater.jpg", 
       dpi = 500, height = 7, width = 10)



### scatter correlations ###

## temp ##

min(auser_filtered$Temperature_Orentano,na.rm = T)
max(auser_filtered$Temperature_Orentano,na.rm = T)
min(auser_filtered$Temperature_Monte_Serra,na.rm = T)
max(auser_filtered$Temperature_Monte_Serra,na.rm = T)
min(auser_filtered$Temperature_Ponte_a_Moriano,na.rm = T)
max(auser_filtered$Temperature_Ponte_a_Moriano,na.rm = T)
min(auser_filtered$Temperature_Lucca_Orto_Botanico,na.rm = T)
max(auser_filtered$Temperature_Lucca_Orto_Botanico,na.rm = T)


auser_filtered1<-auser_filtered%>%
  filter(well==c("LT2", "CoS", "SAL")) #mi serve per selezionare solo i pozzi Target
  
(scatter_temp_orentano <- ggplot(auser_filtered1, 
                                 aes(x = Temperature_Orentano,
                                     y = abs(depth_to_gw.m),
                                     color = well))+
    geom_point(size = 1, alpha = 0.8)+
    theme_classic()+
    xlim(0,31))
ggsave("img/auser/06auser_temp_orentano.jpg",
       dpi = 500, width = 10, height=7)
# from the plot there doesnt seem to be any correlation.... 


(scatter_temp_monteserra <- ggplot(auser_filtered1,
                                   aes(Temperature_Monte_Serra, 
                                       abs(depth_to_gw.m),
                                       color = well))+
    geom_point(size = 1, alpha = 0.8)+
    theme_classic()+
    xlim(0,31))
ggsave("img/auser/07auser_temp_monteserra.jpg",
       dpi = 500, width = 10, height=7)
# same for Monteserra


#temp_ponteamoriano
(scatter_temp_ponteamoriano <- ggplot(auser_filtered1,
                                      aes(Temperature_Ponte_a_Moriano, 
                                          abs(depth_to_gw.m),
                                          color = well))+
    geom_point(size = 1, alpha = 0.8)+
    theme_classic()+
    xlim(0,31))
ggsave("img/auser/08auser_temp_ponteamoriano.jpg",
       dpi = 500, width = 10, height=7)

#temp_lucca
(scatter_temp_lucca <- ggplot(auser_filtered1,
                              aes(Temperature_Lucca_Orto_Botanico, 
                                  abs(depth_to_gw.m),
                                  color = well))+
    geom_point(size = 1, alpha = 0.8)+
    theme_classic()+
    xlim(0,31))
ggsave("img/auser/09auser_temp_lucca.jpg",
       dpi = 500, width = 10, height=7)




#### rain ####

min(auser_filtered$Rainfall_Gallicano,na.rm = T)
max(auser_filtered$Rainfall_Gallicano,na.rm = T)
min(auser_filtered$Rainfall_Pontetetto,na.rm = T)
max(auser_filtered$Rainfall_Pontetetto,na.rm = T)
min(auser_filtered$Rainfall_Monte_Serra,na.rm = T)
max(auser_filtered$Rainfall_Monte_Serra,na.rm = T)
min(auser_filtered$Rainfall_Orentano,na.rm = T)
max(auser_filtered$Rainfall_Orentano,na.rm = T)
min(auser_filtered$Rainfall_Borgo_a_Mozzano,na.rm = T)
max(auser_filtered$Rainfall_Borgo_a_Mozzano,na.rm = T)
min(auser_filtered$Rainfall_Calavorno,na.rm = T)
max(auser_filtered$Rainfall_Calavorno,na.rm = T)
min(auser_filtered$Rainfall_Piaggione,na.rm = T)
max(auser_filtered$Rainfall_Piaggione,na.rm = T)
min(auser_filtered$Rainfall_Croce_Arcana,na.rm = T)
max(auser_filtered$Rainfall_Croce_Arcana,na.rm = T)
min(auser_filtered$Rainfall_Tereglio_Coreglia_Antelminelli ,na.rm = T)
max(auser_filtered$Rainfall_Tereglio_Coreglia_Antelminelli,na.rm = T)
min(auser_filtered$Rainfall_Fabbriche_di_Vallico,na.rm = T)
max(auser_filtered$Rainfall_Fabbriche_di_Vallico,na.rm = T)

#rain_gallicano
(scatter_rain_gallicano <- ggplot(auser_filtered1, 
                                  aes(Rainfall_Gallicano,
                                      abs(depth_to_gw.m),
                                      color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))
ggsave("img/auser/10auser_rain_gallicano.jpg",
       dpi = 500, width = 10, height=7)


#rain_pontetetto
(scatter_rain_pontetetto <- ggplot(auser_filtered1,
                                   aes(Rainfall_Pontetetto,
                                       abs(depth_to_gw.m),
                                       color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))
ggsave("img/auser/10auser_rain_ponteteto.jpg",
       dpi = 500, width = 10, height=7)

#rain)monte Serra
(scatter_rain_Monte_Serra <- ggplot(auser_filtered1,
                                    aes(Rainfall_Monte_Serra,
                                        abs(depth_to_gw.m),
                                        color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))
ggsave("img/auser/10auser_rain_monte_serra.jpg",
       dpi = 500, width = 10, height=7)

#rain Orentano
(scatter_rain_Orentano <- ggplot(auser_filtered1,
                                 aes(Rainfall_Orentano,
                                     abs(depth_to_gw.m),
                                     color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))
ggsave("img/auser/10auser_rain_orentano.jpg",
       dpi = 500, width = 10, height=7)

#rain Borgo a mozzano
(scatter_rain_Borgo_a_Mozzano <- ggplot(auser_filtered1,
                                        aes(Rainfall_Borgo_a_Mozzano,
                                            abs(depth_to_gw.m),
                                            color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))
ggsave("img/auser/10auser_rain_borgo_a_mozzano.jpg",
       dpi = 500, width = 10, height=7)

#rain Calavorno
(scatter_rain_Calavorno <- ggplot(auser_filtered1,
                                  aes(Rainfall_Calavorno,
                                      abs(depth_to_gw.m),
                                      color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))
ggsave("img/auser/10auser_rain_calavorno.jpg",
       dpi = 500, width = 10, height=7)

#rain Piaggione
(scatter_rain_Piaggione <- ggplot(auser_filtered1,
                                  aes(Rainfall_Piaggione,
                                      abs(depth_to_gw.m),
                                      color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))
ggsave("img/auser/10auser_rain_piaggione.jpg",
       dpi = 500, width = 10, height=7)

#rain Croce Arcano
(scatter_rain_Croce_Arcana <- ggplot(auser_filtered1,
                                     aes(Rainfall_Croce_Arcana,
                                         abs(depth_to_gw.m),
                                         color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))
ggsave("img/auser/10auser_rain_croce arcana.jpg",
       dpi = 500, width = 10, height=7)


#rain Tereglio Coreglia
(scatter_rain_Tereglio_Coreglia_Antelminelli <- ggplot(auser_filtered1,
                                                       aes(Rainfall_Tereglio_Coreglia_Antelminelli,
                                                           abs(depth_to_gw.m),
                                                           color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))
ggsave("img/auser/10auser_rain_Tereglio_Coreglia_Antelminelli.jpg",
       dpi = 500, width = 10, height=7)

#rain Fabbriche di Vallico
(scatter_rain_Fabbriche_di_Vallico <- ggplot(auser_filtered1,
                                             aes(Rainfall_Fabbriche_di_Vallico,
                                                 abs(depth_to_gw.m),
                                                 color = well))+
    geom_point(size = 1, alpha = .8)+
    theme_classic()+
    xlim(0,115))

ggsave("img/auser/10auser_rain_Fabbriche_di_Vallico.jpg",
       dpi = 500, width = 10, height=7)


#### saving plots ####

panelled_rain_scatter_auser <- ggarrange(scatter_rain_gallicano,
                                         scatter_rain_pontetetto,
                                         scatter_rain_Monte_Serra,
                                         scatter_rain_Orentano,
                                         scatter_rain_Borgo_a_Mozzano,
                                         scatter_rain_Calavorno,
                                         scatter_rain_Piaggione,
                                         scatter_rain_Croce_Arcana,
                                         scatter_rain_Tereglio_Coreglia_Antelminelli,
                                         scatter_rain_Fabbriche_di_Vallico)
panelled_rain_scatter_auser


ggsave("img/auser/11AUSER_corr_rain.jpg",
       dpi = 500, height = 7, width = 10)



panelled_temp_scatter_auser <- ggarrange(scatter_temp_orentano,
                                         scatter_temp_ponteamoriano,
                                         scatter_temp_lucca,
                                         scatter_temp_monteserra
                                         )
panelled_temp_scatter_auser

ggsave("img/auser/12AUSER_corr_temp.jpg",
       dpi = 500, height= 7, width = 10)


### view auser_filtered.csv 

View(auser_filtered)


# spreading values again 

auser_filtered_save <- auser_filtered %>% 
  spread(well,depth_to_gw.m)

# saving 

write.csv(auser_filtered_save, "processed_data/AUSER_filtered.csv")






################################################################################
################################################################################


#### osservo le variabili target ####

#osservo che il pozzo LT2 ha un comportamento anomalo  improvviso
# da marzo 2020

#continuano ad esserci missing non recuperabili, soprattutto su
#Depth_to_Groundwater_DIEC , CoS, LT2 fino a maggio 2011
#decido di prendere i valori da giugno 2011 in poi
#auser_cut<- auser_cut %>%     
#  filter(Date >= "2011-06-01")
#visdat::vis_dat(auser_cut)
#######   #########


auser1 <- auser_filtered_save %>% 
  gather(key = "pozzi", value = "depth_to_gw.m",
         CoS:SAL) %>% 
  mutate(depth_to_gw.m = abs(depth_to_gw.m))

auser <- auser1 %>% 
  spread(key = "pozzi", value = "depth_to_gw.m")



### vis well by well, to fill in gaps 

## pozzo 1 = CoS target

(auser_p1 <- ggplot(auser, aes(Date, CoS, group = 1))+
    geom_line(size = 1)+
    theme_classic())

## pozzo 2 = DIEC non target

(auser_p2 <- ggplot(auser, aes(Date, DIEC, group = 2))+
    geom_line(size = 1)+
    theme_classic())

## pozzo 3 = LT2 target

(auser_p3 <- ggplot(auser, aes(Date, LT2, group = 3))+
    geom_line(size = 1)+
    theme_classic())

## pozzo 4 = PAG non target

(auser_p4 <- ggplot(auser, aes(Date, PAG, group = 4))+
    geom_line(size = 1)+
    theme_classic())

## pozzo 5 = SAL target

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

## all this was to try and find a way to impute missing data

#### imputeTS visualizzo i missing per ogni pozzo ####


(plot_na_cos<-ggplot_na_distribution(auser$CoS,
                       x_axis_labels=auser$Date,
                       title = "Missing Values pozzo CoS")+
  theme_classic())
ggsave("img/auser/13_0Cos_missing.jpg", dpi = 500, width = 10, height=7)


(plot_na_diec<-ggplot_na_distribution(auser$DIEC,
                       x_axis_labels=auser$Date,
                       title = "Missing Values pozzo DIEC")+
  theme_classic())
ggsave("img/auser/13_1diec_missing.jpg", dpi = 500, width = 10, height=7)


(plot_na_lt2<-ggplot_na_distribution(auser$LT2,
                       x_axis_labels=auser$Date,
                       title = "Missing Values pozzo LT2")+
  theme_classic())
ggsave("img/auser/13_2lt2_missing.jpg", dpi = 500, width = 10, height=7)

                       
                       
(plot_na_pag<-ggplot_na_distribution(auser$PAG,
                       x_axis_labels=auser$Date,
                       title = "Missing Values pozzo PAG")+
  theme_classic())
ggsave("img/auser/13_3pag_missing.jpg", dpi = 500, width = 10, height=7)

                       
                       
(plot_na_sal<-ggplot_na_distribution(auser$SAL,
                       x_axis_labels=auser$Date,
                       title = "Missing Values pozzo SAL")+
  theme_classic())
ggsave("img/auser/13_4Sal_missing.jpg", dpi = 500, width = 10, height=7)

(panelled_pozzi_auser <- ggarrange(plot_na_lt2,
                                  plot_na_diec,
                                  plot_na_cos,
                                  plot_na_pag,
                                  plot_na_sal))
panelled_pozzi_auser

ggsave("img/auser/14AUSER_pozzi_missing.jpg",
       dpi = 500, height= 7, width = 10)                      
                       

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
                  new = c("impCoS","impDIEC",
                          "impLT2","impPAG",
                          "impSAL"))


#rm(auser7)

auser7 <- auser6 %>% 
  gather(key="imp", value = "imputed_depth_to_gw.m",28:32) %>%
  mutate(Date = ymd(Date))
ggsave("img/auser/15AUSER_pozzi_NO_missing.jpg",
       dpi = 500, height= 7, width = 10) 
str(auser7)

#imp1=CoS target
#imp2=DIEC 
#imp3=LT2 target
#imp4=PAG
#imp5=SAL taarget
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
#facet_wrap(vars(imp))
# saving
ggsave("img/auser/16box_auser_pozzi.jpg", dpi = 500, width = 10, height=7)
#ggsave("img/auser/16_1box_auser_pozzi.jpg", dpi = 500, width = 10, height=7)

#### checking singularly 

(impCoS_auser <- ggplot(auser8, aes(y = impCoS))+
    geom_boxplot()+
    theme_classic())
ggsave("img/auser/17box_auser_cos.jpg", dpi = 500, width = 10, height=7)

(impDIEC_auser <- ggplot(auser8, aes(y = impDIEC))+
    geom_boxplot()+
    theme_classic())

(impLT2_auser <- ggplot(auser8, aes(y = impLT2))+
    geom_boxplot()+
    theme_classic())
ggsave("img/auser/18box_auser_lt2.jpg", dpi = 500, width = 10, height=7)

(impPAG_auser <- ggplot(auser8, aes(y = impPAG))+
    geom_boxplot()+
    theme_classic())

(impSAL_auser <- ggplot(auser8, aes(y = impSAL))+
    geom_boxplot()+
    theme_classic())
ggsave("img/auser/19box_auser_sal.jpg", dpi = 500, width = 10, height=7)

## extracting vals of potential outliers 

# for pozzo 1 CoS
outCoS_auser <- boxplot.stats(auser8$impCoS)$out # 0 (repeated x4)
outCoS_auser
outCoS_ind_auser <- which(auser8$impCoS %in% c(outCoS_auser))
outCoS_ind_auser # rows where outliers are found

under_bound <- quantile(auser8$impCoS, 0.025)
under_bound # 4.32

under_bound99 <- quantile(auser8$impCoS, 0.01)
under_bound99 # 4.22 

## checking stats to verify it's an outlier
# grubbs test
testCoS <- grubbs.test(auser8$impCoS)
testCoS # 0 is an outlier (at 5% significance level)

## substituting with q1 value 
auser8$impCoS[auser8$impCoS == 0] <- 4.32
summary(auser8$impCoS)

# for pozzo 2 DIEC ok non ho outliers
outDIEC_auser <- boxplot.stats(auser8$impDIEC)$out # non ci sono valore critici
outDIEC_auser

# for pozzo 3 LT2
outLT2_auser <- boxplot.stats(auser8$impLT2)$out # 0 (repeated x2)
outLT2_auser
outLT2_ind_auser <- which(auser8$impLT2 %in% c(outLT2_auser))
outLT2_ind_auser # rows where outliers are found

under_bound <- quantile(auser8$impLT2, 0.025)
under_bound # 11.85

under_bound99 <- quantile(auser8$impLT2, 0.01)
under_bound99 # 11.78 

## checking stats to verify it's an outlier
# grubbs test
testLT2 <- grubbs.test(auser8$impLT2)
testLT2 # 0 is an outlier 

## substituting with q1 value 
auser8$impLT2[auser8$impLT2 == 0] <- 11.85
summary(auser8$impLT2)


# for pozzo 4 PAG
out4_auser <- boxplot.stats(auser8$impPAG)$out # non ci sono valore critici
out4_auser


# for pozzo 5 SAL
outSAL_auser <- boxplot.stats(auser8$impSAL) # 
outSAL_auser
outSAL_ind_auser <- which(auser8$impSAL %in% c(outSAL_auser))
outSAL_ind_auser # 

under_bound_SAL <- quantile(auser8$impSAL, 0.025)
under_bound_SAL # 4.28

# grubbs test 
testSAL <- grubbs.test(auser8$impSAL)
testSAL # 0 it is an outlier, 8% significance

# substituting 
auser8$impSAL[auser8$impSAL == 0] <- 4.28
summary(auser8$impSAL)
#sostituisco solo i valori pari a 0

boxplot.stats(auser8$impSAL)

outSAL <- boxplot.stats(auser8$impSAL)$out
outSAL 
outSAL_ind <- which(auser8$impSAL %in% c(outSAL))
outSAL_ind

dataSAL <- auser8[outSAL_ind,]




####################

auser9 <- auser8 %>% 
  gather(key = "imp", value = "depth_to_gw.m", 28:32)
View(auser9)

(auser_noout <- ggplot(auser9, aes(y=depth_to_gw.m,color = imp))+
    geom_boxplot()+
    theme_classic())

# saving 

ggsave("img/auser/20auser_boxplot.jpg", dpi = 500, width = 10, height = 7)

### keeping outliers for pozzo 5 SAL- 


#### checking outliers for feature variables ####

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
ggsave("img/auser/21auser_temp.jpg", dpi = 500, width = 10, height = 7)
### noto una anomalia sulle temperature di Ponte a Moriano, i valori 
#a zero sono molto ripetuti soprattutto dall'anno 2017 in poi
#dovro' prendere provvedimenti, escudendo questa valriabile dal modello
# pretty similar distribution

# rain
(rain_hist_dog <- ggplot(auser10, aes(rain.mm))+
    geom_histogram()+
    theme_classic()+
    facet_wrap(vars(rain_sensor)))
ggsave("img/auser/22auser_rain.jpg", dpi = 500, width = 10, height = 7)
# pretty similar here as well


## vis boxplot

# temp 
(temp_box_auser <- ggplot(auser10, aes(y=temp.C, color = temp_sensor))+
    geom_boxplot()+
    theme_classic())
ggsave("img/auser/23auser_temp.jpg", dpi = 500, width = 10, height = 7)
# no outliers 

# rain 
(rain_box_auser <- ggplot(auser10, aes(y = rain.mm, color = rain_sensor))+
    geom_boxplot()+
    theme_classic())
ggsave("img/auser/24auser_rain.jpg", dpi = 500, width = 10, height = 7)
# like in histogram, there's a long tail, 
#which the plot recognises as outliers 


### let's check it statistically 

out_tempor_auser <- boxplot.stats(auser9$Temperature_Orentano)
out_tempor_auser

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
ggsave("img/auser/25auser_rain_borgo_mozzano.jpg",
       dpi = 500, width = 10, height=7)
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
ggsave("img/auser/25_1auser_rain_calavorno.jpg",
       dpi = 500, width = 10, height=7)


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
ggsave("img/auser/25_2_auser_rain_crocearcana.jpg",
       dpi = 500, width = 10, height=7)

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
ggsave("img/auser/25_3_auser_rain_f_vallico.jpg",
       dpi = 500, width = 10, height=7)

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
ggsave("img/auser/25_4_auser_rain_gallicano.jpg",
       dpi = 500, width = 10, height=7)

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
ggsave("img/auser/25_6_auser_rain_monteserra.jpg",
       dpi = 500, width = 10, height=7)

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
ggsave("img/auser/25_7_auser_rain_orentano.jpg",
       dpi = 500, width = 10, height=7)

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
ggsave("img/auser/25_8_auser_rain_piaggione.jpg",
       dpi = 500, width = 10, height=7)

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
ggsave("img/auser/25_9_auser_rain_tca.jpg",
       dpi = 500, width = 10, height=7)


(plot_rain<- ggarrange(plot_rainb_out,
                       plot_rainms_auser,
                       plot_rainp_auser,
                       plot_raino_auser,
                       plot_rainms_auser,
                       plot_raing_auser,
                       plot_rainf_auser,
                       plot_rainc_auser))
plot_rain
ggsave("img/auser/6auser_rain_all.jpg",
       dpi = 500, width = 10, height=7)
    
#### decision to leave them as such 


#### checking out for missing - rain and temp ####

# rain
statsNA(auser8$Rainfall_Borgo_a_Mozzano) #no missing
ggplot_na_distribution(auser8$Rainfall_Borgo_a_Mozzano)

statsNA(auser8$Rainfall_Calavorno) #no missing
ggplot_na_distribution(auser8$Rainfall_Calavorno)

statsNA(auser8$Rainfall_Croce_Arcana) #no missing
ggplot_na_distribution(auser8$Rainfall_Croce_Arcana)

statsNA(auser8$Rainfall_Fabbriche_di_Vallico) #no missing
ggplot_na_distribution(auser8$Rainfall_Fabbriche_di_Vallico)

statsNA(auser8$Rainfall_Gallicano) #no missing
ggplot_na_distribution(auser8$Rainfall_Gallicano)

statsNA(auser8$Rainfall_Monte_Serra) # 6 missing
(plot1<-ggplot_na_distribution(auser8$Rainfall_Monte_Serra,
                               x_axis_labels=auser8$Date,
                               title = "Missing Values RF M.Serra"))
ggsave("img/auser/27auser_rain_missing_monte serra.jpg",
       dpi = 500, width = 10, height=7)

statsNA(auser8$Rainfall_Orentano) #no missing
ggplot_na_distribution(auser8$Rainfall_Orentano)

statsNA(auser8$Rainfall_Piaggione) # 6 missing
(plot2<-ggplot_na_distribution(auser8$Rainfall_Piaggione,
                               x_axis_labels=auser8$Date,
                               title = "Missing Values RF Piaggione"))
ggsave("img/auser/28auser_rain_missing_piaggione.jpg",
       dpi = 500, width = 10, height=7)
(rain_missing<-ggarrange(plot1,
                         plot2)
)
ggsave("img/auser/29auser_rain_missing2.jpg",
       dpi = 500, width = 10, height=7)

statsNA(auser8$Rainfall_Tereglio_Coreglia_Antelminelli) # no missing
ggplot_na_distribution(auser8$Rainfall_Tereglio_Coreglia_Antelminelli)


(rain_auser <- ggplot(auser10, aes(Date, rain.mm, color = rain_sensor))+
    geom_line()+
    theme_classic())

#### temp  ####
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
min(auser9$Date[is.na(auser8$Rainfall_Monte_Serra )])
max(auser9$Date[is.na(auser8$Rainfall_Monte_Serra )])
visdat::vis_dat(auser8)

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

rf_monteserra_ms <- list.files(path = "./data/meteoMonteSerra/",
                               pattern = "*.csv$", 
                               full.names = T) %>%
  map_df(~read_plus(.)) 

## manipulating temp data 
rf_monteserra <- rf_monteserra_ms %>% 
  dplyr::rename(date1 = filename) %>% 
  mutate(date1 = gsub("./data/meteoMonteSerra/", "", date1),
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
  dplyr::rename(Date = date_final) %>%
  dplyr::select(Date, prec) %>%
  arrange(Date)

summary(rf_monteserra)

#### adding new data to prev dataset auser ####
auser8$Rainfall_Monte_Serra[is.na(auser8$Rainfall_Monte_Serra)] <- 
  rf_monteserra$prec[match(auser8$Date[is.na(auser8$Rainfall_Monte_Serra)],
                           rf_monteserra$Date)]


####  Rainfall_Piaggione
min(auser8$Date[is.na(auser8$Rainfall_Piaggione )])
max(auser8$Date[is.na(auser8$Rainfall_Piaggione )])
#osservo che i dati mancanti di Rainfal Piaggione riguardano tutto l'anno 2009
#non recuperabile da 3b meteo
#osservando i dati vedo che ho dei missing anche sulle variabili dell'idrometria
#fino a maggio 2011
#decido di tagliare il dataset fino al 01/06/2011

#### fine dei missing rain e temp ####
auser8<- auser8 %>%     
  filter(Date >= "2011-06-01")
visdat::vis_dat(auser8)

auser8<-auser8 %>%
  dplyr::select(-CoS, -DIEC, -LT2, -PAG, -SAL)
visdat::vis_dat(auser8)

#### variabili sull'idrometria missing####
min(auser8$Date[is.na(auser8$Hydrometry_Monte_S_Quirico )])
max(auser8$Date[is.na(auser8$Hydrometry_Monte_S_Quirico )])

min(auser8$Date[is.na(auser8$Hydrometry_Piaggione )])
max(auser8$Date[is.na(auser8$Hydrometry_Piaggione)])


statsNA(auser8$Hydrometry_Monte_S_Quirico) # ffew messing
(plot3<-ggplot_na_distribution(auser8$Hydrometry_Monte_S_Quirico,
                       x_axis_labels=auser8$Date,
                       title = "Missing Values H Monte_S_Quirico"))

statsNA(auser8$Hydrometry_Piaggione) # few missing
(plot4<-ggplot_na_distribution(auser8$Hydrometry_Piaggione,
                               x_axis_labels=auser8$Date,
                               title = "Missing Values H Piaggione"))

(plot_Hydrometry<- ggarrange(plot3, plot3))
ggsave("img/auser/30auser_Hydrometry.jpg",
       dpi = 500, width = 10, height=7)


#######   #########

# interpolazione su auser8 per hydrometria
#### linear interpolation  hydrometria####

auser8$Hydrometry_Piaggione <-as.numeric(na.interp(auser8$Hydrometry_Piaggione))
auser8$Hydrometry_Monte_S_Quirico <-as.numeric(na.interp(auser8$Hydrometry_Monte_S_Quirico))

visdat::vis_dat(auser8)
ggsave("img/auser/31auser_NO_missing.jpg",
       dpi = 500, width = 10, height=7)


####inserisco la variabile stagioni####
##inserisco le stagioni###
auser8 <- auser8 %>%
  mutate(Season = case_when(month(Date) %in% c(3,4,5) ~ "Spring",                      
                            month(Date) %in% c(6,7,8) ~ "Summer",
                            month(Date) %in% c(9,10,11) ~ "Autumn",
                            month(Date) %in% c(1,2,12) ~ "Winter"))
auser8$Season<-factor(auser8$Season, 
                      levels=c("Winter","Spring", "Summer", "Autumn"))

#### funzione season
## funzione - aggiunta divisione categorica (dummy) per stagioni

add.seasons <- function(data) {
  seasons <- data %>% 
    mutate(Date = lubridate::as_date(Date),
           Year = as.factor(lubridate::year(Date)),
           Month = as.factor(lubridate::month(Date)),
           Day = as.factor(lubridate::day(Date)),
           Month_day = format(Date,format = "%m-%d")) %>% 
    mutate(Spring = factor(ifelse(Month_day >= "03-21" & Month_day < "06-21",
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

#### snow ####
str(auser8)
auser_featured <- add.seasons(auser8) %>%
  mutate(snow.yes.MS = as.factor(ifelse(Temperature_Monte_Serra <= 0 & Rainfall_Monte_Serra > 0, 1,0)),
         snow.no.MS = as.factor(ifelse(Temperature_Monte_Serra > 0 & Rainfall_Monte_Serra <= 0,1,0)))  %>% 
  mutate(snow.yes.Or = as.factor(ifelse(Temperature_Orentano <= 0 & Rainfall_Orentano > 0, 1,0)),
         snow.no.Or = as.factor(ifelse(Temperature_Orentano > 0 & Rainfall_Orentano <= 0,1,0))) 
str(auser_featured)


#### lag rain ####

### changing effect of rain on target, and lagging the effect of rain on the target ###
#dalla matrice di correlazione, vedo che le piogge sono fortemente correlate 
#prendo le localita' piu' rappresentative:
#Rainfall_Monte_Serra Rainfall_Croce_Arcana Rainfall_Calavorno 
#Rainfall_Tereglio_Coreglia_Antelminelli

#Rainfall_Monte_Serra + lag
auser_orig_LagMS <- auser8 %>% 
  mutate(lag1 = lag(Rainfall_Monte_Serra, +1),
         lag3 = lag(Rainfall_Monte_Serra,+3),
         lag5 = lag(Rainfall_Monte_Serra,+5),
         lag7 = lag(Rainfall_Monte_Serra,+7)) 

auser_orig_LagMS1 <- auser_orig_LagMS %>% 
  dplyr::select(-Date)

#Rainfall_Croce_Arcana + lag
auser_orig_LagCA <- auser8 %>% 
  mutate(lag1 = lag(Rainfall_Croce_Arcana, +1),
         lag3 = lag(Rainfall_Croce_Arcana,+3),
         lag5 = lag(Rainfall_Croce_Arcana,+5),
         lag7 = lag(Rainfall_Croce_Arcana,+7)) 

auser_orig_LagCA1 <- auser_orig_LagCA %>% 
  dplyr::select(-Date)

#Rainfall_Rainfall_Calavorno + lag
auser_orig_LagCal <- auser8 %>% 
  mutate(lag1 = lag(Rainfall_Calavorno, +1),
         lag3 = lag(Rainfall_Calavorno,+3),
         lag5 = lag(Rainfall_Calavorno,+5),
         lag7 = lag(Rainfall_Calavorno,+7)) 

auser_orig_LagCal1 <- auser_orig_LagCal %>% 
  dplyr::select(-Date)

#Rainfall_Rainfall_Tereglio_Coreglia_Antelminelli + lag
auser_orig_LagTCA <- auser8 %>% 
  mutate(lag1 = lag(Rainfall_Tereglio_Coreglia_Antelminelli, +1),
         lag3 = lag(Rainfall_Tereglio_Coreglia_Antelminelli,+3),
         lag5 = lag(Rainfall_Tereglio_Coreglia_Antelminelli,+5),
         lag7 = lag(Rainfall_Tereglio_Coreglia_Antelminelli,+7)) 

auser_orig_LagTCA1 <- auser_orig_LagTCA %>% 
  dplyr::select(-Date)




## creating 5 new datasets with different min rainfall levels 
## and with new time lags (trying to represent true effect of rain over target)

#Rainfall_Monte_Serra + lag
#auser_orig_LagMS
auser0.5_MS <- auser_orig_LagMS %>% 
  mutate(rain0.5 = ifelse(Rainfall_Monte_Serra <= 0.5, 0, 
                          Rainfall_Monte_Serra),
         lag1 = lag(rain0.5, +1),
         lag3 = lag(rain0.5,+3),
         lag5 = lag(rain0.5,+5),
         lag7 = lag(rain0.5,+7)) %>% 
  dplyr::select(-Rainfall_Monte_Serra)

auser0.5_MS_1 <- auser0.5_MS %>%  dplyr::select(-Date)

auser1.5_MS <- auser_orig_LagMS %>% 
  mutate(rain1.5 = ifelse(Rainfall_Monte_Serra <= 1.5, 0, 
                          Rainfall_Monte_Serra),
         lag1 = lag(rain1.5, +1),
         lag3 = lag(rain1.5, +3),
         lag5 = lag(rain1.5, +5),
         lag7 = lag(rain1.5, +7)
  ) %>% 
  dplyr::select(-Rainfall_Monte_Serra)

auser1.5_MS_1 <- auser1.5_MS %>%   dplyr::select(-Date)

auser3_MS <- auser_orig_LagMS %>% 
  mutate(rain3 = ifelse(Rainfall_Monte_Serra <= 3,0,
                        Rainfall_Monte_Serra),
         lag1 = lag(rain3, +1),
         lag3 = lag(rain3, +3),
         lag5 = lag(rain3, +5),
         lag7 = lag(rain3, +7)
  ) %>% 
  dplyr::select(-Rainfall_Monte_Serra)

auser3_MS_1 <- auser3_MS %>%  dplyr::select(-Date)

auser5_MS <- auser_orig_LagMS %>% 
  mutate(rain5 = ifelse(Rainfall_Monte_Serra <= 5, 0, 
                        Rainfall_Monte_Serra),
         lag1 = lag(rain5, +1),
         lag3 = lag(rain5, +3),
         lag5 = lag(rain5, +5),
         lag7 = lag(rain5, +7)) %>%
  dplyr::select(-Rainfall_Monte_Serra)

auser5_MS_1 <- auser5_MS %>%   dplyr::select(-Date)


## creating 5 new datasets  with different min rainfall levels 
## and with new time lags (trying to represent true effect of rain over target)
#Rainfall_Croce_Arcana + lag
#auser_orig_LagCA
auser0.5_CA <- auser_orig_LagCA %>% 
  mutate(rain0.5 = ifelse(Rainfall_Croce_Arcana <= 0.5, 0, 
                          Rainfall_Croce_Arcana),
         lag1 = lag(rain0.5, +1),
         lag3 = lag(rain0.5,+3),
         lag5 = lag(rain0.5,+5),
         lag7 = lag(rain0.5,+7)
  ) %>% 
  dplyr::select(-Rainfall_Croce_Arcana)

auser0.5_CA_1 <- auser0.5_CA %>%  dplyr::select(-Date)

auser1.5_CA <- auser_orig_LagCA %>% 
  mutate(rain1.5 = ifelse(Rainfall_Croce_Arcana <= 1.5, 0, 
                          Rainfall_Croce_Arcana),
         lag1 = lag(rain1.5, +1),
         lag3 = lag(rain1.5, +3),
         lag5 = lag(rain1.5, +5),
         lag7 = lag(rain1.5, +7)) %>% 
  dplyr::select(-Rainfall_Croce_Arcana)

auser1.5_CA_1 <- auser1.5_CA %>%   dplyr::select(-Date)

auser3_CA <- auser_orig_LagCA %>% 
  mutate(rain3 = ifelse(Rainfall_Croce_Arcana <= 3,0,
                        Rainfall_Croce_Arcana),
         lag1 = lag(rain3, +1),
         lag3 = lag(rain3, +3),
         lag5 = lag(rain3, +5),
         lag7 = lag(rain3, +7)
  ) %>% 
  dplyr::select(-Rainfall_Croce_Arcana)

auser3_CA_1 <- auser3_CA %>%  dplyr::select(-Date)

auser5_CA <- auser_orig_LagCA %>% 
  mutate(rain5 = ifelse(Rainfall_Croce_Arcana <= 5, 0, 
                        Rainfall_Croce_Arcana),
         lag1 = lag(rain5, +1),
         lag3 = lag(rain5, +3),
         lag5 = lag(rain5, +5),
         lag7 = lag(rain5, +7)) %>%
  dplyr::select(-Rainfall_Croce_Arcana)

auser5_CA_1 <- auser5_CA %>%   dplyr::select(-Date)



#Rainfall_Rainfall_Calavorno + lag
#auser_orig_LagCal
auser0.5_Cal <- auser_orig_LagCal %>% 
  mutate(rain0.5 = ifelse(Rainfall_Calavorno <= 0.5, 0, 
                          Rainfall_Calavorno),
         lag1 = lag(rain0.5, +1),
         lag3 = lag(rain0.5,+3),
         lag5 = lag(rain0.5,+5),
         lag7 = lag(rain0.5,+7)
  ) %>% 
  dplyr::select(-Rainfall_Calavorno)

auser0.5_Cal_1 <- auser0.5_Cal %>%  dplyr::select(-Date)

auser1.5_Cal <- auser_orig_LagCal %>% 
  mutate(rain1.5 = ifelse(Rainfall_Calavorno <= 1.5, 0, 
                          Rainfall_Calavorno),
         lag1 = lag(rain1.5, +1),
         lag3 = lag(rain1.5, +3),
         lag5 = lag(rain1.5, +5),
         lag7 = lag(rain1.5, +7)) %>% 
  dplyr::select(-Rainfall_Calavorno)

auser1.5_Cal_1 <- auser1.5_Cal %>%   dplyr::select(-Date)


auser3_Cal <- auser_orig_LagCal %>% 
  mutate(rain3 = ifelse(Rainfall_Calavorno <= 3,0,
                        Rainfall_Calavorno),
         lag1 = lag(rain3, +1),
         lag3 = lag(rain3, +3),
         lag5 = lag(rain3, +5),
         lag7 = lag(rain3, +7)
  ) %>% 
  dplyr::select(-Rainfall_Calavorno)

auser3_Cal_1 <- auser3_Cal %>%  dplyr::select(-Date)

auser5_Cal <- auser_orig_LagCal %>% 
  mutate(rain5 = ifelse(Rainfall_Calavorno <= 5, 0, 
                        Rainfall_Calavorno),
         lag1 = lag(rain5, +1),
         lag3 = lag(rain5, +3),
         lag5 = lag(rain5, +5),
         lag7 = lag(rain5, +7)) %>%
  dplyr::select(-Rainfall_Calavorno)

auser5_Cal_1 <- auser5_Cal %>%   dplyr::select(-Date)

#Rainfall_Rainfall_Tereglio_Coreglia_Antelminelli + lag
#auser_orig_LagTCA
auser0.5_TCA <- auser_orig_LagTCA %>% 
  mutate(rain0.5 = ifelse(Rainfall_Tereglio_Coreglia_Antelminelli <= 0.5, 0, 
                          Rainfall_Tereglio_Coreglia_Antelminelli),
         lag1 = lag(rain0.5, +1),
         lag3 = lag(rain0.5,+3),
         lag5 = lag(rain0.5,+5),
         lag7 = lag(rain0.5,+7)
  ) %>% 
  dplyr::select(-Rainfall_Tereglio_Coreglia_Antelminelli)

auser0.5_TCA_1 <- auser0.5_TCA %>%  dplyr::select(-Date)

auser1.5_TCA <- auser_orig_LagTCA %>% 
  mutate(rain1.5 = ifelse(Rainfall_Tereglio_Coreglia_Antelminelli <= 1.5, 0, 
                          Rainfall_Tereglio_Coreglia_Antelminelli),
         lag1 = lag(rain1.5, +1),
         lag3 = lag(rain1.5, +3),
         lag5 = lag(rain1.5, +5),
         lag7 = lag(rain1.5, +7)) %>% 
  dplyr::select(-Rainfall_Tereglio_Coreglia_Antelminelli)

auser1.5_TCA_1 <- auser1.5_TCA %>%   dplyr::select(-Date)

auser3_TCA <- auser_orig_LagTCA %>% 
  mutate(rain3 = ifelse(Rainfall_Tereglio_Coreglia_Antelminelli <= 3,0,
                        Rainfall_Tereglio_Coreglia_Antelminelli),
         lag1 = lag(rain3, +1),
         lag3 = lag(rain3, +3),
         lag5 = lag(rain3, +5),
         lag7 = lag(rain3, +7)
  ) %>% 
  dplyr::select(-Rainfall_Tereglio_Coreglia_Antelminelli)

auser3_TCA_1 <- auser3_TCA %>%  dplyr::select(-Date)

auser5_TCA <- auser_orig_LagTCA %>% 
  mutate(rain5 = ifelse(Rainfall_Tereglio_Coreglia_Antelminelli <= 5, 0, 
                        Rainfall_Tereglio_Coreglia_Antelminelli),
         lag1 = lag(rain5, +1),
         lag3 = lag(rain5, +3),
         lag5 = lag(rain5, +5),
         lag7 = lag(rain5, +7)) %>%
  dplyr::select(-Rainfall_Tereglio_Coreglia_Antelminelli)

auser5_TCA_1 <- auser5_TCA %>%   dplyr::select(-Date)


















#### write ####
# salvo il mio dataset auser8 ripulito con le stagioni:
write.csv(auser8,"processed_data/AUSER_to_model.csv")
#str(auser8)
#visdat::vis_dat(auser8)
#summary(auser8)

#### visualizzo andamento delle variabili target ####
auser8 %>%
  dplyr::select(Date, impSAL, impCoS, impLT2) %>%
  melt(., id.vars = "Date") %>%
  ggplot(., aes(Date, value))+
  facet_wrap(variable~., ncol = 1, scales = "free_y")+
  geom_line(size = 1.5, alpha = 0.8, col = "gray65")+
  geom_smooth(method = "loess", color = "firebrick3", size = 1.2, formula = y ~ x, fill = "firebrick4", alpha = 0.32)+
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", limits = as.Date(c("2011-06-01", "2020-06-30")))+
  labs(x = "Date", y = "Value", title = "The distribution of the target variables along with the loess curve",
       subtitle = "in aquifer Auser from 2011") + 
  theme_classic()
ggsave("img/auser/32auser_target_no_missing.jpg",
       dpi = 500, width = 10, height=7)


####
#### Correlation Matrix ####
df <- auser8
df$Date <- NULL
ggcorr(df, label = TRUE, label_round = 2, hjust = 1, size = 4, layout.exp = 4, label_size = 3)
ggsave("img/auser/33auser_correlazione.jpg",
       dpi = 500, width = 10, height=7)
rm(df)


#### temperature ####
auser8 %>%
  dplyr::select(Date, Temperature_Orentano, Temperature_Monte_Serra, Temperature_Ponte_a_Moriano, Temperature_Lucca_Orto_Botanico) %>%
  melt(., id.vars = "Date") %>%
  ggplot(., aes(Date, value, col = variable))+
  geom_line(size = 0.6, alpha = 1)+
  scale_color_viridis_d(option = "inferno", begin = 0.15, end = 0.85, name = "")+
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", limits = as.Date(c("2011-06-01", "2020-06-30")))+
  labs(x = "Date", y = "Temperature C", title = "Temperature depending on the region",
       subtitle = "explanatory variables on aquifer Auser from 2011") + 
  theme_classic()+
  theme(legend.position = "bottom", legend.direction = "vertical")
ggsave("img/auser/34auser_temp.jpg",
       dpi = 500, width = 10, height=7)
## si notano le temperature di Monte  a Moriano a zero da giugno 2017
# si presume un difetto nel sensore


#### Volume ####

auser8 %>%
  dplyr::select(Date, Volume_POL, Volume_CC1, Volume_CC2, Volume_CSA, Volume_CSAL) %>%
  melt(., id.vars = "Date") %>%
  ggplot(., aes(Date, value, col = variable))+
  facet_wrap(variable~., ncol = 2)+
  geom_line(size = 0.7, alpha = 1)+
  scale_color_viridis_d(option = "inferno", begin = 0.45, end = 0.45, name = "")+
  scale_y_continuous(trans = "pseudo_log", breaks = c(0,-100, -10000, -1000000, - 20000000))+
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", limits = as.Date(c("2011-06-01", "2020-06-30")))+
  labs(x = "Date", y = "The amount of water", title = "The amount of water depending on the station",
       subtitle = "explanatory variables on aquifer Auser from 06-2011") + 
  theme_21+
  theme(legend.position = "none")
ggsave("img/auser/35auser_volumi.jpg",
       dpi = 500, width = 10, height=7)


#commento:I valori di queste variabili 
#sono negativi in quanto riguardano la quantità di acqua 
#prelevata dal luogo indicato.
# si puo' pensare di di escludere le variabili che descrivono i 
#serbatoi CC1 e CSA per la loro correlazione con altre variabili, e di scegliere le variabili CC2 e CSAL per la maggiore varianza


#### hydrometry ####

auser8 %>%
  dplyr:: select(Date, Hydrometry_Monte_S_Quirico, Hydrometry_Piaggione) %>%
  melt(., id.vars = "Date") %>%
  ggplot(., aes(Date, value, col = variable))+
  facet_wrap(variable~., ncol = 1)+
  geom_line(size = 0.6, alpha = 1)+
  scale_color_viridis_d(option = "inferno", begin = 0.55, end = 0.55, name = "")+
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", limits = as.Date(c("2011-06-01", "2020-06-30")))+
  labs(x = "Date", y = "Groundwater level meters", title = "Groundwater level depending on the station",
       subtitle = "exp variables on aquifer Auser from 06-2011") + 
  theme_21+
  theme(legend.position = "none")
ggsave("img/auser/36auser_H.jpg",
       dpi = 500, width = 10, height=7)
##commento: osservo piaggione 
#nel modello si puo' pensare di rimuovere questa variabile anche perche'
#molto correlata con altre, guardano la matrice di correlazione





####The distribution of the target variables by season####

auser_featured %>%
  dplyr::select(Season, SAL, CoS, LT2) %>%
  melt(., id.vars = "Season") %>%
  ggplot(., aes(Season, value))+
  facet_wrap(variable~., ncol = 1, scales = "free_y")+
  geom_boxplot(outlier.size = 1.1, outlier.shape = 20, lwd = 0.5, fatten = 1.1, 
               alpha = 0.90, width = 0.70, col = "gray10", fill = "#f8fc9d")+
  scale_x_discrete(limit = c("Spring", "Summer", "Autumn", "Winter"))+
  labs(x = "Season", y = "Value", title = "The distribution of the target variables by season",
       subtitle = "in aq Auser") + 
  theme_classic()
#commento
# per LT2 non ci sono differenze per stagioni
# per CoS e per Sal i pozzi sono piu' profondi in estate e autunno
# ricordo che abbiamo preso il valore assoluto (i valori sono negativi)














################################################################################
############################# Fine #############################################