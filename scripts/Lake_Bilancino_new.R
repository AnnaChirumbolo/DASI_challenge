#### Bilancino Lake ####
# analisi dati 

####inizializzo i dati ####

getwd()
rm(list = ls(all=TRUE)) 

#### carico le librerie ####
library(reshape)
library(stats)
install.packages("factoextra")
install.packages("tibble")
library(class)
library(jpeg)
library(png)
library(tibble)
library("corrplot")
library("readxl")
library("factoextra")
library("plotly")
library("FactoMineR")
library("gplots")
library("ggplot2")
library("graphics")
library("ggpubr")
library("graphics")
library("ggdendro")
library("stats")
library("maps")
library("NbClust")
library(factoextra)
library("fpc")
library("readr")
library("plotly")
library("dbscan")
library("dplyr")
library("cluster")
library("clValid")
library(tidyverse)
library(ggplot2)
library(GGally)
library(dplyr)
library(tidyverse)
library(naniar)
library(visdat)
library(forecast)
library(xts)
library(caTools)

library(tidyverse)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(reshape2)
library(lubridate)
library(viridis)
library(ggrepel)
library(corrplot)
library(caret)
library(boot)
library(class)
library(adabag)
library(randomForestSRC)
library(scales)
library(Metrics)
library(mgcv)

library(varhandle)
library(rsample)
library(gbm)

##### imposto il theme ####
theme_21 <- theme(legend.position = "bottom", legend.direction = "horizontal", axis.text = element_text(size = 14), 
                     plot.caption = element_text(color = "gray25", face = "bold", size = 8), legend.text = element_text(size = 15), 
                     axis.title = element_text(size = 14.5, face = "bold", color = "gray25"), legend.title = element_text(size = 14), axis.line = element_line(size = 0.4), 
                     plot.title = element_text(size = 19), plot.subtitle = element_text(size = 14.5), strip.text = element_text(size = 14, face = "bold"))

core_col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))



##### carico il dataset ####
Lake_Bilancino<-read.csv("./data/Lake_Bilancino.csv")
#Trasformo la colonn data in data
Lake_Bilancino$Date<-as.Date(Lake_Bilancino$Date, format = "%d/%m/%Y")

str(Lake_Bilancino) # visiono il contenuto
names(Lake_Bilancino) # guardo il nome delle variabili e della variabile trget
summary(Lake_Bilancino) # per avere una visione del dataframe e dei valori nullu

#### controllo i missing ####
vis_dat <- Lake_Bilancino
colnames(vis_dat) <- gsub("_"," ",colnames(vis_dat))
visdat::vis_dat(vis_dat)
  


ggsave("img/bilancino/01Bilancino_Inizio.jpg", dpi = 500, width = 10, height=7)
#01Bilancino_Inizio.jpg



# noto i valori  mancanti delle variabili dall'inizio del dataset fino alla fine del 2003
max(Lake_Bilancino$Date[is.na(Lake_Bilancino$Rainfall_S_Piero)])
#2003-12-31"

#### elimino i missing antecedenti al 2004
cutdate <- as.Date("2004-01-01")
Lake_Bilancino_cut <- Lake_Bilancino[(Lake_Bilancino$Date > cutdate), ]
summary(Lake_Bilancino_cut)
str(Lake_Bilancino_cut)

dim(Lake_Bilancino_cut)
dim(na.omit(Lake_Bilancino_cut)) #controllo di aver eliminato tutti i null
df<-Lake_Bilancino_cut

colnames(df) <- gsub("_"," ",colnames(df))
visdat::vis_dat(df) #missing eliminati
ggsave("img/bilancino/02Bilancino_cut_no_missing.jpg",
       dpi = 500, width = 10, height=7)
rm(df)
       
#### aggiungo la variabile season ####
# raggruppo i mesi in stagioni utile per studiare la stagionalita'
Lake_Bilancino_cut <- Lake_Bilancino_cut  %>%
 mutate(Season = case_when(month(Date) %in% c(3,4,5) ~ "Spring",                      
                           month(Date) %in% c(6,7,8) ~ "Summer",
                           month(Date) %in% c(9,10,11) ~ "Autumn",
                           month(Date) %in% c(1,2,12) ~ "Winter"))
Lake_Bilancino_cut$Season<-factor(Lake_Bilancino_cut$Season,
                                  levels=c("Winter","Spring", "Summer", "Autumn"))


## funzione - aggiunta divisione categorica (dummy) per stagioni

add.seasons <- function(data) {
  seasons <- data %>% 
    mutate(
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


#### guardo le variabili target ####
## Le mie variabili TARGET sono: Lake_Level, Flow_Rate
##Sembra che ci siano alcuni picchi Flow_Rate a volte a gennaio, a volte in primavera,
#non tutti gli anni. 
#### variabile target Flow_Rate ####
df <- Lake_Bilancino_cut %>% dplyr::select(Date, 
                                Flow_Rate) %>%
  pivot_longer(., cols = c(Flow_Rate),
               names_to = "Var", values_to = "Val", values_drop_na = FALSE)
df <- df[complete.cases(df), ]
ggplot(df, aes(x = Date, y = Val)) +
  geom_line(color="salmon") +
  ggtitle("Lake Bilancino: Flow Rate in L/s\n") +
  ylab("Flow Rate") +
  theme_classic()+
  xlab("")
ggsave("img/bilancino/03Bilancino_Flow_Rate.jpg",
       dpi = 500, width = 10, height=7)
rm(df)

mean(Lake_Bilancino_cut$Flow_Rate)# media flusso d'acqua molto bassa 2,78 l/s
min(Lake_Bilancino_cut$Flow_Rate)# 0,45 l/s minimo flusso
max(Lake_Bilancino_cut$Flow_Rate)# 74,65 l/s punta di flusso massima nel 2014
#essendo un lago artificiale, possiamo pensare che il dato sia stato pilotato, il flusso d'acqua
#serve a
#compensare il picco verso il basso, la scarista' d'acqua, avuta nel 2012/2013
#day_max_flow<-Lake_Bilancino_cut$Date[max(Lake_Bilancino_cut$Flow_Rate)]
#day_max_flow #"2014-02-11" giorno di picco con 74,65 l/s punta di flusso massima



#### variabile target Lake_Level ####
#Il comportamento Lake Level è pressoché ricorrente 
#(salvo alcuni anni) con un livello costante e 
#con una diminuzione verso novembre. Un picco verso il basso c'e' stato nel
#2012 / 2013
#ma e' risalito velocemente e si e' stabilizzato poco piu' in basso del livello precedente.
#Il Lake_level ha una variabilità molto bassa, 
#in quanto il livello dell'acqua è ancora compreso tra 243 e 253 m
#nel corso di oltre 16 anni

df <- Lake_Bilancino_cut %>% dplyr::select(Date, 
                                Lake_Level) %>%
  pivot_longer(., cols = c(Lake_Level),
               names_to = "Var", values_to = "Val", values_drop_na = FALSE)
df <- df[complete.cases(df), ]
summary(df)
ggplot(df, aes(x = Date, y = Val)) +
  geom_line(color="salmon") +
  ggtitle("Lake Bilancino: Lake Level in meters\n") +
  theme_classic()+
  ylab("Lake Level") +
  xlab("")
ggsave("img/bilancino/04Bilancino_Lake_level.jpg",
       dpi = 500, width = 10, height=7)
rm(df)

mean(Lake_Bilancino_cut$Lake_Level)# 249,55 m media livello d'acqua del lago
min(Lake_Bilancino_cut$Lake_Level)# 243,53 m minimo livello d'acqua del lago
max(Lake_Bilancino_cut$Lake_Level)# 252,76 m massimo livello d'acqua del lago
#day_max_level<-Lake_Bilancino_cut$Date[max(Lake_Bilancino_cut$Lake_Level)]
#day_max_level




#### variabili target comparate ####
#Il livello di flusso d'acqua è solitamente molto basso - normalmente fino a 20 m3/s, 
#ma a volte aumenta bruscamente fino a 70 m3/s nel 2014, 
#quindi questo cambiamento è caratterizzato da una variazione maggiore 
#rispetto al livello dell'acqua.
#Il livello dell'acqua normalmente e' abbastanza stabile, con variabiita' bassa, In 16 anni ha
#avuto poca varianza, valore compreso tra compreso tra 243 e 253 m.
#Quindi abbiamo un livello abbastanza costante e 
#con una diminuzione verso novembre. Un picco verso il basso c'e' stato nel
#2012/2013
#poi l'acqua si e' velocemente ripristinata, posizionandosi ad un livello
#leggermente inferiore al livello precedente.

###distribuzione comparata
df<-Lake_Bilancino_cut
colnames(df) <- gsub("_"," ",colnames(df))
df %>%
  dplyr::select(Date, "Lake Level", "Flow Rate") %>%
  melt(., id.vars = "Date") %>%
  ggplot(., aes(Date, value))+
  facet_wrap(variable~., ncol = 1, scales = "free_y")+
  geom_line(size = 1.2, alpha = 0.8, col = "gray65")+
  geom_smooth(method = "loess", color = "firebrick3", size = 1.0, formula = y ~ x, fill = "firebrick4", alpha = 0.32)+
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", limits = as.Date(c("2004-01-01", "2020-06-30")))+
  labs(x = "Date", y = "L/s                                        m", 
       title = "Distribution of the target variables (along with the loess curve)",
       subtitle = "lake Bilancio from 01-2004") + 
  theme_classic()
ggsave("img/bilancino/05Bilancino_target.jpg",
       dpi = 500, width = 10, height=7)

#### Flow_rate e Lake_Level in relazione alle stagioni####
bilancino_season <- add.seasons(Lake_Bilancino_cut)


df %>%
  dplyr::select(Season, "Lake Level",  "Flow Rate") %>%
  melt(., id.vars = "Season") %>%
  ggplot(., aes(Season, value))+
  facet_wrap(variable~., ncol = 1, scales = "free_y")+
  geom_boxplot(outlier.size = 1.1, outlier.shape = 20, lwd = 0.5, fatten = 1.1, 
               alpha = 0.95, width = 0.75, col = "gray10", fill = "#f8fc9d")+
  scale_x_discrete(limit = c("Spring", "Summer", "Autumn", "Winter"))+
  labs(x = "Season", y = "Value", title = "Distribution of the explained variables by season",
       subtitle = "in lake Bilancino\n") + 
  theme_classic()
ggsave("img/bilancino/06Bilancino_target_season.jpg",
       dpi = 500, width = 10, height=7)

#ci sono differenze tra le stagioni e le variabili dipendenti.
#Il livello del lago è più basso in autunno,
#mentre e' più alto in primavera. 
#In estate e in inverno la mediana è di circa 250 metri. 
#Per il Flow_rate, le differenze non molte. 
#Gli outlayers si trovano principalmente in primavera e in inverno.

#### outliers singolarmente####

(LL_bilancino <- ggplot(Lake_Bilancino_cut, aes(y = Lake_Level))+
   geom_boxplot()+
   theme_classic())
ggsave("img/bilancino/07box_lakelevel.jpg", dpi = 500, width = 10, height=7)

(FR_bilancino <- ggplot(Lake_Bilancino_cut, aes(y = Flow_Rate))+
    geom_boxplot()+
    theme_classic())
ggsave("img/bilancino/08box_flowrate.jpg", dpi = 500, width = 10, height=7)

plot1<-ggarrange(LL_bilancino, FR_bilancino)
plot1
ggsave("img/bilancino/09box_target.jpg", dpi = 500, width = 10, height=7)

## boxplots




#### Rainfall analysis ####

#### PRECIPITAIZIONI comparazione tra localita'####
#Abbiamo precipitazioni da cinque diverse regioni e 
#la loro quantità, varia da 0 a 125 mm. Gli aumenti rapidi si applicano
#a tutte le variabili e I FENOMENI SONO FORTEMENTE CORRELATI. 
# DECIDO di lasciare 2 variabili, per il processo di modellazione 
#scelgo la prima coppia delle regioni "S Piero" e "Mangona", 
#perché questa coppia è la meno correlata (Vedi correlazione piu' avanti) e quindi ci fornisce maggiori informazioni

df %>%
  dplyr::select(Date, "Rainfall S Piero", "Rainfall Mangona", "Rainfall S Agata", 
  "Rainfall Cavallina", "Rainfall Le Croci") %>%
  melt(., id.vars = "Date") %>%
  ggplot(., aes(Date, value, col = variable))+
  facet_wrap(variable~., ncol = 2)+
  geom_line(size = 0.4, alpha = 1)+
  scale_color_viridis_d(option = "inferno", begin = 0.15, end = 0.15)+
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", limits = as.Date(c("2004-01-01", "2020-06-30")))+
  labs(x = "", y = "Quantity of rain falling in mm", title = "Rain falling depending on the region",
       subtitle = "Rainfalls variabilies lake Bilancino from 01-2004") + 
  theme_classic()+
  theme(legend.position = "none")
ggsave("img/bilancino/10Bilancino_rain.jpg",
       dpi = 500, width = 10, height=7)



#### Temperature analysis ####
mean(Lake_Bilancino_cut$Temperature_Le_Croci)
### Temperature: the temperature mean is 14.53 °C
#La temperatura alla localita' Le_Croci e' stagionale, tipica di una regione
#del centro Italia.
#La temperatura e' quindi una delle variabili, dipendente, continua.
#Nella  maggioranza dei casi è compresa tra 0 e 30 gradi C, 
#con stagionalità visibile e connessione con le stagioni. 
#Essendo l'unica variabile sulla temperatura, 
#la utilizzo sicuramente per creare il modello
# Temperature analysis
df <- df %>% dplyr::select(Date, "Temperature Le Croci" ) %>%
  pivot_longer(., cols = c("Temperature Le Croci" ),
               names_to = "Var", values_to = "Val")
df <- df[complete.cases(df), ]
ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() + ggtitle("Temperature °C -  Lake Bilancino") +
  ylab("Temperature °C") + xlab("Date")
rm(df)
#stesso grafico riplottato
df<-Lake_Bilancino_cut
colnames(df) <- gsub("_"," ",colnames(df))
df %>%
  dplyr::select(Date, "Temperature Le Croci") %>%
  melt(., id.vars = "Date") %>%
  ggplot(., aes(Date, value, col = variable))+
  facet_wrap(variable~., ncol = 1)+
  geom_line(size = 0.6, alpha = 1)+
  scale_color_viridis_d(option = "inferno", begin = 0.85, end = 0.85, name = "")+
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", limits = as.Date(c("2004-01-01", "2020-06-30")))+
  labs(x = "", y = "Temperature in C", title = "Temperature region Le Croci",
       subtitle = "explanatory variables on lake Bilancino from 01-2004") + 
  theme_classic()+
  theme(legend.position = "none")
ggsave("img/bilancino/12Bilancino_temp.jpg",
       dpi = 500, width = 10, height=7)
#rm(df)
#posso pensare di analizzare i casi sotto allo zero, per vedere se sono collegati
#a precipitazioni nevose

# salvo il mio dataset Lake_Bilancino_cut ripulito con le stagioni:
write.csv(Lake_Bilancino_cut,"processed_data/BILANCINO_to_model.csv")


#### correlazione Correlation Matrix ####
#df <- Lake_Bilancino_cut
df$Date <- NULL
df$Season<-NULL
ggcorr(df, label = TRUE, 
       label_round = 2, hjust = 1, size = 4, layout.exp = 4, 
       label_size = 3)
ggsave("img/bilancino/11Bilancino_correlazione.jpg",
       dpi = 500, width = 10, height=7)
rm(df)

####correlazione metodo di spearman####
#si può notare che le variabili riguardanti le precipitazioni 
#sono fortemente correlate positivamente tra loro, tra 0,8 e 0,9 per queste variabili, 
#quindi si potra' fare una riduzione. Le restanti relazioni sono molto 
#basse  
#quindi la rimozione delle variabili riguarderà solo le precipitazioni.

Lake_Bilancino_cut %>% 
  select(!c("Date", "Season")) %>%
  cor(., method = "spearman", use = "complete.obs") %>%
  corrplot(., method = "color", type = "upper", col = core_col(100), number.cex = 1, addCoef.col = "gray8",
           tl.col = "black",tl.srt = 35, diag = T, tl.cex = 0.85)


####Correlazione Livello del Lago - Precipitazioni: ####
#esiste una correlazione -0,03% tra la media delle precipitazioni
#e il livello del Lago. Ma notiamo che quando 
#i livelli di pioggia sono alti il livello del lago è basso e viceversa. 
#Il Lake level scende soprattutto nell'ultima parte dell'anno ma risale a 
#dopo gennaio, dopo le piogge; possiamo considerare 5 mesi di ritardo 
#per ripristinare il livello dell'acqua
# correlation Lake_level - Rainfall (compare plots 1)

df <- Lake_Bilancino_cut
df$Rainfall_mean <- rowMeans(df[,c("Rainfall_S_Piero","Rainfall_Mangona",
                                                                   "Rainfall_S_Agata","Rainfall_Cavallina",
                                                                   "Rainfall_Le_Croci" )])


df$Rainfall_mean <- runmean(df$Rainfall_mean,30)
df$Lake_Level <- df$Lake_Level/20
colnames(df) <- gsub("_"," ",colnames(df))
df <- df %>% dplyr::select(Date, "Lake Level", "Rainfall mean") %>%
  pivot_longer(., cols = c("Lake Level", "Rainfall mean" ),
               names_to = "Var", values_to = "Val", values_drop_na = FALSE)
(plot4<-ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() + ggtitle("Lake Level / Rainfall - Lake Bilancino") +
  ylab("Lake Level - Rainfall") +   xlab(""))

ggsave("img/bilancino/13Bilancino_LL_rainfall.jpg",
       dpi = 500, width = 10, height=7)
rm(df)

# correlation Lake_level - Rainfall (compare plots 2) dal 2016 
#guardo gli ultimi anni

cutdate <- as.Date("2016-01-01")
df <- Lake_Bilancino_cut
df <- df[(df$Date > cutdate), ]
df$Rainfall_mean <- runmean(df$Rainfall_mean,30)/10
df$Lake_Level <- (df$Lake_Level/20)-12
df <- df %>% dplyr::select(Date, Lake_Level, Rainfall_mean) %>%
  pivot_longer(., cols = c(Lake_Level, Rainfall_mean ),
               names_to = "Var", values_to = "Val", values_drop_na = FALSE)
ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() + ggtitle("Lake Level - Rainfall - Lake Bilancino from 2016") +
  ylab("Lake Level - Rainfall") +   xlab("Date")
ggsave("img/bilancino/13_0Bilancino_rainfall_LL.jpg",
       dpi = 500, width = 10, height=7)


####Correlazione Flow_Rate - Rainfall: ####
#esiste una correlazione dello 0,17% tra 
#la media delle precipitazioni e il livello del lago
# correlation Flow_Rate - Rainfall (compare plots)
df <- Lake_Bilancino_cut
df$Rainfall_mean <- rowMeans(df[,c("Rainfall_S_Piero","Rainfall_Mangona",
                                   "Rainfall_S_Agata","Rainfall_Cavallina",
                                   "Rainfall_Le_Croci" )])

df$Rainfall_mean <- runmean(df$Rainfall_mean,30)
df$Flow_Rate <- df$Flow_Rate/10
df <- df %>% dplyr::select(Date, Flow_Rate, Rainfall_mean) %>%
  pivot_longer(., cols = c(Flow_Rate, Rainfall_mean ),
               names_to = "Var", values_to = "Val", values_drop_na = FALSE)
(plot5<-ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() + ggtitle("Flow Rate - Rainfall - Lake Bilancino") +
  ylab("Flow Rate - Rainfall") +   xlab("Date"))
ggsave("img/bilancino/14Bilancino_rainfall_FR.jpg",
       dpi = 500, width = 10, height=7)

plot6<-ggarrange (plot4, plot5)
plot6
ggsave("img/bilancino/15Bilancino_rainfall_FR_LL.jpg",
       dpi = 500, width = 10, height=7)


# correlation Flow_Rate - Rainfall (ggpair)
df <- Lake_Bilancino_cut
df$Rainfall_mean <- rowMeans(df[,c("Rainfall_S_Piero","Rainfall_Mangona",
                                   "Rainfall_S_Agata","Rainfall_Cavallina",
                                   "Rainfall_Le_Croci" )])

df$Rainfall_mean <- runmean(df$Rainfall_mean,30)
Pairvar <- c("Rainfall_mean", "Flow_Rate")
Pairdf <- df[Pairvar]
ggpairs(Pairdf,lower = list(continuous = wrap("smooth",color = "Green")), title = "Correlation Matrix")
rm(df)



#### snow ####
#Temperature sotto lo zero / eventuale pioggia/neve, da analizzare


#rm(bilancino_featured)
bilancino_featured <- add.seasons(Lake_Bilancino_cut) %>%
  mutate(snow.yes = as.factor(ifelse(Temperature_Le_Croci <=0 & Rainfall_Le_Croci > 0, 1,0)),
         snow.no = as.factor(ifelse(Temperature_Le_Croci > 0 & Rainfall_Le_Croci <= 0,1,0))) 
str(bilancino_featured)


### rain le croci ###

bilancino_months <- bilancino_featured %>% 
  mutate(lag1 = lag(Rainfall_Le_Croci, +1),
         lag3 = lag(Rainfall_Le_Croci,+3),
         lag5 = lag(Rainfall_Le_Croci,+5),
         lag7 = lag(Rainfall_Le_Croci,+7),
         )
str(bilancino_months)

write.csv(bilancino_months, "processed_data/BILANCINO_to_model+lags.csv")


####
### checking for rainfall ###
## changing mm levels 


## Rainfall_Le_Croci 5 datasets with 5 levels of min rain changed to 0:

bilancino_rain0_Le_Croci <- bilancino_months %>% 
  mutate(rain1 = ifelse(Rainfall_Le_Croci <= 0.5, 0, Rainfall_Le_Croci)) 

bilancino_rain1_Le_Croci <- bilancino_months %>%  # whenever rain is lower than 1mm/day, = 0
  mutate(rain2 = ifelse(Rainfall_Le_Croci <= 1.5, 0, Rainfall_Le_Croci))

bilancino_rain3_Le_Croci <- bilancino_months %>% 
  mutate(rain3 = ifelse(Rainfall_Le_Croci <= 3,0,Rainfall_Le_Croci))

bilancino_rain5_Le_Croci <- bilancino_months %>% 
  mutate(rain4 = ifelse(Rainfall_Le_Croci <= 5, 0, Rainfall_Le_Croci))

## creating 5 new datasets per dataset...
## ... or 5 new variables 

bilancino_rain0_Le_Croci.lag <- bilancino_rain0_Le_Croci %>% 
  dplyr::mutate(lag1 = lag(rain1, +1),
         lag3 = lag(rain1,+3),
         lag5 = lag(rain1,+5),
         lag7 = lag(rain1,+7)) 

bilancino_rain1_Le_Croci.lag <- bilancino_rain1_Le_Croci %>% 
  mutate(lag1 = lag(rain2, +1),
         lag3 = lag(rain2, +3),
         lag5 = lag(rain2, +5),
         lag7 = lag(rain2, +7))

bilancino_rain3_Le_Croci.lag <- bilancino_rain3_Le_Croci %>% 
  dplyr::mutate(lag1 = lag(rain3, +1),
         lag3 = lag(rain3, +3),
         lag5 = lag(rain3, +5),
         lag7 = lag(rain3, +7)) 

bilancino_rain5_Le_Croci.lag <- bilancino_rain5_Le_Croci %>% 
  dplyr::mutate(lag1 = lag(rain4, +1),
         lag3 = lag(rain4, +3),
         lag5 = lag(rain4, +5),
         lag7 = lag(rain4, +7)) 

### dalla matrice di correlazione(piu sotto)
#vedo le variabile rainfall fortemente correlate,
#ne tengo un paio: Rainfall_S_Piero, Rainfall_Mangona (le piu' rappressentative)
# creo i dataset per i lag

## Rainfall_S_Piero 5 datasets with 5 levels of min rain changed to 0:

bilancino_rain0_S_Piero <- bilancino_months %>% 
  mutate(rain1 = ifelse(Rainfall_S_Piero <= 0.5, 0, Rainfall_S_Piero),
         seq.rain.val = sequence(rle(as.character(rain1))$lengths)) 

bilancino_rain1_S_Piero <- bilancino_months %>%  # whenever rain is lower than 1mm/day, = 0
  mutate(rain2 = ifelse(Rainfall_S_Piero <= 1.5, 0, Rainfall_S_Piero),
         seq.rain.val = sequence(rle(as.character(rain2))$lengths))

bilancino_rain3_S_Piero <- bilancino_months %>% 
  mutate(rain3 = ifelse(Rainfall_S_Piero <= 3,0,Rainfall_S_Piero),
         seq.rain.val = sequence(rle(as.character(rain3))$lengths))

bilancino_rain5_S_Piero <- bilancino_months %>% 
  mutate(rain4 = ifelse(Rainfall_S_Piero <= 5, 0, Rainfall_S_Piero),
         seq.rain.val = sequence(rle(as.character(rain4))$lengths))

## creating 5 new datasets per dataset...S_Piero
## ... or 5 new variables 

bilancino_rain0_S_Piero.lag <- bilancino_rain0_S_Piero %>% 
  dplyr::mutate(lag1 = lag(rain1, +1),
                lag3 = lag(rain1,+3),
                lag5 = lag(rain1,+5),
                lag7 = lag(rain1,+7)) %>%
  write.csv(., "processed_data/bilancino_rain0_S_Piero+lag.csv")

bilancino_rain1_S_Piero.lag <- bilancino_rain1_S_Piero %>% 
  mutate(lag1 = lag(rain2, +1),
         lag3 = lag(rain2, +3),
         lag5 = lag(rain2, +5),
         lag7 = lag(rain2, +7))%>% 
  write.csv(., "processed_data/bilancino_rain1_S_Piero+lag.csv")

bilancino_rain3_S_Piero.lag <- bilancino_rain3_S_Piero %>% 
  dplyr::mutate(lag1 = lag(rain3, +1),
                lag3 = lag(rain3, +3),
                lag5 = lag(rain3, +5),
                lag7 = lag(rain3, +7)) %>% 
  write.csv(., "processed_data/bilancino_rain1_S_Piero+lag.csv")

bilancino_rain5_S_Piero.lag <- bilancino_rain5_S_Piero %>% 
  dplyr::mutate(lag1 = lag(rain4, +1),
                lag3 = lag(rain4, +3),
                lag5 = lag(rain4, +5),
                lag7 = lag(rain4, +7)) %>% 
  write.csv(., "processed_data/bilancino_rain5_S_Piero+lag.csv")

## Rainfall_Mangona  5 datasets with 5 levels of min rain changed to 0:

bilancino_rain0_Mangona  <- bilancino_months %>% 
  mutate(rain1 = ifelse(Rainfall_Mangona  <= 0.5, 0, Rainfall_Mangona ),
         seq.rain.val = sequence(rle(as.character(rain1))$lengths)) 

bilancino_rain1_Mangona  <- bilancino_months %>%  # whenever rain is lower than 1mm/day, = 0
  mutate(rain2 = ifelse(Rainfall_Mangona  <= 1.5, 0, Rainfall_Mangona ),
         seq.rain.val = sequence(rle(as.character(rain2))$lengths))

bilancino_rain3_Mangona  <- bilancino_months %>% 
  mutate(rain3 = ifelse(Rainfall_Mangona  <= 3,0,Rainfall_Mangona ),
         seq.rain.val = sequence(rle(as.character(rain3))$lengths))

bilancino_rain5_Mangona <- bilancino_months %>% 
  mutate(rain4 = ifelse(Rainfall_Mangona  <= 5, 0, Rainfall_Mangona ),
         seq.rain.val = sequence(rle(as.character(rain4))$lengths))

## creating 5 new datasets per dataset...Mangona 
## ... or 5 new variables 

bilancino_rain0_Mangona.lag <- bilancino_rain0_Mangona  %>% 
  dplyr::mutate(lag1 = lag(rain1, +1),
                lag3 = lag(rain1,+3),
                lag5 = lag(rain1,+5),
                lag7 = lag(rain1,+7)) %>%
  write.csv(., "processed_data/bilancino_rain0_Mangona+lag.csv")

bilancino_rain1_Mangona.lag <- bilancino_rain1_Mangona %>% 
  mutate(lag1 = lag(rain2, +1),
         lag3 = lag(rain2, +3),
         lag5 = lag(rain2, +5),
         lag7 = lag(rain2, +7))%>% 
  write.csv(., "processed_data/bilancino_rain1_Mangona+lag.csv")

bilancino_rain3_Mangona.lag <- bilancino_rain3_Mangona %>% 
  dplyr::mutate(lag1 = lag(rain3, +1),
                lag3 = lag(rain3, +3),
                lag5 = lag(rain3, +5),
                lag7 = lag(rain3, +7)) %>% 
  write.csv(., "processed_data/bilancino_rain1_Mangona+lag.csv")

bilancino_rain5_Mangona.lag <- bilancino_rain5_Mangona %>% 
  dplyr::mutate(lag1 = lag(rain4, +1),
                lag3 = lag(rain4, +3),
                lag5 = lag(rain4, +5),
                lag7 = lag(rain4, +7)) %>% 
  write.csv(., "processed_data/bilancino_rain5_Mangona+lag.csv")


#### Random Forest ####
Lake_Bilancino_cut <- Lake_Bilancino_cut  %>%
  mutate(Season = case_when(month(Date) %in% c(3,4,5) ~ "Spring",                      
                            month(Date) %in% c(6,7,8) ~ "Summer",
                            month(Date) %in% c(9,10,11) ~ "Autumn",
                            month(Date) %in% c(1,2,12) ~ "Winter"))
Lake_Bilancino_cut$Season<-factor(Lake_Bilancino_cut$Season,
                                  levels=c("Winter","Spring", "Summer", "Autumn"))


Lake_Bilancino_Season <- dummyVars(~Season, data = Lake_Bilancino_cut, fullRank = F)
Lake_Bilancino_Season <- as.data.frame(predict(Lake_Bilancino_Season, newdata = Lake_Bilancino_cut))


#### RF con Rainfall_S_Piero, Rainfall_Mangona , Rainfall_Cavallina####
Lake_Bilancino_cut1 <- Lake_Bilancino_cut %>%
  dplyr::select(Lake_Level, Flow_Rate, Temperature_Le_Croci, 
                Rainfall_Cavallina,
                Rainfall_S_Piero, Rainfall_Mangona)

Lake_Bilancino_cut1 <- cbind(Lake_Bilancino_cut1, Lake_Bilancino_Season)

Lake_Bilancino_cut1 <- Lake_Bilancino_cut1[complete.cases(Lake_Bilancino_cut1),]

bilancino_LL<-Lake_Bilancino_cut1%>%
  dplyr::select(-Flow_Rate)
bilancino_FL<-Lake_Bilancino_cut1%>%
  dplyr::select(-Lake_Level)

set.seed(2021)
rand_Lake_Bilancino <- sample(nrow(bilancino_LL), nrow(bilancino_LL)* 1/3, replace = F)
test_Lake_Bilancino <- bilancino_LL[rand_Lake_Bilancino,]
train_Lake_Bilancino <- bilancino_LL[-rand_Lake_Bilancino,]


cat("Number of rows in the training set:", nrow(train_Lake_Bilancino), "\n")
#Number of rows in the training set: 4017 
cat("Number of rows in the test set:", nrow(test_Lake_Bilancino))
#Number of rows in the test set: 2008

#ho diviso per il set di addestramento
#in base al rapporto di due a uno, e inizio il processo di modellazione

#### random forest lake level ####

rf_Lake_Bilancino_Lake_Level <- rfsrc(Lake_Level~Season.Autumn+Season.Spring+Season.Summer+Season.Winter+Temperature_Le_Croci+
                                        Rainfall_S_Piero+Rainfall_Mangona+Rainfall_Cavallina, 
                                      data = train_Lake_Bilancino, block.size = 1, 
                                      importance = T, samptype = "swr", 
                                      var.used = "all.trees", ntree = 200)

plot(rf_Lake_Bilancino_Lake_Level, verbose = F)

#ggsave("img/bilancino/16Bilancino_RF.jpg",dpi = 500, width = 10, height=7)
#
#L'autunno ha maggiore influenza sul modello. 
#Ha un impatto sul modello oltre 3 volte maggiore rispetto alla 
#seconda variabile più importante, che è la stagione della primavera.
#
#Le piogge dalle regioni incluse,si sono rivelate avere 
#il minor impatto sul modello.

#### errore RMSE Test  per rf lake level####

pred_rf_Lake_Bilancino_Lake_Level <- predict(rf_Lake_Bilancino_Lake_Level, newdata = test_Lake_Bilancino)
pred_rf_Lake_Bilancino_Lake_Level

cat("RMSE Test:", round(rmse(pred_rf_Lake_Bilancino_Lake_Level$predicted, test_Lake_Bilancino$Lake_Level),2))
#### errore RMSE Test: 1.78 LL ####
# la varianza spiega circa il 33% dei casi


# predicted 
pred_Lake_Bilancino_Lake_Level <- data.frame(pred = pred_rf_Lake_Bilancino_Lake_Level$predicted, 
                                             real = test_Lake_Bilancino$Lake_Level)

pred_Lake_Bilancino_Lake_Level$above <- ifelse(pred_Lake_Bilancino_Lake_Level$pred>pred_Lake_Bilancino_Lake_Level$real,
                                               "Too high predict value", "Too low predicted value")

#ggplot(pred_Lake_Bilancino_Lake_Level, aes(real, pred, fill = above))

# plot predicted vs actual

(ggplot(pred_Lake_Bilancino_Lake_Level) +
    geom_point(aes(x = pred,
                   y = real,
                   color = pred - real),
               alpha = .7, size = 1) +
    theme_classic())
ggsave("img/bilancino/17_0lake_level_pred.jpg",
         dpi = 500, width = 10, height=7)



## plotting pred vs actual 

reg <- lm(real ~ pred, data = pred_Lake_Bilancino_Lake_Level)
reg
#Coefficients:
#(Intercept)  Lake_level coeff
# 46.6117       0.8133 

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),
             "\nr.squared = ",r.sq)
eq
# plot
(RF_actualvspred <- ggplot(pred_Lake_Bilancino_Lake_Level) +
    geom_point(aes(x = pred,
                   y = real,
                   color = pred - real
                  ),
               alpha = .7, size = 2) +
    geom_abline(intercept = 46.6117 ,slope = 0.8133 , 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 250.5, y = 244, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (RF): Lake Level Bilancino\n",
         subtitle = "m")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())
ggsave("img/bilancino/17_1Bilancino_lakelevel_pred_act.jpg",
       dpi = 500, width = 10, height=7)

















ggplot(pred_Lake_Bilancino_Lake_Level, aes(real, pred, fill = above))+
  geom_point(size = 4, shape = 21, alpha = 0.8)+
  theme_classic()+
  scale_fill_viridis_d(option = "inferno", begin = 0.25, end = 0.85, name = "")+
  labs(x = "Real values in the test set (m)", y = "Predicted values in the test set (m)", 
       title = "The predicted and true values on test set", fill = "", 
       subtitle = "Random forest model / Lake Level variable / Bilancino") + 
  theme_classic()+
  theme(legend.position = "bottom", legend.direction = "vertical")
ggsave("img/bilancino/17Bilancino_RF_LakeLevel.jpg",
       dpi = 500, width = 10, height=7)




#### random forest flow rate ####
set.seed(2021)
rand_Lake_Bilancino <- sample(nrow(bilancino_FL), nrow(bilancino_FL)* 1/3, replace = F)
test_Lake_Bilancino <- bilancino_FL[rand_Lake_Bilancino,]
train_Lake_Bilancino <- bilancino_FL[-rand_Lake_Bilancino,]

rf_Lake_Bilancino_Flow_Rate <- rfsrc(Flow_Rate~Season.Autumn+Season.Spring+Season.Summer+Season.Winter+Temperature_Le_Croci+
                                       Rainfall_S_Piero+Rainfall_Mangona+Rainfall_Cavallina, 
                                     data = train_Lake_Bilancino, 
                                     block.size = 1, importance = T, 
                                     samptype = "swr", var.used = "all.trees", ntree = 200)

plot(rf_Lake_Bilancino_Flow_Rate, verbose = F)
#ggsave("img/bilancino/18Bilancino_RF_flowrate.jpg", dpi = 500, width = 10, height=7)

###
#### errore RMSE Test  per rf per flow rate####
pred_rf_Lake_Bilancino_Flow_Rate <- predict(rf_Lake_Bilancino_Flow_Rate, newdata = test_Lake_Bilancino)
pred_rf_Lake_Bilancino_Flow_Rate

cat("RMSE Test:", round(rmse(pred_rf_Lake_Bilancino_Flow_Rate$predicted, test_Lake_Bilancino$Flow_Rate),2))

#### RMSE Test: 4.22 RF flow rate#### 
#variance 3.32


#### ####
# predicted 

pred_Lake_Bilancino_Flow_Rate <- data.frame(pred = pred_rf_Lake_Bilancino_Flow_Rate$predicted, 
                                            real = test_Lake_Bilancino$Flow_Rate)

pred_Lake_Bilancino_Flow_Rate$above <- ifelse(pred_Lake_Bilancino_Flow_Rate$pred>pred_Lake_Bilancino_Flow_Rate$real,
                                              "Too high predicted value", "Too low predicted value")


# predicted 

# plot predicted vs actual

(ggplot(pred_Lake_Bilancino_Flow_Rate) +
    geom_point(aes(x = pred,
                   y = real,
                   color = pred - real),
               alpha = .7, size = 1) +
    theme_classic())
ggsave("img/bilancino/20_0flowrate_pred.jpg",
       dpi = 500, width = 10, height=7)



## plotting pred vs actual 

reg <- lm(real ~ pred, data = pred_Lake_Bilancino_Flow_Rate)
reg
#Coefficients:
#(Intercept)  Flow rate coeff
#  1.2065       0.5632 

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)

eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1),
             "\nr.squared = ",r.sq)
eq
# plot
(RF_actualvspred <- ggplot(pred_Lake_Bilancino_Flow_Rate) +
    geom_point(aes(x = pred,
                   y = real,
                   color = pred - real
    ),
    alpha = .7, size = 2) +
    geom_abline(intercept = 1.2065 ,slope = 0.5632 , 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 250.5, y = 244, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (RF): Flow Rate Bilancino\n",
         subtitle = "L/s")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())
ggsave("img/bilancino/20_1Bilancino_flowratel_pred_act.jpg",
       dpi = 500, width = 10, height=7)


















############################################################################
##############################################################################

####  GBM  gradient boost model####



#### target flow rate + target lake_level ####
#BILANCINO_to_model

#dalla matrice di correlazione vista prima
# scelgo di escludere rainfall S_Agata e rainfall le croci
bilancino <- read.csv("processed_data/BILANCINO_to_model.csv")%>%
  dplyr::select(-X, -Date, -Rainfall_mean, -Rainfall_S_Agata,
                -Rainfall_Le_Croci)
# in regression date doesnt really matter 

  bilancino$Season<-factor(bilancino$Season, 
                       levels=c("Winter","Spring", "Summer", "Autumn"))


#target Flow_rate e trget Lake_Level creo due dataset

Flow_Rate_df <- bilancino %>% dplyr::select(-Lake_Level)
Lake_Level_df <- bilancino %>% dplyr::select(-Flow_Rate)

#### computing stepwise regression for variable selection ####
# creating function
step.wisef <- function(x, DATA){
  set.seed(123)
  train.control <- trainControl(method = "cv", number = 10)
  step.model <- train(as.formula(paste(x,"~.")), data = DATA, 
                      method = "leapSeq", 
                      tuneGrid = data.frame(nvmax = 1:11),
                      trControl = train.control,
                      na.action = na.omit)
  return(step.model)
}

#### Flow_Rate_df GBM ####

flow_rate_gb <- Flow_Rate_df 
flow_rate_gb_Season <- dummyVars(~Season, data = flow_rate_gb, fullRank = F)
flow_rate_gb_Season <- as.data.frame(predict(flow_rate_gb_Season, newdata = flow_rate_gb))
flow_rate_gb <- cbind(flow_rate_gb, flow_rate_gb_Season)
flow_rate_gb <-flow_rate_gb%>% dplyr::select(-Season)

#visdat::vis_dat(flow_rate_gb)

#### GBM con target Flow_Rate ####

#flow_rate_sw <- step.wisef("Flow_Rate", flow_rate_gb)
flow_rate_sw$bestTune 
flow_rate_sw$finalModel
coef(flow_rate_sw$finalModel, 10)

#### testing and training split Flow_Rate ####


set.seed(123)
flow_rate.split <- initial_split(flow_rate_gb, prop = .7)
flow_rate.train <- training(flow_rate.split)
flow_rate.test <- testing(flow_rate.split)

flow_rate_fit1 <- gbm::gbm(Flow_Rate ~ .,
                           data = flow_rate_gb,
                           verbose = T, 
                           shrinkage = 0.01,
                           interaction.depth = 3, 
                           n.minobsinnode = 5,
                           n.trees = 5000,
                           cv.folds = 10)
perf_gbm1 <- gbm.perf(flow_rate_fit1, method = "cv")
#ggsave("img/bilancino/21flowrate_GB.jpg",dpi = 500, width = 10, height=7)

## make predictions 
flow_rate_pred1 <- stats::predict(object = flow_rate_fit1,
                                  newdata = flow_rate.test,
                                  n.trees = perf_gbm1)
rmse_fit1 <- Metrics::rmse(actual = flow_rate.test$Flow_Rate,
                           predicted = flow_rate_pred1)
print(rmse_fit1) 
#### RMSE 3.4211 Flow_RAte GB ####


#### plot flow rate GB ####
#plot - rain S Piero
gbm::plot.gbm(flow_rate_fit1, i.var = 1)
# plot - rain Mangona
plot.gbm(flow_rate_fit1, i.var = 2)
# plot - rain Cavallina
plot.gbm(flow_rate_fit1, i.var = 3)
# plot - temp le croci
plot.gbm(flow_rate_fit1, i.var = 4)



## interactions of two features on the variable 
gbm::plot.gbm(flow_rate_fit1, i.var = c(1,3)) # rain-rain
plot.gbm(flow_rate_fit1, i.var = c(1,2)) # rain-rain
plot.gbm(flow_rate_fit1, i.var = c(3,4)) # temp-rain

### impact of different features on predicting depth to gw 

# summarise model 

flow_rate_effects <- tibble::as_tibble(gbm::summary.gbm(flow_rate_fit1,
                                                     plotit = F))
flow_rate_effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 6 features
flow_rate_effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 6 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")
ggsave("img/bilancino/21flowrate_features.jpg",
       dpi = 500, width = 10, height=7)

## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

flow_rate.test$predicted <- as.integer(predict(flow_rate_fit1,
                                               newdata = flow_rate.test,
                                               n.trees = perf_gbm1))

# plot predicted vs actual

(ggplot(flow_rate.test) +
  geom_point(aes(x = predicted,
                 y = Flow_Rate,
                 color = predicted - Flow_Rate),
             alpha = .7, size = 1) +
  theme_fivethirtyeight()+
ggsave("img/bilancino/22flow_rate_pred.jpg",
       dpi = 500, width = 10, height=7)
)



## plotting pred vs actual 

reg <- lm(Flow_Rate ~ predicted, data = flow_rate.test)
reg
#Coefficients:
#(Intercept)  Flow_Rate coeff 
#-0.4054       1.3179 

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)

eq <- paste0("y = ", round(coeff[2],1), "*x", round(coeff[1],1),
             "\nr.squared = ",r.sq)
eq
# plot
(gbm_actualvspred <- ggplot(flow_rate.test) +
    geom_point(aes(x = predicted,
                   y = Flow_Rate,
                   color = predicted - Flow_Rate),
               alpha = .7, size = 2) +
    geom_abline(intercept = -0.4054,slope = 1.3179, 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 15, y = 30, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (GBM): Flow_Rate Bilancino\n",
         subtitle = "l/s")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())
ggsave("img/bilancino/23Bilancino_flowrate_pred_act.jpg",
       dpi = 500, width = 10, height=7)



#### RMSE RMSE 3.4211 Flow_RAte GB  ### BEST MODEL ####


#### Lake_level_df GBM ####

lake_level_gb <- Lake_Level_df 
lake_level_gb_Season <- dummyVars(~Season, data = lake_level_gb, fullRank = F)
lake_level_gb_Season <- as.data.frame(predict(lake_level_gb_Season, newdata = lake_level_gb))
lake_level_gb <- cbind(lake_level_gb, lake_level_gb_Season)
lake_level_gb <-lake_level_gb%>% dplyr::select(-Season)



#### GBM con target Flow_Rate ####

#lake_level_sw <- step.wisef("Lake_Level", lake_level_gb)
lake_level_sw$bestTune 
lake_level_sw$finalModel
coef(lake_level_sw$finalModel, 10)

#### testing and training split Flow_Rate ####


set.seed(123)
lake_level.split <- initial_split(lake_level_gb, prop = .7)
lake_level.train <- training(lake_level.split)
lake_level.test <- testing(lake_level.split)

lake_level_fit1 <- gbm::gbm(Lake_Level ~ .,
                           data = lake_level_gb,
                           verbose = T, 
                           shrinkage = 0.01,
                           interaction.depth = 3, 
                           n.minobsinnode = 5,
                           n.trees = 5000,
                           cv.folds = 10)
perf_gbm1 <- gbm.perf(lake_level_fit1, method = "cv")
#ggsave("img/bilancino/24lake_level_GB.jpg",dpi = 500, width = 10, height=7)

## make predictions 
lake_level_pred1 <- stats::predict(object = lake_level_fit1,
                                  newdata = lake_level.test,
                                  n.trees = perf_gbm1)
rmse_fit1 <- Metrics::rmse(actual = lake_level.test$Lake_Level,
                           predicted = lake_level_pred1)
print(rmse_fit1) 
#### RMSE 1.5908 Lake_Level GB ####


#### plot flow rate GB ####
#plot - rain S Piero
gbm::plot.gbm(lake_level_fit1, i.var = 1)
# plot - rain Mangona
plot.gbm(lake_level_fit1, i.var = 2)
# plot - rain Cavallina
plot.gbm(lake_level_fit1, i.var = 3)
# plot - temp le croci
plot.gbm(lake_level_fit1, i.var = 4)



## interactions of two features on the variable 
gbm::plot.gbm(lake_level_fit1, i.var = c(1,3)) # rain-rain
plot.gbm(lake_level_fit1, i.var = c(1,2)) # rain-rain
plot.gbm(lake_level_fit1, i.var = c(3,4)) # temp-rain

### impact of different features on predicting depth to gw 

# summarise model 

lake_level_effects <- tibble::as_tibble(gbm::summary.gbm(lake_level_fit1,
                                                        plotit = F))
lake_level_effects %>% utils::head()
# this creates new dataset with var, factor variable with variables 
# in our model, and rel.inf - relative influence each var has on model pred 

# plot top 6 features
lake_level_effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # it's already only 6 vars
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")
ggsave("img/bilancino/25lake_level_features.jpg",
       dpi = 500, width = 10, height=7)

## vis distribution of predicted compared with actual target values 
# by predicting these vals and plotting the difference 

# predicted 

lake_level.test$predicted <- as.integer(predict(lake_level_fit1,
                                               newdata = lake_level.test,
                                               n.trees = perf_gbm1))

# plot predicted vs actual

(ggplot(lake_level.test) +
    geom_point(aes(x = predicted,
                   y = Lake_Level,
                   color = predicted - Lake_Level),
               alpha = .7, size = 1) +
    theme_fivethirtyeight()+
    ggsave("img/bilancino/26lake_level_pred.jpg",
           dpi = 500, width = 10, height=7)
)



## plotting pred vs actual 

reg <- lm(Lake_Level ~ predicted, data = lake_level.test)
reg
#Coefficients:
#(Intercept)  Lake_level coeff
#-17.403        1.072 

r.sq <- format(summary(reg)$r.squared,digits = 2)

coeff <- coefficients(reg)

eq <- paste0("y = ", round(coeff[2],1), "*x", round(coeff[1],1),
             "\nr.squared = ",r.sq)
eq
# plot
(gbm_actualvspred <- ggplot(lake_level.test) +
    geom_point(aes(x = predicted,
                   y = Lake_Level,
                   color = predicted - Lake_Level),
               alpha = .7, size = 2) +
    geom_abline(intercept = -17.403 ,slope = 1.072 , 
                color = "darkred", linetype ="dashed")+
    geom_text(x = 250.5, y = 244, label = eq, color = "darkred")+
    labs(title = "Predicted vs Actual values (GBM): Lake_Level Bilancino\n",
         subtitle = "m")+
    ylab("Actual\n")+
    xlab("\nPredicted")+
    scale_color_continuous(name = "Difference\npredicted - actual")+
    theme_classic())
ggsave("img/bilancino/27Bilancino_lakelevel_pred_act.jpg",
       dpi = 500, width = 10, height=7)

















#### GBM ####

#### anna with gbm for bilancino (original) + lags ####



str(bilancino_months)

bilancino_months1 <- bilancino_months %>% 
  dplyr::select(-Date)

bilancino_months1[,9:14] <- unfactor(bilancino_months1[,9:14])
str(bilancino_months1)



set.seed(123)

bilancino_orig.fl <- bilancino_months1 %>% dplyr::select(-Lake_Level)

bilancino_orig.fl.split <- initial_split(bilancino_orig.fl,prop =.7)
bilancino_orig.fl.train <- training(bilancino_orig.fl.split)
bilancino_orig.fl.test <- testing(bilancino_orig.fl.split)

bilancino_orig.fl.fit <- gbm::gbm(Flow_Rate ~ .,
                                data = bilancino_orig.fl,
                                verbose = T, 
                                shrinkage = 0.01,
                                interaction.depth = 3, 
                                n.minobsinnode = 5,
                                n.trees = 600,
                                cv.folds = 12)

bilancino_orig.fl.fit.perf <- gbm.perf(bilancino_orig.fl.fit, method = "cv")
ggsave("img/bilancino/20Bilancino_Sqerror1.jpg",
       dpi = 500, width = 10, height=7)
## make predictions 

bilancino_orig.fl.fit.pred <- stats::predict(object = bilancino_orig.fl.fit,
                                           newdata = bilancino_orig.fl.test,
                                           n.trees = bilancino_orig.fl.fit.perf)
bilancino_orig.fl.rmse <- Metrics::rmse(actual = bilancino_orig.fl.test$Flow_Rate,
                                          predicted = bilancino_orig.fl.fit.pred)
print(bilancino_orig.fl.rmse) 
#### RMSE 2.88 ### BEST MODEL ####

#plot 
# summarise model 

bilancino_orig_effects <- tibble::as_tibble(gbm::summary.gbm(bilancino_orig.fl.fit,
                                                                        plotit = F))
bilancino_orig_effects %>% utils::head()
# plot top 6 features
bilancino_orig_effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # 
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")
ggsave("img/arno/27.jpg", dpi = 500, width = 10, height=7)
#### il primo classificato ####






## rain 0 le croci 

str(bilancino_rain0_Le_Croci.lag)

lecroci <- read.csv("processed_data/bilancino_rain0_Le_Croci+lag.csv") %>%
  dplyr::select(-X,-Date)
str(lecroci)
set.seed(123)
lecroci.split <- initial_split(lecroci,prop =.7)

lecroci.train <- training(lecroci.split)
lecroci.test <- testing(lecroci.split)

lecroci.fit <- gbm::gbm(Flow_Rate ~ .,
                                  data = lecroci,
                                  verbose = T, 
                                  shrinkage = 0.01,
                                  interaction.depth = 3, 
                                  n.minobsinnode = 5,
                                  n.trees = 600,
                                  cv.folds = 12)

lecroci.perf <- gbm.perf(lecroci.fit, method = "cv")

## make predictions 

lecroci.pred<- stats::predict(object = lecroci.fit,
                                             newdata = lecroci.test,
                                             n.trees = lecroci.perf)
lecroci.rmse <- Metrics::rmse(actual = lecroci.test$Flow_Rate,
                                        predicted = lecroci.pred)
print(lecroci.rmse) # 
#### 2.04 RMSE lecroci the best ####
#bilancino_rain0_Le_Croci.lag
#plot 
# summarise model 

LL_effects <- tibble::as_tibble(gbm::summary.gbm(lecroci.fit,
                                                    plotit = F))
LL_effects %>% utils::head()
# plot top 6 features
LL_effects %>% 
  arrange(desc(rel.inf)) %>% 
  top_n(6) %>%  # 
  ggplot(aes(x = fct_reorder(.f = var,
                             .x = rel.inf),
             y = rel.inf,
             fill = rel.inf))+
  geom_col()+
  coord_flip()+
  scale_color_brewer(palette = "Dark2")
ggsave("img/bilancino/21.jpg", dpi = 500, width = 10, height=7)
#### il primo classificato ####



## rain 1 le croci 

lecroci1 <- read.csv("processed_data/bilancino_rain1_Le_Croci+lag.csv") %>% 
  dplyr::select(-X,-Date)
str(lecroci1)

lecroci1.split <- initial_split(lecroci1,prop =.7)

lecroci1.train <- training(lecroci1.split)
lecroci1.test <- testing(lecroci1.split)

lecroci1.fit <- gbm::gbm(Flow_Rate ~ .,
                        data = lecroci1,
                        verbose = T, 
                        shrinkage = 0.01,
                        interaction.depth = 3, 
                        n.minobsinnode = 5,
                        n.trees = 600,
                        cv.folds = 12)

lecroci1.perf <- gbm.perf(lecroci1.fit, method = "cv")

## make predictions 

lecroci1.pred<- stats::predict(object = lecroci1.fit,
                              newdata = lecroci1.test,
                              n.trees = lecroci1.perf)
lecroci1.rmse <- Metrics::rmse(actual = lecroci1.test$Flow_Rate,
                              predicted = lecroci1.pred)
print(lecroci1.rmse) # 2.12 ### BEST MODEL 

## rain 5 le croci 

lecroci5 <- read.csv("processed_data/bilancino_rain5_Le_Croci+lag.csv") %>% 
  dplyr::select(-X,-Date)

lecroci5.split <- initial_split(lecroci5,prop =.7)

lecroci5.train <- training(lecroci5.split)
lecroci5.test <- testing(lecroci5.split)

lecroci5.fit <- gbm::gbm(Flow_Rate ~ .,
                        data = lecroci5,
                        verbose = T, 
                        shrinkage = 0.01,
                        interaction.depth = 3, 
                        n.minobsinnode = 5,
                        n.trees = 600,
                        cv.folds = 12)

lecroci5.perf <- gbm.perf(lecroci5.fit, method = "cv")

## make predictions 

lecroci5.pred<- stats::predict(object = lecroci5.fit,
                              newdata = lecroci5.test,
                              n.trees = lecroci5.perf)
lecroci5.rmse <- Metrics::rmse(actual = lecroci5.test$Flow_Rate,
                              predicted = lecroci5.pred)
print(lecroci5.rmse) # 2.18




### rain 0 s piero 

sp <- read.csv("processed_data/bilancino_rain0_S_Piero+lag.csv") %>% 
  dplyr::select(-X,-Date)

sp.split <- initial_split(sp,prop =.7)

sp.train <- training(sp.split)
sp.test <- testing(sp.split)

sp.fit <- gbm::gbm(Flow_Rate ~ .,
                        data = sp,
                        verbose = T, 
                        shrinkage = 0.01,
                        interaction.depth = 3, 
                        n.minobsinnode = 5,
                        n.trees = 600,
                        cv.folds = 12)

sp.perf <- gbm.perf(sp.fit, method = "cv")

## make predictions 

sp.pred<- stats::predict(object = sp.fit,
                              newdata = sp.test,
                              n.trees = sp.perf)
sp.rmse <- Metrics::rmse(actual = sp.test$Flow_Rate,
                              predicted = sp.pred)
print(sp.rmse
      ) # 2.03

### sp rain 1 

sp1 <- read.csv("processed_data/bilancino_rain1_S_Piero+lag.csv") %>% 
  dplyr::select(-X,-Date)

sp1.split <- initial_split(sp1,prop =.7)

sp1.train <- training(sp1.split)
sp1.test <- testing(sp1.split)

sp1.fit <- gbm::gbm(Flow_Rate ~ .,
                   data = sp1,
                   verbose = T, 
                   shrinkage = 0.01,
                   interaction.depth = 3, 
                   n.minobsinnode = 5,
                   n.trees = 1000,
                   cv.folds = 12)

sp1.perf <- gbm.perf(sp1.fit, method = "cv")

## make predictions 

sp1.pred<- stats::predict(object = sp1.fit,
                         newdata = sp1.test,
                         n.trees = sp1.perf)
sp1.rmse <- Metrics::rmse(actual = sp1.test$Flow_Rate,
                         predicted = sp1.pred)
print(sp1.rmse
) # 1.98


## rain 5 s piero 

sp5 <- read.csv("processed_data/bilancino_rain5_S_Piero+lag.csv") %>% 
  dplyr::select(-X,-Date)

sp5.split <- initial_split(sp5,prop =.7)

sp5.train <- training(sp5.split)
sp5.test <- testing(sp5.split)

sp5.fit <- gbm::gbm(Flow_Rate ~ .,
                   data = sp5,
                   verbose = T, 
                   shrinkage = 0.01,
                   interaction.depth = 3, 
                   n.minobsinnode = 5,
                   n.trees = 600,
                   cv.folds = 12)

sp5.perf <- gbm.perf(sp5.fit, method = "cv")

## make predictions 

sp5.pred<- stats::predict(object = sp5.fit,
                         newdata = sp5.test,
                         n.trees = sp5.perf)
sp5.rmse <- Metrics::rmse(actual = sp5.test$Flow_Rate,
                         predicted = sp5.pred)
print(sp5.rmse) # 1.98 ## BEST MODEL 


### rain 0 mangona 

mg <- read.csv("processed_data/bilancino_rain0_Mangona+lag.csv") %>% 
  dplyr::select(-X,-Date)

mg.split <- initial_split(mg,prop =.7)

mg.train <- training(mg.split)
mg.test <- testing(mg.split)

mg.fit <- gbm::gbm(Flow_Rate ~ .,
                   data = mg,
                   verbose = T, 
                   shrinkage = 0.01,
                   interaction.depth = 3, 
                   n.minobsinnode = 5,
                   n.trees = 1000,
                   cv.folds = 12)

mg.perf <- gbm.perf(mg.fit, method = "cv")

## make predictions 

mg.pred<- stats::predict(object = mg.fit,
                         newdata = mg.test,
                         n.trees = mg.perf)
mg.rmse <- Metrics::rmse(actual = mg.test$Flow_Rate,
                         predicted = mg.pred)
print(mg.rmse
) # 2.223

## rain 1 mg 

mg1 <- read.csv("processed_data/bilancino_rain1_Mangona+lag.csv") %>% 
  dplyr::select(-X,-Date)

mg1.split <- initial_split(mg1,prop =.7)

mg1.train <- training(mg1.split)
mg1.test <- testing(mg1.split)

mg1.fit <- gbm::gbm(Flow_Rate ~ .,
                   data = mg1,
                   verbose = T, 
                   shrinkage = 0.01,
                   interaction.depth = 3, 
                   n.minobsinnode = 5,
                   n.trees = 5000,
                   cv.folds = 12)

mg1.perf <- gbm.perf(mg1.fit, method = "cv")

## make predictions 

mg1.pred<- stats::predict(object = mg1.fit,
                         newdata = mg1.test,
                         n.trees = mg1.perf)
mg1.rmse <- Metrics::rmse(actual = mg1.test$Flow_Rate,
                         predicted = mg1.pred)
print(mg1.rmse) # 1.70  ## BEST MODEL 


### rain 5 mg 

mg5 <- read.csv("processed_data/bilancino_rain5_Mangona+lag.csv") %>% 
  dplyr::select(-X,-Date)

mg5.split <- initial_split(mg5,prop =.7)

mg5.train <- training(mg5.split)
mg5.test <- testing(mg5.split)

mg5.fit <- gbm::gbm(Flow_Rate ~ .,
                   data = mg5,
                   verbose = T, 
                   shrinkage = 0.01,
                   interaction.depth = 3, 
                   n.minobsinnode = 5,
                   n.trees = 1000,
                   cv.folds = 12)

mg5.perf <- gbm.perf(mg5.fit, method = "cv")

## make predictions 

mg5.pred<- stats::predict(object = mg5.fit,
                         newdata = mg5.test,
                         n.trees = mg5.perf)
mg5.rmse <- Metrics::rmse(actual = mg5.test$Flow_Rate,
                         predicted = mg5.pred)
print(mg5.rmse) # 2.07


### best model...: 

print(bilancino_orig.fl.rmse) # 3.14 ### BEST MODEL 

print(lecroci1.rmse) # 2.12 ### BEST MODEL 

print(sp5.rmse) # 1.98 ## BEST MODEL 

print(mg1.rmse) # 1.70  ## BEST MODEL 


### and mangona lowest error of all 

## Lake Level

bilancino_orig.ll <- bilancino_months1 %>% dplyr::select(-Flow_Rate)

bilancino_orig.ll.split <- initial_split(bilancino_orig.ll,prop =.7)
bilancino_orig.ll.train <- training(bilancino_orig.ll.split)
bilancino_orig.ll.test <- testing(bilancino_orig.ll.split)

bilancino_orig.ll.fit <- gbm::gbm(Lake_Level ~ .,
                                  data = bilancino_orig.ll,
                                  verbose = T, 
                                  shrinkage = 0.01,
                                  interaction.depth = 3, 
                                  n.minobsinnode = 5,
                                  n.trees = 2000,
                                  cv.folds = 12)

bilancino_orig.ll.fit.perf <- gbm.perf(bilancino_orig.ll.fit, method = "cv")

## make predictions 

bilancino_orig.ll.fit.pred <- stats::predict(object = bilancino_orig.ll.fit,
                                             newdata = bilancino_orig.ll.test,
                                             n.trees = bilancino_orig.ll.fit.perf)
bilancino_orig.ll.rmse <- Metrics::rmse(actual = bilancino_orig.ll.test$Lake_Level,
                                        predicted = bilancino_orig.ll.fit.pred)
print(bilancino_orig.ll.rmse) # 2.08 ### BEST MODEL 


### rain 0 le croci 

lecroci.fit.ll <- gbm::gbm(Lake_Level ~ .,
                        data = lecroci,
                        verbose = T, 
                        shrinkage = 0.01,
                        interaction.depth = 3, 
                        n.minobsinnode = 5,
                        n.trees = 600,
                        cv.folds = 12)

lecroci.perf.ll <- gbm.perf(lecroci.fit.ll, method = "cv")

## make predictions 

lecroci.pred.ll <- stats::predict(object = lecroci.fit.ll,
                              newdata = lecroci.test,
                              n.trees = lecroci.perf.ll)
lecroci.ll.rmse <- Metrics::rmse(actual = lecroci.test$Lake_Level,
                              predicted = lecroci.pred.ll)
print(lecroci.ll.rmse) # 1.70 ### BEST MODEL


## rain 1 le croci 

lecroci1.fit.ll <- gbm::gbm(Lake_Level ~ .,
                         data = lecroci1,
                         verbose = T, 
                         shrinkage = 0.01,
                         interaction.depth = 3, 
                         n.minobsinnode = 5,
                         n.trees = 600,
                         cv.folds = 12)

lecroci1.perf.ll <- gbm.perf(lecroci1.fit.ll, method = "cv")

## make predictions 

lecroci1.pred.ll<- stats::predict(object = lecroci1.fit.ll,
                               newdata = lecroci1.test,
                               n.trees = lecroci1.perf.ll)
lecroci1.rmse.ll <- Metrics::rmse(actual = lecroci1.test$Lake_Level,
                               predicted = lecroci1.pred.ll)
print(lecroci1.rmse.ll) # 1.73


## rain 5 le croci 

lecroci5.fit.ll <- gbm::gbm(Lake_Level ~ .,
                         data = lecroci5,
                         verbose = T, 
                         shrinkage = 0.01,
                         interaction.depth = 3, 
                         n.minobsinnode = 5,
                         n.trees = 600,
                         cv.folds = 12)

lecroci5.perf.ll <- gbm.perf(lecroci5.fit.ll, method = "cv")

## make predictions 

lecroci5.pred.ll<- stats::predict(object = lecroci5.fit.ll,
                               newdata = lecroci5.test,
                               n.trees = lecroci5.perf.ll)
lecroci5.rmse.ll <- Metrics::rmse(actual = lecroci5.test$Lake_Level,
                               predicted = lecroci5.pred.ll)
print(lecroci5.rmse.ll) # 1.77


### rain 0 s piero 

sp <- read.csv("processed_data/bilancino_rain0_S_Piero+lag.csv") %>% 
  dplyr::select(-X,-Date)

sp.split <- initial_split(sp,prop =.7)

sp.train <- training(sp.split)
sp.test <- testing(sp.split)

sp.fit <- gbm::gbm(Flow_Rate ~ .,
                   data = sp,
                   verbose = T, 
                   shrinkage = 0.01,
                   interaction.depth = 3, 
                   n.minobsinnode = 5,
                   n.trees = 600,
                   cv.folds = 12)

sp.perf <- gbm.perf(sp.fit, method = "cv")

## make predictions 

sp.pred<- stats::predict(object = sp.fit,
                         newdata = sp.test,
                         n.trees = sp.perf)
sp.rmse <- Metrics::rmse(actual = sp.test$Flow_Rate,
                         predicted = sp.pred)
print(sp.rmse
) # 2.03

### sp rain 1 

sp1 <- read.csv("processed_data/bilancino_rain1_S_Piero+lag.csv") %>% 
  dplyr::select(-X,-Date)

sp1.split <- initial_split(sp1,prop =.7)

sp1.train <- training(sp1.split)
sp1.test <- testing(sp1.split)

sp1.fit <- gbm::gbm(Flow_Rate ~ .,
                    data = sp1,
                    verbose = T, 
                    shrinkage = 0.01,
                    interaction.depth = 3, 
                    n.minobsinnode = 5,
                    n.trees = 1000,
                    cv.folds = 12)

sp1.perf <- gbm.perf(sp1.fit, method = "cv")

## make predictions 

sp1.pred<- stats::predict(object = sp1.fit,
                          newdata = sp1.test,
                          n.trees = sp1.perf)
sp1.rmse <- Metrics::rmse(actual = sp1.test$Flow_Rate,
                          predicted = sp1.pred)
print(sp1.rmse
) # 1.98


## rain 5 s piero 

sp5 <- read.csv("processed_data/bilancino_rain5_S_Piero+lag.csv") %>% 
  dplyr::select(-X,-Date)

sp5.split <- initial_split(sp5,prop =.7)

sp5.train <- training(sp5.split)
sp5.test <- testing(sp5.split)

sp5.fit <- gbm::gbm(Flow_Rate ~ .,
                    data = sp5,
                    verbose = T, 
                    shrinkage = 0.01,
                    interaction.depth = 3, 
                    n.minobsinnode = 5,
                    n.trees = 600,
                    cv.folds = 12)

sp5.perf <- gbm.perf(sp5.fit, method = "cv")

## make predictions 

sp5.pred<- stats::predict(object = sp5.fit,
                          newdata = sp5.test,
                          n.trees = sp5.perf)
sp5.rmse <- Metrics::rmse(actual = sp5.test$Flow_Rate,
                          predicted = sp5.pred)
print(sp5.rmse) # 1.98 ## BEST MODEL 


### rain 0 mangona 

mg <- read.csv("processed_data/bilancino_rain0_Mangona+lag.csv") %>% 
  dplyr::select(-X,-Date)

mg.split <- initial_split(mg,prop =.7)

mg.train <- training(mg.split)
mg.test <- testing(mg.split)

mg.fit <- gbm::gbm(Flow_Rate ~ .,
                   data = mg,
                   verbose = T, 
                   shrinkage = 0.01,
                   interaction.depth = 3, 
                   n.minobsinnode = 5,
                   n.trees = 1000,
                   cv.folds = 12)

mg.perf <- gbm.perf(mg.fit, method = "cv")

## make predictions 

mg.pred<- stats::predict(object = mg.fit,
                         newdata = mg.test,
                         n.trees = mg.perf)
mg.rmse <- Metrics::rmse(actual = mg.test$Flow_Rate,
                         predicted = mg.pred)
print(mg.rmse
) # 2.223

## rain 1 mg 

mg1 <- read.csv("processed_data/bilancino_rain1_Mangona+lag.csv") %>% 
  dplyr::select(-X,-Date)

mg1.split <- initial_split(mg1,prop =.7)

mg1.train <- training(mg1.split)
mg1.test <- testing(mg1.split)

mg1.fit <- gbm::gbm(Flow_Rate ~ .,
                    data = mg1,
                    verbose = T, 
                    shrinkage = 0.01,
                    interaction.depth = 3, 
                    n.minobsinnode = 5,
                    n.trees = 5000,
                    cv.folds = 12)

mg1.perf <- gbm.perf(mg1.fit, method = "cv")

## make predictions 

mg1.pred<- stats::predict(object = mg1.fit,
                          newdata = mg1.test,
                          n.trees = mg1.perf)
mg1.rmse <- Metrics::rmse(actual = mg1.test$Flow_Rate,
                          predicted = mg1.pred)
print(mg1.rmse) # 1.70  ## BEST MODEL 


### rain 5 mg 

mg5 <- read.csv("processed_data/bilancino_rain5_Mangona+lag.csv") %>% 
  dplyr::select(-X,-Date)

mg5.split <- initial_split(mg5,prop =.7)

mg5.train <- training(mg5.split)
mg5.test <- testing(mg5.split)

mg5.fit <- gbm::gbm(Flow_Rate ~ .,
                    data = mg5,
                    verbose = T, 
                    shrinkage = 0.01,
                    interaction.depth = 3, 
                    n.minobsinnode = 5,
                    n.trees = 1000,
                    cv.folds = 12)

mg5.perf <- gbm.perf(mg5.fit, method = "cv")

## make predictions 

mg5.pred<- stats::predict(object = mg5.fit,
                          newdata = mg5.test,
                          n.trees = mg5.perf)
mg5.rmse <- Metrics::rmse(actual = mg5.test$Flow_Rate,
                          predicted = mg5.pred)
print(mg5.rmse) # 2.07














