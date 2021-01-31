# Bilancino Lake

getwd()
rm(list = ls(all=TRUE)) 

library(reshape)
library(stats)
install.packages("factoextra")
install.packages("tibble")
library(class)
library(jpeg)
library(png)
library(tibble)
library("dplyr")
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
library(corrplot)
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

########## Imposto theme
theme_21 <- theme(legend.position = "bottom", legend.direction = "horizontal", axis.text = element_text(size = 14), 
                     plot.caption = element_text(color = "gray25", face = "bold", size = 8), legend.text = element_text(size = 15), 
                     axis.title = element_text(size = 14.5, face = "bold", color = "gray25"), legend.title = element_text(size = 14), axis.line = element_line(size = 0.4), 
                     plot.title = element_text(size = 19), plot.subtitle = element_text(size = 14.5), strip.text = element_text(size = 14, face = "bold"))

core_col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))



##### prima prova prova prova
bilancino<-read_csv("./acea-water-prediction/Lake_Bilancino.csv")
#Trasformo la colonn data in data
bilancino$Date<-as.Date(bilancino$Date, format = "%d/%m/%Y")
str(bilancino) # visionare
names(bilancino)
summary(bilancino) #per avere una visione del dataframe

visdat::vis_dat(bilancino)

######## Secondo approccio
Lake_Bilancino<-read_csv("./acea-water-prediction/Lake_Bilancino.csv")
#Trasformo la colonn data in data
Lake_Bilancino$Date<-as.Date(Lake_Bilancino$Date, format = "%d/%m/%Y")

# raggruppo i mesi in stagioni
#Lake_Bilancino <- Lake_Bilancino %>%
 # mutate(Season = case_when(month(Date) %in% c(3,4,5) ~ "Spring",                      
#                           month(Date) %in% c(6,7,8) ~ "Summer",
#                            month(Date) %in% c(9,10,11) ~ "Autumn",
#                           month(Date) %in% c(1,2,12) ~ "Winter"))

####controllo i missing prima prova
vis_dat(Lake_Bilancino)

#### elimino i missing antecedenti al 2004
cutdate <- as.Date("2004-01-01")
Lake_Bilancino <- Lake_Bilancino[(Lake_Bilancino$Date > cutdate), ]
summary(Lake_Bilancino)
str(Lake_Bilancino)
#is.na(Lake_Bilancino)
dim(Lake_Bilancino)
dim(na.omit(Lake_Bilancino)) #controllo di aver eliminato tutti i null

## Le mie variabili TARGET sono: Lake_Level, Flow_Rate
##Sembra che ci siano alcuni picchi Flow_Rate a volte a gennaio, a volte in primavera,
#non tutti gli anni. 

df <- Lake_Bilancino %>% select(Date, 
                                Flow_Rate) %>%
  pivot_longer(., cols = c(Flow_Rate),
               names_to = "Var", values_to = "Val", values_drop_na = FALSE)
df <- df[complete.cases(df), ]
ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() +
  ggtitle("Lake Bilancino: Flow Rate (l/s)") +
  ylab("Flow_Rate") +
  xlab("Date")
rm(df)

#correlazione Correlation Matrix
df <- Lake_Bilancino
df$Date <- NULL
ggcorr(df, label = TRUE, label_round = 2, hjust = 1, size = 4, layout.exp = 4, label_size = 3)
rm(df)


#Il comportamento Lake Level è pressoché ricorrente 
#(salvo alcuni anni) con un livello costante e 
#con una diminuzione verso novembre. Un picco verso il basso c'e' stato nel 2012 
#e deve ancora tornare ai livelli precedenti
# il Lake_level ha una variabilità molto bassa, 
#in quanto il livello dell'acqua è ancora compreso tra 243 e 253 m
#nel corso di oltre 17 anni

df <- Lake_Bilancino %>% select(Date, 
                                Lake_Level) %>%
  pivot_longer(., cols = c(Lake_Level),
               names_to = "Var", values_to = "Val", values_drop_na = FALSE)
df <- df[complete.cases(df), ]
ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() +
  ggtitle("Lake Bilancino: Lake Level (meters)") +
  ylab("Lake_Level") +
  xlab("Date")
rm(df)

# Rainfall analysis
#Rainfall: the rainfall mean is 2,86 mm, the month with typically less rain is July
Lake_Bilancino$Rainfall_mean <- rowMeans(Lake_Bilancino[,c("Rainfall_S_Piero","Rainfall_Mangona",
                                                           "Rainfall_S_Agata","Rainfall_Cavallina",
                                                           "Rainfall_Le_Croci" )])
df <- Lake_Bilancino
df$Rainfall_mean <- runmean(df$Rainfall_mean,30)
df <- df %>% select(Date,  Rainfall_mean) %>%
  pivot_longer(., cols = c( Rainfall_mean),
               names_to = "Var", values_to = "Val", values_drop_na = FALSE)
ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() + ggtitle("Rainfall (mm) - Lake Bilancino") +
  ylab("Rainfall") +   xlab("Date")
rm(df)



### Temperature: the temperature mean is 14.53 °C

# Temperature analysis
df <- Lake_Bilancino %>% select(Date, Temperature_Le_Croci ) %>%
  pivot_longer(., cols = c(Temperature_Le_Croci ),
               names_to = "Var", values_to = "Val")
df <- df[complete.cases(df), ]
ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() + ggtitle("Temperature (°C) -  Lake Bilancino") +
  ylab("Temperature") + xlab("Date")
rm(df)


##Correlazione (Livello Lago - Precipitazioni): 
#esiste una correlazione -0,070% tra la media delle precipitazioni
#e il livello del Lago. Tuttavia, se guardiamo la trama notiamo che quando 
#i livelli di pioggia sono alti il livello del lago è basso e viceversa, 
#quindi il lago scende il livello nell'ultima parte dell'anno ma risale a 
#cavallo di gennaio dopo le piogge ; possiamo considerare 5 mesi di ritardo 
#per ripristinare il livello dell'acqua
# correlation Lake_level - Rainfall (compare plots)
df <- Lake_Bilancino
df$Rainfall_mean <- runmean(df$Rainfall_mean,30)
df$Lake_Level <- df$Lake_Level/20
df <- df %>% select(Date, Lake_Level, Rainfall_mean) %>%
  pivot_longer(., cols = c(Lake_Level, Rainfall_mean ),
               names_to = "Var", values_to = "Val", values_drop_na = FALSE)
ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() + ggtitle("Lake Level - Rainfall - Lake Bilancino") +
  ylab("Lake Level - Rainfall") +   xlab("Date")
rm(df)

# correlation Lake_level - Rainfall (compare plots)
df <- Lake_Bilancino
cutdate <- as.Date("2015-01-01")
df <- Lake_Bilancino[(Lake_Bilancino$Date > cutdate), ]
df$Rainfall_mean <- runmean(df$Rainfall_mean,30)/10
df$Lake_Level <- (df$Lake_Level/20)-12
df <- df %>% select(Date, Lake_Level, Rainfall_mean) %>%
  pivot_longer(., cols = c(Lake_Level, Rainfall_mean ),
               names_to = "Var", values_to = "Val", values_drop_na = FALSE)
ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() + ggtitle("Lake Level - Rainfall - Lake Bilancino") +
  ylab("Lake Level - Rainfall") +   xlab("Date")
rm(df)

#Correlazione (Flow_Rate - Rainfall): esiste una correlazione dello 0,343% tra 
#la media delle precipitazioni e il livello del lago
# correlation Flow_Rate - Rainfall (compare plots)
df <- Lake_Bilancino
df$Rainfall_mean <- runmean(df$Rainfall_mean,30)
df$Flow_Rate <- df$Flow_Rate/10
df <- df %>% select(Date, Flow_Rate, Rainfall_mean) %>%
  pivot_longer(., cols = c(Flow_Rate, Rainfall_mean ),
               names_to = "Var", values_to = "Val", values_drop_na = FALSE)
ggplot(df, aes(x = Date, y = Val, col = Var)) +
  geom_line() + ggtitle("Flow Rate - Rainfall - Lake Bilancino") +
  ylab("Flow Rate - Rainfall") +   xlab("Date")
rm(df)

# correlation Flow_Rate - Rainfall (ggpair)
df <- Lake_Bilancino
df$Rainfall_mean <- runmean(df$Rainfall_mean,30)
Pairvar <- c("Rainfall_mean", "Flow_Rate")
Pairdf <- df[Pairvar]
ggpairs(Pairdf,lower = list(continuous = wrap("smooth",color = "Green")), title = "Correlation Matrix")
rm(df)


####controllo i missing terza prova ricarico il dataset + le stagioni
Lake_Bilancino<-read_csv("./acea-water-prediction/Lake_Bilancino.csv")
#Trasformo la colonn data in data
Lake_Bilancino$Date<-as.Date(Lake_Bilancino$Date, format = "%d/%m/%Y")
# raggruppo i mesi in stagioni
Lake_Bilancino <- Lake_Bilancino %>%
 mutate(Season = case_when(month(Date) %in% c(3,4,5) ~ "Spring",                      
                           month(Date) %in% c(6,7,8) ~ "Summer",
                           month(Date) %in% c(9,10,11) ~ "Autumn",
                           month(Date) %in% c(1,2,12) ~ "Winter"))
Lake_Bilancino$Season<-as.factor(Lake_Bilancino$Season)

#missinig
Lake_Bilancino %>% 
  is.na %>%
  melt %>%
  ggplot(data = .,aes(Var2, Var1))+
  geom_raster(aes(fill = value))+
  scale_fill_brewer(palette = "Reds", labels = c("Present","Missing"))+
  theme(axis.text.x  = element_text(angle=45, vjust=0.5))+ 
  coord_flip()+
  labs(x = "Variable name", y = "Observations", title = "Missing values",  fill = "",
       subtitle = "in the variables describing lake Bilancino") + 
  theme_21
#Il livello di flusso è solitamente molto basso - fino a 20 m3 / s, 
#ma a volte aumenta bruscamente fino a 70 m3 al secondo nel 2014, 
#quindi questo cambiamento è caratterizzato da una variazione maggiore 
#rispetto al livello dell'acqua.
#Il livello dell'acqua normalmente e' abbastanza stabile, In 17 anni ha
#avuto poca varianza.
# peche' ha un livello abbastanza costante e 
#con una diminuzione verso novembre. Un picco verso il basso c'e' stato nel 2012 
#e deve ancora tornare ai livelli precedenti
# il Lake_level ha una variabilità molto bassa, 
#in quanto il livello dell'acqua è ancora compreso tra 243 e 253 m
#nel corso di oltre 17 anni
###distribuzione comparata
Lake_Bilancino %>%
  select(Date, Lake_Level, Flow_Rate) %>%
  melt(., id.vars = "Date") %>%
  ggplot(., aes(Date, value))+
  facet_wrap(variable~., ncol = 1, scales = "free_y")+
  geom_line(size = 1.6, alpha = 0.8, col = "gray65")+
  geom_smooth(method = "loess", color = "firebrick3", size = 2.2, formula = y ~ x, fill = "firebrick4", alpha = 0.32)+
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", limits = as.Date(c("2004-01-01", "2020-06-30")))+
  labs(x = "Date", y = "Value", title = "The distribution of the explained variables (along with the loess curve)",
       subtitle = "in lake Bilancio (from 01-2004)") + 
  theme_21

##correlazione
#si può notare che le variabili riguardanti le precipitazioni 
#sono fortemente correlate positivamente tra loro. Il coefficiente di c
#orrelazione del rango di Spearman varia tra 0,8 e 0,9 per queste variabili, 
#quindi sarà necessaria una riduzione. Le restanti relazioni sono molto 
#basse o inesistenti (è la temperatura e le variabili dipendenti), 
#quindi la rimozione delle variabili riguarderà solo le precipitazioni.

Lake_Bilancino %>%
  select(!c("Date", "Season")) %>%
  cor(., method = "spearman", use = "complete.obs") %>%
  corrplot(., method = "color", type = "upper", col = core_col(100), number.cex = 1, addCoef.col = "gray8",
           tl.col = "black",tl.srt = 35, diag = T, tl.cex = 0.85)


######### PRECIPITAIZIONI
#Abbiamo precipitazioni da cinque diverse regioni e 
#la loro quantità, varia da 0 a 125 mm. Gli aumenti rapidi si applicano
#a tutte le variabili e I FENOMENI SONO FORTEMENTE CORRELATI. 
# DECIDO di lasciare 2 variabili, per il processo di modellazione 
#scelgo la prima coppia delle regioni "S Piero" e "Mangona", 
#perché questa coppia è la meno correlata e quindi ci fornisce maggiori informazioni
Lake_Bilancino %>%
  select(Date, Rainfall_S_Piero, Rainfall_Mangona, Rainfall_S_Agata, Rainfall_Cavallina, Rainfall_Le_Croci) %>%
  melt(., id.vars = "Date") %>%
  ggplot(., aes(Date, value, col = variable))+
  facet_wrap(variable~., ncol = 2)+
  geom_line(size = 0.4, alpha = 1)+
  scale_color_viridis_d(option = "inferno", begin = 0.15, end = 0.15)+
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", limits = as.Date(c("2004-01-01", "2020-06-30")))+
  labs(x = "Date", y = "Quantity of rain falling (in mm)", title = "Quantity of rain falling depending on the region",
       subtitle = "explanatory variables on lake Bilancino (from 01-2004)") + 
  theme_21+
  theme(legend.position = "none")

##Temperature
#Il secondo tipo di variabile dipendente continua è la temperatura dal sito
#"Le Croci". Nella  maggioranza dei casi è compresa tra 0 e 30 gradi Celsius, 
#con stagionalità visibile e connessione con le stagioni. 
#Essendo l'unica variabile sulla temperatura, senza dubbio aggiungila al database utilizzato per creare il modello
Lake_Bilancino %>%
  select(Date, Temperature_Le_Croci) %>%
  melt(., id.vars = "Date") %>%
  ggplot(., aes(Date, value, col = variable))+
  facet_wrap(variable~., ncol = 1)+
  geom_line(size = 0.6, alpha = 1)+
  scale_color_viridis_d(option = "inferno", begin = 0.85, end = 0.85, name = "")+
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", limits = as.Date(c("2004-01-01", "2020-06-30")))+
  labs(x = "Date", y = "Temperature (in degrees Celsius)", title = "Temperature depending on the region",
       subtitle = "explanatory variables on lake Bilancino (from 01-2004)") + 
  theme_21+
  theme(legend.position = "none")






