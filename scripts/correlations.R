#################
# correlations  #
# and PCA       #
## Anna         #


library(tidyverse)
library("corrplot")
library("naniar")
library("factoextra")
library("plotly")
library("FactoMineR")
library("gplots")
library("graphics")
library("ggpubr")
library("graphics")

###########

doganella <- read.csv("doganella_filtered.csv")

missing_doganella <- doganella %>% 
  miss_var_summary()
print(missing_doganella)

### let's try imputation of missing 

var_missing_doganella1 <- missing_doganella[[1]][1]
var_missing_doganella2 <- missing_doganella[[1]][2]
var_missing_doganella3 <- missing_doganella[[1]][3]
var_missing_doganella4 <- missing_doganella[[1]][4]
var_missing_doganella5.6 <- missing_doganella[[1]][5]
var_missing_doganella7 <- missing_doganella[[1]][6]
var_missing_doganella8 <- missing_doganella[[1]][7]
var_missing_doganella9<- missing_doganella[[1]][8]

hist(doganella$Volume_Pozzo_1)
hist(doganella$Volume_Pozzo_2)
hist(doganella$Volume_Pozzo_3)
hist(doganella$Volume_Pozzo_4)
hist(doganella$Volume_Pozzo_5.6)
hist(doganella$Volume_Pozzo_7)
hist(doganella$Volume_Pozzo_8)
hist(doganella$Volume_Pozzo_9)

# mean or median 
mean_1 <- mean(doganella[,var_missing_doganella1],na.rm = T) #1529.376
mean_2 <- mean(doganella[,var_missing_doganella2],na.rm = T)
mean_3 <- mean(doganella[,var_missing_doganella3], na.rm =T)
mean_4 <- mean(doganella[,var_missing_doganella4],na.rm = T)  
mean_5.6<- mean(doganella[,var_missing_doganella5.6],na.rm = T)  
mean_7 <- mean(doganella[,var_missing_doganella7],na.rm = T)
mean_8 <- mean(doganella[,var_missing_doganella8],na.rm = T)
mean_9 <- mean(doganella[,var_missing_doganella9],na.rm = T)

doganella1 <- doganella %>% 
  mutate(Volume_Pozzo_1 = replace_na(Volume_Pozzo_1, mean_1),
         Volume_Pozzo_2 = replace_na(Volume_Pozzo_2, mean_2),
         Volume_Pozzo_3 = replace_na(Volume_Pozzo_3, mean_3),
         Volume_Pozzo_4 = replace_na(Volume_Pozzo_4, mean_4),
         Volume_Pozzo_5.6 = replace_na(Volume_Pozzo_5.6, mean_5.6),
         Volume_Pozzo_7 = replace_na(Volume_Pozzo_7, mean_7),
         Volume_Pozzo_8 = replace_na(Volume_Pozzo_8, mean_8),
         Volume_Pozzo_9 = replace_na(Volume_Pozzo_9, mean_9))

summary(doganella1)


missing_doganella1 <- doganella1 %>% 
  miss_var_summary()
print(missing_doganella1)

hist(doganella1$Temperature_Monteporzio)
hist(doganella1$Temperature_Velletri)

hist(doganella1$Rainfall_Monteporzio)
hist(doganella1$Rainfall_Velletri)

var_missing_doganella_temp1 <- missing_doganella1[[1]][1] #velletri
var_missing_doganella_temp2 <- missing_doganella1[[1]][2] # monteporzio

var_missing_doganella_rain1 <- missing_doganella1[[1]][3] # monteporzio
var_missing_doganella_rain2 <- missing_doganella1[[1]][4] # velletri

mean_temp1 <- mean(doganella1[,var_missing_doganella_temp1],na.rm = T)
mean_temp2 <- mean(doganella1[,var_missing_doganella_temp2],na.rm = T)
mean_rain1 <- mean(doganella1[,var_missing_doganella_rain1],na.rm = T)
mean_rain2 <- mean(doganella1[,var_missing_doganella_rain2],na.rm = T)


doganella2 <- doganella1 %>% 
  mutate(Temperature_Velletri = replace_na(Temperature_Velletri, mean_temp1),
         Temperature_Monteporzio = replace_na(Temperature_Monteporzio, mean_temp2),
         Rainfall_Monteporzio = replace_na(Rainfall_Monteporzio, mean_rain1),
         Rainfall_Velletri = replace_na(Rainfall_Velletri, mean_rain2))


summary(doganella2)


doganella_cor <- doganella2 %>% 
  select(-X, -Date, -well, -depth_to_groundwater.m)



##################

## now that missing values have been filled: correlation 

# variances and covariances 

cov_matrix1 <- cov(doganella_cor)
print(cov_matrix1[1:4,1:4])

# associated correlation matrix 

corr_matrix1 <- cor(doganella_cor)
print(corr_matrix1[1:4,1:4])


# is normalisation necessary? 

normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

doganella_cor_norm <- normalise(doganella_cor)
cov_matrix2 <- cov(doganella_cor_norm)
print(cov_matrix2[1:4,1:4])


#### CORRPLOT

corrplot(corr_matrix1)


restest <- cor.mtest(doganella_cor, conf.level=.95)
corrplot(corr_matrix1, p.mat = restest$p, sig.level = .05)


######### pca ##########

pca_doganella <- prcomp(doganella_cor, center = T, scale = T)
summary(pca_doganella)

fviz_eig(pca_doganella, addlabels = T, ylim = c(0,45))

## first two, three dimensions already capture good % of the variance 


####

res.var <- get_pca_var(pca_doganella)
res.var$coord[,1:4]

# or 

corrplot(res.var$cos2, is.corr = F)


## valutare graficamente correlazione tra features e prime due dimensioni

fviz_pca_var(pca_doganella, 
             col.var = "contrib",
             gradient.cols =  c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T)



res.var$cos2



### contributo della singola variabile alla singola componente principale

res.var$contrib


##### relazione PCA - individui 

res.ind <- get_pca_ind(pca_doganella)
res.ind$coord[1:10,1:4]


### grafico usando prime 3 pc

Y = as.data.frame(pca_doganella$x)
plot_ly(Y, x = ~PC1, y = ~PC2, z =~PC3, size =1)



res.ind$cos2[1:10,1:4]
res.ind$contrib[1:10,1:4]
