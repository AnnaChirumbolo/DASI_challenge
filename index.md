# DASI Challenge - ACEA Smart Water Analytics

## Executive Summary.

The analysis work was approached by the team by assigning each one a type of water basin, in an attempt to be able to “specialize” in the short time available on a specific water system.

Finding in the first analysis many missing values, the use of enriching the datasets using external sources was fundamental, especially for the meteorological variables. In particular, we found a good fit with the datasets provided by 3bmeteo.com.

We have adopted a very decisive approach in selecting and privileging the analysis of portions of recent datasets and with more or less continuous data, filling small gaps with weighted automatic attribution techniques (using “imputeTS” library).

The features and datasets that present behavioral anomalies, arbitrarily attributed by us to anthropic interventions without explanation, have been removed to favor an analysis as much aware of the phenomena under study.

The features engineering activity was important as we diversified the contribution of rain by diversifying the contribution between rain and snow, created precipitation bands to try to extrapolate physical behaviors such as runoff as well as considering different levels of lag for absorption. of rainfall.

We have also launched control systems for the choice of predictive models using Rapidminer as a working platform which confirmed the results obtained through packages such as AutoML.

We are therefore satisfied with the results achieved in the few days of dedicated work and we believe that with greater detail on the data, on the geophysical characteristics of the water basins and technical support with advanced 3D modeling models, we can significantly increase the results achieved.

We therefore report below the error table (RMSE) that our modeling has produced.




## Summary: 

 <a href="#section1"> 0. Importing Datasets and Libraries</a>

### <a href="#section2"> 1. Data Cleaning and Feature Engineering</a>

  <a href="#subsect1"><sub> 1.1 Aquifers </sub></a>
  
  <a href="#subsect2"><sub> 1.2 Water Springs </sub></a>
  
  <a href="#subsect3"><sub> 1.3 River </sub></a>
  
  <a href="#subsect4"><sub> 1.4 Lake </sub></a>
  
### <a href="#section3"> 2. Modelling and Forecasting</a>

  <a href="#subsect5"><sub> 2.1 Aquifers </sub></a>
  
  <a href="#subsect6"><sub> 2.2 Water Springs </sub></a>
  
  <a href="#subsect7"><sub> 2.3 River </sub></a>
  
  <a href="#subsect8"><sub> 2.4 Lake </sub></a>
  

<a name="#section1"></a>

# 0. Importing Datasets and Libraries

```
## Here are all the libraries used for this project 

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

```

<a name="#section2"></a>

# 1. Data Cleaning and Feature Engineering


text bla bla 


<a name="#subsect1"></a>

## 1.1 Aquifers

<a name="#subsect2"></a>

## 1.2 Water Springs

<a name="#subsect3"></a>

## 1.3 River 

<a name="#subsect4"></a>

## 1.4 Lake


## Feature Engineering

In addition to the feature variables provided by Acea, we decided to carry out some feature engineering, in order to extract new features that would perhaps ease the model performance and improve its prediction capabilities. 

These features have been created through one-hot encoding procedures. These include: 

- Seasons 

We customised a function to split the years over the four seasons (set to change at solstices and equinoxes). We created four new features, each representing one season as dummy variable.

- Occurrence of snow 

We engineered two new features, again as dummy variables. One representing occurrence of snow - assuming temperature is below or equal to 0, and rainfall is greater than 0. The second one representing absence of snow - assuming temperature is greather than 0.

- Changes in minimum rainfall levels 

We modified minimum rainfall levels to 0, assuming that small rainfall values would have a minimal, thus neglectable, effect over the target variable. By looking at the distribution of rainfall values we arbitrarily set four different minimum rainfall values to 0. We thus obtained five different datasets per waterbody, each with a different minimum rainfall values (including the original dataset).

- Lag effect of rainfall (when present) over the target variable 

Lastly, we created five new features for each new dataset, with five different time lags (set at 1, 3, 5, 7 and 9 days) representing the potentially delayed effect of rainfall on the target variable.



<a name="#section3"></a>

# 2. Modelling and Forecasting


<a name="#subsect5"></a>

## 2.1 Aquifers



<a name="#subsect6"></a>

## 2.2 Water Springs 




<a name="#subsect7"></a>

## 2.3 River 



<a name="#subsect8"></a>

## 2.4 Lake 