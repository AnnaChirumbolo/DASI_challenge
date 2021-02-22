# DASI Challenge - ACEA Smart Water Analytics



## Executive Summary.


We analysed datasets provided by Acea, to **predict the amount of available drinkable water in central Italy**.

Each team member was assigned a type of water basin, in an attemp to 'specialise' in teh short-term on the characteristics and dynamics relative to that water system.

During the first observational analysis, we found large gaps of data particularly in the earliest years of the timeseries at our disposal. For this reason, we adopted **a very decisive approach** in selecting and privileging the analyses towards portions of the **most recent records** the datasets, as they presented more or less continuous data, while we decided to **fill in** small gaps remaining via **weighted automatic attribution techniques** (see *imputeTs* package). 

Concerning meteorological data, we opted for **data enrichment** methods, by extrapolating data from external sources and integrating them to the datasets provided by Acea. We found a particularly good fit with the data provided by [3bmeteo.com](https://www.3bmeteo.com/meteo/ora/storico).

We carried out an important acitivity of **feature engineering** towards *precipitation*:

1. We created new features diversifying the impact of precipitation between **rainfall** and **snowfall** (neglecting effects of temperature on the soil). We did so in an attempt to highlight the **slower effect of snow on water availability of the water system**;

2. We aimed to detect **irrelevant impacts of precipitation** on the water system, by selecitng a minimum threshold of weekly rainfall that, accumulating over time, would have an actual effect over the water levels of the water system. By doing this, aimed to eliminate the *noise* of neglectable weekly rainfall imputs;

3. We created different different **time lags for rainfall being absorbed into the water system** compared to when it has been recorded, to try and simulate the delayed impact of this meteorological variable on the target. 



Once feature engineering was carried out, we launched **control systems** for choice of best predictive models for each of the water systems under study, using **autoML** and **autoTS** packages and benchmarking their results by implementing **Rapidminer**.

We are very satisfied with the achieved results in the few days of dedicated work. We report below the RMSE error table that our models produced.


**TABLE HERE**





# Summary: 

## [0. Importing libraries and datasets](docs/0.md)

## [1. Data cleaning and feature engineering](docs/1.md)
  
## [2. Modelling and forecasting](docs/2.md)




# should i keep this here? 

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

