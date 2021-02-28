# Executive Summary.


We analysed datasets provided by Acea, to **predict the amount of available drinkable water in central Italy**.

Each team member was assigned a type of water basin, in an attemp to 'specialise' in the short-term on the characteristics and dynamics relative to that water system.

During the first observational analysis, we found large gaps of data particularly in the earliest years of the timeseries at our disposal. For this reason, we adopted **a very decisive approach** in selecting and privileging the analyses towards portions of the **most recent records** the datasets, as they presented more or less continuous data, while we decided to **fill in** small gaps remaining via **weighted automatic attribution techniques** (see *imputeTs* package). 

Concerning meteorological data, we opted for **data enrichment** methods, by extrapolating data from external sources and integrating them to the datasets provided by Acea. We found a particularly good fit with the data provided by [3bmeteo.com](https://www.3bmeteo.com/meteo/ora/storico).


We carried out important work of **feature engineering**, particularly on *precipitation*:

1. We split each year over the four seasons (set to change at solstices and equinoxes). We created four new features, each representing one season as dummy variable;

2. We created new features diversifying the impact of precipitation between **rainfall** and **snowfall** (neglecting effects of temperature on the soil). We did so in an attempt to highlight the **slower effect of snow on water availability of the water system**;

3. We aimed to detect **irrelevant impacts of precipitation** on the water system, by selecitng a minimum threshold of weekly rainfall that, accumulating over time, would have an actual effect over the water levels of the water system. By doing this, aimed to eliminate the *noise* of neglectable weekly rainfall imputs;

4. We created different different **time lags for rainfall being absorbed into the water system** compared to when it has been recorded, to try and simulate the delayed impact of this meteorological variable on the target. 



Once feature engineering was carried out, we launched **control systems** for choice of best predictive models for each of the water systems under study, using **autoML** and **autoTS** packages and benchmarking their results by implementing **Rapidminer**.

We are very satisfied with the achieved results in the few days of dedicated work. We report below the **RMSE error table** that our models produced.

<br>

### Aquifers 

| Target | GBM: RMSE | GBM: R.sq | RF: RMSE |
| :--: | :---: | :---: | :----: |
| *Doganella: wells* | 
| :-------------: | 
| 1 | 2.94 | 0.83 |
| 2 | 1.04 | 0.87 |
| 3 | 1.59 | 0.77 | 
| 4 | 0.52 | 0.85 |
| 5 | 1.12 | 0.81 |
| 6 | 1.00 | 0.79 |
| 7 | 0.71 | 0.48 |
| 8 | 0.77 | 0.82 |
| 9 | 2.68 | 0.76 |
| *Luco: wells* |
| :-----------: |
| 1 | 0.14 | ? | 
| 3 | 0.25 | ? |
| *Auser: wells* |
| :------------: |
| CoS | 0.22 | 0.93 | 0.22 |
| LT2 | 0.09 | 0.77 | 0.07 | 
| SAL | 0.11 | ???? | 0.10 |

<br>

### Water Springs 

#### Madonna di Canneto 

| Target | GBM: RMSE | GBM: R.sq | 
| :---: | :---: | :---: | 
| Flow Rate (L/s) | 26.05 | 0.34 | 

<br>

### River Arno 

| Target | RF: RMSE | GBM: RMSE | GBM: R.sq | 
| :---: | :---: | :---: | :---: |
| Hydrometry (L/s) | 0.37 | 0.34 | ??? | 

<br>

### Lake Bilancino 

| Target | RF: RMSE | RF: R.sq | GBM: RMSE | GBM: R.sq | 
| :---: | :---: | :---: | :---: | :---: |
| Lake Level (m) | 1.78 | 0.33 | 1.60 | 0.35 |
| Flow Rate (L/s) | 4.22 | 0.099 | 3.42 | 0.46 |