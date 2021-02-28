<br>

# Auser 
{: #top}

* [Data Cleaning and Feature Engineering](#datacleaning)
  * [Target variable](#target)
  * [Feature variables](#features)
  * [Feature Selection and Engineering](#engineering)
* [Modelling and Forecasting](#modelling)
  * [Choosing the best model](#choosing)
  * [Model Results](#results)

------------------------------------------------

## Data Cleaning and Feature Engineering
{: #datacleaning}

Auser presents data from five different wells, which are subdivided into two groups: the Northern wells, where SAL, PAG, CoS and DIEC are found, and the Southern wells, with LT2. Out of these wells, three present our target variables (SAL, CoS, LT2).

We first visualised the dataset and noticed that, like the others, it presented large gaps in the earlier years. 

![01Auser](https://user-images.githubusercontent.com/43357858/109395125-0aff7300-792b-11eb-85a9-acf1752b01d7.jpg)

We found that target data lacked more than half their values, as did feature variables - i.e. rainfall. You can see the percentage of missing data in greater detail below. 

```
# A tibble: 27 x 3
      variable                     n_miss    pct_miss
        <chr>                       <int>      <dbl>
1 Depth_to_Groundwater_DIEC         4884       59.9
2 Depth_to_Groundwater_PAG          4347       53.3
3 Depth_to_Groundwater_CoS          3839       47.1
4 Depth_to_Groundwater_SAL          3609       44.3
5 Depth_to_Groundwater_LT2          3352       41.1
6 Rainfall_Piaggione                3224       39.5
7 Rainfall_Monte_Serra              2865       35.1
```

### Target variable: depth to groundwater (m)
{: #target}

We filtered out the dataset to minimise the amount of missing data for the target variables, as these could not be retrieved from external sources. 

The division between Northern and Southern wells is more clear by looking at their behaviour over time (absolute values).

![04AUSER_filtered_depth_to_groundwater](https://user-images.githubusercontent.com/43357858/109395134-15217180-792b-11eb-8c5c-227b25f78333.jpg)

We observed in more detail where the remaining missing (after the filtering) were located over time, and decided to impute their values by calculating the moving average. 

![32auser_target_no_missing](https://user-images.githubusercontent.com/43357858/109395144-21a5ca00-792b-11eb-9440-42dfc557ca7d.jpg)

Then we checked for outliers. For repeating outliers, as in the case of SAL well, we could not intervene. In the case of CoS, which had an outlier equal to 0, we subsituted the value with the value of the lowest quartile in the distribution. 

```
Grubbs test for one outlier:
Data: auser (CoS)

G = 4.77370, U = 0.99569, p-value = 0.004673

Alternative hypothesis: lowest value 0 is an outlier
```

![16box_auser_pozzi](https://user-images.githubusercontent.com/43357858/109395154-2e2a2280-792b-11eb-9f7d-15693ae80d56.jpg)

### Feature variables 
{: #features}

#### Temperature 

No outliers are present in the temperature variables. 

![23auser_temp](https://user-images.githubusercontent.com/43357858/109396958-6f730000-7934-11eb-8b83-c652e1db7b53.jpg)

We noticed that for the sensor in Ponte a Moriano, from 2017 values result 0 °C for consecutive months - we therefore decided to **exclude this variable** due clear faluts in data collection. 

![34auser_temp](https://user-images.githubusercontent.com/43357858/109320067-e5516b80-784f-11eb-92ab-866f9c887baa.jpg)

There we no missing data for temperature.

There does not seem to be any strong correlation between temperature and the targets. 

![12AUSER_corr_temp](https://user-images.githubusercontent.com/43357858/109395488-f7eda280-792c-11eb-99c6-9d1bc37b0746.jpg)

#### Rainfall 

We checked for outliers in precipitation variables - they present long upper tails outisde of the quartile range. 

![24auser_rain](https://user-images.githubusercontent.com/43357858/109319169-da4a0b80-784e-11eb-8279-4311dbc383af.jpg)

We visualised correlations between targets and meteorological features: temperature and rainfall. The differentiation between Northern and Southern wells is also clear here. 

We enriched the variables by retrieving data from [3bmeteo.com](https://www.3bmeteo.com/meteo/ora/storico). 

![29auser_rain_missing2](https://user-images.githubusercontent.com/43357858/109395529-279caa80-792d-11eb-8f3e-084670396cc1.jpg)

#### Hydrometry 

The few missing values present were imputed by calculating the moving average. 

![30auser_Hydrometry](https://user-images.githubusercontent.com/43357858/109395539-32efd600-792d-11eb-9ddd-4b1542a86c4d.jpg)

#### Volume

The values are negative because these features reflect the volumes of water *extracted* from the well. 

![35auser_volumi](https://user-images.githubusercontent.com/43357858/109395548-3aaf7a80-792d-11eb-90cf-4ddd50cad857.jpg)

### Feature selection and Engineering
{: #engineering}

#### Correlation Matrix 

We correlated all variables with one another, before proceeding with feature engineering. As expected, rainfall and temperature variables correlated strongly between and within each other. 

We considered excluding **CC1 and CSA** volume variables and choose CC2 and CSAL for the **greater variance**. 

**Hydrometry (Piaggione)** is strongly correlated with the others, so we decided to **exclude** it. 

![33auser_correlazione](https://user-images.githubusercontent.com/43357858/109321783-d9ff3f80-7851-11eb-88c9-3d08aa297adc.jpg)

#### Feature engineering 

We engineered seasons, presence/absence of snow, new threshold for minimum rainfall and time lags as new features to input in the model. 

Distribution of target variables over the seasons does not change much for LT2 well. For CoS and SAL wells are deeper during the summer and in autumn (absolute value).

![37auser_season_target](https://user-images.githubusercontent.com/43357858/109395558-4602a600-792d-11eb-9c62-aee369df9e9f.jpg)

Minimum rainfall levels were set at 0.5 mm, 1.5 mm, 3 mm and 5 mm and time lags were set at +1, +3, +5 and +7 days.  


## Modelling and Forecasting 
{: #modelling}

### Choosing the model 
{: #choosing}

From our autoML and RapdMiner tests, Random Forest and Gradient Boost Machine resulted to be the best performing models. 

### Model Results 
{: #results}

#### Random Forest 

##### Target 1: CoS

PAG well seems to have the largest influence on the target, followed by POL volume. Rainfall variables have the least influence. **RMSE: 0.22**

![40_1auser_RF_cos](https://user-images.githubusercontent.com/43357858/109397002-c547a800-7934-11eb-8a4a-1cae32232fa6.jpg)

##### Target 2: LT2 

CSAL well has the largest influence on the target, rainfall has again little importance. **RMSE: 0.07**

![41_1auser_RF_LT2](https://user-images.githubusercontent.com/43357858/109397008-ca0c5c00-7934-11eb-9401-3a4667ed1aaf.jpg)

##### Target 3: SAL

DIEC well has the largest impact in the model, **3 times greater** than the second most important feature: PAG well. Rainfall has the least influence in the model over the target. **RMSE: 0.1**

![42_1auser_RF_SAL](https://user-images.githubusercontent.com/43357858/109397015-ce387980-7934-11eb-970e-23b693a0efb5.jpg)

#### Gradient Boost Machine 

##### Target 1: CoS 

**RMSE: 0.22, R squared: 0.93**

![45pozzo_cos_pred](https://user-images.githubusercontent.com/43357858/109397062-1ce61380-7935-11eb-9fd1-2c8a6c95fe9c.jpg)

![44auser_pozzocos_features](https://user-images.githubusercontent.com/43357858/109397025-d85a7800-7934-11eb-805f-f65bf952ac4f.jpg)

##### Target 2: LT2 

**RMSE: 0.09, R squared: 0.77**

![48_1pozzo_lt2_pred](https://user-images.githubusercontent.com/43357858/109397086-36875b00-7935-11eb-936f-a1bc4dbb41c2.jpg)

![47auser_pozzolt2_features](https://user-images.githubusercontent.com/43357858/109397029-dee8ef80-7934-11eb-8db3-3466a7458d2d.jpg)

##### Target 2: SAL

**RMSE: 0.11, R squared:0.69** 

![51pozzo_sal_pred](https://user-images.githubusercontent.com/43357858/109418377-5caa0b00-79c8-11eb-8e81-e559146dc3b1.jpg)

![50auser_pozzosal_features](https://user-images.githubusercontent.com/43357858/109397034-e4ded080-7934-11eb-90af-51be7d0a2e1a.jpg)

------------------------------------------------------

**Back to the** [**top of the page**](#top) 
