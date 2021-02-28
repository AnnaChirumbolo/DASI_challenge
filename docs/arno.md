<br>

# River Arno 
{: #top}

* [Data Cleaning and Feature Engineering](#datacleaning)
  * [Target variable](#target)
  * [Feature variables](#features)
  * [Feature selection](#selection)
  * [Feature engineering](#engineering)
* [Modelling and Forecasting](#modelling)
  * [Choosing the best model](#choosing)
  * [Model Results](#results)

------------------------------------------------

## Data Cleaning and Feature Engineering 
{: #datacleaning}

River Arno consisted of many feature variables collected by monitors which ran from the source of the river down to the valley. 

We first had an initial look at the dataset at our disposal and we noticed straight away a large gap of data in the earliest  and also in the latest years, particularly for rainfall variables and for temperature measured in Florence.

![01River_Arno_Inizio](https://user-images.githubusercontent.com/43357858/109392231-442fe700-791b-11eb-958b-cf847e7e7138.jpg)

Here we present a more detailed quantification of missing values, after filtering out the early years with the gap. 

```
# A tibble: 17 x 3
variable                     n_miss      pct_miss
  <chr>                      <int>        <dbl>
1 Rainfall_Stia               4743         78.7
2 Rainfall_Consuma            4743         78.7
3 Rainfall_S_Savino           4743         78.7
4 Rainfall_Laterina           4743         78.7
5 Rainfall_Camaldoli          4743         78.7
6 Rainfall_Montevarchi        4379         72.7
7 Rainfall_Bibbiena           3648         60.5
8 Rainfall_Vernio             1743         28.9
9 Rainfall_Incisa             1458         24.2
10 Temperature_Firenze        1082         18.0
11 Hydrometry_Nave_di_Rosano    3          0.05
```

### Target variable: hydrometry 
{: #target}

Concerning the target variable, **hydrometry (m)**, we found few missing data scattered throughout the timeseries, which we imputed with the moving average. 

![13Hydrometry_Nave_di_Rosano_missing](https://user-images.githubusercontent.com/43357858/109392273-85c09200-791b-11eb-9bfd-63809397bd83.jpg)

Hydrometry represents the level of the subterrean waters monitored from the hydrometric station. 

Observing the graphs, we noticed its seasonal behaviour, with the highest peaks showing between Novermber and May. There are some sudden drops at 0, as of drought, which straight away go back to the seasonal trend. 

### Feature variables: rainfall and temperature 
{: #features}

#### Temperature 

Temperature measured around Florence area is the only variable available for this meteorological feature. It is characterised by its typical seasonal variations, and the majority of its values lies between 0 and 30 degrees C, in line with typical central-Italy values. 

This graph shows the distribution of missing data in the **temperature** variable (Florence).

![12Temperature_Firenze_missing](https://user-images.githubusercontent.com/43357858/109392286-907b2700-791b-11eb-8679-2075caa92827.jpg)

#### Rainfall

It was 9 localities, in particular, which presented very large gaps in the latest years, up to 2020, for **rainfall**.

![11_1All_rain_missing](https://user-images.githubusercontent.com/43357858/109392300-9cff7f80-791b-11eb-8795-0353027f8dbc.jpg)

While **only 5 localities out of 14** had no missing data. 

![11_2All_rain_NOmissing](https://user-images.githubusercontent.com/43357858/109392304-a38df700-791b-11eb-9c5c-24093c2f87d6.jpg)

We **could not have used these variables as they were**, so we enriched and filled all the gaps by retrieving data from [3bmeteo.com](https://www.3bmeteo.com/meteo/ora/storico), **from 2011 until 2020**.

Even then, there were data that we could not access, for the years previous 2011. We thus decided to further **filter out** data up to 2011, before being able to continue.

![14River_Arno_cut_fewmissing](https://user-images.githubusercontent.com/43357858/109392309-ab4d9b80-791b-11eb-94d6-c90c51166a38.jpg)

### Feature selection 
{: #selection}

We started doing some feature selection, to prepare the dataset for modelling. 

As we assumed, most rainfall variables were strongly and positively correlated with one another, while, on the opposite, rainfall and temperature variables were strongly and negatively correlated. Hydrometry is inversely correlated to temperature. 

We noticed that rainfall variables could be subdivided into two groups: one containing 6 of the variables whose geographical location is found where the major river's affluent, the Silve river, runs. In the second group, we found the remaining 8 variables, located from the source of the river. Most interestingly, we noticed that the variables **within the group were strongly correlated with one another**, while **poorly correlated between groups**. 

Thus, we decided to input few representatives from each group into the final model.

![17Correlation_matrix](https://user-images.githubusercontent.com/43357858/109143545-7517ec80-7760-11eb-9428-c0f111f974d1.jpg)

### Feature engineering 
{: #engineering}

We added seasons as features and observed how target values were distributed by season. From this, we could infer that **seasons** explained little of the target variable - few variations in the median were observed in spring and winter, but neglectable. 

![21target+seasons](https://user-images.githubusercontent.com/43357858/109392345-ddf79400-791b-11eb-9550-640523dc7f18.jpg)

We then set new minimum thresholds of rainfall at 0.5 mm, 1.5 mm, 3 mm and 5 mm, and we added time lags based on the distance from the pluviometric sensors to the hydrometric sensor. 

We divided the localities in three groups. 

For the first group (Incisa, Consuma, Montevarchi, S. Plero), we did not add a lag, as the sensors were less than 50 km distant. 

For the second group (Laterina, S. Agata, Le Croci, Cavallina, Mangona) we added a time lag of 1 day, as they are up to 100 km distant. 

For localities at a greater distance than 100 km (Stia, Camaldoli, Bibbiena, S. Avinro, Vernio) we added a 2-day time lag. 

## Modelling and Forecasting 
{: #modelling}

### Model selection 
{: #choosing}

We carried out Random Forest and Gradient Boost Machine tests, especially after carrying out test with autoML packages and RapidMiner.

```
# Comparing model errors

    RandomForest                 RMSE                 
test 1 (Le Croci/Stia)           0.48
test 2 (Cavallina/Bibbiena)      0.48
test 3 (lags)                    0.37

    Gradient Boost Machine       RMSE
test 1 (lags)                    0.34
```

### Model results 
{: #results} 

#### Random Forest 

The best RF model was number 3, with the added time lag variables. 

![25RF_LAG_037](https://user-images.githubusercontent.com/43357858/109146846-9b3f8b80-7764-11eb-9e5c-e60628f8b9d6.jpg)

#### Gradient Boost Machine 

The gbm model outperformed all others (**RMSE: 0.34, R squared: 0.45**), also done with RF.

![27_2Hydrometry_Nave_di_Rosano_pred_act](https://user-images.githubusercontent.com/43357858/109416873-0eddd480-79c1-11eb-8d22-11cafcbc128a.jpg)

Here we show the top features with descenting order of relative importance in explaining the target variable. 

![27Variabili_LAG_GB_thebest_03385](https://user-images.githubusercontent.com/43357858/109392410-2b740100-791c-11eb-8076-3a974e77828e.jpg)

Also another test done with Le Croci, Cavallina localities (for the first group), Bibbiena and Stia (for the second group) resulted in a lower **RMSE** compared to RF results: **0.34**. 

![29_2Hydrometry_Nave_di_Rosano_pred_act](https://user-images.githubusercontent.com/43357858/109417595-9bd65d00-79c4-11eb-8145-f7ba47e072ca.jpg)

Here we shot the top 6 features with descending order of relative importance in explaining the target variable. 

![29Variabili_GB1](https://user-images.githubusercontent.com/43357858/109392414-3169e200-791c-11eb-87c3-e6b87289dfc1.jpg)

Third model: 

![33_2Hydrometry_Nave_di_Rosano_pred_act](https://user-images.githubusercontent.com/43357858/109417628-b90b2b80-79c4-11eb-802d-a684be682c70.jpg)

![31Variabili_GB2](https://user-images.githubusercontent.com/43357858/109417632-bb6d8580-79c4-11eb-8164-387fad3f99a5.jpg)

------------------------------------------------------

**Head back to** [**the top of the page**](#top)