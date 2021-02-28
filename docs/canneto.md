<br>

# Madonna di Canneto
{: #top}

* [Data Cleaning and Feature Engineering](#datacleaning)
  * [Cleaning](#cleaning)
  * [Feature Engineering](#engineering)
* [Modelling and Forecasting](#modelling)
  * [Choosing the best model](#choosing)
  * [Model Results](#results)

------------------------------------------------

## Data Cleaning and Feature Engineering 
{: #datacleaning}

Madonna di Canneto presented few variables, 3 features (two meteorological and time) and Flow Rate (L/s) as target. 

### Cleaning 
{: #cleaning}

From the first look at the percentage of missing data, we noticed that the target variable presented the largest % of missing variables. 

```
# A tibble: 4 x 3
  variable                     n_miss pct_miss
  <chr>                         <int>    <dbl>
1 Flow_Rate_Madonna_di_Canneto   1726   55.4  
2 Rainfall_Settefrati             556   17.9  
3 Temperature_Settefrati          556   17.9  
```

We then observed the distribution of the target variable over time to visualise where missing values were **located**.

![first_look_target_canneto](https://user-images.githubusercontent.com/43357858/108752197-73251200-7543-11eb-99e4-53a7b49c0ab4.jpg)

Effectively, the major gap in data was located in the earlier years available in the dataset, including a large gap throughout 2015. We opted to filter the dataset from the beginning of 2016 to proceed with our analyses. 

Even after removing the first gaps, the target variable still presented major areas of missing values, as shown in the image below. We proceeded with imputing the missing values with the moving average (*imputeTS* package).

![na_dist_cannetof](https://user-images.githubusercontent.com/43357858/108750159-fe50d880-7540-11eb-9b7d-d0f9e9a0aeca.jpg)

Also the features presented gaps after filtering out the dataset until 2016, which we enriched with data retrieved from [3bmeteo archive, location: Settefrati](https://www.3bmeteo.com/meteo/settefrati/storico).

So we could finally see both meteorological values in their full and continuous distribution over time. 

![panel_meteo_canneto](https://user-images.githubusercontent.com/43357858/108750164-fee96f00-7540-11eb-9fd0-ddc690ec8dd1.jpg)

Finally, we checked for outliers and found none neither in the target nor in the temperature variable. For precipitation we decided not to modify them as they were many and scattered throughout the dataset, and not showing any particularly odd behaviour. 

### Feature Engineering 
{: #engineering}

In terms of feature engineering, after calculating mean weekly rainfall values, we decided to set the new minimum threshold values at 0.8mm, 1.5mm, 2.8mm and 4.4mm. 

```
# summary of mean weekly rainfall values
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.7607  2.7857  4.3265  5.8893 29.3714 
```

![panelled_rain](https://user-images.githubusercontent.com/43357858/108883045-45080680-7605-11eb-99aa-c096ed7ed960.jpg)

The Aikaike Information Criteria (AIC) suggested the best number of lags to be of 8 days, and so we added this new feature to each new timeseries.

## Modelling and Forecasting
{: #modelling}

### Choosing the best model
{: #choosing}

After performing autoML on all datasets of Madonna di Canneto (original and featured), **GBM** resulted to be the best regression model. This coincided with a check we did on RapidMiner.

  ![varimp h2o jpg](https://user-images.githubusercontent.com/43357858/108821918-9d67e580-75be-11eb-82ec-276d1e28cbe2.png)

Instead, after performing autoTS we found tbats to be the best performance as timeseries model.

```
# Comparison of TS model errors (RMSE)
  prophet   ets sarima tbats  bats  stlm shortterm bagged
    <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>     <dbl>  <dbl>
1    32.9  2.58   7.64  1.27  2.54  6.08      2.76   3.59
```

![autoTS_res jpg](https://user-images.githubusercontent.com/43357858/108823840-431c5400-75c1-11eb-99e0-e8cb8dd83e10.png)

We also plotted predicted vs actual values for tbats.

![tbats](https://user-images.githubusercontent.com/43357858/108824227-c76ed700-75c1-11eb-90ab-e921b2c10269.jpg)

From the plot above we decided not to use tbats, but just to continue with gbm. 

### Model results 
{: #results}

#### Gradient Boost Machine 

We compared GBM error results between the five different timeseries with changing thresholds, and concluded that the best fit was for the original timseries, with no changes to the minimum rainfall values. 

We then compared RMSE results between the original timeseries and all its features, and the original timseries with feature selection made with stepwise regression. The former GBM model resulted in an RMSE of 26.05 L/s, while the latter resulted in 25.97 L/s. We thus chose the second model. 

The following graph shows the relationship between predicted and actual target values of the best model. 

![gbm_act_pred](https://user-images.githubusercontent.com/43357858/108829952-179d6780-75c9-11eb-8271-834b9b4456b1.jpg)

The model showed that temperature has the greatest influence over flow rate (50%), followed by the 8-day time lag of rainfall. 

![rel_infl gbm](https://user-images.githubusercontent.com/43357858/108829984-1f5d0c00-75c9-11eb-9920-d2bbc18bc9cc.jpg)

------------------------------------------------------

**Head back to** [**the top of the page**](#top)
