<br>

# Doganella 
{: #top}

### Aquifer

* [Data Cleaning and Feature Engineering](#datacleaning)
  * [Missing data](#missing)
  * [Outliers](#outliers)
* [Modelling and Forecasting](#modelling)
  * [Choosing the best model](#choosing)
  * [Model Results](#results)

------------------------------------------------

## Data Cleaning and Feature Engineering 
{: #datacleaning}

Doganella has 9 target variables for each well which characterises the aquifer, all reflecting **depth to groundwater (m)**.

### Missing data 
{: #missing}

First thing we observe of the target variables is that around **60%** of data is missing, particularly in the early years of the dataset. 

Not only the target variables, but also their features present a large percentage of missing values. In particular, volumes lack 70% of their values, followed by 30% of temperature values and 10% of rainfall values. 

For this reason, we decided to filter out the dataset for the largest gap present in well 1 (77.5%).

The largest portion remaining of missing data was found in meteorological and target variables. 

```
# A tibble: 15 x 3
   variable                n_miss pct_miss
   <chr>                    <int>    <dbl>
 1 Temperature Monteporzio   7245   59.1  
 2 Temperature Velletri      7236   59.0  
 3 Rainfall Monteporzio      3186   26.0  
 4 Rainfall Velletri         2430   19.8  
 5 Depth to groundwater      2329   19.0  
```
We first imputed the missing target values by substituting them with their dynamic average. 

![imp_vis](https://user-images.githubusercontent.com/43357858/108858042-04e85a00-75ec-11eb-9592-c3fd503832e1.jpg)

We did the same for volume variables.

![imp_volumes](https://user-images.githubusercontent.com/43357858/108874213-3bc66c00-75fc-11eb-9d1f-769f9e4564f4.jpg)

Then, we enriched meteorological data by integrating them with data we found and retrieved from [3bmeteo.com/Velletri](3bmeteo.com/meteo/velletri/storico) and [3bmeteo.com/Monteporzio](https://www.3bmeteo.com/meteo/monte+porzio+catone/storico).

![panel_meteo](https://user-images.githubusercontent.com/43357858/108879215-54855080-7601-11eb-8ef5-3ce2250fe41b.jpg)

### Outliers 
{: #outliers}

We looked at the frequency distribution of the target variables by creating boxplots. 

![boxplot_target](https://user-images.githubusercontent.com/43357858/108859183-3f9ec200-75ed-11eb-8701-de6fff7265f4.jpg)

We decided to substitute the largest outlier found in wells 2 and 8 with their respective value at Q3. We decided not to modify the remaining outliers as they were many and scattered throughout the timeseries.  

## Modelling and Forecasting
{: #modelling}

### Choosing the best model 
{: #choosing}

After performing autoML on all datasets of Doganella (original and featured), **GBM** resulted to be the best regression model, also outperforming timeseries models. 

![automl](https://user-images.githubusercontent.com/43357858/109181335-19fbef00-778c-11eb-970d-1bf53a9c1d43.png)

### Model results (Gradient Boost Machine)
{: #results}

#### Well 1 

In well 1, the best fit resulted to be in the engineered timeseries, where the minimum rainfall threshold was set to **0.5 mm**. **RMSE: 2.94, R squared: 0.83**

![1_act_vs_pred](https://user-images.githubusercontent.com/43357858/109396253-0047dc80-7931-11eb-9fd9-66fecea78fe5.jpg)

The most influential feature in the model is volume, followed by temperature. 

![1_rel_infl gbm](https://user-images.githubusercontent.com/43357858/109396254-00e07300-7931-11eb-9af2-847dd3262970.jpg)

#### Well 2 

In well 2, the best result was in the timeseries where minimum rainfall threshold was set to **2.83 mm**. **RMSE: 1.04, R squared: 0.87**

![2_act_vs_pred](https://user-images.githubusercontent.com/43357858/109396256-00e07300-7931-11eb-87c9-6a6320b415ad.jpg)

Most influential feature is volume, followed by **the 5-day time lag**. 

![2_rel_infl gbm](https://user-images.githubusercontent.com/43357858/109396257-01790980-7931-11eb-94ed-44d333694794.jpg)

#### Well 3 

In well 3, the best result was in the timeseries where minimum rainfall threshold was set to **1.5 mm**. **RMSE: 1.59, R squared: 0.77**

![3_act_vs_pred](https://user-images.githubusercontent.com/43357858/109396258-01790980-7931-11eb-8dc8-76d96310ec81.jpg)

Most influential feature is volume, followed by temperature and 1-day lag. 

![3_rel_infl gbm](https://user-images.githubusercontent.com/43357858/109396259-01790980-7931-11eb-88dd-d9b49bf727a4.jpg)

#### Well 4 

In well 4, the best result was in the **original timeseries**, where no threshold was set. **RMSE: 0.52, R squared: 0.85**

![4_act_vs_pred](https://user-images.githubusercontent.com/43357858/109396261-0211a000-7931-11eb-9d9e-b6b630a82bad.jpg)

Most influential feature is volume, followed by temperature and 1-day lag. 

![4_rel_infl gbm](https://user-images.githubusercontent.com/43357858/109396262-0211a000-7931-11eb-9c53-d9c016bc41c7.jpg)

#### Well 5 

In well 5, the best result was in the **original timeseries**, where no threshold was set. **RMSE: 1.12, R squared: 0.81**

![5_act_vs_pred](https://user-images.githubusercontent.com/43357858/109396263-02aa3680-7931-11eb-9b2a-d77ae1f7f2a9.jpg)

Most influential feature is volume, followed by temperature and **10-day lag**. 

![5_rel_infl gbm](https://user-images.githubusercontent.com/43357858/109396264-02aa3680-7931-11eb-88a5-67ffd11f7966.jpg)

#### Well 6 

In well 6, the best result was in the timeseries with **minimum rainfall threshold set at 0.5 mm**. **RMSE: 1.00, R squared: 0.79**. 

![6_act_vs_pred](https://user-images.githubusercontent.com/43357858/109396267-02aa3680-7931-11eb-9e6a-f7ea2b991195.jpg)

Most influential feature is volume, followed by temperature and **spring**.

![6_rel_infl gbm](https://user-images.githubusercontent.com/43357858/109396268-0342cd00-7931-11eb-9d8d-8c03058b840a.jpg)

#### Well 7

In well 7, the best result was in the timeseries with **minimum rainfall threshold set at 1.5 mm**. **RMSE: 0.71, R squared: 0.48**

![7_act_vs_pred](https://user-images.githubusercontent.com/43357858/109396270-0342cd00-7931-11eb-9b84-7b0ffecedc7a.jpg)

Most influential feature is volume, followed by temperature and **rain (featured with the new threshold: 1.5 mm)**.

![7_rel_infl gbm](https://user-images.githubusercontent.com/43357858/109396271-0342cd00-7931-11eb-830a-7ccbe7d9a771.jpg)

#### Well 8 

In well 8, the best result was in the timeseries with **minimum rainfall threshold set at 1.5 mm**. **RMSE: 0.77, R squared: 0.82**

![8_act_vs_pred](https://user-images.githubusercontent.com/43357858/109396273-03db6380-7931-11eb-9d42-e96e2393f0cd.jpg)

Most influential feature is volume, followed by **autumn** and temperature. 

![8_rel_infl gbm](https://user-images.githubusercontent.com/43357858/109396274-03db6380-7931-11eb-942b-67e73919b776.jpg)

#### Well 9 

In well 9, the best result was in the **original timeseries**, where no minimum rainfall threshold was set. **RMSE: 2.68, R squared: 0.76**

![9_act_vs_pred](https://user-images.githubusercontent.com/43357858/109396276-0473fa00-7931-11eb-8389-2717f1d189be.jpg)

Most influential feature is volume, followed by temperature and **1-day lag**.

![9_rel_infl gbm](https://user-images.githubusercontent.com/43357858/109396251-ffaf4600-7930-11eb-85dd-04c9144c07aa.jpg)


-------------------------------------------

Head back to the [**top of the page**](#top)