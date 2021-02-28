<br>

# Luco 
{: #top}

### Aquifer

* [Data Cleaning and Feature Engineering](#datacleaning)
  * [Target variable](#target)
  * [Feature variables](#features)
  * [Feature selection and engineering](#selection)
* [Modelling and Forecasting](#modelling)
  * [Choosing the best model](#choosing)
  * [Model Results](#results)

------------------------------------------------

## Data Cleaning and Feature Engineering 
{: #datacleaning}

Luco aquifer presents 4 target variables for each of the 4 wells, reflecting **depth to groundwater (m)**.

Large data gap found particularly in the first years of the timeseries, both for meteorological and target variables.

![Immagine1](https://user-images.githubusercontent.com/43357858/109385146-f652b900-78f1-11eb-8efd-ba468e1b11af.png)

### Target variable: depth to groundwater (m)
{: #target}

We visualised target value distribution over the years, for the 4 wells. The initial data gap is clearly evident. For this reason we filtered out the dataset up until 2016.

In addition, Podere Casetta lacks data in the more recent years, which are too important for modelling and forecasting and could not be retrieved from other sources. Also well 4 presents variations which can hardly be attributed to natural and stochastic phenomena, and rather indicate dependency to anthropogenic and artificial factors that we could not identify from the dataset. We therfore decided to **remove those targets from our analysis**.

![plot1](https://user-images.githubusercontent.com/43357858/109385301-1b93f700-78f3-11eb-9b46-60d4fba70871.jpg)

We proceeded with the analysis on target variables from wells 1 and 3, as they present up to date and coherent data with respect to their waterbody. 

![plot_pozzi1e3](https://user-images.githubusercontent.com/43357858/109385327-3fefd380-78f3-11eb-9235-bc9dc4841783.jpg)

These wells still present few data gaps over the timeseries, as you can see below. 

Well 1 (no abs. value):

![na_pozzo1](https://user-images.githubusercontent.com/43357858/109385367-93622180-78f3-11eb-8067-802de77eabf2.jpg)

Well 2 (no abs. value):

![na_pozzo2](https://user-images.githubusercontent.com/43357858/109385368-93fab800-78f3-11eb-8c2f-9ad0e932591b.jpg)

We filled those gaps by imputing their moving average. You can see the result below (no abs. value):

![no_na](https://user-images.githubusercontent.com/43357858/109415295-25cbf900-79b8-11eb-9eee-be6cda217ec9.jpg)

#### Outliers 

We found outliers in well 1.

![out_target](https://user-images.githubusercontent.com/43357858/109385535-d96bb500-78f4-11eb-91ba-b4fb1eb85be8.jpg)

Thus, we decided to substitute them with the equivalent of the upper quartile value in the distribution. 

![no_out](https://user-images.githubusercontent.com/43357858/109385536-da044b80-78f4-11eb-8298-abc1e5ad9cb0.jpg)

### Features: meteorological 
{: #features}

Features did not present any missing values, nor outliers to take into consideration. So we left them as they were. 

### Feature selection and engineering 
{: #selection}

We created a correlation matrix between all variables in the dataset, finding a strong correlation within and between rainfall and temperature values (as expected).

![corrplot](https://user-images.githubusercontent.com/43357858/109385567-10da6180-78f5-11eb-9dd5-63474fb6f609.jpeg)

From the meteorological variables, we decided to keep temperature from Mensano locality and rainfall from Mensano and Montalcinello localities. 

We engineered seasons, absence/presence of snow, set new minimum rainfall thresholds at 5 mm, 10 mm, 15 mm and 30 mm, and added +1, +3, +5, +6, +7, +9 days of lag between rainfall and target variables. 

## Modelling and Forecasting 
{: #modelling}

### Choosing the best model 
{: #choosing}

We prepared a pipeline test on RapidMiner to compare ML and DL models for both target variables. 

For well 1 we found Gradient Boost Machine to be the best performing model (smallest RMSE: 0.16). This was also confirmed by the other autoML model run on R. 

![RM_Luco_imp1](https://user-images.githubusercontent.com/43357858/109385711-2d2ace00-78f6-11eb-800e-d6dc7ba5b691.png)

Also for well 3 we found Gradient Boost Machine as the best performing model (RMSE: 0.32). This was confirmed by the autoML test run on R. 

![RM_Luco_imp3](https://user-images.githubusercontent.com/43357858/109385714-2ef49180-78f6-11eb-965c-98ae1ea8ffee.png)

### Model Results: Gradient Boost Machine 
{: #results}

#### Target 1 (well 1): depth to groundwater (m)

From our GBM run on R, we obtained an **RMSE of 0.14 and R squared of 0.83**, with volume being the most influential feature on the target variable, followed by temperature. 

![FI_imp1](https://user-images.githubusercontent.com/43357858/109385787-afb38d80-78f6-11eb-9604-b1be4d1af044.jpeg)

And the following graph between predicted and actual target values. 

![RM_Luco_imp1_PC](https://user-images.githubusercontent.com/43357858/109385790-b17d5100-78f6-11eb-8199-73e14204d843.png)

#### Target 2 (well 3): depth to groundwater (m)

From our GBM we obtained **RMSE: 0.25 and R squared of 0.70**, with volume as the most influential variable on the target, followed, again by temperature. 

![FI_imp3](https://user-images.githubusercontent.com/43357858/109385789-b0e4ba80-78f6-11eb-8860-0239fcd2ddf0.jpeg)

And the following graph correlating predicted with actual target values. 

![RM_Luco_imp3_PC](https://user-images.githubusercontent.com/43357858/109385791-b215e780-78f6-11eb-9aa5-9631ca477ecf.png)

--------------------------------------------------

**Head back to** [**the top of the page**](#top)