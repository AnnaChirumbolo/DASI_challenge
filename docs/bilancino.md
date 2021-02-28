<br>

# Bilancino
{: #top}

### Lake

* [Data Cleaning and Feature Engineering](#datacleaning)
  * [Target variables](#target)
  * [Feature variables](#features)
  * [Feature Selection and Engineering](#engineering)
* [Modelling and Forecasting](#modelling)
  * [Choosing the best model](#choosing)
  * [Model Results](#results)

------------------------------------------------

<br>

## Data Cleaning and Feature Engineering 
{: #datacleaning}

Lake Bilancino is an **artifical waterbody** in the Muggello area, encountering the Sieve river (both inlet and outlet). 

Lake Bilancino presents five rainfall variables and one temperature variables as features, and two targets: **lake level (m)** and **flow rate (L/s)**. 

We first observed presence of missing data and decided to filter out the dataset up until the end of 2003. 

![01Bilancino_Inizio](https://user-images.githubusercontent.com/43357858/109392487-8efe2e80-791c-11eb-9f24-b05f257ea888.jpg)

### Target variables: flow rate and lake level 
{: #target}

**Mean flow rate** is very low: **2.78 L/s**. Sometimes it presents very strong and sudden peaks, soon returing back to a more constant trend. **Lake level** has very low variability, with a constant range **between 243 m and 253 m** over more than 16 years. 

![05Bilancino_target](https://user-images.githubusercontent.com/43357858/109392492-96bdd300-791c-11eb-953f-110fe4407d7b.jpg)

We observed distribution of target variables and decided to keep the outliers as they are considering that, being an artificial lake, flux alterations must have not been stochastic. 

![09box_target](https://user-images.githubusercontent.com/43357858/109392500-9de4e100-791c-11eb-837b-b7bd7283169f.jpg)

### Features: meteorological 
{: #features}

The 5 different rainfall variables range between 0 mm and 125 mm. 

![10Bilancino_rain](https://user-images.githubusercontent.com/43357858/109392520-c240bd80-791c-11eb-8624-402a37601761.jpg)

Mean temperature value is of 15 °C, it is seasonal and typical of central Italy, ranging mainly between 0 °C and 30 °C. 

![12Bilancino_temp](https://user-images.githubusercontent.com/43357858/109392523-c8cf3500-791c-11eb-8235-477efde7324c.jpg)

### Feature Selection and Engineering 
{: #engineering}

Rainfall variables are strongly correlated to one another, as expected. We decided to keep **3 of them for modelling**: S. Piero, Mangona and Cavallina localities, as these are least correlated to one another. We kept temperature there being only one variable as representative. 

![11Bilancino_correlazione](https://user-images.githubusercontent.com/43357858/109392543-e00e2280-791c-11eb-8eaf-f8ddf526dbd6.jpg)

We looked at correlations between rainfall and lake level, and found **inverse relationship**. Lake levels **fall** particularly at **end of the year**, rising again in January after the rains: we can conclude **there must be a lag of a few months for the water level to recharge**. 

![13Bilancino_LL_rainfall](https://user-images.githubusercontent.com/43357858/109392542-dbe20500-791c-11eb-93e1-13eacc9b8aed.jpg)

We carried out feature engineering and added seasons, presence/absence of snow, rainfall thresholds and time lags (+1, +3, +5, +7 days).

We looked at the distribution of the target variables over the seasons, and found that lake level is lowest during autumn, while highest in the spring. For flow rate there isn't much of difference between seasons. Outliers are present particularly in spring and autumn, but we did not do any modifications to them. 

![06Bilancino_target_season](https://user-images.githubusercontent.com/43357858/109392548-f1572f00-791c-11eb-8e2b-f5c8d59429f7.jpg)

## Modelling and Forecasting 
{: #modelling}

### Choosing the model 
{: #choosing}

From autoML and RapidMiner test for choice of model, we found that Random Forest and Gradient Boost Machine reflected the best fit. 

### Model results 
{: #results}

### Random Forest 

#### Target 1: Lake Level 

**RMSE: 1.78, 33% variance.**

Autumn has the greatest influence in the model over the target variable (as seen in the boxplot, when comparing seasons), 3 times greater than spring - the second most influential feature in the model. 

Rainfall variables seem to have the least influence on the target. 

![16Bilancino_RF_LL](https://user-images.githubusercontent.com/43357858/109328262-8d1f6700-7859-11eb-84a0-2306bc223f03.jpg)

#### Target 2: Flow Rate 

**RMSE: 4.22, 9.9% variance.**

In this case, Rainfall at S. Piero locality and temperature have the greatest influence. 

![18Bilancino_RF_flowrate](https://user-images.githubusercontent.com/43357858/109328482-c48e1380-7859-11eb-893c-6b3a810af870.jpg)

### Gradient Boost Machine 

#### Target 1: Lake Level 

**RMSE: 1.60, R squared: 0.35**

![17_0lake_level_pred](https://user-images.githubusercontent.com/43357858/109392776-434c8480-791e-11eb-8511-52b388d434a7.jpg)

Most influential feature on lake level in this model is autumn, followed by temperature (Le Croci locality) and winter. 

![25lake_level_features](https://user-images.githubusercontent.com/43357858/109392842-b9e98200-791e-11eb-9e22-ad890e7c51b1.jpg)

#### Target 2: Flow Rate 

**RMSE: 3.42, R squared: 0.46**

![23Bilancino_flowrate_pred_act](https://user-images.githubusercontent.com/43357858/109392868-d1286f80-791e-11eb-9d33-957008db3abd.jpg)

Most influential features in this model are rainfall Cavallina and Mangona, followed by temperature (Le Croci locality).

![21flowrate_features](https://user-images.githubusercontent.com/43357858/109392859-ca99f800-791e-11eb-885d-d7c2b53e0bda.jpg)

------------------------------------------------------

Head back to the [**top of the page**](#top)