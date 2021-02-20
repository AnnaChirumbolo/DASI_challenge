## Executive Summary.

The analysis work was approached by the team by assigning each one a type of water basin, in an attempt to be able to “specialize” in the short time available on a specific water system.

Finding in the first analysis many missing values, the use of enriching the datasets using external sources was fundamental, especially for the meteorological variables. In particular, we found a good fit with the datasets provided by 3bmeteo.com.

We have adopted a very decisive approach in selecting and privileging the analysis of portions of recent datasets and with more or less continuous data, filling small gaps with weighted automatic attribution techniques (using “imputeTS” library).

The features and datasets that present behavioral anomalies, arbitrarily attributed by us to anthropic interventions without explanation, have been removed to favor an analysis as much aware of the phenomena under study.

The features engineering activity was important as we diversified the contribution of rain by diversifying the contribution between rain and snow, created precipitation bands to try to extrapolate physical behaviors such as runoff as well as considering different levels of lag for absorption. of rainfall.

We have also launched control systems for the choice of predictive models using Rapidminer as a working platform which confirmed the results obtained through packages such as AutoML.

We are therefore satisfied with the results achieved in the few days of dedicated work and we believe that with greater detail on the data, on the geophysical characteristics of the water basins and technical support with advanced 3D modeling models, we can significantly increase the results achieved.

We therefore report below the error table (RMSE) that our modeling has produced.




### Summary: 

#### <a href="#section1"> 0. Importing Datasets and Libraries </a>
#### <a href="#section2"> 1. Data Cleaning and Feature Engineering </a>
  <a href="#subsect1"><sub> 1.1 Aquifers </sub></a>
  
  <a href="#subsect2"><sub> 1.2 Water Springs </sub></a>
  
  <a href="#subsect3"><sub> 1.3 River </sub></a>
  
  <a href="#subsect4"><sub> 1.4 Lake </sub></a>
  
#### <a href="#section3"> 2. Modelling and Forecasting </a>
  <a href="#subsect5"><sub> 1.1 Aquifers </sub></a>
  
  <a href="#subsect6"><sub> 1.2 Water Springs </sub></a>
  
  <a href="#subsect7"><sub> 1.3 River </sub></a>
  
  <a href="#subsect8"><sub> 1.4 Lake </sub></a>
  
