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

## [0. Importing libraries and datasets](docs/0.md)

## [1. Data cleaning and feature engineering](docs/1.md)
  
## [2. Modelling and forecasting](docs/2.md)
