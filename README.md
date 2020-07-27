# R package for the analysis of user journey data
This repository consists of a R package and is intended as a framework for user journey analysis in the domain of digital health interventions. It includes functions for the creation of user journey data, feature creation, and an analysis wrapper.

![](assets/process.png)

## Setting and other information

* Clone this repository to your computer or download the repository.
* In the terminal, navigate to the folder where the downloaded repository is in. 
* To build package, type 'R CMD build UJ-Analysis'. 
* To install package, type 'R CMD INSTALL UJAnalysis_0.1.0.tar.gz'.
* In some cases, the dependencies must be installed manually.
* It is now possible to access the help functions such as '?reshapeData'.
* Note: The package is still in development and computational efficiency is improved.

## Included steps 

### Data transformation
Often, a first step to analyze user journey data is the tranformation of raw data into a wide format. The function 'reshapeDate' supports this task. When transforming the raw data, it is important to specify the time window defining the time interval for which individual touchpoints are aggregated. The choice of the time window depends on the density of the observations in the raw data and must be chosen by the user. If multiple observations of the same type occur within a time window, the user must also decide how to aggregate these values. The help function of 'reshapeData' includes more information and an example.

### Feature engineering
Feature engineering is often applied when using machine learning models. Generic approaches for including additional features into the data are supported by this package. Interaction terms such as the product of two original features might increase predictive performance. Binning can be used to pre-process data and reduce observation errors. Furthermore, time-window-based features, which aggregate features for a specific time-interval defined by the user have shown to produce benefits in terms of predictive performance. The help functions of 'createInteractionTerms', 'binning', and 'TimeWindow' include more information and an example.

### Statistical analysis and model evaluation
![](assets/ML.png)
