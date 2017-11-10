---
title: "Modelling the City of Melbourne Pedestrian Data"
output: github_document
---

The City of Melbourne provides access pedestrian data collected from pedestrian sensors placed throughout Melbourne's CBD. Through data analysis and exploration, the aim is analyse the pedestrian behaviour at different locations in the CBD in order to predict pedestrian counts. Issues in the dataset, with missing values and false zero counts, are investigated and solutions proposed. An improved imputation method is proposed, using different models based on the proportion of missing values. For sensor locations with a small proportion of missing values used generalised linear models using only time and date based variables as predictors, while sensors with a large proportion of missings used neighbouring sensor counts as predictors. Using this imputed data, predictive models can be trained more effectively, producing better estimates. A predictive model for on-the-fly predictions was wrapped into a function to facilitate point estimates in an easy and tidy format.


