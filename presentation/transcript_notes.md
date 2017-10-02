Untitled
================

Presentation Transcript
=======================

I'm Gavin and I've been working on modelling Melbourne CBD pedestrian data.

The City of Melbourne provide an open data portal, which makes council data publicly available.
This project will focus on the pedestrian sensor data.

Currently, the City of Melbourne has 43 sensors installed around the Melbourne CBD. These sensors are positioned under an awning or street pole, and count the number of people pass through the counting zone each hour. This data is then sent to a server.

The public can access the pedestrian data a number of ways. A raw csv file can be downloaded at the official City of Melbourne open data portal website. The data available there is updated on a monthly basis.
Alternatively, the City of Melbourne also hosts an interative visualisation of the pedestrian data, which provides csv files containing data for a single day.
Using the rwalkr package by Earo, we can easily access the Melbourne pedestrian data to use for analysis. This package imports the data in tidy and easy to use format. For this project, we use the single day data provided by the visualisation site through the walk\_melb function in R. We use this dataset instead of the one available at the Open Data Portal because missing values are explicit, as well as because it is updated daily.

The data we are working with provides three main variables. The time and date, sensor id or location, and the hourly pedestrian count. The data is provided in a long format. For most of the analysis, a wide data format has been used instead, providing a single observation for each hour, and the counts for the hour at each location being a column in the data.
This reduces the number of rows in the data, and makes data manipulations, such as generating time and date based variables, a lot more efficient because we only need to do a single calculation for each hour of data rather 43 calculations for each sensor.

The objective of this project is to be able to predict how many people will pass through each sensor for a given time and date. We want to built the prediction model using 2014-2016 data to train it, leaving the current 2017 data as a test set. We have chosen not to use data prior to 2014 because of a very high proportion of missing data as many sensors were not installed before 2013.
