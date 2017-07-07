# honours2017

library(readr)
x <- read_csv("https://compedapi.herokuapp.com/api/bydatecsv/25-05-2017?", skip=8)


## to-do list  
- get rid of unncessary variables in data  
- finish code to scrape latest data and append to main dataset  
- continue with missing data imputation code:  
  - create dataframe for missing values by sensor id  
  - create GLM poisson for each sensor location  
  - append imputed values to main dataset
