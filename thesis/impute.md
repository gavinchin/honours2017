## Imputation Literature Review Notes 
#### IMPUTATION OF MISSING CLASSIFIED TRAFFIC DATA DURING WINTER SEASON - _Hyuk-Jae Roh, Satish Sharma, Prasanta K. Sahu_  

http://ir.lib.uwo.ca/cgi/viewcontent.cgi?article=1250&context=csce2016  
- traffic counters with 40% to 60% missing values
- heuristic methods
  - using "good historical values" or historical average values resulted
    in MARE (mean absolute relative error) reaching up to 80%
  - using moving average values worked better than other heuristic methods
  - all heuristic methods inhenrently cannot reflect sudden fluctuations during abnormal periods
- pattern matching methods
  - find "candidate" volume pattern to compare to "study curve" and use the candidate which best matches the study curve
- ARIMA
  - did not work well with multiple seasonalities
- AI methods
  - genetic algorithms (GAs) and artificial neural networks (ANNs)
 
Method used in the study was kNN with an interaction using a weather model, which they found to work best with large variations in traffic during winter
(severe weather conditions affecting traffic volume)  

#### Flexible and Robust Method for Missing Loop Detector Data Imputation - _K. C. Henrickson, Y. Zou, and Y. Wang_  

http://docs.trb.org/prp/15-5804.pdf  
- also use kNN

#### Key Factors of K-nearest Neighbors Nonparametric Regression in Short-time Traffic Flow Forecasting
http://www.springer.com/cda/content/document/cda_downloaddocument/9789462391017-c2.pdf?SGWID=0-0-45-1492476-p177147492


#### K-NEAREST NEIGHBOR ALGORITHM FOR UNIVARIATE TIME SERIES PREDICTION

http://webbut.unitbv.ro/BU2012/Series%20III/BULETIN%20III/Sasu%20A.pdf
