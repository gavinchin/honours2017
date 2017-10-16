library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
load("data/model_est.RData")

daytype <- function(date){
  if(!is.Date(date)){stop("Object is not a date.")}
  x_day <- wday(date, label = TRUE, abbr = FALSE)
  x_daytype <- ifelse(x_day == "Sunday", "Sunday",
                ifelse(x_day == "Saturday", "Saturday",
                ifelse(x_day == "Monday", "Monday",
                ifelse(x_day == "Friday", "Friday",
                          "Midweek"))))
  return(x_daytype)
}

ped_predict <- function(pred_date = today(tzone = "Australia/Melbourne"),
                        t_hour = 12,
                        is_pub_hol = FALSE) {
  if(!(t_hour %in% seq(0, 23))){
    stop("Hour is not between 0:00 and 23:00. Please give hour value between 0 and 23")}
  if(!is.Date(pred_date)){stop("Date given to predict is not a date. Use lubridate::ymd()")}
 
  date_pred <- data.frame(Time = as.factor(t_hour)) %>%
               mutate(DayType = daytype(pred_date),
                      Month = lubridate::month(pred_date ,label = TRUE, abbr = F),
                      Date = pred_date)
  
  preds <- numeric()
  for(i in names(pred_model)){
    preds[i] <- predict(pred_model[[i]], newdata = date_pred, type = "response")
  }
  preds_df <- data.frame("fit" = preds, "Sensor_ID" = names(preds)) %>%
    spread(Sensor_ID, fit) %>% cbind(date_pred)
  
  return(preds_df)
}
