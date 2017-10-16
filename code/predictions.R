library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(roxygen2)
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
#' Predicted pedestrian counts at various sensor locations in Melbourne
#'
#' @param pred_date input date
#' @param t_hour input integer
#' @param is_pub_hol input logical
#' @return data frame of the fitted values at each location in wide format, with time and date
#' @export
#' @examples
#' ped_predict()
#' ped_predict("2017-12-25", 18, TRUE)
ped_predict <- function(pred_date = today(tzone = "Australia/Melbourne"),
                        t_hour = 12,
                        is_pub_hol = FALSE) {
  if(!(t_hour %in% seq(0, 23))){
    stop("Hour is not between 0:00 and 23:00. Please give hour value between 0 and 23")}
  pred_date <- ymd(pred_date)
  if(!is.Date(pred_date)){
    stop(paste("Date given to predict cannot be parsed as a date. Use the date format 'yyyy-mm-dd'. Example: 25 January 2018 is '2018-01-25'."))
    }
 
  date_pred <- data.frame(Time = as.factor(t_hour)) %>%
               mutate(DayType = daytype(pred_date),
                      Month = lubridate::month(pred_date ,label = TRUE, abbr = F),
                      Date = pred_date)
  
  preds <- numeric()
  for(i in names(pred_model)){
    preds[i] <- predict(pred_model[[i]], newdata = date_pred, type = "response")
  }
  preds_df <- data.frame("fit" = preds, "Sensor_ID" = names(preds)) %>%
    spread(Sensor_ID, fit) %>% cbind(date_pred) %>% as_tibble
  
  return(preds_df)
}

ped_predict_day <- function(pred_date = today(tzone = "Australia/Melbourne"),
                            is_pub_hol = FALSE){
  day_df <- data.frame()
      for(t in 0:23){
        t_preds <- ped_predict(pred_date, t_hour = t, is_pub_hol)
        suppressWarnings(day_df <- bind_rows(day_df, t_preds))
      }
  return(as.tibble(day_df))
}
