## updating pedestrian data between monthly updates
library(lubridate)
library(ibdreg)
library(readr)
library(dplyr)
ped_df <- read_csv("data/Pedestrian_volume__updated_monthly_.csv")

  last_date <- parse_date(tail(ped_df$Date_Time, 1), "%d-%b-%Y %H:%M")
  last_today_date <- as.Date(today()-1, "%d-%m-%y")
x <- 0  
n_sensors <- length(unique(ped_df$Sensor_ID))
  for (i in as.list(seq(last_date, last_today_date, by = "day"))){
    if (mday(i) < 10) {
      i_date <- paste(0, mday(i), sep = "")
    }
      else {
      i_date <- as.character(mday(i))
      }
    if (month(i) < 10) {
      i_month <- paste(0, month(i), sep = "")
                        }
      else {
      i_month <- as.character(month(i))
      }
    i_year <- year(i)
    last_day_date <- paste(i_date, i_month, i_year, sep ="-")
    url_to_scrape <- sprintf("https://compedapi.herokuapp.com/api/bydatecsv/%s?", last_day_date)
    x <- read_csv(url_to_scrape, skip = 8, n_max = n_sensors)
    
  }
