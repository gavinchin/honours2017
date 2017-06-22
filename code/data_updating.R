## updating pedestrian data between monthly updates
library(lubridate)
library(ibdreg)
library(readr)
library(dplyr)
ped_df <- read_csv("data/Pedestrian_volume__updated_monthly_.csv")

  last_date <- sr.strsplit(tail(ped_df$Date_Time, 1), " ", drop = TRUE)
  last_today_date <- as.Date(today()-1, "%d-%m-%y")
  last_date_day <- mday(today())

##x <- read_csv(sprintf("https://compedapi.herokuapp.com/api/bydatecsv/%s?", last_today_date), skip=8)
