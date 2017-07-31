## updating pedestrian data between monthly updates
library(lubridate)
library(ibdreg)
library(readr)
library(dplyr)
library(tidyr)
ped_df <- read_csv("data/Pedestrian_volume__updated_monthly_.csv")

  last_date <- parse_date(tail(ped_df$Date_Time, 1), "%d-%b-%Y %H:%M")
  last_today_date <- as.Date(today()-1, "%d-%m-%y")

n_sensors <- length(unique(ped_df$Sensor_ID))
new_ped_df <- tail(ped_df, 1) %>% select(Day, Year, Month, Time, Sensor_Name, Hourly_Counts)

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
    
    x_wide <- gather(x, key = Time, value = Hourly_Counts, 2:25, factor_key = TRUE)
    x_wide$Day <- wday(i, label = TRUE, abbr = FALSE)
    x_wide$Year <- year(i)
    x_wide$Month <- month(i, label = T, abbr = F)
    x_wide$Sensor_Name <- x_wide$Sensor
    x_wide <- x_wide[, -1]
    
    
    new_ped_df <- rbind(new_ped_df, x_wide)
   }
## function to convert string time to hour integer
time_name_to_hour <- function(x)
  {
  n <- length(x)
  new_x <- rep(0, n)
  for (i in seq(1,n)) {
    
    if (x[i] == "Midnight") {
      new_x[i] <- 0
    }
    if (x[i] == "1am") {
      new_x[i] <- 1
    }
    if (x[i] == "2am") {
      new_x[i] <- 2
      }
    if (x[i] == "3am") {
      new_x[i] <- 3
      }
    if (x[i] == "4am") {
      new_x[i] <- 4
      }
    if (x[i] == "5am") {
      new_x[i] <- 5
    }
    if (x[i] == "6am") {
      new_x[i] <- 6
    }
    if (x[i] == "7am") {
      new_x[i] <- 7
    }
    if (x[i] == "8am") {
      new_x[i] <- 8
    }
    if (x[i] == "9am") {
      new_x[i] <- 9
    }
    if (x[i] == "10am") {
      new_x[i] <- 10
    }
    if (x[i] == "11am") {
      new_x[i] <- 11
    }
    if (x[i] == "12pm") {
      new_x[i] <- 12
    }
    if (x[i] == "1pm") {
      new_x[i] <- 13
    }
    if (x[i] == "2pm") {
      new_x[i] <- 14
    }
    if (x[i] == "3pm") {
      new_x[i] <- 15
    }
    if (x[i] == "4pm") {
      new_x[i] <- 16
    }
    if (x[i] == "5pm" ){
      new_x[i] <- 17
    }
    if (x[i] == "6pm") {
      new_x[i] <- 18
    }
    if (x[i] == "7pm") {
      new_x[i] <- 19
    }
    if (x[i] == "8pm") {
      new_x[i] <- 20
    }
    if (x[i] == "9pm") {
      new_x[i] <- 21
    }
    if (x[i] == "10pm") {
      new_x[i] <- 22
    }
    if (x[i] == "11pm") {
      new_x[i] <- 23
    }

  }
  
  return(new_x)
  
}

new_ped_df$Time <- time_name_to_hour(new_ped_df$Time)

