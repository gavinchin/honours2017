# Make missing values explict
library(purrr)
library(magrittr)
library(tidyr)
library(dplyr)
library(lubridate)

ped_df <- ped_df %>%
  mutate(Date_Time = dmy_hm(Date_Time))

df_time <- ped_df %>% 
  split(.$Sensor_ID) %>% 
  map(extract2, "Date_Time") %>% 
  map(full_seq, period = 3600)
sensors_ls <- ped_df %>% 
  distinct(Sensor_ID, Sensor_Name) %>% 
  arrange(Sensor_ID) %>% 
  split(.$Sensor_ID)
df_time <- map2(df_time, sensors_ls, data.frame) %>% 
  map(set_colnames, c("Date_Time", "Sensor_ID", "Sensor_Name"))

ped_full <- ped_df %>% 
  split(.$Sensor_ID) %>% 
  map2(df_time, right_join, 
       by = c("Date_Time", "Sensor_ID", "Sensor_Name")) %>% 
  bind_rows()

ped_full <- ped_full %>% 
  mutate(
    Year = year(Date_Time),
    Month = as.character(month(Date_Time, label = TRUE)),
    Mdate = day(Date_Time),
    Day = as.character(wday(Date_Time, label = TRUE)),
    Time = hour(Date_Time),
    Date = as_date(Date_Time),
    Holiday = if_else(Date %in% pub_hdays$Date, "Y", "N"),
    HDay = if_else(Holiday == "Y", "Holiday", Day)
  )

# Fit a glm model using dummies
mlevels <- unique(ped_full$Month)
dlevels <- unique(ped_full$Day)[c(6:7, 1:5)]
hdlevels <- unique(ped_full$HDay)[c(7:8, 2:6, 1)]
ped_dummy <- ped_full %>% 
  mutate(
    Year = factor(Year, levels = 2014:2016),
    Month = factor(Month, levels = mlevels),
    Day = factor(Day, levels = dlevels),
    HDay = factor(HDay, levels = hdlevels),
    Time = factor(Time, levels = 0:23)
  )

ped_list <- ped_dummy %>% 
  split(.$Sensor_ID)

ped_fit <- ped_list %>% 
  map(arrange, Date_Time) %>% 
  map(~ glm(Hourly_Counts ~ Year * HDay * Time + Month, data = .,
    family = poisson())
  )

ped_resid <- ped_df %>% 
  split(.$Sensor_ID) %>% 
  map(select, Date_Time) %>% 
  map2(ped_fit, ~ mutate(.x, Residuals = residuals(.y))) %>% 
  map2(ped_list, ~ right_join(.x, .y, by = "Date_Time")) %>% 
  bind_rows()

# Missing values index
nr_ped_list <- vapply(ped_list, nrow, integer(1))
na_idx <- ped_list %>% 
  map(~ which(is.na(.$Hourly_Counts)))
na_datetime <- ped_list %>% 
  map2(na_idx, ~ .x$Date_Time[.y])

fitted_dummy <- ped_fit %>% 
  map2(ped_list, ~ predict(.x, newdata = .y[, -1], type = "response"))

ped_dummy_aug <- ped_list %>% 
  map2(fitted_dummy, ~ mutate(.x, Fitted = .y)) %>% 
  map(~ gather(., Type, Counts, c(Hourly_Counts, Fitted), na.rm = TRUE)) %>% 
  map2(na_datetime, 
    ~ mutate(., 
        Type = factor(Type, levels = c("Hourly_Counts", "Fitted")),
        Fill = if_else(Date_Time %in% .y, "Y", "N"))) %>% 
  bind_rows() %>% 
  mutate(
    .day = case_when(
      .$Day == "Mon" ~ ymd("2016-01-04"),
      .$Day == "Tues" ~ ymd("2016-01-05"),
      .$Day == "Wed" ~ ymd("2016-01-06"),
      .$Day == "Thurs" ~ ymd("2016-01-07"),
      .$Day == "Fri" ~ ymd("2016-01-08"),
      .$Day == "Sat" ~ ymd("2016-01-09"),
      .$Day == "Sun" ~ ymd("2016-01-10")
    )
  ) %>% 
  mutate(
    .day_hour = ymd_hms(paste(.day, paste(Time, "00", "00", sep = ":")))
  ) %>% 
  mutate(
    .group_id = case_when(
      .$Year == 2015 ~ .$Week + 53,
      .$Year == 2016 ~ .$Week + 106,
      TRUE ~ .$Week
    )
  )
