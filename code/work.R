
library(tidyverse)
library(purrr)
library(magrittr)
library(lubridate)
library(knitr)
library(lubridate)
library(tidytext)
library(speedglm)
library(SpatioTemporal)

ped_df <- read_csv("data/Pedestrian_volume__updated_monthly_.csv")

  ped_df$Month <- as.factor(ped_df$Month)
  ped_df$Month <- factor(ped_df$Month, levels(ped_df$Month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
  ped_df$Time <- as.factor(ped_df$Time)
  ped_df$Day <- as.factor(ped_df$Day)
  ped_df$Day <- factor(ped_df$Day, levels(ped_df$Day)[c(2,6,7,5,1,3,4)])

ped_loc <- read_csv("data/Pedestrian_sensor_locations.csv")

for (i in 1:dim(ped_loc)[1])
{
  ped_df$Lat[ped_df$Sensor_ID == ped_loc$`Sensor ID`[i]] <- ped_loc$Latitude[i]
  ped_df$Lon[ped_df$Sensor_ID == ped_loc$`Sensor ID`[i]] <- ped_loc$Longitude[i]
}

##############################################################################
## generate variable for day of the week/public holiday factor, HDay

pub_hday14 <- read_csv("http://data.gov.au/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/56a5ee91-8e94-416e-81f7-3fe626958f7e/download/australianpublicholidays-201415.csv---australianpublicholidays.csv.csv")
pub_hday15 <- read_csv("http://data.gov.au/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/13ca6df3-f6c9-42a1-bb20-6e2c12fe9d94/download/australianpublicholidays-201516.csv")
pub_hday16 <- read_csv("http://data.gov.au/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/a24ecaf2-044a-4e66-989c-eacc81ded62f/download/australianpublicholidays-201617.csv")

  pub_hdays <- rbind(pub_hday14, pub_hday15, pub_hday16)
  
  pub_hdays$Date <- ymd(pub_hdays$Date)
  pub_hdays$Month <- pub_hdays$Date %>% month(label = TRUE, abbr = FALSE)
  pub_hdays$VIC <- 0
  pub_hdays$VIC[grep(glob2rx("*VIC*"), pub_hdays$`Applicable To`)] <- 1
  pub_hdays$VIC[grep("NAT", pub_hdays$`Applicable To`)] <- 1

  vic_p_hday <- filter(pub_hdays, VIC == 1)
  
  ped_df$HDay <- 0
  ped_df$Date <- ymd(paste(ped_df$Year, ped_df$Month, ped_df$Mdate, sep = "-"))
  ped_df$Year <- as.integer(ped_df$Year)
  
  ped_testdf <- filter(ped_df, Sensor_ID == 1, Year > 2013, Year < 2017)
for (i in 1:nrow(ped_testdf)){
  if (ped_testdf$Date[i] %in% pub_hdays$Date[pub_hdays$VIC == 1]) {
      ped_testdf$HDay[i] <- "Holiday"
  }
  else {ped_testdf$HDay[i] <- ped_testdf$Day[i]}
  
}
####
## earo's code
  # Make missing values explict

  ped_df <- read_csv("data/Pedestrian_volume__updated_monthly_.csv")
  
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
  
  ped_list <- ped_dummy %>% filter(Date > "2013-12-31") %>%
    split(.$Sensor_ID)
  
  fit_test <- ped_list$`2`
### 20 seconds to run single GLM for sensor id 1, without filtering < 2014
  # 7 seconds if filter for only use post-2014 data
system.time(fitglm <- glm(Hourly_Counts ~ Month + HDay*Time, data = fit_test, family = 'poisson'))
  
# not working
# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
# contrasts can be applied only to factors with 2 or more levels
# Timing stopped at: 2008.54 70.098 2219.358 
# system.time(ped_fit <- ped_list %>% 
#   map(arrange, Date_Time) %>% 
#   map(~ glm(Hourly_Counts ~ Year * HDay * Time + Month, data = .,
#             family = poisson()))
#   )

## working, only takes 387 seconds

ped_fit <- list()
system.time(for (i in 1:43) {
  fit_test <- ped_list[[i]]
  ped_fit[[i]] <- glm(Hourly_Counts ~ Month + HDay*Time, data = fit_test, family = 'poisson')
})


# Error in .f(.x[[i]], ...) : object 'Date_Time' not found

  ped_resid <- ped_df %>% filter(Date_Time > "2013-12-31") %>% split(.$Sensor_ID) %>%
              map(select, Date_Time) %>%
              map2(ped_fit, ~ mutate(.x, Residuals = residuals(.y))) %>%
              map2(ped_list, ~ right_join(.x, .y, by = "Date_Time")) %>%
              bind_rows()

ped_resid <- list()
# ped_list %>% map(select_(Date_Time)) %>% 
#   map2(ped_fit, ~ mutate(.x, Residuals = residuals(.y))) %>%
#   map2(ped_list, ~ right_join(.x, .y, by = "Date_Time")) %>%
#   bind_rows()
# )

for (i in 1:43) {
  ped_list[[i]] %>% select_("Date_Time") %>% map2(ped_fit[[i]], ~ mutate(.x, Residuals = residuals(.y)))
}

nr_ped_list <- vapply(ped_list, nrow, integer(1))
na_idx <- ped_list %>% 
  map(~ which(is.na(.$Hourly_Counts)))
na_datetime <- ped_list %>% 
  map2(na_idx, ~ .x$Date_Time[.y])









