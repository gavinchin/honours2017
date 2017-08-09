library(lubridate)
library(tidyverse)
library(ibdreg)
library(readr)
library(dplyr)
library(tidyr)
library(rwalkr)

ped_df <- read_csv("data/ped_df.csv")
ped_df$X1 <- NULL
ped_df$Date_Time <- ymd_h(paste(ped_df$Date, ped_df$Time))
ped_df$Day <- wday(ped_df$Date, label = TRUE)


glimpse(ped_df)


ggplot(ped_df %>% filter(Sensor == "State Library")) +
  geom_line(aes(y = Count, x = Date_Time))
