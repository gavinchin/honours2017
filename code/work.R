

library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(knitr)
library(lubridate)
library(tidytext)
library(speedglm)

ped_df <- read_csv("data/Pedestrian_volume__updated_monthly_.csv")
  ped_df$Year <- as.factor(ped_df$Year)
  ped_df$Month <- as.factor(ped_df$Month)
  ped_df$Month <- factor(ped_df$Month, levels(ped_df$Month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
  ped_df$Time <- as.factor(ped_df$Time)
  ped_df$Timeplot <- as.integer(ped_df$Time)
  ped_df$MTime <- as.integer(ped_df$Time) + ((as.integer(ped_df$Mdate)-1) * 24)
  ped_df$Day <- as.factor(ped_df$Day)
  ped_df$Day <- factor(ped_df$Day, levels(ped_df$Day)[c(2,6,7,5,1,3,4)])

ped_loc <- read_csv("data/Pedestrian_sensor_locations.csv")

pub_hday14 <- read_csv("http://data.gov.au/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/56a5ee91-8e94-416e-81f7-3fe626958f7e/download/australianpublicholidays-201415.csv---australianpublicholidays.csv.csv")
pub_hday15 <- read_csv("http://data.gov.au/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/13ca6df3-f6c9-42a1-bb20-6e2c12fe9d94/download/australianpublicholidays-201516.csv")
pub_hday16 <- read_csv("http://data.gov.au/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/a24ecaf2-044a-4e66-989c-eacc81ded62f/download/australianpublicholidays-201617.csv")

  pub_hdays <- rbind(pub_hday14, pub_hday15, pub_hday16)
  
  pub_hdays$Date <- ymd(pub_hdays$Date)
  pub_hdays$Month <- pub_hdays$Date %>% month(label = TRUE, abbr = FALSE)
  pub_hdays$VIC <- 0
  pub_hdays$VIC[grep(glob2rx("*VIC*"), pub_hdays$`Applicable To`)] <- 1
  pub_hdays$VIC[grep("NAT", pub_hdays$`Applicable To`)] <- 1

  ped_df$HDay <- 0
  ped_df$Date <- ymd(paste(ped_df$Year, ped_df$Month, ped_df$Mdate, sep = "-"))
  
  ped_testdf <- filter(ped_df, Sensor_ID == 1)
for (i in 1:nrow(ped_testdf)){
  if (ped_testdf$Date[i] %in% pub_hdays$Date[pub_hdays$VIC == 1]) {
      ped_testdf$HDay[i] <- "Holiday"
  }
  else {ped_testdf$HDay[i] <- ped_testdf$Day[i]}
}
  
  