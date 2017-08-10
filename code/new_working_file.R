library(lubridate)
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(rwalkr)

ped_df <- read_csv("data/ped_df.csv")
ped_df$X1 <- NULL
ped_df$Date_Time <- ymd_h(paste(ped_df$Date, ped_df$Time))
ped_df$Day <- wday(ped_df$Date, label = TRUE, abbr = FALSE)


glimpse(ped_df)


ggplot(ped_df %>% filter(Sensor == "State Library")) +
  geom_line(aes(y = Count, x = Date_Time))

    
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
    
    hday_14_16 <- data.frame()
    
    dates1 <- seq(as.Date(head(ped_df$Date, 1)), as.Date(tail(ped_df$Date, 1)), by = "day")
    hdayvals <- rep(0, length(dates1))
    hdayvals[dates1 %in% pub_hdays$Date[pub_hdays$VIC == 1]] <- 1
    dates2 <- as.Date(rep(dates1, each = 43*24))
    hdayvals2 <- as.numeric(rep(hdayvals, each = 43*24))
    
    ped_df$IsHDay <- hdayvals2
    ped_df$HDay <- ped_df$Day
    levels(ped_df$HDay) <- c(levels(ped_df$Day), "Holiday")
        ped_df$HDay[ped_df$IsHDay == 1] <- "Holiday"
    
    
    
    
    