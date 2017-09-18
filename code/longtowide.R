
library(tidyverse)
library(lubridate)
library(foreach)
library(doSNOW)

##############################################
ped_data <- read_csv("data/ped_df.csv")
ped_data$X1 <- NULL

ped_data$Sensor_ID <- ped_data$Sensor
ped_data$Hourly_Counts <- ped_data$Count
ped_data$Date_Time <- ymd_h(paste(ped_data$Date, ped_data$Time, sep = " "))
ped_data$Day <- lubridate::wday(ped_data$Date_Time, label = TRUE, abbr = F)
ped_data$Month <- lubridate::month(ped_data$Date_Time, label = TRUE, abbr = F)
ped_data$Time <- as.factor(ped_data$Time)

pub_hday14 <- read.csv("http://data.gov.au/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/56a5ee91-8e94-416e-81f7-3fe626958f7e/download/australianpublicholidays-201415.csv---australianpublicholidays.csv.csv")
pub_hday15 <- read.csv("http://data.gov.au/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/13ca6df3-f6c9-42a1-bb20-6e2c12fe9d94/download/australianpublicholidays-201516.csv")
pub_hday16 <- read.csv("http://data.gov.au/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/a24ecaf2-044a-4e66-989c-eacc81ded62f/download/australianpublicholidays-201617.csv")
pub_hdays <- rbind(pub_hday14, pub_hday15, pub_hday16)

pub_hdays$Date <- ymd(pub_hdays$Date)
pub_hdays$Month <- lubridate::month(pub_hdays$Date, label = TRUE, abbr = F)
pub_hdays$VIC <- 0
pub_hdays$VIC[grep(glob2rx("*VIC*"), pub_hdays$`Applicable To`)] <- 1
pub_hdays$VIC[grep("NAT", pub_hdays$`Applicable To`)] <- 1

dates1 <- full_seq(ped_data$Date_Time, period = 3600)
hdayvals <- rep(0, length(dates1))
hdayvals[as.Date(dates1) %in% pub_hdays$Date[pub_hdays$VIC == 1]] <- 1
dates2 <- as.Date(rep(dates1, each = 43))
hdayvals2 <- as.numeric(rep(hdayvals, each = 43))

ped_data$IsHDay <- hdayvals2
ped_data$HDay <- ped_data$Day
levels(ped_data$HDay) <- c(levels(ped_data$Day), "Holiday")
ped_data$HDay[ped_data$IsHDay == 1] <- "Holiday"

ped_data$DayType <- ifelse(ped_data$Day == "Sunday", "Sunday",
                     ifelse(ped_data$Day == "Saturday", "Saturday",
                    ifelse(ped_data$Day == "Monday", "Monday",
                     ifelse(ped_data$Day == "Friday", "Friday",
                                  "Midweek"))))
ped_data$DayType[ped_data$IsHDay == 1] <- "Holiday"

ped_data %>% select(Sensor_ID, Count, Date_Time, DayType, Time, Month) %>% 
  spread(Sensor_ID, Count) -> dfa

ped_loc <- read_csv("data/Pedestrian_sensor_locations.csv")
lats <- ped_loc$Latitude
lons <- ped_loc$Longitude
ids <- ped_loc$`Sensor Description`


setdiff(loc$ids, ped_data$Sensor)

ids[ids == "Flinders Street Station Underpass"] <- "Flinders St Station Underpass"
ids[ids == "Flinders St-Spark La"] <- "Flinders St-Spark Lane"
ids[ids == "Queen St (West)"] <-  "Queen Street (West)"
ids[ids == "The Arts Centre"] <- "Vic Arts Centre"
ids[ids == "Lonsdale St-Spring St (West)"] <- "Spring St-Lonsdale St (South)"
ids[ids == "Lygon St (East)"] <- "Lygon Street (East)"
ids[ids == "QV Market-Elizabeth St (West)"] <- "QV Market-Elizabeth (West)"
ids[ids == "Melbourne Convention Exhibition Centre"] <- "Convention/Exhibition Centre"
ids[ids == "St Kilda Rd-Alexandra Gardens"] <- "St. Kilda-Alexandra Gardens"

loc <- data.frame(lons, lats, ids)
rownames(loc) <- ids

################################
## strip GLM to bare minimum ##

### code from:
## http://www.win-vector.com/blog/2014/05/trimming-the-fat-from-glm-models-in-r/
stripGlmLR = function(cm) {
  cm$y = c()
  cm$model = c()
  
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()  
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  
  
  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  attr(cm$terms,".Environment") = c()
  attr(cm$formula,".Environment") = c()
  
  cm
}

#######################################

n_obs <- nrow(dfa)
threshold <- 0.05
threshold_miss_hrs <- 50
max_miss_hrs <- 100
geo_match = TRUE

#######################################

##################################
# parallel computation setup #

cl <- makeCluster(2)

registerDoSNOW(cl)
options(na.action = na.omit)

##------------------------------##
run_lengths <- foreach(i = unique(ped_data$Sensor)) %dopar% 
  {
    return(rle(dfa[, i]))
  }

models <- list()
for (i in unique(ped_data$Sensor)){
  ## check percentage of missings
    dfa[is.na(dfa[, i]), i] <- 0
    na_length_check <- rle(as.numeric(unlist(dfa[,i])))
    
  maybeNA <- rep((na_length_check$lengths > threshold_miss_hrs), times = na_length_check$lengths)
  dfa[maybeNA, i] <- NA
  na_prop <- sum(is.na(dfa[, i])) / n_obs
  if(max(na_length_check$lengths) > max_miss_hrs) 
  {
    model <- NULL
  }
  
  sensor_df <- unlist(dfa[, i])
  
  if(na_prop < threshold)
    {
    model <- glm(sensor_df ~  DayType*Time, family = 'quasipoisson', data = dfa)
    }
  else{
        sensor_loc <- loc[i, ]
        sensor_dists <-  dplyr::mutate(filter(loc, ids != i), lons_dist = (sensor_loc$lons - lons),
                               lats_dist = (sensor_loc$lats - lats),
                               dist = sqrt(lons_dist^2 + lats_dist^2))
        closest_sensor <- head(arrange(sensor_dists, dist),2)
        close_sensor_counts <- unlist(dfa[, as.character(closest_sensor[1, ]$ids)])
        close_sensor_counts_2 <- unlist(dfa[, as.character(closest_sensor[2, ]$ids)])
        model <- lm(sensor_df ~ dfa$Time*close_sensor_counts + dfa$Time*close_sensor_counts_2)
    
      }
  models[[i]] <- model
  }

model_summaries <- map(models, summary)
# na_prop <- numeric()
# 
# for (i in unique(ped_data$Sensor)){
#   na_prop[i] <- sum(is.na(df[, i])) / n_obs
# }
# 
# df[, c(22, 14, 9, 1, 17, 27, 35, 19, 24, 38, 31)] %>% melt(id = "Date_Time") %>%
#   ggplot() + geom_line(aes(x = Date_Time, y = value, colour = variable), alpha = 0.25)
# 
# good_sensors <- data.frame(sensor = unique(ped_data$Sensor), na_prop) %>%
#                 filter(na_prop < 0.01) %>% select(sensor)
# 
# knn_testmodel <- knn.reg(df, test = NULL, y = df$`Southbank`, k = 3)
