
library(tidyverse)
library(lubridate)
library(foreach)
library(doSNOW)
library(rwalkr)

##############################################
ped_data <- NULL
if(file.exists("data/ped_df.csv")){
  ped_data <- read_csv("data/ped_df.csv")
  ped_data$X1 <- NULL
  }

###############################################
## update data from latest to 
last_df_date <- ifelse(file.exists("data/ped_df.csv"),
                        ped_data$Date %>% sort() %>%
                        tail(1),
                        as.Date("2013-12-31")) + 1 %>% as.Date(origin = "1970-01-01")

if(last_df_date != today(tzone = "Australia/Melbourne")){
  new_data <- rwalkr::walk_melb(from = last_df_date, to = today()-1)
  ped_data <- rbind(ped_data, new_data)
  write_csv(new_data, path = "data/ped_df.csv", append = TRUE)
  }

###############################################

ped_data$Sensor_ID <- ped_data$Sensor
ped_data$Hourly_Counts <- ped_data$Count
ped_data$Date_Time <- ymd_h(paste(ped_data$Date, ped_data$Time, sep = " "))
ped_data$Day <- lubridate::wday(ped_data$Date_Time, label = TRUE, abbr = F)
ped_data$Month <- lubridate::month(ped_data$Date_Time, label = TRUE, abbr = F)
ped_data$Time <- as.factor(ped_data$Time)

###############################################
## long to wide
ped_data %>% select(Sensor_ID, Count, Date_Time, Time, Month, Day) %>% 
  spread(Sensor_ID, Count) -> dfa

###############################################

pub_hday14 <- read.csv("http://data.gov.au/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/56a5ee91-8e94-416e-81f7-3fe626958f7e/download/australianpublicholidays-201415.csv---australianpublicholidays.csv.csv")
pub_hday15 <- read.csv("http://data.gov.au/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/13ca6df3-f6c9-42a1-bb20-6e2c12fe9d94/download/australianpublicholidays-201516.csv")
pub_hday16 <- read.csv("http://data.gov.au/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/a24ecaf2-044a-4e66-989c-eacc81ded62f/download/australianpublicholidays-201617.csv")
pub_hdays <- rbind(pub_hday14, pub_hday15, pub_hday16)

pub_hdays$Date <- ymd(pub_hdays$Date)
pub_hdays$Month <- lubridate::month(pub_hdays$Date, label = TRUE, abbr = F)
pub_hdays$VIC <- 0
pub_hdays$VIC[grep(glob2rx("*VIC*"), pub_hdays$`Applicable.To`)] <- 1
pub_hdays$VIC[grep("NAT", pub_hdays$`Applicable.To`)] <- 1


dfa <- dfa %>% mutate(IsHDay = (date(Date_Time) %in% pub_hdays$Date[pub_hdays$VIC == 1]))

dfa$DayType <-       ifelse(dfa$Day == "Sunday", "Sunday",
                     ifelse(dfa$Day == "Saturday", "Saturday",
                    ifelse(dfa$Day == "Monday", "Monday",
                     ifelse(dfa$Day == "Friday", "Friday",
                                  "Midweek"))))
dfa$DayType[dfa$IsHDay == 1] <- "Holiday"



ped_loc <- read_csv("data/Pedestrian_sensor_locations.csv")
lats <- ped_loc$Latitude
lons <- ped_loc$Longitude
ids <- ped_loc$`Sensor Description`


setdiff(loc$ids, ped_data$Sensor)

## name fix dictionary

ids[ids == "Flinders Street Station Underpass"] <- "Flinders St Station Underpass"
ids[ids == "Flinders St-Spark La"] <- "Flinders St-Spark Lane"
ids[ids == "Queen St (West)"] <-  "Queen Street (West)"
ids[ids == "The Arts Centre"] <- "Vic Arts Centre"
ids[ids == "Lonsdale St-Spring St (West)"] <- "Spring St-Lonsdale St (South)"
ids[ids == "Lygon St (East)"] <- "Lygon Street (East)"
ids[ids == "QV Market-Elizabeth St (West)"] <- "QV Market-Elizabeth (West)"
ids[ids == "Melbourne Convention Exhibition Centre"] <- "Convention/Exhibition Centre"
ids[ids == "St Kilda Rd-Alexandra Gardens"] <- "St. Kilda-Alexandra Gardens"

# ped_name_fix <- function(x) 
#   {
#   x <- as.character(x)
#   x <- gsub("The A", "Vic A", x)
#   x <- gsub("Street", "St", x)
#   x <- gsub("Lane", "La", x)
#   x <- gsub("Convention/", "Melbourne Convention" , x)
#   x <- gsub(" Rd-", "-", x)
#   return(x)
#   }

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
ori <- dfa
#######################################
dfa <- ori
dfa <- dfa %>% filter(Date_Time < "2017-01-01")

#######################################

n_obs <- nrow(dfa)
threshold <- 0.1
threshold_miss_hrs <- 50
max_miss_hrs <- 100
geo_match = TRUE

#######################################

##################################
# parallel computation setup #
# 
# cl <- makeCluster(2)
# 
# registerDoSNOW(cl)
# options(na.action = na.omit)

##------------------------------##
# run_lengths <- foreach(i = unique(ped_data$Sensor)) %dopar%
#   {
#     return(rle(dfa[, i]))
#   }

### classify suspect 0 values as NA

  for (i in unique(ped_data$Sensor)){
    dfa[is.na(dfa[, i]), i] <- 0
    na_length_check <- rle(as.numeric(unlist(dfa[,i])))
    
    maybeNA <- rep((na_length_check$lengths > threshold_miss_hrs), times = na_length_check$lengths)
    dfa[maybeNA, i] <- NA
    }


### get list of missings proportions
na_prop <- numeric()

for (i in unique(ped_data$Sensor)){
  na_prop[i] <- sum(is.na(dfa[, i])) / n_obs
}

h_miss_sensors <- names(na_prop[na_prop > threshold])
l_miss_sensors <- setdiff(names(na_prop), h_miss_sensors)

models <- list()
fitted_data <- list()
used_sensors <- list()
p <- progress_estimated(length(unique(ped_data$Sensor)))
### models suitable for GLM
for (i in l_miss_sensors)
  {
  p$tick()$print()
  print(paste("Current sensor being modelled is:", i, sep = " "))
  
  options(na.action = na.omit)
  # if(max(na_length_check$lengths) > max_miss_hrs) 
  # {
  #   model <- NULL
  # }
  
  sensor_df <- unlist(dfa[, i])
  
  model <- glm(sensor_df ~  Month + DayType*Time, family = 'quasipoisson', data = dfa)
  
  # models[[i]] <- stripGlmLR(model)
  models[[i]] <- model
  options(na.action = na.pass)
  fitted_data[[i]] <- predict(model, newdata = dfa, type = "response")
  options(na.action = na.omit)
  dfa[is.na(dfa[, i]), i] <- fitted_data[[i]][is.na(dfa[, i])]
  }



  for (i in h_miss_sensors)
  {
    p$tick()$print()
    print(paste("Current sensor being modelled is:", i, sep = " "))
    
    sensor_df <- unlist(dfa[, i])
    
    sensor_loc <- loc[i, ]
    sensor_dists <-  dplyr::mutate(filter(loc, ids != i, ids %in% l_miss_sensors),
                                   lons_dist = (sensor_loc$lons - lons),
                                   lats_dist = (sensor_loc$lats - lats),
                                   dist = sqrt(lons_dist^2 + lats_dist^2))
    closest_sensor <- head(arrange(sensor_dists, dist),2)
    close_sensor_counts <- unlist(dfa[, as.character(closest_sensor[1, ]$ids)])
    csc_sc <- scale(close_sensor_counts)
    close_sensor_counts_2 <- unlist(dfa[, as.character(closest_sensor[2, ]$ids)])
    csc_sc2 <- scale(close_sensor_counts_2)
    model <- lm(sensor_df ~ dfa$Time*csc_sc + dfa$Time*csc_sc2)
    # models[[i]] <- stripGlmLR(model)
    models[[i]] <- model
    fitted_data[[i]] <- predict.lm(model, type = "response")
    dfa[is.na(dfa[, i]), i] <- fitted_data[[i]][is.na(dfa[, i])]
  }

map(fitted_data, is.na) %>% map(sum) %>% unlist

model_summaries <- map(models, summary)

rsq_lm <- numeric()
for (i in 1:18){
  rsq_lm[i] <- model_summaries[[i + 25]]$r.squared
}
names(rsq_lm) <- h_miss_sensors

rsq_lm %>% sort

models <- map(models, stripGlmLR)

ggplot()

# 
# df[, c(22, 14, 9, 1, 17, 27, 35, 19, 24, 38, 31)] %>% melt(id = "Date_Time") %>%
#   ggplot() + geom_line(aes(x = Date_Time, y = value, colour = variable), alpha = 0.25)
# 
# good_sensors <- data.frame(sensor = unique(ped_data$Sensor), na_prop) %>%
#                 filter(na_prop < 0.01) %>% select(sensor)
# 
# knn_testmodel <- knn.reg(df, test = NULL, y = df$`Southbank`, k = 3)
