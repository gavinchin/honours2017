library(tidyverse)
library(lubridate)

# install.packages('ggdendro')
library(ggdendro)

# devtools::install_github("earowang/rwalkr")
# library(rwalkr)

ped_df <- read_csv("data/ped_df.csv")
ped_df$X1 <- NULL
ped_df$Date_Time <- ymd_h(paste(ped_df$Date, ped_df$Time))
ped_df$Day <- wday(ped_df$Date, label = TRUE, abbr = FALSE)

ped_loc <- read_csv("data/Pedestrian_sensor_locations.csv")

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

dates1 <- seq(as.Date(head(ped_df$Date, 1)), as.Date(tail(ped_df$Date, 1)),
              by = "day")
hdayvals <- rep(0, length(dates1))
hdayvals[dates1 %in% pub_hdays$Date[pub_hdays$VIC == 1]] <- 1
dates2 <- as.Date(rep(dates1, each = 43*24))
hdayvals2 <- as.numeric(rep(hdayvals, each = 43*24))

ped_df$IsHDay <- hdayvals2
ped_df$HDay <- ped_df$Day
levels(ped_df$HDay) <- c(levels(ped_df$Day), "Holiday")
ped_df$HDay[ped_df$IsHDay == 1] <- "Holiday"
ped_df$Month <- month(ped_df$Date, label = TRUE, abbr = TRUE)

ped_list <- ped_df %>% split(.$Sensor)
p <- progress_estimated(43)
ped_fit <- list()  
for (i in 1:43) {
  p$tick()$print()
  fit_data <- ped_list[[i]]
  ped_fit[[i]] <- glm(Count ~ Month + HDay*Time, data = fit_data,
                      family = 'poisson')
}

fitted_dummy <- ped_fit %>% 
  map2(ped_list, ~ predict(.x, newdata = .y[, -1], type = "response"))

for (i in 1:43) {
  ped_list[[i]]$Fitted <- fitted_dummy[[i]]
}


## clustering of glm coefficients

fit_coeff <- data.frame()

for (i in 1:43) {
  coeff <- as.data.frame(ped_fit[[i]]$coefficients)
  coeff$var <- rownames(coeff)
  coeff$ID <- i
  fit_coeff <- rbind.data.frame(fit_coeff, coeff)
}

rownames(fit_coeff) <- NULL
colnames(fit_coeff) <- c("coefficient", "variable", "id")

sensor_names <- sort(ped_loc$`Sensor Description`)
glm_coeffs <- fit_coeff %>% spread(variable, coefficient)

rownames(glm_coeffs) <- sensor_names

hcluster_glm_coeff <- hclust(dist(glm_coeffs[, -1]))
plot(hcluster_glm_coeff)
ggdendrogram(cluster_glm_coeff, rotate = T)

kclust <- kmeans(x = dist(glm_coeffs[, -1]), centers = 5)
glm_coeffs$kmeansgroup <- kclust$cluster

ggplot(filter(ped_list[[12]], Date > "2017-06-30")) +
  geom_line(aes(x = Date_Time, y = Count), colour = "blue") +
  geom_line(aes(x = Date_Time, y = Fitted), colour = "red")

glm_coeffs$location <- sensor_names

cluster_glm_coeff <- hclust(dist(glm_coeffs[, -1], method = "manhattan"))
plot(cluster_glm_coeff)

ggplot(filter(ped_list[[16]], Date > "2017-06-30")) +
  geom_line(aes(x = Date_Time, y = Count), colour = "blue") +
  geom_line(aes(x = Date_Time, y = Fitted), colour = "red")

ped_df$Wk <- week(ped_df$Date)
ped_df$HDayTime <- paste(ped_df$HDay, ped_df$Time, sep = "_")

ped_list_2 <- ped_df %>% split(.$Sensor)

p <- progress_estimated(43)
ped_fit2 <- list()  
for (i in 1:43) {
  p$tick()$print()
  fit_data <- ped_list[[i]]
  ped_fit2[[i]] <- glm(Count ~ Month + HDay*Time, data = fit_data,
                       family = 'gaussian')
}

fitted_dummy2 <- ped_fit2 %>% 
  map2(ped_list, ~ predict(.x, newdata = .y[, -1], type = "response"))

for (i in 1:43) {
  ped_list[[i]]$Fitted2 <- fitted_dummy2[[i]]
}

ggplot(filter(ped_list[[12]], Date > "2017-06-30")) +
  geom_line(aes(x = Date_Time, y = Count), colour = "blue") +
  geom_line(aes(x = Date_Time, y = Fitted), colour = "red") +
  geom_line(aes(x = Date_Time, y = Fitted2), colour = "green")

p <- progress_estimated(43)
ped_fit3 <- list() 
for (i in 1:43) {
  p$tick()$print()
  fit_data <- ped_list_2[[i]]
  ped_fit3[[i]] <- glm(Count ~ HDayTime, data = fit_data, family = 'gaussian')
}

fitted_dummy3 <- ped_fit3 %>% 
  map2(ped_list_2, ~ predict(.x, newdata = .y[, -1], type = "response"))

for (i in 1:43) {
  ped_list[[i]]$Fitted3 <- fitted_dummy3[[i]]
}


ggplot(filter(ped_list[[12]], Date > "2017-06-30")) +
  geom_line(aes(x = Date_Time, y = Count), colour = "blue") +
  geom_line(aes(x = Date_Time, y = Fitted), colour = "red") +
  geom_line(aes(x = Date_Time, y = Fitted2), colour = "green") +
  geom_line(aes(x = Date_Time, y = Fitted3), colour = "orange")

p <- progress_estimated(43)
ped_fit4 <- list() 
for (i in 1:43) {
  p$tick()$print()
  fit_data <- ped_list_2[[i]]
  ped_fit4[[i]] <- glm(Count ~ Month + HDayTime, data = fit_data,
                       family = 'poisson')
}

fitted_dummy4 <- ped_fit4 %>% 
  map2(ped_list_2, ~ predict(.x, newdata = .y[, -1], type = "response"))

for (i in 1:43) {
  ped_list[[i]]$Fitted4 <- fitted_dummy4[[i]]
}


ggplot(filter(ped_list[[4]], Date > "2017-06-30")) +
  geom_line(aes(x = Date_Time, y = Count), colour = "blue") +
  geom_line(aes(x = Date_Time, y = Fitted), colour = "red") +
  geom_line(aes(x = Date_Time, y = Fitted2), colour = "green") +
  geom_line(aes(x = Date_Time, y = Fitted3), colour = "orange") +
  geom_line(aes(x = Date_Time, y = Fitted4), colour = "lightblue")

ggplot(ped_list[[23]]) + 
  geom_path(aes(x = Time, y = Count),     colour = 'black', alpha = 0.05) +
  geom_smooth(aes(x = Time, y = Fitted4), colour = 'blue', alpha = 0.25) +
  geom_smooth(aes(x = Time, y = Fitted),  colour = 'green', alpha = 0.25) +
  geom_smooth(aes(x = Time, y = Fitted3), colour = 'yellow', alpha = 0.25) +
  facet_wrap(~ HDay)

ped_df$DayType <- ifelse(ped_df$Day == "Sunday", "Sunday",
                  ifelse(ped_df$Day == "Saturday", "Saturday",
                  ifelse(ped_df$Day == "Monday", "Monday",
                  ifelse(ped_df$Day == "Friday", "Friday",
                                                 "Midweek"))))
ped_df$DayType[ped_df$IsHDay == 1] <- "Holiday"
ped_df$DayType_Time <- paste(ped_df$DayType, ped_df$Time, sep = "_")
ped_list_4 <- ped_df %>% split(.$Sensor)

p <- progress_estimated(43)
ped_fit5 <- list() 
for (i in 1:43) {
  p$tick()$print()
  fit_data <- ped_list_4[[i]]
  ped_fit5[[i]] <- glm(Count ~ Month + DayType_Time, data = fit_data,
                       family = 'poisson')
}

fitted_dummy5 <- ped_fit5 %>% 
  map2(ped_list_4, ~ predict(.x, newdata = .y[, -1], type = "response"))

for (i in 1:43) {
  ped_list[[i]]$Fitted5 <- fitted_dummy5[[i]]
}


ggplot(ped_list[[27]]) + geom_path(aes(x = Time, y = Count), colour = 'black',
                                   alpha = 0.05) +
                         geom_path(aes(x = Time, y = Fitted5), colour = 'blue',
                                   alpha = 0.05) +
                         facet_wrap(~ HDay)

p <- progress_estimated(43)
ped_fit6 <- list() 
for (i in 1:43) {
  p$tick()$print()
  fit_data <- ped_list_4[[i]]
  ped_fit6[[i]] <- glm(Count ~ Month + DayType_Time, data = fit_data,
                       family = 'gaussian')
}

fitted_dummy6 <- ped_fit6 %>% 
  map2(ped_list_4, ~ predict(.x, newdata = .y[, -1], type = "response"))

## improved glm model plot
for (i in 1:43) {
  ped_list[[i]]$Fitted6 <- fitted_dummy6[[i]]
}

ggplot(ped_list[[23]]) + geom_path(aes(x = Time, y = Count), colour = 'black',
                                   alpha = 0.05) +
  geom_path(aes(x = Time, y = Fitted6), colour = 'blue',
            alpha = 0.05) +
  geom_path(aes(x = Time, y = Fitted5), colour = 'red',
            alpha = 0.025) +
  facet_wrap(~ HDay)

 ## plotting residuals
for (i in 1:43) {
  ped_list[[i]]$Fitted6Resid <- fitted_dummy6[[i]] - ped_list[[i]]$Count
}

ggplot(ped_list[[23]]) + geom_path(aes(x = Time, y = Fitted6Resid),
                                   colour = 'black', alpha = 0.05) +
                         facet_wrap(~ HDay)

ggplot(ped_list[[23]]) + geom_path(aes(x = Time, y = I(Fitted6Resid^2)),
                                   colour = 'black', alpha = 0.05) +
  facet_wrap(~ HDay)

# clustering of glm coefficients

fit_coeff2 <- data.frame()

for (i in 1:43) {
  coeff <- as.data.frame(ped_fit6[[i]]$coefficients)
  coeff$var <- rownames(coeff)
  coeff$ID <- i
  fit_coeff2 <- rbind.data.frame(fit_coeff2, coeff)
}

rownames(fit_coeff2) <- NULL
colnames(fit_coeff2) <- c("coefficient", "variable", "id")

sensor_names <- sort(ped_loc$`Sensor Description`)
glm_coeffs2 <- fit_coeff2 %>% spread(variable, coefficient)

rownames(glm_coeffs2) <- sensor_names

hcluster_glm_coeff2 <- hclust(dist(glm_coeffs2[, -1]))
plot(hcluster_glm_coeff2)
ggdendrogram(hcluster_glm_coeff2, rotate = T)

kclust2 <- kmeans(x = dist(glm_coeffs2[, -1]), centers = 5)
glm_coeffs2$kmeansgroup <- kclust2$cluster

glm_coeffs2$location <- sensor_names

cluster_glm_coeff2 <- hclust(dist(glm_coeffs2[, -1], method = "manhattan"))
plot(cluster_glm_coeff2)

#######################

p <- progress_estimated(43)
ped_fit7 <- list() 
for (i in 1:43) {
  p$tick()$print()
  fit_data <- ped_list_4[[i]]
  ped_fit7[[i]] <- glm(Count ~ Month + DayType_Time, data = fit_data,
                       family = 'quasipoisson')
}

fitted_dummy7 <- ped_fit7 %>% 
  map2(ped_list_4, ~ predict(.x, newdata = .y[, -1], type = "response"))

## improved glm model plot no2
for (i in 1:43) {
  ped_list[[i]]$Fitted7 <- fitted_dummy7[[i]]
}
ggplot(ped_list[[23]]) + geom_path(aes(x = Time, y = Count), colour = 'black',
                                   alpha = 0.05) +
  geom_path(aes(x = Time, y = Fitted7), colour = 'blue',
            alpha = 0.05) +
  facet_wrap(~ HDay)

for (i in 1:43) {
  ped_list[[i]]$Fitted7Resid <- fitted_dummy7[[i]] - ped_list[[i]]$Count
}

ggplot(ped_list[[23]]) + geom_point(aes(x = Time, y = Fitted7Resid),
                                   colour = 'black', alpha = 0.05) +
                        geom_point(aes(x = Time, y = Fitted6Resid),
                                   colour = 'red', alpha = 0.015) +
  facet_wrap(~ HDay)

ggplot(ped_list[[23]]) + geom_point(aes(x = Time, y = I(Fitted7Resid^2)),
                                      colour = 'black', alpha = 0.05) +
                         geom_point(aes(x = Time, y = I(Fitted6Resid^2)),
                                      colour = 'red', alpha = 0.05) +
  facet_wrap(~ HDay)
