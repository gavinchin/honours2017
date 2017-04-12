library(shiny)
library(plotly)
library(dplyr)
library(maps)
library(mapproj)
library(ggplot2)
library(readr)
library(ggmap)
##desktop
  setwd("C:/Users/Gavin/Documents/honours2017/honours2017/")
  Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiYm9iYnl6MHIiLCJhIjoiMTk5MDczNWVmY2MzZjc2MmY0ZDI0YmZiYzEyNDc1MDYifQ.EFS7w-Ni47OoMpwy-zT_5A')
  
  ped_df <- read_csv("data/Pedestrian_volume__updated_monthly_.csv")
  ped_df$Year <- as.factor(ped_df$Year)
  ped_df$Month <- as.factor(ped_df$Month)
  ped_df$Month <- factor(ped_df$Month, levels(ped_df$Month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
  ped_df$Time <- as.factor(ped_df$Time)
  ped_df$MTime <- as.integer(ped_df$Time) + ((as.integer(ped_df$Mdate)-1) * 24)
  
  ped_loc <- read_csv("data/Pedestrian_sensor_locations.csv")
  
  ped_df$Lat <- 0
  ped_df$Lon <- 0

for (i in 1:dim(ped_loc)[1])
      {
      ped_df$Lat[ped_df$Sensor_ID == ped_loc$`Sensor ID`[i]] <- ped_loc$Latitude[i]
      ped_df$Lon[ped_df$Sensor_ID == ped_loc$`Sensor ID`[i]] <- ped_loc$Longitude[i]
      }

  melb_zoom <- get_map(location=c(144.9671,-37.81709), zoom=16)
  
  melb <- get_map(location=c(mean(range(ped_loc$Longitude)),
                             mean(range(ped_loc$Latitude))),
                            zoom=14)

# UI
ui <- shinyUI(fluidPage(
  
  titlePanel("Melbourne CBD Pedestrian Traffic Map"),
  sidebarLayout(
    
    sidebarPanel(
      selectInput("year_p",
                  "Year to plot",
                  levels(ped_df$Year),
                  selected = "2016"),
      selectInput("month_p",
                  "Month",
                  levels(ped_df$Month),
                  selected = "January"),
      sliderInput("time_p",
                  "Time of Day",
                  min = 0, max = 23, value = 12),
      checkboxInput("zoom", "Zoom in to Flinders Street Station",
                    value = F),
      width = 3),
    
    mainPanel(
      plotOutput("mapPlot")
    )
  )
))


# Define server logic
server <- shinyServer(function(input, output) {
  
  output$mapPlot <- renderPlot({
    if(input$zoom == 1) 
         (p <- ggmap(melb_zoom))
    else (p <- ggmap(melb))
    p + geom_point(data=ped_df[ped_df$Year == input$year_p & ped_df$Month == input$month_p & ped_df$Time == input$time_p, ], 
                   aes(x=Lon, y=Lat, colour = Hourly_Counts, size = Hourly_Counts), alpha=0.5) +
                   scale_size_continuous(range = c(1, 18)) + theme(legend.position = "bottom")
  },
    width = 800, height = 600)
  
})



# Run the application 
shinyApp(ui = ui, server = server)

