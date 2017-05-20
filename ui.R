library(ggplot2)
library(leaflet)
dataset <- read.csv('data/data.csv')
map_dataset <- read.csv('data/map_data.csv')


#color palettes
qpalAverageTemperature <- colorNumeric(
  colorRamp(c("#FFFFFF","#FF3D00"), interpolate="spline"),
  domain = map_dataset$AverageTemprature)

qpalAverageMaximumTemperature <- colorNumeric(
  colorRamp(c("#FFFFFF","#880E4F"), interpolate="spline"),
  domain = map_dataset$AverageMaximumTemperature)

qpalAverageMinimumTemperature <- colorNumeric(
  colorRamp(c("#FFFFFF","#1A237E"), interpolate="spline"),
  domain = map_dataset$AverageMinimumTemperature)

qpalAverageSunriseTime <- colorNumeric(
  colorRamp(c("#FFFF00","#D50000"), interpolate="spline"),
  domain = map_dataset$AverageSunriseTime)

qpalAverageNumberofRainyDays <- colorNumeric(
  colorRamp(c("#80D8FF","#01579B"), interpolate="spline"),
  domain = map_dataset$AverageNumberofRainyDays)

qpalMonthlyTotalRainfallAverage <- colorNumeric(
  colorRamp(c("#69F0AE","#1B5E20"), interpolate="spline"),
  domain = map_dataset$MonthlyTotalRainfallAverage)

fluidPage(

  title = "Weather",
  
  br(),
  tabsetPanel(
   tabPanel("MAP", map <- leaflet(height = 780) %>% setView(lng = 35.65, lat = 38.96375, zoom = 7) %>%
              # Base groups
              addTiles(group = "OSM (default)") %>%
          
              # Circle groups
              addCircleMarkers(map_dataset$Lon,map_dataset$Lat,radius = 10, popup = paste(as.character(map_dataset$City)," | ","Average Temperature: ", as.character(map_dataset$AverageTemprature), sep=""),color = qpalAverageTemperature(map_dataset$AverageTemprature) ,fillOpacity = 1, fill = TRUE,group = "Average Temperature") %>% 
              addCircleMarkers(map_dataset$Lon,map_dataset$Lat,radius = 10, popup = paste(as.character(map_dataset$City)," | ","Average Maximum Temperature: ", as.character(map_dataset$AverageMaximumTemperature), sep=""),color = qpalAverageMaximumTemperature(map_dataset$AverageMaximumTemperature) ,fillOpacity = 1, fill = TRUE,group = "Average Maximum Temperature") %>% 
              addCircleMarkers(map_dataset$Lon,map_dataset$Lat,radius = 10, popup = paste(as.character(map_dataset$City)," | ","Average Minimum Temperature: ", as.character(map_dataset$AverageMinimumTemperature), sep=""),color = rev(qpalAverageMinimumTemperature(map_dataset$AverageMinimumTemperature)) ,fillOpacity = 1, fill = TRUE,group = "Average Minimum Temperature") %>% 
              addCircleMarkers(map_dataset$Lon,map_dataset$Lat,radius = 10, popup = paste(as.character(map_dataset$City)," | ","Average Sunrise Time: ", as.character(map_dataset$AverageSunriseTime), sep=""),color = qpalAverageSunriseTime(map_dataset$AverageSunriseTime) ,fillOpacity = 1, fill = TRUE,group = "Average Sunrise Temperature") %>% 
              addCircleMarkers(map_dataset$Lon,map_dataset$Lat,radius = 10, popup = paste(as.character(map_dataset$City)," | ","Average Number of Rainy Days: ", as.character(map_dataset$AverageNumberofRainyDays), sep=""),color = qpalAverageNumberofRainyDays(map_dataset$AverageNumberofRainyDays) ,fillOpacity = 1, fill = TRUE,group = "Average Number of Rainy Days") %>% 
              addCircleMarkers(map_dataset$Lon,map_dataset$Lat,radius = 10, popup = paste(as.character(map_dataset$City)," | ","Monthly Total Rainfall Average: ", as.character(map_dataset$MonthlyTotalRainfallAverage), sep=""),color = qpalMonthlyTotalRainfallAverage(map_dataset$MonthlyTotalRainfallAverage) ,fillOpacity = 1, fill = TRUE,group = "Monthly Total Rainfall Average") %>% 
        
              # Layers control
              addLayersControl(
                baseGroups <- c("Average Temperature", "Average Maximum Temperature","Average Minimum Temperature","Average Sunrise Temperature", "Average Number of Rainy Days","Monthly Total Rainfall Average"),
                options = layersControlOptions(collapsed = FALSE)
               
              )%>%
              
              addLegend("bottomright", 
                       pal = qpalAverageTemperature, 
                       values = map_dataset$AverageTemprature,
                       title = "C",
                       opacity = 1
                       
              )  %>%
              
              addLegend("bottomright", 
                       pal = qpalAverageMaximumTemperature, 
                       values = map_dataset$AverageMaximumTemperature,
                       title = "C",
                       opacity = 1
                       
              ) %>%
              
              addLegend("bottomright", 
                       pal = qpalAverageMinimumTemperature, 
                       values = map_dataset$AverageMinimumTemperature,
                       title = "C",
                       opacity = 1
                       
              ) %>%
              
              addLegend("bottomright", 
                       pal = qpalAverageSunriseTime, 
                       values = map_dataset$AverageSunriseTime,
                       title = "C",
                       opacity = 1
                       
              ) %>%
              
              addLegend("bottomright", 
                       pal = qpalAverageNumberofRainyDays, 
                       values = map_dataset$AverageNumberofRainyDays,
                       title = "C",
                       opacity = 1
                       
              ) %>%
              
              addLegend("bottomright", 
                       pal = qpalMonthlyTotalRainfallAverage, 
                       values = map_dataset$MonthlyTotalRainfallAverage,
                       title = "C",
                       opacity = 1
                       
              ),

            
              column(12,
                    h3("ANNUAL AVERAGE VALUES BETWEEN 1926 AND 2016")
                  ),
            
              column(12,
                    h4("Ayse DILEK - Cihan SELIM")
              )
              
   ),
   
   tabPanel("HISTOGRAM", br(),
            fluidPage(
              column(12, 
                     selectInput('x', 'CITY', dataset$City, selected = TRUE, width = '25%')
              ),
              tabsetPanel(
                tabPanel("MONTHS",
                         column(4,
                                plotOutput('plot1'),
                                plotOutput('plot2')
                         ),
                         column(4,
                                plotOutput('plot3'),
                                plotOutput('plot4')
                                
                         ),
                         column(4,
                                plotOutput('plot5'),
                                plotOutput('plot6')
                                
                         )),
                tabPanel("SEASONS",
                         column(4,
                                plotOutput('plot7'),
                                plotOutput('plot8')
                         ),
                         column(4,
                                plotOutput('plot9'),
                                plotOutput('plot10')
                                
                         ),
                         column(4,
                                plotOutput('plot11'),
                                plotOutput('plot12')
                                
                         ))
              )
            ))
  )
)
