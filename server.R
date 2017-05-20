library(ggplot2)
 
function(input, output) {
 
  dataset <- read.csv('data/data.csv')
  seasons <- read.csv('data/seasons.csv')
 
  output$plot1 <- renderPlot({
    
    # AverageTemprature
    average_temperature <- subset(dataset,
                                  select = c("Months","AverageTemprature") ,
                                  subset = (City == input$x)) 
    
    positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
    ggplot(data = average_temperature,
           aes(Months,AverageTemprature)) +
      theme(axis.text.x = element_text(size=15,angle = 90, hjust = 1),axis.title.y = element_text(size=18),axis.text.y = element_text(size=15)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#FF3D00"))
  })
  
  output$plot2 <- renderPlot({
    
    average_maximum_temperature <- subset(dataset,
                                          select = c("Months","AverageMaximumTemperature") ,
                                          subset = (City == input$x))
    
    # AverageMaximumTemperature    
    positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
    ggplot(data = average_maximum_temperature,
           aes(Months,AverageMaximumTemperature)) +
      theme(axis.text.x = element_text(size=15,angle = 90, hjust = 1),axis.title.y = element_text(size=18),axis.text.y = element_text(size=15)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#880E4F"))
  })
  
  output$plot3 <- renderPlot({
    
    average_minimum_temperature<- subset(dataset,
                                         select = c("Months","AverageMinimumTemperature") ,
                                         subset = (City == input$x))
    # AverageMinimumTemperature
    positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
    ggplot(data = average_minimum_temperature,
           aes(Months,AverageMinimumTemperature)) +
      theme(axis.text.x = element_text(size=15,angle = 90, hjust = 1),axis.title.y = element_text(size=18),axis.text.y = element_text(size=15)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#1A237E"))
  })
  
  output$plot4 <- renderPlot({
    
    average_number_of_rainy_days <- subset(dataset,
                                           select = c("Months","AverageNumberofRainyDays") ,
                                           subset = (City == input$x))
    
    # AverageNumberofRainyDays
    positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
    ggplot(data = average_number_of_rainy_days,
           aes(Months,AverageNumberofRainyDays)) +
      theme(axis.text.x = element_text(size=15,angle = 90, hjust = 1),axis.title.y = element_text(size=18),axis.text.y = element_text(size=15)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#D50000"))
  })
  
  output$plot5 <- renderPlot({
    
    average_sunrise_time <- subset(dataset,
                                   select = c("Months","AverageSunriseTime") ,
                                   subset = (City == input$x))
    
    # AverageSunriseTime
    positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
    ggplot(data = average_sunrise_time,
           aes(Months,AverageSunriseTime)) +
      theme(axis.text.x = element_text(size=15,angle = 90, hjust = 1),axis.title.y = element_text(size=18),axis.text.y = element_text(size=15)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#01579B"))
  })
  
  output$plot6 <- renderPlot({
    
    monthly_total_rainfall_average <- subset(dataset,
                                             select = c("Months","MonthlyTotalRainfallAverage") ,
                                             subset = (City == input$x))
    
    # MonthlyTotalRainfallAverage
    positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
    ggplot(data = monthly_total_rainfall_average,
           aes(Months,MonthlyTotalRainfallAverage)) +
      theme(axis.text.x = element_text(size=15,angle = 90, hjust = 1),axis.title.y = element_text(size=18),axis.text.y = element_text(size=15)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#1B5E20"))
  })
  
  output$plot7 <- renderPlot({
    
    # AverageTemprature
    average_temperature <- subset(seasons,
                                  select = c("Season","AverageTemprature") ,
                                  subset = (City == input$x)) 
    
    positions <- c("WINTER", "SUMMER", "SPRING", "AUTUMN")
    ggplot(data = average_temperature,
           aes(Season,AverageTemprature)) +
      theme(axis.text.x = element_text(size=15,angle = 90, hjust = 1),axis.title.y = element_text(size=18),axis.text.y = element_text(size=15)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#FF3D00"))
  })
  
  output$plot8 <- renderPlot({
    
    average_maximum_temperature <- subset(seasons,
                                          select = c("Season","AverageMaximumTemperature") ,
                                          subset = (City == input$x))
    
    # AverageMaximumTemperature    
    positions <- c("WINTER", "SUMMER", "SPRING", "AUTUMN")
    ggplot(data = average_maximum_temperature,
           aes(Season,AverageMaximumTemperature)) +
      theme(axis.text.x = element_text(size=15,angle = 90, hjust = 1),axis.title.y = element_text(size=18),axis.text.y = element_text(size=15)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#880E4F"))
  })
  
  output$plot9 <- renderPlot({
    
    average_minimum_temperature<- subset(seasons,
                                         select = c("Season","AverageMinimumTemperature") ,
                                         subset = (City == input$x))
    # AverageMinimumTemperature
    positions <- c("WINTER", "SUMMER", "SPRING", "AUTUMN")
    ggplot(data = average_minimum_temperature,
           aes(Season,AverageMinimumTemperature)) +
      theme(axis.text.x = element_text(size=15,angle = 90, hjust = 1),axis.title.y = element_text(size=18),axis.text.y = element_text(size=15)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#1A237E"))
  })
  
  output$plot10 <- renderPlot({
    
    average_number_of_rainy_days <- subset(seasons,
                                           select = c("Season","AverageNumberofRainyDays") ,
                                           subset = (City == input$x))
    
    # AverageNumberofRainyDays
    positions <- c("WINTER", "SUMMER", "SPRING", "AUTUMN")
    ggplot(data = average_number_of_rainy_days,
           aes(Season,AverageNumberofRainyDays)) +
      theme(axis.text.x = element_text(size=15,angle = 90, hjust = 1),axis.title.y = element_text(size=18),axis.text.y = element_text(size=15)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#D50000"))
  })
  
  output$plot11 <- renderPlot({
    
    average_sunrise_time <- subset(seasons,
                                   select = c("Season","AverageSunriseTime") ,
                                   subset = (City == input$x))
    
    # AverageSunriseTime
    positions <- c("WINTER", "SUMMER", "SPRING", "AUTUMN")
    ggplot(data = average_sunrise_time,
           aes(Season,AverageSunriseTime)) +
      theme(axis.text.x = element_text(size=15,angle = 90, hjust = 1),axis.title.y = element_text(size=18),axis.text.y = element_text(size=15)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#01579B"))
  })
  
  output$plot12 <- renderPlot({
    
    monthly_total_rainfall_average <- subset(seasons,
                                             select = c("Season","MonthlyTotalRainfallAverage") ,
                                             subset = (City == input$x))
    
    # MonthlyTotalRainfallAverage
    positions <- c("WINTER", "SUMMER", "SPRING", "AUTUMN")
    ggplot(data = monthly_total_rainfall_average,
           aes(Season,MonthlyTotalRainfallAverage)) +
      theme(axis.text.x = element_text(size=15,angle = 90, hjust = 1),axis.title.y = element_text(size=18),axis.text.y = element_text(size=15)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#1B5E20"))
  })
  
  

  
}
