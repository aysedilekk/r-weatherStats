library(ggplot2)
 
function(input, output) {
 
  dataset <- read.csv('data/data.csv')
 
  output$plot1 <- renderPlot({
    
    # AverageTemprature
    average_temperature <- subset(dataset,
                                  select = c("Months","AverageTemprature") ,
                                  subset = (City == input$x)) 
    
    positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
    ggplot(data = average_temperature,
           aes(Months,AverageTemprature)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#F44336"))
  })
  
  output$plot2 <- renderPlot({
    
    average_maximum_temperature <- subset(dataset,
                                          select = c("Months","AverageMaximumTemperature") ,
                                          subset = (City == input$x))
    
    # AverageMaximumTemperature    
    positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
    ggplot(data = average_maximum_temperature,
           aes(Months,AverageMaximumTemperature)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#EC407A"))
  })
  
  output$plot3 <- renderPlot({
    
    average_minimum_temperature<- subset(dataset,
                                         select = c("Months","AverageMinimumTemperature") ,
                                         subset = (City == input$x))
    # AverageMinimumTemperature
    positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
    ggplot(data = average_minimum_temperature,
           aes(Months,AverageMinimumTemperature)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#7E57C2"))
  })
  
  output$plot4 <- renderPlot({
    
    average_number_of_rainy_days <- subset(dataset,
                                           select = c("Months","AverageNumberofRainyDays") ,
                                           subset = (City == input$x))
    
    # AverageNumberofRainyDays
    positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
    ggplot(data = average_number_of_rainy_days,
           aes(Months,AverageNumberofRainyDays)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#26A69A"))
  })
  
  output$plot5 <- renderPlot({
    
    average_sunrise_time <- subset(dataset,
                                   select = c("Months","AverageSunriseTime") ,
                                   subset = (City == input$x))
    
    # AverageSunriseTime
    positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
    ggplot(data = average_sunrise_time,
           aes(Months,AverageSunriseTime)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#FFB74D"))
  })
  
  output$plot6 <- renderPlot({
    
    monthly_total_rainfall_average <- subset(dataset,
                                             select = c("Months","MonthlyTotalRainfallAverage") ,
                                             subset = (City == input$x))
    
    # MonthlyTotalRainfallAverage
    positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
    ggplot(data = monthly_total_rainfall_average,
           aes(Months,MonthlyTotalRainfallAverage)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_x_discrete(limits = positions) +
      stat_summary(fun.y = sum, 
                   geom = "bar",
                   fill=I("#42A5F5"))
  })
  
}
