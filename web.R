install.packages('rvest')
library(rvest)


# Scraping the 81 city names of Turkey
cities_url <- "https://www.mgm.gov.tr/veridegerlendirme/il-ve-ilceler-istatistik.aspx"
city_webpage <- read_html(cities_url)
city_webpage

city_names_html <- html_nodes(city_webpage,'.kk_div1')
city_names <- html_text(city_names_html)
city_names

city_names <- gsub("\r\n    \r\n        ", "", city_names)
city_names <- gsub("\r\n    \r\n    ", "", city_names)
city_names <- gsub("i", "ı", city_names)
city_names <- gsub("ş", "s", city_names)
city_names <- gsub("ü", "u", city_names)
city_names <- gsub("ç", "c", city_names)
city_names <- gsub("ğ", "g", city_names)
city_names <- gsub("ö", "o", city_names)
city_names <- gsub("Ş", "S", city_names)
city_names <- gsub("Ç", "C", city_names)
city_names <- gsub("ı", "I", city_names)
city_names

install.packages("stringi")
library(stringi)
cities <- data.frame(words = unlist(stri_extract_all_words(stri_trans_tolower(city_names))))

cities <- cities$words <- as.character(cities$words)
cities <- toupper(cities)
cities<- gsub("ı", "I", cities)
cities<- gsub("KAHRAMANMARAS", "K.MARAS", cities)

cities

######################################


library(XML)
library(httr)


# Scraping tables' urls
GetUrls <- function(S) {
  URL <- paste("https://www.mgm.gov.tr/veridegerlendirme/il-ve-ilceler-istatistik.aspx?m=", S, sep="" )
}


url_list = list()
for (city in cities) {
  dat <- data.frame(all_urls <- GetUrls(city))
  dat$city <- city 
  url_list[[city]] <- dat 
}
url_list = do.call(rbind, url_list)
colnames(url_list) <- c("url","city")
all_urls <- url_list$url
all_urls <- as.character(all_urls)
all_urls[1]



# Scraping all tables for each cities
data_list = list()
for (url in all_urls){
  tables <- GET(url)
  datatable <- readHTMLTable (rawToChar (tables$content), which=1 )
  
  colnames(datatable) <- c(url,"JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER", "YILLIK")
  datatable$YILLIK <- NULL
  datatable$url <- NULL
  datatable
  
  datatable <- datatable[-c(1,8,9,10),]  
  datatable
  
  for(i in 1:12 ) {
    datatable[,i] <- as.numeric(levels(datatable[,i]))[datatable[,i]]
    
  }
  City <- url
  Months <- colnames(datatable)
  AverageTemprature <- as.numeric(c(datatable[1,]))
  AverageMaximumTemperature <- as.numeric(c(datatable[2,]))
  AverageMinimumTemperature <- as.numeric(c(datatable[3,]))
  AverageSunriseTime <- as.numeric(c(datatable[4,]))
  AverageNumberofRainyDays <- as.numeric(c(datatable[5,]))
  MonthlyTotalRainfallAverage <- as.numeric(c(datatable[6,]))

  
  son_data <- data.frame(City,Months,AverageTemprature,AverageMaximumTemperature,AverageMinimumTemperature,AverageSunriseTime,AverageNumberofRainyDays,MonthlyTotalRainfallAverage)
  data_list[[url]] <- son_data

}

final_list = do.call(rbind, data_list)
final_list
final_list$City <-  gsub(".*=","",final_list$City)

for (i in seq(1,nrow(final_list),12)) {
  final_list <- final_list[-i,]
}


location_list = list()
for (i in cities){
  xx <- data.frame(geocode(i))
  location_list[[i]] <- xx
  location_list[[i]] <- location_list[[i]][rep(row.names(xx), 12),]

}

location = do.call(rbind, location_list)
location

final_list[, "Lon"] <- location$lon
final_list[, "Lat"] <- location$lat

final_list


write.csv(final_list, file = "C:/Users/Ayse/Desktop/Web/data.csv")

###########################################################

# Subset

colnames(final_list)

average_temperature <- subset(final_list,
                          select = c("Months","AverageTemprature","Lon","Lat") ,
                          subset = (City == "ANKARA")) 

average_maximum_temperature <- subset(final_list,
                                      select = c("Months","AverageMaximumTemperature","Lon","Lat") ,
                                      subset = (City == "ANKARA"))


average_minimum_temperature<- subset(final_list,
                                      select = c("Months","AverageMinimumTemperature","Lon","Lat") ,
                                      subset = (City == "ANKARA"))

average_number_of_rainy_days <- subset(final_list,
                                      select = c("Months","AverageNumberofRainyDays","Lon","Lat") ,
                                      subset = (City == "ANKARA"))

average_sunrise_time <- subset(final_list,
                                     select = c("Months","AverageSunriseTime","Lon","Lat") ,
                                     subset = (City == "ANKARA"))

monthly_total_rainfall_average <- subset(final_list,
                                 select = c("Months","MonthlyTotalRainfallAverage","Lon","Lat") ,
                                 subset = (City == "ANKARA"))


## Plots 

library(ggplot2)
# AverageTemprature
positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
ggplot(data = average_temperature,
       aes(Months,AverageTemprature)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(limits = positions) +
  stat_summary(fun.y = sum, 
               geom = "bar",
               fill=I("red"))

# AverageMaximumTemperature    
positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
ggplot(data = average_maximum_temperature,
       aes(Months,AverageMaximumTemperature)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(limits = positions) +
  stat_summary(fun.y = sum, 
               geom = "bar",
               fill=I("blue"))


# AverageMinimumTemperature
positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
ggplot(data = average_minimum_temperature,
       aes(Months,AverageMinimumTemperature)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(limits = positions) +
  stat_summary(fun.y = sum, 
               geom = "bar",
               fill=I("green"))

# AverageNumberofRainyDays
positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
ggplot(data = average_number_of_rainy_days,
       aes(Months,AverageNumberofRainyDays)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(limits = positions) +
  stat_summary(fun.y = sum, 
               geom = "bar",
               fill=I("Purple"))


# AverageSunriseTime
positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
ggplot(data = average_sunrise_time,
       aes(Months,AverageSunriseTime)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(limits = positions) +
  stat_summary(fun.y = sum, 
               geom = "bar",
               fill=I("yellow"))


# MonthlyTotalRainfallAverage
positions <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
ggplot(data = monthly_total_rainfall_average,
       aes(Months,MonthlyTotalRainfallAverage)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(limits = positions) +
  stat_summary(fun.y = sum, 
               geom = "bar",
               fill=I("pink"))





###########################################################

## MAP
library(ggmap)
library(grid)

geocode("Turkey")
map <- get_map(location = c(lon=35.24332,lat=38.96375),zoom = 6,maptype = "roadmap",source = "google")
map <- ggmap(map)
map


aa <- subset(final_list,
                               select = c("Months","AverageTemprature","Lon","Lat") ,
                               subset = (Months == "MAY"))
aa


map + geom_point(data=aa, aes(x=aa$Lon, y=aa$Lat, color=AverageTemprature, size=AverageTemprature)) +
  scale_color_gradient(low = "yellow", high = "red")



