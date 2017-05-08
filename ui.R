library(ggplot2)
library(leaflet)
dataset <- read.csv('data/data.csv')

fluidPage(

  title = "Weather",
  
  br(),
  tags$style(HTML("
    div#htmlwidget-c06079682f0c96c81442                {width:100%; height:100%; background-color: aqua;  color:black}
  ")),
  tabsetPanel(
   tabPanel("MAP", map <- leaflet() %>% setView(lng = 35.24332, lat = 38.96375, zoom = 6) %>%
              # Base groups
              addTiles(group = "OSM (default)") %>%
              addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
              addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
              # Overlay groups
              #addCircles(~long, ~lat, ~10^mag/5, stroke = F, group = "Quakes") %>%
              #addPolygons(data = outline, lng = ~long, lat = ~lat,
               #           fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>%
              # Layers control
              addLayersControl(
                baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                overlayGroups = c("Quakes", "Outline"),
                options = layersControlOptions(collapsed = FALSE)
              )
   ),
   
   tabPanel("HISTOGRAMS", br(),
            fluidPage(
              column(6, offset = 3,
                     selectInput('x', 'CITY', dataset$City, selected = TRUE, width = '100%'), 
                     selectInput('y', 'FILTER', colnames(dataset), width = '100%')
              ), 
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
                     
              )
            )),
   tabPanel("OTHER", tableOutput("table"))
  )
)