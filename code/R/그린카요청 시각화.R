library(sp)
library(leaflet)
h <- read.csv('그린카page1_62_위경도값 (2).csv')

tcu_map <- "https://api.mapbox.com/styles/v1/lihit98/ciob24gdc004vagm8r6x6l65m/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibGloaXQ5OCIsImEiOiJjaW84ZGdidDgwMm9qdnpsemgzNzhmOG5kIn0.pREIC-3rIY7ZXQFQdh86uQ"

m <- leaflet() %>% addTiles()

m <- m %>% setView(127.258282, 37.569690, zoom = 9)%>%addCircles(lng =h$X , lat =h$Y, radius = 0.1,color = 'dodgerblue', opacity = 1)%>%addTiles(urlTemplate = tcu_map)
m

