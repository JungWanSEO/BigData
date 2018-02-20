library(sp)
library(leaflet)
h <- read.csv('(통합)성남수원용인거주지인근적합충전소위치0816.csv')

tcu_map <- "https://api.mapbox.com/styles/v1/lihit98/ciob24gdc004vagm8r6x6l65m/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibGloaXQ5OCIsImEiOiJjaW84ZGdidDgwMm9qdnpsemgzNzhmOG5kIn0.pREIC-3rIY7ZXQFQdh86uQ"

m <- leaflet() %>% addTiles()

m <- m %>% setView(127.258282, 37.569690, zoom = 9)%>%addCircles(lng =h2$LON , lat =h2$LAT, radius = 0.1,color = 'red', opacity = 1)%>%addCircles(lng =h1$LON , lat =h1$LAT, radius = 0.1,opacity = 1)%>%addTiles(urlTemplate = tcu_map)
m



summary(h$branch)
#h1 <- h[h$branch=='전기충전소',]
h2 <- h[h$branch=='주유소',] #navy
h3 <- h[h$branch=='주요시설',] #red
h4 <- h[h$branch=='관광지',] #saddlebrown
h5 <- h[h$branch=='산업단지',] #black

m <- m %>% setView(127.258282, 37.569690, zoom = 9)%>%addCircles(lng =h5$LON , lat =h5$LAT, radius = 10, stroke = T,opacity = 1, fillOpacity = 1, color = 'black', weight = 5, fill = FALSE)%>%addTiles(urlTemplate = tcu_map)
m

?addCircles
blue4

summary(h$branch)
