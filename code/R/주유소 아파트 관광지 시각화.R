library(sp)
library(leaflet)
h <- read.csv('(통합)성남수원용인거주지인근적합충전소위치_완본.csv')

summary(h)

tcu_map <- "https://api.mapbox.com/styles/v1/lihit98/ciob24gdc004vagm8r6x6l65m/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibGloaXQ5OCIsImEiOiJjaW84ZGdidDgwMm9qdnpsemgzNzhmOG5kIn0.pREIC-3rIY7ZXQFQdh86uQ"

summary(h$branch)
#h1 <- h[h$branch=='전기충전소',]
h2 <- h[h$branch=='주유소',] #navy
h3 <- h[h$branch=='주요시설',] #red
h4 <- h[h$branch=='관광지',] #saddlebrown
#h5 <- h[h$branch=='산업단지',] #black

m <- leaflet() %>% addTiles()

m <- m %>% setView(127.258282, 37.569690, zoom = 9)%>%
  addCircles(lng =h2$LON , lat =h2$LAT, radius = 0.1,color = 'navy', opacity = 1)%>%
  addCircles(lng =h3$LON , lat =h3$LAT, radius = 0.1,color = 'red', opacity = 0)%>%
  addCircles(lng =h4$LON , lat =h4$LAT, radius = 0.1,color = 'saddlebrown', opacity = 0)%>%
  addTiles(urlTemplate = tcu_map)


#주유소만 이거
m <- m %>% setView(127.258282, 37.569690, zoom = 9)%>%
  addCircles(lng =h2$LON , lat =h2$LAT, radius = 0.1,color = 'navy', opacity = 1)%>%
  addTiles(urlTemplate = tcu_map)


m
