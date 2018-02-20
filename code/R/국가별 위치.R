library(sp)
library(leaflet)
h <- read.csv('국가별 위치.csv')

tcu_map <- "https://api.mapbox.com/styles/v1/lihit98/ciob24gdc004vagm8r6x6l65m/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibGloaXQ5OCIsImEiOiJjaW84ZGdidDgwMm9qdnpsemgzNzhmOG5kIn0.pREIC-3rIY7ZXQFQdh86uQ"

summary(h$branch)
h1 <- h[h$국가=='대한민국',]
h2 <- h[h$국가=='일본',]


m <- leaflet() %>% addTiles()

m <- m %>% setView(134.932067, 37.191338, zoom = 6)%>%addCircles(lng =h1$경도, lat =h1$위도, radius = 0.1,color = 'blue', opacity = 0.5)%>%addCircles(lng =h2$경도 , lat =h2$위도, radius = 0.1,opacity = 0.5, color = 'red')%>%addTiles(urlTemplate = tcu_map)
m

