library(sp)
library(leaflet)
h <- read.csv('안양시 만안 위경도 클러스터링.csv')

tcu_map <- "https://api.mapbox.com/styles/v1/lihit98/ciob24gdc004vagm8r6x6l65m/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibGloaXQ5OCIsImEiOiJjaW84ZGdidDgwMm9qdnpsemgzNzhmOG5kIn0.pREIC-3rIY7ZXQFQdh86uQ"

m <- leaflet() %>% addTiles()

summary(h$juyuso1.kmeans.cluster)
h1 <- h[h$juyuso1.kmeans.cluster==1,] #dodgerblue
h2 <- h[h$juyuso1.kmeans.cluster==2,] #skyblue
h3 <- h[h$juyuso1.kmeans.cluster==3,] #darkorange

summary()

m <- m %>% setView(126.998827, 37.434839, zoom = 9)%>%
  addCircles(lng =h1$경도 , lat =h1$위도, radius = 0.1,color = 'blue', opacity = 1)%>%
  addCircles(lng =h2$경도 , lat =h2$위도, radius = 0.1,color = 'skyblue', opacity = 1)%>%
  addCircles(lng =h3$경도 , lat =h3$위도, radius = 0.1,color = 'darkorange', opacity = 1)%>%
  addTiles(urlTemplate = tcu_map)
m

