#install.packages("leaflet")

library(leaflet)


library(leaflet)
m <- leaflet() %>%
  fitBounds(-20,65,22,39) %>% 
  addProviderTiles(providers$CartoDB.Positron)
 addTiles()%>% 
addMarkers(lng=2.3522219, lat=48.856614, popup="Nombre de commande")
m

#Pour le zoom

#m <- leaflet() %>%
 # addTiles() %>%  # Add default OpenStreetMap map tiles
 # addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map