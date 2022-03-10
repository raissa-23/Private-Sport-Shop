
#La carte de l'europe

#install.packages("leaflet")
library(dplyr)

#pré-traitement des données
total_vente <- read.table("pays_ventes.csv") 
tot_vente <- total_vente %>% 
  filter(!is.na(total_vente$countries))
coord <- read_excel("Coordonees.xlsx")
coo <- coord %>% 
  filter(coord$Countries!="Andorre")
vente_coord <- cbind(coo,tot_vente) %>% 
  select(-countries) %>% 
  rename(countries=Countries)

View(vente_coord)
#str(vente_coord)



#Pour generer la carte
leaflet(vente_coord,options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
  fitBounds(-20,65,22,39) %>%
  addTiles()%>%
  
  #pour le design de la carte on a :
  addProviderTiles(providers$Esri.NatGeoWorldMap)%>%
  #ou
  #addProviderTiles("Stamen.TonerHybrid")%>%
  #addProviderTiles(providers$CartoDB.Positron)%>%
  
  
  #pouvoir afficher un message ? l'atitude et longitude design?
  #addMarkers(lng=2.209667, lat=46.232193, popup="Nous sommes les chmpions du maraton Web")
  
  #recuperation des données pour les afficher dans la carte
  #On peut Remplacer en argument l'information que l'on veut afficher dans la carte (ex:total_ventes_pays2021)
  addMarkers(~long, ~lat, popup = ~as.character(total_ventes_pays2021), label = ~as.character(total_ventes_pays2021))
  

#Le code ci-dessous c'est pour afficher les cercle dans la carte
#addCircleMarkers(data=vente_coord, radius=~sqrt(total_ventes_pays2021)/40,weight = 1, color = "#a500a5", fillOpacity = 0.5) 
#m












