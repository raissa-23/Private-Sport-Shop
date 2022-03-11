library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)
install.packages(lubridate)
library(lubridate)
#Chargement pour les fichiers Adresses
addresses_2021 <- read.table("addresses/addresses_2021.csv", sep=";", header= TRUE)
addresses_2020 <- read.table("addresses/addresses_2020.csv", sep=";", header= TRUE )
addresses_2019 <- read.table("addresses/addresses_2019.csv", sep=";", header= TRUE)
addresses_2018 <- read.table("addresses/addresses_2018.csv", sep=";", header= TRUE)
addresses_2017 <- read.table("addresses/addresses_2017.csv", sep=";", header= TRUE)
addresses_2016 <- read.table("addresses/addresses_2016.csv", sep=";", header= TRUE)
addresses_2015 <- read.table("addresses/addresses_2015.csv", sep=";", header= TRUE)
addresses_2014 <- read.table("addresses/addresses_2014.csv", sep=";", header= TRUE)
addresses_2013 <- read.table("addresses/addresses_2013.csv", sep=";", header= TRUE)
addresses_2012 <- read.table("addresses/addresses_2012.csv", sep=";", header= TRUE)
addresses_2011 <- read.table("addresses/addresses_2011.csv", sep=";", header= TRUE)

#Chargement pour les fichiers Orders
orders_21 <- read.table("orders/orders_2021.csv", sep=";", header= TRUE)
orders_20 <- read.table("orders/orders_2020.csv", sep=";", header= TRUE)
orders_19 <- read.table("orders/orders_2019.csv", sep=";", header= TRUE)
orders_18 <- read.table("orders/orders_2018.csv", sep=";", header= TRUE)
orders_17 <- read.table("orders/orders_2017.csv", sep=";", header= TRUE)
orders_16 <- read.table("orders/orders_2016.csv", sep=";", header= TRUE)
orders_15 <- read.table("orders/orders_2015.csv", sep=";", header= TRUE)
orders_14 <- read.table("orders/orders_2014.csv", sep=";", header= TRUE)
orders_13 <- read.table("orders/orders_2013.csv", sep=";", header= TRUE)
orders_12 <- read.table("orders/orders_2012.csv", sep=";", header= TRUE)
orders_11 <- read.table("orders/orders_2011.csv", sep=";", header= TRUE)

#On s'occupe tout d'abord des dataset des commandes, l'id_address permettra de joindre les commandes aux addresses et pays des fichiers address_XX 
# Suppression de colonnes
#On conserve uniquement les colonnes id_customer et id_address pour chaque année des dataset "Orders"

orders_21 <-orders_21%>%
  select(id_customer,id_address)

orders_20 <-orders_20%>%
  select(id_customer,id_address)

orders_19 <-orders_19%>%
  select(id_customer,id_address)

orders_18 <-orders_18%>%
  select(id_customer,id_address)

orders_17 <-orders_17%>%
  select(id_customer,id_address)

orders_16 <-orders_16%>%
  select(id_customer,id_address)

orders_15 <-orders_15%>%
  select(id_customer,id_address)

orders_14 <-orders_14%>%
  select(id_customer,id_address)

orders_13 <-orders_13%>%
  select(id_customer,id_address)

orders_12 <-orders_12%>%
  select(id_customer,id_address)

orders_11 <-orders_11%>%
  select(id_customer,id_address)

##########

#On ajoute les dataset de "orders" à la suite pour réunir toutes les années

orders <- rbind(orders_21,orders_20)
orders <- rbind(orders,orders_19)
orders <- rbind(orders,orders_18)
orders <- rbind(orders,orders_17)
orders <- rbind(orders,orders_16)
orders <- rbind(orders,orders_15)
orders <- rbind(orders,orders_14)
orders <- rbind(orders,orders_13)
orders <- rbind(orders,orders_12)
orders <- rbind(orders,orders_11)



# Nettoyage Country par année (Andorre, Monaco etc...)

###2021###
newadress2021 <- addresses_2021 %>% 
  mutate(country_mod=(ifelse(str_sub((addresses_2021$country),1,8)%in%c("France -","France","SECTEUR ","Monaco") ,"France",addresses_2021$country)))

newadress2021 <- newadress2021%>% 
  mutate(country_mod=(ifelse(newadress2021$country_mod%in%c("Andorre") ,"Espagne",newadress2021$country_mod)))

###2020####

newadress2020 <- addresses_2020 %>% 
  mutate(country_mod=(ifelse(str_sub((addresses_2020$country),1,8)%in%c("France -","France","SECTEUR ","Monaco") ,"France",addresses_2020$country)))

newadress2020 <- newadress2020%>% 
  mutate(country_mod=(ifelse(newadress2020$country_mod%in%c("Andorre") ,"Espagne",newadress2020$country_mod)))

###2019###
newadress2019 <- addresses_2019 %>% 
  mutate(country_mod=(ifelse(str_sub((addresses_2019$country),1,8)%in%c("France -","France","SECTEUR ","Monaco") ,"France",addresses_2019$country)))

newadress2019 <- newadress2019%>% 
  mutate(country_mod=(ifelse(newadress2019$country_mod%in%c("Andorre") ,"Espagne",newadress2019$country_mod)))

###2018###
newadress2018 <- addresses_2018 %>% 
  mutate(country_mod=(ifelse(str_sub((addresses_2018$country),1,8)%in%c("France -","France","SECTEUR ","Monaco") ,"France",addresses_2018$country)))

newadress2018 <- newadress2018%>% 
  mutate(country_mod=(ifelse(newadress2018$country_mod%in%c("Andorre") ,"Espagne",newadress2018$country_mod)))

###2017###
newadress2017 <- addresses_2017 %>% 
  mutate(country_mod=(ifelse(str_sub((addresses_2017$country),1,8)%in%c("France -","France","SECTEUR ","Monaco") ,"France",addresses_2017$country)))

newadress2017 <- newadress2017%>% 
  mutate(country_mod=(ifelse(newadress2017$country_mod%in%c("Andorre") ,"Espagne",newadress2017$country_mod)))

###2016###
newadress2016 <- addresses_2016 %>% 
  mutate(country_mod=(ifelse(str_sub((addresses_2016$country),1,8)%in%c("France -","France","SECTEUR ","Monaco") ,"France",addresses_2016$country)))

newadress2016 <- newadress2016%>% 
  mutate(country_mod=(ifelse(newadress2016$country_mod%in%c("Andorre") ,"Espagne",newadress2016$country_mod)))

###2015###
newadress2015 <- addresses_2015 %>% 
  mutate(country_mod=(ifelse(str_sub((addresses_2015$country),1,8)%in%c("France -","France","SECTEUR ","Monaco") ,"France",addresses_2015$country)))

newadress2015 <- newadress2015%>% 
  mutate(country_mod=(ifelse(newadress2015$country_mod%in%c("Andorre") ,"Espagne",newadress2015$country_mod)))

###2014###
newadress2014 <- addresses_2014 %>% 
  mutate(country_mod=(ifelse(str_sub((addresses_2014$country),1,8)%in%c("France -","France","SECTEUR ","Monaco") ,"France",addresses_2014$country)))

newadress2014 <- newadress2014%>% 
  mutate(country_mod=(ifelse(newadress2014$country_mod%in%c("Andorre") ,"Espagne",newadress2014$country_mod)))

###2013###

newadress2013 <- addresses_2013 %>% 
  mutate(country_mod=(ifelse(str_sub((addresses_2013$country),1,8)%in%c("France -","France","SECTEUR ","Monaco") ,"France",addresses_2013$country)))

newadress2013 <- newadress2013%>% 
  mutate(country_mod=(ifelse(newadress2013$country_mod%in%c("Andorre") ,"Espagne",newadress2013$country_mod)))

###2012###
newadress2012 <- addresses_2012 %>% 
  mutate(country_mod=(ifelse(str_sub((addresses_2012$country),1,8)%in%c("France -","France","SECTEUR ","Monaco") ,"France",addresses_2012$country)))

newadress2012 <- newadress2012%>% 
  mutate(country_mod=(ifelse(newadress2012$country_mod%in%c("Andorre") ,"Espagne",newadress2012$country_mod)))

###2011###
newadress2011 <- addresses_2011 %>% 
  mutate(country_mod=(ifelse(str_sub((addresses_2011$country),1,8)%in%c("France -","France","SECTEUR ","Monaco") ,"France",addresses_2011$country)))

newadress2011 <- newadress2011%>% 
  mutate(country_mod=(ifelse(newadress2011$country_mod%in%c("Andorre") ,"Espagne",newadress2011$country_mod)))


##################################
#Tout comme pour les dataset des commandes, nous allons procéder à quelque modification
#Conserver uniquement la nouvelle colonne des pays (country_mod et l'id_address)
#Voir un exemple ci-dessous

View(newadress2018)

#On supprime l'ancienne colonne des country

newadress2021 <-newadress2021%>%
  select(id_address,country_mod)

newadress2020 <-newadress2020%>%
  select(id_address,country_mod)

newadress2019 <-newadress2019%>%
  select(id_address,country_mod)

newadress2018 <-newadress2018%>%
  select(id_address,country_mod)

newadress2017 <-newadress2017%>%
  select(id_address,country_mod)

newadress2016 <-newadress2016%>%
  select(id_address,country_mod)

newadress2015 <-newadress2015%>%
  select(id_address,country_mod)

newadress2014 <-newadress2014%>%
  select(id_address,country_mod)

newadress2013 <-newadress2013%>%
  select(id_address,country_mod)

newadress2012 <-newadress2012%>%
  select(id_address,country_mod)

newadress2011 <-newadress2011%>%
  select(id_address,country_mod)


#####
#On ajoute tous les dataset les uns après les autres comme un peu plus haut


newadress <- rbind(newadress2021,newadress2020)
newadress <- rbind(newadress,newadress2019)
newadress <- rbind(newadress,newadress2018)
newadress <- rbind(newadress,newadress2017)
newadress <- rbind(newadress,newadress2016)
newadress <- rbind(newadress,newadress2015)
newadress <- rbind(newadress,newadress2014)
newadress <- rbind(newadress,newadress2013)
newadress <- rbind(newadress,newadress2012)
newadress <- rbind(newadress,newadress2011)

#Désormais nous allons joindre le dataset newadress et orders à l'aide de leurs colonnne commune qui id_address


id <- merge(newadress,orders,by="id_address",all=TRUE)
View(id)

identifiant <- id%>%
  group_by(country_mod) %>% 
  count(id_customer,name="NbClients")%>%
  arrange(desc(NbClients))%>% 
  select(-id_customer) 

sommeClientsParPays <- identifiant %>% 
  group_by(country_mod) %>% 
  mutate(sommePays=sum(NbClients)) %>% 
  select(-NbClients) %>% 
  distinct() %>% 
  rename(countries=country_mod) %>% 
  rename(NbClients=sommePays)
  
  
#########
#Visualisation

sommeClientsParPays <-sommeClientsParPays%>%drop_na()

#Visualisation avec la France des nb de comptes par pays

Visu_F <-ggplot(data=sommeClientsParPays, mapping = aes(x=countries, y=NbClients)) +
            geom_col()
Visu_F

#Visualisation sans la France

sansFrance <-sommeClientsParPays%>%
  filter(countries!="France")

Visu_sansFrance <- ggplot(data=sansFrance, mapping = aes(x=countries, y=NbClients)) +
                      geom_col()
Visu_sansFrance

#Sans le top 5 (France,Italie, Espagne, Belgique et Allemagne)

sansVersion <-sommeClientsParPays%>%
  filter(countries!="France")%>%
  filter(countries!="Espagne")%>%
  filter(countries!="Italie")%>%
  filter(countries!="Belgique")%>%
  filter(countries!="Allemagne")

Visu_sanstop5 <- ggplot(data=sansVersion, mapping = aes(x=countries, y=NbClients)) +
                    geom_col()
Visu_sanstop5

