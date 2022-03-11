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

#Changer les noms de colonnes "id_customer"

orders_21 <- orders_21%>%
  rename(id_customer2021=id_customer)

orders_20 <- orders_20%>%
  rename(id_customer2020=id_customer)

orders_19 <- orders_19%>%
  rename(id_customer2019=id_customer)

orders_18 <- orders_18%>%
  rename(id_customer2018=id_customer)

orders_17 <- orders_17%>%
  rename(id_customer2017=id_customer)

orders_16 <- orders_16%>%
  rename(id_customer2016=id_customer)

orders_15 <- orders_15%>%
  rename(id_customer2015=id_customer)

orders_14 <- orders_14%>%
  rename(id_customer2014=id_customer)

orders_13 <- orders_13%>%
  rename(id_customer2013=id_customer)

orders_12 <- orders_12%>%
  rename(id_customer2012=id_customer)

orders_11 <- orders_11%>%
  rename(id_customer2011=id_customer)



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
#Tout comme pour les dataset des commandes, nous allons procédons à quelque modification
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


#On joint les datasets "orders" et "address" par année

id_2021 <- merge(newadress2021,orders_21,by="id_address",all=TRUE)
id_2020 <- merge(newadress2020,orders_20,by="id_address",all=TRUE)
id_2019 <- merge(newadress2019,orders_19,by="id_address",all=TRUE)
id_2018 <- merge(newadress2018,orders_18,by="id_address",all=TRUE)
id_2017 <- merge(newadress2017,orders_17,by="id_address",all=TRUE)
id_2016 <- merge(newadress2016,orders_16,by="id_address",all=TRUE)
id_2015 <- merge(newadress2015,orders_15,by="id_address",all=TRUE)
id_2014 <- merge(newadress2014,orders_14,by="id_address",all=TRUE)
id_2013 <- merge(newadress2013,orders_13,by="id_address",all=TRUE)
id_2012 <- merge(newadress2012,orders_12,by="id_address",all=TRUE)
id_2011 <- merge(newadress2011,orders_11,by="id_address",all=TRUE)

#On enlève les éventuels données manquantes pour chaque datasets "id_XXXX" pour chaque année

id_2021 <- id_2021%>%drop_na()
id_2020 <- id_2020%>%drop_na()
id_2019 <- id_2019%>%drop_na()
id_2018 <- id_2018%>%drop_na()
id_2017 <- id_2017%>%drop_na()
id_2016 <- id_2016%>%drop_na()
id_2015 <- id_2015%>%drop_na()
id_2014 <- id_2014%>%drop_na()
id_2013 <- id_2013%>%drop_na()
id_2012 <- id_2012%>%drop_na()
id_2011 <- id_2011%>%drop_na()

View(id_2018)

####Somme Clients par Pays & Année


#############2021################

identifiant21 <- id_2021%>%
  group_by(country_mod) %>% 
  count(id_customer2021,name="NbClients")%>%
  arrange(desc(NbClients))%>% 
  select(-id_customer2021) 

Nb_client2021 <- identifiant21 %>% 
  group_by(country_mod) %>% 
  mutate(sommePays2021=sum(NbClients)) %>% 
  select(-NbClients) %>% 
  distinct()

###2020
identifiant20 <- id_2020%>%
  group_by(country_mod) %>% 
  count(id_customer2020,name="NbClients")%>%
  arrange(desc(NbClients))%>% 
  select(-id_customer2020) 

Nb_client2020 <- identifiant20 %>% 
  group_by(country_mod) %>% 
  mutate(sommePays2020=sum(NbClients)) %>% 
  select(-NbClients) %>% 
  distinct()
####2019
identifiant19 <- id_2019%>%
  group_by(country_mod) %>% 
  count(id_customer2019,name="NbClients")%>%
  arrange(desc(NbClients))%>% 
  select(-id_customer2019) 

Nb_client2019 <- identifiant19 %>% 
  group_by(country_mod) %>% 
  mutate(sommePays2019=sum(NbClients)) %>% 
  select(-NbClients) %>% 
  distinct()

###2018

identifiant18 <- id_2018%>%
  group_by(country_mod) %>% 
  count(id_customer2018,name="NbClients")%>%
  arrange(desc(NbClients))%>% 
  select(-id_customer2018) 

Nb_client2018 <- identifiant18 %>% 
  group_by(country_mod) %>% 
  mutate(sommePays2018=sum(NbClients)) %>% 
  select(-NbClients) %>% 
  distinct()
###2017
identifiant17 <- id_2017%>%
  group_by(country_mod) %>% 
  count(id_customer2017,name="NbClients")%>%
  arrange(desc(NbClients))%>% 
  select(-id_customer2017) 

Nb_client2017 <- identifiant17 %>% 
  group_by(country_mod) %>% 
  mutate(sommePays2017=sum(NbClients)) %>% 
  select(-NbClients) %>% 
  distinct()
##2016
identifiant16 <- id_2016%>%
  group_by(country_mod) %>% 
  count(id_customer2016,name="NbClients")%>%
  arrange(desc(NbClients))%>% 
  select(-id_customer2016) 

Nb_client2016 <- identifiant16 %>% 
  group_by(country_mod) %>% 
  mutate(sommePays2016=sum(NbClients)) %>% 
  select(-NbClients) %>% 
  distinct()
####2015
identifiant15 <- id_2015%>%
  group_by(country_mod) %>% 
  count(id_customer2015,name="NbClients")%>%
  arrange(desc(NbClients))%>% 
  select(-id_customer2015) 

Nb_client2015 <- identifiant15 %>% 
  group_by(country_mod) %>% 
  mutate(sommePays2015=sum(NbClients)) %>% 
  select(-NbClients) %>% 
  distinct()

#2014

identifiant14 <- id_2014%>%
  group_by(country_mod) %>% 
  count(id_customer2014,name="NbClients")%>%
  arrange(desc(NbClients))%>% 
  select(-id_customer2014) 

Nb_client2014 <- identifiant14 %>% 
  group_by(country_mod) %>% 
  mutate(sommePays2014=sum(NbClients)) %>% 
  select(-NbClients) %>% 
  distinct()

#2013

identifiant13 <- id_2013%>%
  group_by(country_mod) %>% 
  count(id_customer2013,name="NbClients")%>%
  arrange(desc(NbClients))%>% 
  select(-id_customer2013) 

Nb_client2013 <- identifiant13 %>% 
  group_by(country_mod) %>% 
  mutate(sommePays2013=sum(NbClients)) %>% 
  select(-NbClients) %>% 
  distinct()

#2012

identifiant12 <- id_2012%>%
  group_by(country_mod) %>% 
  count(id_customer2012,name="NbClients")%>%
  arrange(desc(NbClients))%>% 
  select(-id_customer2012) 

Nb_client2012 <- identifiant12 %>% 
  group_by(country_mod) %>% 
  mutate(sommePays2012=sum(NbClients)) %>% 
  select(-NbClients) %>% 
  distinct()

#2011

identifiant11 <- id_2011%>%
  group_by(country_mod) %>% 
  count(id_customer2011,name="NbClients")%>%
  arrange(desc(NbClients))%>% 
  select(-id_customer2011) 

Nb_client2011 <- identifiant11 %>% 
  group_by(country_mod) %>% 
  mutate(sommePays2011=sum(NbClients)) %>% 
  select(-NbClients) %>% 
  distinct()

View(Nb_client2011)

table <- left_join(Nb_client2021,Nb_client2020, by="country_mod")
table <- left_join(table,Nb_client2019, by="country_mod")
table <- left_join(table,Nb_client2018, by="country_mod")
table <- left_join(table,Nb_client2017, by="country_mod")
table <- left_join(table,Nb_client2016, by="country_mod")
table <- left_join(table,Nb_client2015, by="country_mod")
table <- left_join(table,Nb_client2014, by="country_mod")
table <- left_join(table,Nb_client2013, by="country_mod")
table <- left_join(table,Nb_client2012, by="country_mod")
table <- left_join(table,Nb_client2011, by="country_mod")

View(table)
