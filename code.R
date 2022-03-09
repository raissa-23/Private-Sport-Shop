library(dplyr)
library(stringr)
library(ggplot2)
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



# Nettoyage Country

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

#Merge par Année (De Orders & Addresses~newadress)

mergeOrderAdress21 <- left_join(orders_21,newadress2021,by="id_address")
mergeOrderAdress20 <- left_join(orders_20,newadress2020,by="id_address")
mergeOrderAdress19 <- left_join(orders_19,newadress2019,by="id_address")
mergeOrderAdress18 <- left_join(orders_18,newadress2018,by="id_address")
mergeOrderAdress17 <- left_join(orders_17,newadress2017,by="id_address")
mergeOrderAdress16 <- left_join(orders_16,newadress2016,by="id_address")
mergeOrderAdress15 <- left_join(orders_15,newadress2015,by="id_address")
mergeOrderAdress14 <- left_join(orders_14,newadress2014,by="id_address")
mergeOrderAdress13 <- left_join(orders_13,newadress2013,by="id_address")
mergeOrderAdress12 <- left_join(orders_12,newadress2012,by="id_address")
mergeOrderAdress11 <- left_join(orders_11,newadress2011,by="id_address")

# On renomme les noms de colonnes 


OrderAdress21 <- mergeOrderAdress21%>% 
  rename(id_address2021=id_address) %>% 
  rename(version2021=version) %>%
  rename(total_paid2021=total_paid) %>%
  rename(order_date2021=order_date) %>%
  rename(country2021=country_mod) %>%
  select(-id_order,-country)


OrderAdress20 <- mergeOrderAdress20%>% 
  rename(id_address2020=id_address) %>% 
  rename(version2020=version) %>%
  rename(total_paid2020=total_paid) %>%
  rename(order_date2020=order_date) %>%
  rename(country2020=country_mod) %>%
  select(-id_order,-country)


OrderAdress19 <- mergeOrderAdress19%>% 
  rename(id_address2019=id_address) %>% 
  rename(version2019=version) %>%
  rename(total_paid2019=total_paid) %>%
  rename(order_date2019=order_date) %>%
  rename(country2019=country_mod) %>%
  select(-id_order,-country)


OrderAdress18 <- mergeOrderAdress18%>% 
  rename(id_address2018=id_address) %>% 
  rename(version2018=version) %>%
  rename(total_paid2018=total_paid) %>%
  rename(order_date2018=order_date) %>%
  rename(country2018=country_mod) %>%
  select(-id_order,-country)


OrderAdress17 <- mergeOrderAdress17%>% 
  rename(id_address17=id_address) %>% 
  rename(version2017=version) %>%
  rename(total_paid2017=total_paid) %>%
  rename(order_date2017=order_date) %>%
  rename(country2017=country_mod) %>%
  select(-id_order,-country)


OrderAdress16 <- mergeOrderAdress16%>% 
  rename(id_address2016=id_address) %>% 
  rename(version2016=version) %>%
  rename(total_paid2016=total_paid) %>%
  rename(order_date2016=order_date) %>%
  rename(country2016=country_mod) %>%
  select(-id_order,-country)

OrderAdress15 <- mergeOrderAdress15%>% 
  rename(id_address2015=id_address) %>% 
  rename(version2015=version) %>%
  rename(total_paid2015=total_paid) %>%
  rename(order_date2015=order_date) %>%
  rename(country2015=country_mod) %>%
  select(-id_order,-country)

OrderAdress14 <- mergeOrderAdress14%>% 
  rename(id_address2014=id_address) %>% 
  rename(version2014=version) %>%
  rename(total_paid2014=total_paid) %>%
  rename(order_date2014=order_date) %>%
  rename(country2014=country_mod) %>%
  select(-id_order,-country)


OrderAdress13 <- mergeOrderAdress13%>% 
  rename(id_address2013=id_address) %>% 
  rename(version2013=version) %>%
  rename(total_paid2013=total_paid) %>%
  rename(order_date2013=order_date) %>%
  rename(country2013=country_mod) %>%
  select(-id_order,-country)

OrderAdress12 <- mergeOrderAdress12%>% 
  rename(id_address2012=id_address) %>% 
  rename(version2012=version) %>%
  rename(total_paid2012=total_paid) %>%
  rename(order_date2012=order_date) %>%
  rename(country2012=country_mod) %>%
  select(-id_order,-country)


OrderAdress11 <- mergeOrderAdress11%>% 
  rename(id_address2011=id_address) %>% 
  rename(version2011=version) %>%
  rename(total_paid2011=total_paid) %>%
  rename(order_date2011=order_date) %>%
  rename(country2011=country_mod) %>%
  select(-id_order,-country)

# Ajout des données de chaque année

OrderAdressAnnee <- left_join(OrderAdress21,OrderAdress20,by="id_customer")
OrderAdressAnnee <- left_join(OrderAdressAnnee,OrderAdress19, by="id_customer")
OrderAdressAnnee <- left_join(OrderAdressAnnee,OrderAdress18, by="id_customer")
OrderAdressAnnee <- left_join(OrderAdressAnnee,OrderAdress17, by="id_customer")
OrderAdressAnnee <- left_join(OrderAdressAnnee,OrderAdress16, by="id_customer")
OrderAdressAnnee <- left_join(OrderAdressAnnee,OrderAdress15, by="id_customer")
OrderAdressAnnee <- left_join(OrderAdressAnnee,OrderAdress14, by="id_customer")
OrderAdressAnnee <- left_join(OrderAdressAnnee,OrderAdress13, by="id_customer")
OrderAdressAnnee <- left_join(OrderAdressAnnee,OrderAdress12, by="id_customer")
OrderAdressAnnee <- left_join(OrderAdressAnnee,OrderAdress11, by="id_customer")



#########

OrderAdressAnneeCL <- OrderAdressAnnee %>% 
  group_by(id_customer) %>% 
  mutate(total_ventes_CL2011=sum(total_paid2011)) %>% 
  mutate(total_ventes_CL2012=sum(total_paid2012)) %>% 
  mutate(total_ventes_CL2013=sum(total_paid2013)) %>% 
  mutate(total_ventes_CL2014=sum(total_paid2014)) %>% 
  mutate(total_ventes_CL2015=sum(total_paid2015)) %>% 
  mutate(total_ventes_CL2016=sum(total_paid2016)) %>% 
  mutate(total_ventes_CL2017=sum(total_paid2017)) %>% 
  mutate(total_ventes_CL2018=sum(total_paid2018)) %>% 
  mutate(total_ventes_CL2019=sum(total_paid2019)) %>% 
  mutate(total_ventes_CL2020=sum(total_paid2020)) %>% 
  mutate(total_ventes_CL2021=sum(total_paid2021)) %>% 
  select(id_customer,total_paid2021,total_paid2020,total_paid2019,total_paid2018,total_paid2017)

#################


OrderAdressAnnepays21 <- OrderAdress21 %>% 
  group_by(country2021) %>% 
  mutate(total_ventes_pays2021=sum(total_paid2021)) %>% 
  select(country2021,total_ventes_pays2021) %>% 
  distinct() %>% 
  rename(countries=country2021)

OrderAdressAnnepays20 <- OrderAdress20 %>% 
  group_by(country2020) %>% 
  mutate(total_ventes_pays2020=sum(total_paid2020)) %>% 
  select(country2020,total_ventes_pays2020) %>% 
  distinct() %>% 
  rename(countries=country2020) 

OrderAdressAnnepays19 <- OrderAdress19 %>% 
  group_by(country2019) %>% 
  mutate(total_ventes_pays2019=sum(total_paid2019)) %>% 
  select(country2019,total_ventes_pays2019) %>% 
  distinct() %>% 
  rename(countries=country2019)

OrderAdressAnnepays18 <- OrderAdress18 %>% 
  group_by(country2018) %>% 
  mutate(total_ventes_pays2018=sum(total_paid2018)) %>% 
  select(country2018,total_ventes_pays2018) %>% 
  distinct() %>% 
  rename(countries=country2018)

OrderAdressAnnepays17 <- OrderAdress17 %>% 
  group_by(country2017) %>% 
  mutate(total_ventes_pays2017=sum(total_paid2017)) %>% 
  select(country2017,total_ventes_pays2017) %>% 
  distinct() %>% 
  rename(countries=country2017)

OrderAdressAnnepays16 <- OrderAdress16 %>% 
  group_by(country2016) %>% 
  mutate(total_ventes_pays2016=sum(total_paid2016)) %>% 
  select(country2016,total_ventes_pays2016) %>% 
  distinct() %>% 
  rename(countries=country2016)

OrderAdressAnnepays15 <- OrderAdress15 %>% 
  group_by(country2015) %>% 
  mutate(total_ventes_pays2015=sum(total_paid2015)) %>% 
  select(country2015,total_ventes_pays2015) %>% 
  distinct() %>% 
  rename(countries=country2015)

OrderAdressAnnepays14 <- OrderAdress14 %>% 
  group_by(country2014) %>% 
  mutate(total_ventes_pays2014=sum(total_paid2014)) %>% 
  select(country2014,total_ventes_pays2014) %>% 
  distinct() %>% 
  rename(countries=country2014)

OrderAdressAnnepays13 <- OrderAdress13 %>% 
  group_by(country2013) %>% 
  mutate(total_ventes_pays2013=sum(total_paid2013)) %>% 
  select(country2013,total_ventes_pays2013) %>% 
  distinct() %>% 
  rename(countries=country2013)

OrderAdressAnnepays12 <- OrderAdress12 %>% 
  group_by(country2012) %>% 
  mutate(total_ventes_pays2012=sum(total_paid2012)) %>% 
  select(country2012,total_ventes_pays2012) %>% 
  distinct() %>% 
  rename(countries=country2012)

OrderAdressAnnepays11 <- OrderAdress11 %>% 
  group_by(country2011) %>% 
  mutate(total_ventes_pays2011=sum(total_paid2011)) %>% 
  select(country2011,total_ventes_pays2011) %>% 
  distinct() %>% 
  rename(countries=country2011)


pays_ventes <- merge(OrderAdressAnnepays11,OrderAdressAnnepays12,by="countries",all = TRUE)

pays_ventes <- merge(pays_ventes,OrderAdressAnnepays13,by="countries",all = TRUE)

pays_ventes <- merge(pays_ventes,OrderAdressAnnepays14,by="countries",all = TRUE)

pays_ventes <- merge(pays_ventes,OrderAdressAnnepays15,by="countries",all = TRUE)

pays_ventes <- merge(pays_ventes,OrderAdressAnnepays16,by="countries",all = TRUE)

pays_ventes <- merge(pays_ventes,OrderAdressAnnepays17,by="countries",all = TRUE)

pays_ventes <- merge(pays_ventes,OrderAdressAnnepays18,by="countries",all = TRUE)

pays_ventes <- merge(pays_ventes,OrderAdressAnnepays19,by="countries",all = TRUE)

pays_ventes <- merge(pays_ventes,OrderAdressAnnepays20,by="countries",all = TRUE)

pays_ventes <- merge(pays_ventes,OrderAdressAnnepays21,by="countries",all = TRUE) 

pays_ventes$countries <- ifelse(pays_ventes$countries=="RÃ©publique TchÃ¨que","République Tchèque",as.character(pays_ventes$countries))
pays_ventes$countries <- ifelse(pays_ventes$countries=="NorvÃ¨ge","Norvège",as.character(pays_ventes$countries))
pays_ventes$countries <- ifelse(pays_ventes$countries=="SlovÃ©nie","Slovénie",as.character(pays_ventes$countries))
pays_ventes$countries <- ifelse(pays_ventes$countries=="SuÃ¨de","Suède",as.character(pays_ventes$countries))
  
View(pays_ventes)

#Prévision

France <- pays_ventes %>%
  filter(countries=="France")%>%

glimpse(France)  



