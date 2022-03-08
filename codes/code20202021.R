
newadress2021 <- addresses_2021 %>%
  mutate(country_mod=(ifelse(str_sub((addresses_2021$country),1,8)%in%c("France -","France","SECTEUR ","Monaco") ,"France",addresses_2021$country)))

#milet!!!
#%>%mutate(V3=(case_when(addresses_2021$country=="Andorre") ~"Espagne")) et MONACO !!!

mergeOrderAdress21 <- left_join(orders_2021,newadress2021,by="id_address") 

newadress2020 <- addresses_2020 %>%
  mutate(country_mod=(ifelse(str_sub((addresses_2020$country),1,8)%in%c("France -","France","SECTEUR ","Monaco") ,"France",addresses_2020$country)))

#milet!!!
#%>%mutate(V3=(case_when(addresses_2021$country=="Andorre") ~"Espagne")) et MONACO !!!

mergeOrderAdress21 <- left_join(orders_2021,newadress2021,by="id_address")

OrderAdress21 <- mergeOrderAdress21%>% 
  rename(id_address2021=id_address) %>% 
  rename(version2021=version) %>%
  rename(total_paid2021=total_paid) %>%
  rename(order_date2021=order_date) %>%
  rename(country2021=country_mod) %>%
  select(-id_order,-country)

mergeOrderAdress20 <- left_join(orders_2020,newadress2020,by="id_address")

OrderAdress20 <- mergeOrderAdress20%>% 
  rename(id_address2020=id_address) %>% 
  rename(version2020=version) %>%
  rename(total_paid2020=total_paid) %>%
  rename(order_date2020=order_date) %>%
  rename(country2020=country_mod) %>%
  select(-id_order,-country)

OrderAdress2020_2021 <- merge(OrderAdress20,OrderAdress21,by="id_customer")


OrderAdress2020_2021CL <- OrderAdress2020_2021 %>% 
  group_by(id_customer) %>% 
  mutate(total_ventes_CL2020=sum(total_paid2020)) %>% 
  mutate(total_ventes_CL2021=sum(total_paid2021))

OrderAdress2020_2021pays <- OrderAdress2020_2021 %>% 
  group_by(country2020) %>% 
  mutate(total_ventes_pays2020=sum(total_paid2020)) %>% 
  mutate(total_ventes_pays2021=sum(total_paid2021)) %>% 
  select(country2020,total_ventes_pays2020) %>% 
  distinct() 

df1 <- data.frame("CA2020",sum(OrderAdress2020_2021pays$total_ventes_pays2020))
names(df1)=c("CA2020","somme") 
OrderAdress2020_2021paysBIND <- OrderAdress2020_2021pays %>%
  rbind(OrderAdress2020_2021pays,df1)
#length(unique(merge$id_customer))
