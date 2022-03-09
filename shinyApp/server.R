#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

cust_all=read.csv2("Data/customers/cust_all.csv")

ordr=read.csv2("Data/orders/orders_all.csv")
ordr_id=subset(ordr, select=c("id_order", "version", "annee"))
ordr_ts= subset(ordr, select=c("total_paid", "order_date", "annee"))
ordr_v= subset(ordr, select=c("total_paid", "version", "annee"))

table_res <- cust_all %>% 
  group_by(annee, version) %>% 
  summarise(nb_client=n())

table_res_ordr <- ordr_id %>% 
  group_by(version, annee) %>% 
  summarise(nb_commandes=n())


#table_res_vente <- ordr_v %>% 
  #group_by(version, annee) %>% 
  #summarise(nb_ventes=n())


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$cust_graph <- renderPlot({

      ggplot(data = table_res %>% filter(version%in% input$Choix_pays),aes(x=annee,y=nb_client, xlab(label="Années"), ylab(label="Nombre de clients"),fill=version))+
        geom_bar(stat = "identity")
      
    })    
      output$ordr_graph<- renderPlot({
        
        ggplot(data = table_res_ordr %>% filter(version%in% input$par_pays),aes(x=annee,y=nb_commandes, xlab(label="Années"), ylab(label="Nombre de commandes"),fill=version))+
          geom_bar(stat = "identity")
      
    })

})
