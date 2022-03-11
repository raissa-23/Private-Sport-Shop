#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(ggplot2)
library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$graphAnn<- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(2011)

        # draw the histogram with the specified number of bins
    
        if(input$Années =="2011"){
            ggplot(data=pays_sans_sites, aes(x=countries, y=total_ventes_pays2011)) + labs(x="pays",y="total_ventes")+
            geom_bar(stat="identity", fill="#7FFFD4")+
            geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
            theme_minimal()
        }
        else if(input$Années =="2012"){
          ggplot(data=pays_sans_sites, aes(x=countries, y=total_ventes_pays2012,fill=countries)) + labs(x="pays",y="total_ventes")+
            geom_bar(stat="identity")+
            scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00"))+
            geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
            theme_minimal()
        }
        else if(input$Années =="2013"){
          ggplot(data=pays_sans_sites, aes(x=countries, y=total_ventes_pays2013,fill=countries)) + labs(x="pays",y="total_ventes")+
            geom_bar(stat="identity")+
            scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00"))+
            geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
            theme_minimal()
        }else if(input$Années =="2014"){
          ggplot(data=pays_sans_sites, aes(x=countries, y=total_ventes_pays2014,fill=countries)) + labs(x="pays",y="total_ventes")+
            geom_bar(stat="identity")+
            scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00","Slovénie"="steelblue"))+
            geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
            theme_minimal()
        }else if(input$Années =="2015"){
          ggplot(data=pays_sans_sites, aes(x=countries, y=total_ventes_pays2015,fill=countries)) + labs(x="pays",y="total_ventes")+
            geom_bar(stat="identity")+
            scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00"))+
            geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
            theme_minimal()
        }else if(input$Années =="2016"){
          ggplot(data=pays_sans_sites, aes(x=countries, y=total_ventes_pays2016,fill=countries)) + labs(x="pays",y="total_ventes")+
            geom_bar(stat="identity")+
            scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00","Slovénie"="steelblue"))+
            geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
            theme_minimal()
        }else if(input$Années =="2017"){
          ggplot(data=pays_sans_sites, aes(x=countries, y=total_ventes_pays2017,fill=countries)) + labs(x="pays",y="total_ventes")+
            geom_bar(stat="identity")+
            scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00","Slovénie"="steelblue"))+
            geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
            theme_minimal()
        }else if(input$Années =="2018"){
          ggplot(data=pays_sans_sites, aes(x=countries, y=total_ventes_pays2018,fill=countries)) + labs(x="pays",y="total_ventes")+
            geom_bar(stat="identity")+
            scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00","Slovénie"="steelblue","Portugal"="#000000"))+
            geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
            theme_minimal()
        }else if(input$Années =="2019"){
          ggplot(data=pays_sans_sites, aes(x=countries, y=total_ventes_pays2019,fill=countries)) + labs(x="pays",y="total_ventes")+
            geom_bar(stat="identity")+
            scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00","Slovénie"="steelblue","Portugal"="#000000"))+
            geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
            theme_minimal()
        }else if(input$Années =="2020"){
          ggplot(data=pays_sans_sites, aes(x=countries, y=total_ventes_pays2020,fill=countries)) + labs(x="pays",y="total_ventes")+
            geom_bar(stat="identity")+
            scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00","Slovénie"="steelblue","Portugal"="#000000"))+
            geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
            theme_minimal()
        }else if(input$Années =="2021"){
          ggplot(data=pays_sans_sites, aes(x=countries, y=total_ventes_pays2021,fill=countries)) + labs(x="pays",y="total_ventes")+
            geom_bar(stat="identity")+
            scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00","Slovénie"="steelblue","Portugal"="#000000"))+
            geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
            theme_minimal()
        }
    })

    output$graphCL<- renderPlot({   
      
      if(input$Années =="2011"){
        ggplot(data=tableCl, aes(x=countries, y=sommeClients2011)) + labs(x="pays",y="total_clients")+
          geom_bar(stat="identity", fill="#7FFFD4")+
          geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
          theme_minimal()
      }
      else if(input$Années =="2012"){
        ggplot(data=tableCl, aes(x=countries, y=sommeClients2012,fill=countries)) + labs(x="pays",y="total_clients")+
          geom_bar(stat="identity")+
          scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00"))+
          geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
          theme_minimal()
      }
      else if(input$Années =="2013"){
        ggplot(data=tableCl, aes(x=countries, y=sommeClients2013,fill=countries)) + labs(x="pays",y="total_clients")+
          geom_bar(stat="identity")+
          scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00"))+
          geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
          theme_minimal()
      }else if(input$Années =="2014"){
        ggplot(data=tableCl, aes(x=countries, y=sommeClients2014,fill=countries)) + labs(x="pays",y="total_clients")+
          geom_bar(stat="identity")+
          scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00"))+
          geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
          theme_minimal()
      }else if(input$Années =="2015"){
        ggplot(data=tableCl, aes(x=countries, y=sommeClients2015,fill=countries)) + labs(x="pays",y="total_clients")+
          geom_bar(stat="identity")+
          scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00"))+
          geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
          theme_minimal()
      }else if(input$Années =="2016"){
        ggplot(data=tableCl, aes(x=countries, y=sommeClients2016,fill=countries)) + labs(x="pays",y="total_clients")+
          geom_bar(stat="identity")+
          scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00","Slovénie"="steelblue","Portugal"="#000000"))+
          geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
          theme_minimal()
      }else if(input$Années =="2017"){
        ggplot(data=tableCl, aes(x=countries, y=sommeClients2017,fill=countries)) + labs(x="pays",y="total_clients")+
          geom_bar(stat="identity")+
          scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00","Slovénie"="steelblue","Portugal"="#000000"))+
          geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
          theme_minimal()
      }else if(input$Années =="2018"){
        ggplot(data=tableCl, aes(x=countries, y=sommeClients2018,fill=countries)) + labs(x="pays",y="total_clients")+
          geom_bar(stat="identity")+
          scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00","Slovénie"="steelblue","Portugal"="#000000"))+
          geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
          theme_minimal()
      }else if(input$Années =="2019"){
        ggplot(data=tableCl, aes(x=countries, y=sommeClients2019,fill=countries)) + labs(x="pays",y="total_clients")+
          geom_bar(stat="identity")+
          scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00","Slovénie"="steelblue","Portugal"="#000000"))+
          geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
          theme_minimal()
      }else if(input$Années =="2020"){
        ggplot(data=tableCl, aes(x=countries, y=sommeClients2020,fill=countries)) + labs(x="pays",y="total_clients")+
          geom_bar(stat="identity")+
          scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00","Slovénie"="steelblue","Portugal"="#000000"))+
          geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
          theme_minimal()
      }else if(input$Années =="2021"){
        ggplot(data=tableCl, aes(x=countries, y=sommeClients2021,fill=countries)) + labs(x="pays",y="total_clients")+
          geom_bar(stat="identity")+
          scale_fill_manual(values = c("Suisse" = "#7FFFD4","Luxembourg"="#FF8C00","Slovénie"="steelblue","Portugal"="#000000"))+
          geom_text(aes(label=countries), vjust=-0.3, size=3.5)+
          theme_minimal()
      }
     
})
    
})
