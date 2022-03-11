#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel(h1("Les totaux des ventes et des nouveaux clients par année des pays n'ayant pas encore de version de site  Private Sport Shop :",
                  style = "font-weight: 500; color: #696969;"
                  
                  )),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            tags$head(
                tags$style(type="text/css", "select { max-width: 240px; }"),
                tags$style(type="text/css", ".span4 { max-width: 290px; }"),
                tags$style(type="text/css", ".well { max-width: 280px; }")
            ),
            selectInput(
                        inputId = "Années",
                        label = "Choisissez une année",
                        choices = c("2011","2012","2013","2014","2015"
                                    ,"2016","2017","2018","2019","2020"
                                    ,"2021"
                                    )
                        #tags$head(tags$style(HTML('.navbar-brand {width: 300px; font-size:35px; text-align:center;}')))                   
                        
                        
        )),
        

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "graphAnn"),
            plotOutput(outputId = "graphCL")
        )
    )
))
