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
  
  #mainPanel(
   # img(src = "rstudio.png", height = 140, width = 400)
  #)
  
  navbarPage("App Title",
             tabPanel("Plot"),
             navbarMenu("More",
                        tabPanel("Summary"),
                        "----",
                        "Section header",
                        tabPanel("Table")
             )
  )
  
    titlePanel("Statistiques pour les données des clients et des commandes par années et par version de site visité"),
    # Application title
    titlePanel("Les  clients"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          h2("Les  clients"),
          selectInput(
            inputId = "Choix_pays",
            label="Pays",
            choices = unique(table_res$version),
            multiple = T,
            selected = "fr"
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("cust_graph")
        )
        
    ),
    titlePanel("Les commandes"),
    sidebarLayout(
      sidebarPanel(
        h2("Les commandes"),
        selectInput(
          inputId = "par_pays",
          label="Pays",
          choices = unique(table_res_ordr$version),
          multiple = T,
          selected = "fr"
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h1("First level title"),
        h2("Second level title"),
        plotOutput("ordr_graph")
      )
      
    )
))



