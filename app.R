library(shinydashboard)
library(shiny)
library(tidyverse)
library(wesanderson) 
library(rworldmap)
library(sf)
library(sp)
library(rgdal)
library(maptools)
library(mapdata)
library(rworldmap)
library(RColorBrewer)
<<<<<<< HEAD
library(WDI)

=======
>>>>>>> d90ac302dcbb27cae26e68954e18778a3c285681

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "The App of Happiness"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Global Perspective", tabName = "widget1", icon = icon("globe")),
      menuItem("Comparisons", tabName = "widget2", icon = icon("bar-chart")),
      menuItem("Table", tabName = "widget3", icon = icon("table"))
    )),
  
  dashboardBody(
    tabItems(
      
      #First tab content
      tabItem(tabName = "dashboard",
            h2("Introduce our App")),
      #Second tab content
      tabItem(tabName = "widget1",
              fluidPage(
                
                fluidRow(h1("")),
                
                fluidRow(
                  column(2,
                         
                         h3("World Map"),
      
                         selectInput("variable2", "Category",
                                     c("Rank" = "Rank",
                                       "Score" = "Score",
                                       "GDP" = "GDP",
                                       "Family" = "Family",
                                       "Health" = "Health",
                                       "Freedom" = "Freedom",
                                       "Trust" = "Trust",
                                       "Dystopia Residual" = "DysRes"
                                       )),
            

                         
                         selectInput("colourPalette", "colourPalette",
                                     c("zissou" = "zissou",
                                       "rushmore" = "rushmore"
                                     )), 

                         checkboxInput("addLegend", "addLegend", TRUE) #var,name
                  ),
                  
                  column(10,
                         
                        tabsetPanel(
                          tabPanel("2015", plotOutput("mapplot15", width = "800", height = "500")),
                          tabPanel("2016", plotOutput("mapplot16", width = "800", height = "500")),
                          tabPanel("2017", plotOutput("mapplot17", width = "800", height = "500"))
                                  
                          )
                  )
                )
                
              ))
      , 
      #Third tab
      tabItem(tabName = "widget2",
        fluidRow(
          box(plotOutput("plot1", height = 250)),
      
          box(
            title = "Country Comparisons",
            sliderInput("slider", "Country Happiness Range:", 1, 100, 50)
          ))),
      
      
      #Fourth tab
      tabItem(tabName = "widget3",
        fluidRow(
          box(selectInput("variable", "Select Characteristic:",
                      c("GDP"="GDP", "Health" = "Health", "Trust" = "Trust", "Generosity" = "Generosity"))),
          box(sliderInput("Rank", "Happiness Ranking - \nClick & Drag for 2 sliders:", min = 1, max = 160, happy_all$Rank)),
          box(tableOutput("mini"))
        )
        )
        )))


server <- function(input, output){
 

 
vertical <- joinCountryData2Map(vertical
                                  , joinCode = "ISO3"
                                  , nameJoinColumn="ISO")

#colourPalette <- brewer.pal(10,'Spectral')
zissou <- wes_palette("Zissou", type = "continuous")
moonrise <- wes_palette("Moonrise1", type = "continuous")

output$mapplot15 <- renderPlot({
  
  mapplot15 <- mapCountryData(vert15,
                              nameColumnToPlot = input$variable2,
                              catMethod = 'categorical',
                              colourPalette = rushmore,
                              missingCountryCol = "grey60",
                              addLegend = input$addLegend)

})
  

output$mapplot16 <- renderPlot({
  
  mapplot16 <- mapCountryData(vert16,
                              nameColumnToPlot = input$variable2,
                              catMethod = 'categorical',
                              colourPalette = input$colourPalette,
                              missingCountryCol = "grey60",
                              addLegend = input$addLegend)
})

output$mapplot17 <- renderPlot({
  
  mapplot17 <- mapCountryData(vert17,
                              nameColumnToPlot = input$variable2,
                              catMethod = 'categorical',
                              colourPalette = input$colourPalette,
                              missingCountryCol = "grey90",
                              oceanCol = "slategray1",
                              addLegend = input$addLegend)
})

  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
    })



#output$bubble <- renderPlot({
  #  ggplot(happy_1516, aes(x=input$VariableX,y=input$VariableY)+
  #           geom_point(aes(size = Rank, color = input$Region), alpha = 0.5)+
  #           theme_classic())
    
  #})
  output$mini <- renderTable({
    filtered <-
      happy_all %>% 
      filter(Rank >= input$Rank[1], 
             Rank <= input$Rank[2])
    filtered[c("Year", "Country", "Rank",input$variable)]
  })
}

shinyApp(ui,server)