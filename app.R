library(shinydashboard)
library(shiny)
library(tidyverse)
library(wesanderson) 
library(rworldmap)
library(rgdal)
library(WDI)

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
              fluidRow(h1("Map")),
              fluidRow(column(4,
                              
                              h3("rworldmapUI"),
                              
                              radioButtons("Year", "Year :",
                                          c("2015" = "2015",
                                            "2016" = "2016",
                                            "2017" = "2017"
                                          )),
                              
                              
                              selectInput("Variable", "Category",
                                          c("Rank" = "Rank",
                                            "Score" = "Score",
                                            "GDP" = "GDP",
                                            "Family" = "Family",
                                            "Health" = "Health",
                                            "Freedom" = "Freedom",
                                            "Trust" = "Trust",
                                            "Dystopia Residual" = "DysRes"
                                          )),
                              
                              sliderInput("numCats", "# of Countries", 
                                          min =  1,
                                          max = 155, 
                                          value = 10),
                              
                              selectInput("colourPalette", "Wes Anderson Color Palette :",
                                          c("Zissou" = "zissou",
                                            "YlGnBu" = "YlGnBu",
                                            "Purples" = "Purples",
                                            "PuBuGn" = "PuBuGn",
                                            "Greens" = "Greens"
                                          )),
                              checkboxInput("addLegend", "addLegend", TRUE) 
              
                              ),
                       
                       mainPanel("Map View", plotOutput("mapplot"))
                       )
                       
                       )
            
                       
                       
            ),
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
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
    })
  
sPDF <- joinCountryData2Map(world_happiness
                                     , joinCode = "ISO3"
                                     , nameJoinColumn="ISO")
output$mapplot <- renderPlot({
  mapplot <- mapCountryData(sPDF, 
                              nameColumnToPlot = "Rank15",
                              catMethod = 'categorical',
                              colourPalette = zissou,
                              missingCountryCol = "grey60",
                              addLegend = FALSE
                            )
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

#?Working!