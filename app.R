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
library(plotly)

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
              fluidPage(
                
                fluidRow(h1("Introduce our App")),
                fluidRow(
                  
                  column(3,
                         selectInput("Information","Information",
                                     c("Introduction"="Introduction",
                                       "Data Source"="Data Source",
                                       "Descriptions"="Descriptions")
                                     )
                         )
                ),
               
                 mainPanel(
                  textOutput("Information"))
                
              )),
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

                         #selectInput("colors", "colors",
                                     #c("spectral" = "spectral")
                                     
                                     #), 

                         checkboxInput("addLegend", "addLegend", TRUE) 
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
        fluidPage(

            box(selectInput("variableA", "",
                      c("GDP"="GDP", "Family" = "Family", "Health" = "Health", "Freedom" = "Freedom", "Trust" = "Trust"),
                      selected = "GDP"),
                title = "Select Characteristic:",
                status = "primary",
                solidHeader = T),

            box(selectInput("variableB", "",
                      c("GDP"="GDP", "Family" = "Family", "Health" = "Health", "Freedom" = "Freedom", "Trust" = "Trust"),
                      selected = "Health"),
                title = "Select Characteristic:",
                status = "primary",
                solidHeader = T),

            box(sliderInput("Rank", "Click & Drag for 2 sliders:",1,160,c(20,50)),
                title = "Happiness Ranking:",
                status = "primary",
                solidHeader = T),

            box(radioButtons("Year", "", 
                             c("2015" = "2015", "2016" = "2016", "2017" = "2017")),
                title = "Year:",
                status = "primary",
                solidHeader = T)
              
              ),

            box(tableOutput("mini"),
                align = "center",
                title = "Table Summary",
                status = "success",
                solidHeader = T),

            box(plotlyOutput("bubble"),
                title = "Graphical Summary",
                status = "success",
                solidHeader = T))
        )
        ))
      








server <- function(input, output){
 
  output$Information <- renderText({ 
    "You have selected this"
  })
  
  
vertical <- joinCountryData2Map(vertical
                                  , joinCode = "ISO3"
                                  , nameJoinColumn="ISO")


output$mapplot15 <- renderPlot({
  
  mapplot15 <- mapCountryData(vert15,
                              nameColumnToPlot = input$variable2,
                              numCats = 155,
                              catMethod = 'FixedWidth',
                              colourPalette = spectral,
                              missingCountryCol = "grey60",
                              addLegend = input$addLegend)

})
  

output$mapplot16 <- renderPlot({
  
  mapplot16 <- mapCountryData(vert16,
                              nameColumnToPlot = input$variable2,
                              numCats=155,
                              catMethod = 'FixedWidth',
                              colourPalette = spectral,
                              missingCountryCol = "grey60",
                              addLegend = input$addLegend)
})

output$mapplot17 <- renderPlot({
  
  mapplot17 <- mapCountryData(vert17,
                              nameColumnToPlot = input$variable2,
                              numCats=155,
                              catMethod = 'FixedWidth',
                              colourPalette = spectral,
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
    filtered <- vertical_table%>% 
      filter(Rank >= input$Rank[1], 
             Rank <= input$Rank[2],
             Year == input$Year) %>% 
      arrange(Rank, Country)
    
    filtered[c("Year", "Country", "Score", input$variableA, input$variableB)]
  })
  
  output$bubble <- renderPlotly({
    
    bubble_table <- vertical_table %>% 
      filter(Rank >= input$Rank[1],
             Rank <= input$Rank[2],
             Year == input$Year) %>% 
      arrange(Rank, Country)
      #around(input$variableA, decimals = 1)
    
    colnames(bubble_table) <- c("Year","Country","Rank","Score","x","y")
    bubble_table$x <- round(bubble_table$x, digits = c(2))
    bubble_table$y <- round(bubble_table$y, digits = c(2))
    
    
    plot_ly(bubble_table, x= ~x, y= ~y,
            text = ~Country, color = ~Rank,
            type = 'scatter', mode = 'markers',
            marker = list(size = 15, opacity = 3)) %>% 
      layout(title = paste(input$variableA, "vs. ", input$variableB),
             xaxis = list(title = input$variableA),
             yaxis = list(title = input$variableB))

  })
}

shinyApp(ui,server)