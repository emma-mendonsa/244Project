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

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "The App of Happiness"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Parameter Descriptions", tabName = "description", icon = icon("book")),
      menuItem("Global Perspective", tabName = "widget1", icon = icon("globe")),
      menuItem("Comparisons", tabName = "widget3", icon = icon("bar-chart"))
    )),
  
  dashboardBody(
    tabItems(
      
      
      #First tab content
      tabItem(tabName = "dashboard",
              fluidPage(

                fluidRow(
                  column(9,
                  h1("The App of Happiness"))),
                
                #fluidRow( 
                #  column(3,

                #       selectInput("Information","",
                #                 c("Background" = "background",
                #                   "Data Source"="datasource",
                #                   "Descriptions"="descriptions")
                #                    ))),                
                
                #box(textOutput("Information")),
                
                fluidRow(
                  column(9,
                  h4(p(strong("This app presents the each country's happiness in terms of GPD, Health, Family, Trust, and more.")), 
                    h4(p("Through globalization each country's happiness level has the ability to impact the health of international relations."),
                       p("The UN Sustainable Development Solutions Network has led a survey in 156 countries over the past few years where
                         and participants are asked to rank their happiness between 0 and 10. These were combined with quantitative country-specic 
                         data to better understand country characteristics that lead to happiness."))))),
                  

                fluidRow(
                  column(9,
              
                    h4(
                      p("Data Source: All data utilized in this app was found at: https://www.kaggle.com/unsdsn/world-happiness/data"),
                      p("See 'Parameter Descriptions' for further information on each parameter."))),
                  
                    column(12,
                  img(src = "flower.png", 
                      #size = "page",
                      align = "left",
                      height = 500, width = 1200
                      )))
                )),
               
                 #mainPanel(
                  #textOutput("Information"))

      #Second tab content
      tabItem(tabName = "description",
              fluidPage(
                column(12,
                       img(src = "table_descrip.jpg", 
                           size = "auto",
                           align = "center"
                           #height = 500, width = 1200
                       ))
              )
              ),      
      
      
      #Third tab content
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
                
              )),
      
      
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

            box(sliderInput("Rank", "",1,160,c(1,145)),
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
      






source("happyData.R", local = TRUE)

server <- function(input, output){
 
  
  
#vertical <- joinCountryData2Map(vertical
#                                  , joinCode = "ISO3"
#                                  , nameJoinColumn="ISO")


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
            marker = list(size = 15, opacity = .5)) %>% 
      layout(title = paste(input$variableA, "vs. ", input$variableB),
             xaxis = list(range = c(0,2), title = input$variableA, showgrid = FALSE),
             yaxis = list(range = c(0,2), title = input$variableB, showgrid = FALSE),
             font = list(size = 12))

  })
}

shinyApp(ui,server)