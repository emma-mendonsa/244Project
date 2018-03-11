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
  dashboardHeader(title = "App of Happiness"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Global Perspective", tabName = "widget1", icon = icon("globe")),
      menuItem("Comparisons", tabName = "widget3", icon = icon("bar-chart")),
      menuItem("Variable Descriptions", tabName = "variable", icon=icon("info-circle"))
    )),
  
  dashboardBody(
    tabItems(
      
      
      #First tab content
      tabItem(tabName = "dashboard",
              fluidPage(

                fluidRow(
                  column(9,
                  h1("The App of Happiness"))),
                

                fluidRow(
                  column(9,
                  h4(p(strong("This app presents the each country's happiness in terms of GPD, Health, Family, Trust, and more.")), 
                   h4(p("Through globalization each country's happiness level has the ability to impact the health of international relations."),
                      p("The UN Sustainable Development Solutions Network has led a survey in 156 countries over the past few years where
                         and participants are asked to rank their happiness between 0 and 10. This rating was then compared to participant responses on how health, family, etc impact their happiness rating. Please see the 'Variable Descriptions' tab for further information on each parameter and the survey questions."))))),
                  

                fluidRow(
                  column(9,
              
                    h4(
                      p("Data Source: All data utilized in this app was found at: https://www.kaggle.com/unsdsn/world-happiness/data"))),
                  
                    column(12,
                  img(src = "flower.png", 
                      #size = "page",
                      align = "left",
                      height = 500, width = 1200
                      )))
                )),
               
                 #mainPanel(
                  #textOutput("Information"))

     
      
      
      #Third tab content
      tabItem(tabName = "widget1",
              fluidPage(
                
                fluidRow(h1("World Map:"),(h2("Display each variable to get the global perspective."))),
                
                
                fluidRow(
                  column(2,
                         
                         
                         selectInput("variable2", "Variable to display:",
                                     c("Rank" = "Rank",
                                       "Score" = "Score",
                                       "GDP" = "GDP",
                                       "Family" = "Family",
                                       "Health" = "Health",
                                       "Freedom" = "Freedom",
                                       "Trust" = "Trust",
                                       "Dystopia Residual" = "DysRes" 
                                     )),

                         selectInput("colors", "Color palette options: ",
                                     c("Diverging"="diverging",
                                       "Heat" = "heat",
                                       "Rainbow"="rainbow",
                                       "Terrain"="terrain",
                                       "Topo"="topo",
                                       #"spectral"="spectral",
                                       "Awesome"="palette")
                                     
                                     ), 

                         checkboxInput("addLegend", "addLegend", TRUE),
                         h6("Countries with no data represented in grey")
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
                solidHeader = T)),
      

#Fourth tab

          tabItem(tabName = "variable",
              fluidPage(
                
                fluidRow(
                  column(10,
                         h1("Description of Variables in the World Happiness Project (WHR)"),
                         h4("Authors of WHR evaluate information gathered from annual Gallup World Poll (GWP) surveys. The following variables assessed for their relative 
                            contributions to survery participants' overall happiness score and national trends.")),
                  column(10,
                         h4("Rank (range: 1-155,156,158 for 2015, 2016, and 2017, respectively)"),
                         h5("Ranks are based on countries' average score."),
                         h4("Score (range: 1-10)"),
                         h5("Survey participants answered the following question. Each country's score represents the national average response."),
                         h5("“Please imagine a ladder, with steps numbered from 0 at the bottom to 10 at the top. 
The top of the ladder represents the best possible life for you and the bottom of the ladder represents the worst possible life for you. 
On which step of the ladder would you say you personally feel you stand at this time?”"),
                         h4("GDP (WHR range: 0-2)"),
                         h5("GDP information was gathered from the World Development Indicators (WDI) 2016 report. For countries with missing data, forecasts were made 
                            from previous years, with inflation accounted for. The same method was used in 2017 for all countries, since the 2017 summary GDP information wasn't available when the WHR data was released."),
                         h4("Health (WHR range: 0-2)"),
                         h5("Health pertains specifically to healthy life expectancy. 
                            Time series of healthy life expectancy from birth were calculated from
                            data produced by the World Health Organization (WHO) and WDI."),
                         h4("Social Support (WHR range: 0-2)"),
                         h5("GWP survey participants were asked to answer the following 
                            binary question. WHR values are based on the national average."),
                         h5("“If you were in trouble, do you have relatives or friends you can count on to help you
                            whenever you need them, or not?”"),
                         h4("Freedom (WHR range: 0-2)"),
                         h5("GWP survey participants answered the following question. WHR values are based on the national average."),
                         h5("“Are you satisfied or dissatisfied with your freedom to choose what
you do with your life?”"),
                         h4("Generosity (WHR range: 0-2)"),
                         h5("GWP survey participants answered the following binary question. WHR values are based on the residual regression of national averages."),
                         h5("“Have you donated money to a charity in the past month?”"),
                         h4("Trust (WHR range: 0-2)"),
                         h5("GWP survey participants answered the following question. WHR values are based on the proportion of survey participants who reported that most people can be trusted."),
                         h5("“Generally speaking, would you say that most people can be trusted or that you have to be careful in dealing with people?”"),
                       h1(""),
                         
                         h6("Data Source: Helliwell, John et al. (2017) 'Statistical Appendix for 'The social foundations of world happiness' World Happiness Report.    'http://worldhappiness.report/wp-content/uploads/sites/2/2017/03/StatisticalAppendixWHR2017.pdf"))

                  )
                        )
                ) 

    
        ))
    )
   





#source("happyData.R")


server <- function(input, output){
 
  
  
#vertical <- joinCountryData2Map(vertical
 #                                , joinCode = "ISO3"
  #                                , nameJoinColumn="ISO")


output$mapplot15 <- renderPlot({
  
  mapplot15 <- mapCountryData(vert15,
                              nameColumnToPlot = input$variable2,
                              numCats = 155,
                              catMethod = 'FixedWidth',
                              colourPalette = input$colors,
                              missingCountryCol = "grey60",
                              oceanCol = "slategray1",
                              addLegend = input$addLegend)

})
  

output$mapplot16 <- renderPlot({
  
  mapplot16 <- mapCountryData(vert16,
                              nameColumnToPlot = input$variable2,
                              numCats=155,
                              catMethod = 'FixedWidth',
                              colourPalette = input$colors,
                              missingCountryCol = "grey60",
                              oceanCol = "slategray1",
                              addLegend = input$addLegend)
})

output$mapplot17 <- renderPlot({
  
  mapplot17 <- mapCountryData(vert17,
                              nameColumnToPlot = input$variable2,
                              numCats=155,
                              catMethod = 'FixedWidth',
                              colourPalette = input$colors,
                              missingCountryCol = "grey60",
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