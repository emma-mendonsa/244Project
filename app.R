library(shinydashboard)
library(wesanderson) 
library(tidyverse)

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
            h2("Claire's beautiful map")),
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
          box(radioButtons("Year", "Year", 
                           c("2015" = "2015", "2016" = "2016", "2017" = "2017"))),
          box(sliderInput("Rank", "Happiness Ranking - Click for 2 sliders:", 
                          1, 160, c(20,50))),
          box(tableOutput("mini"))
        )
        )
        )))


server <- function(input, output){
  set.seed(122)
  histdata <- rnorm(500)
  
  output
  
  
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
             Rank <= input$Rank[2]) %>% 
      filter(Year == input$Year) %>% 
      arrange(Rank, Country)
      
    #filtered_order<- filtered[order(Rank,Country),]
    filtered[c("Year", "Country", "Rank",input$variable)]
  })
}

shinyApp(ui,server)

#?Working!