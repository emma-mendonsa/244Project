library(shinydashboard)
library(wesanderson) 

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "The App of Happiness"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Global Perspective", tabName = "widget1", icon = icon("globe")),
      menuItem("Comparisons", tabName = "widget2", icon = icon("bar-chart"))
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
          )
        ),
        fluidRow(
          checkboxGroupInput(inputId = "Region", "Select Region(s) of Interest:", choiceNames = "Regions", 
                             inline = TRUE),
          sliderInput(inputId = "Year", "Year", min = 2015, max = 2016),
          radioButtons(inputId = "VariableX", label = "Select X-axis:"),
          radioButtons(inputId = "VariableY", label = "Select Y-axis:"),
          plotOutput(outputId = "bubble")
          
        )

      )
    )
  ))


server <- function(input, output){
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  
  })
  output$bubble <- renderPlot({
    ggplot(happy_1516, aes(x=input$VariableX,y=input$VariableY)+
             geom_point(aes(size = Rank, color = input$Region), alpha = 0.5)+
             theme_classic())
    
  })
}

shinyApp(ui,server)

#?Working!