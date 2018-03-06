library(shinydashboard)

#LALAL
#Okay one more time:

ui <- dashboardPage(
  dashboardHeader(title = "An App of Happiness"),
  
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
}

shinyApp(ui,server)