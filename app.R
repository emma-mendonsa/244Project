library(shinydashboard)

#"Push"/Send Updates to GIT Procedure: 
# 1) Add code
# 2) Save code
# 3) Press commit
# 4) Add comment in pop-up
# 5) Click blue "Stage chunk" button
# 6) Press commit button
# 7) Press green arrow "Push"
# 8) See pop-up confirming code sent to Git
# 9) Go online and see the updated code

#"Pulled"/Downloaded code from GIT Procedure:
# In RStudio,
# 1) Under Git tab, press blue down arrow
# 2) Pop up will show file downloading
# 3) File on computer will update automatically
# 4) Begin adding to the code :)

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
