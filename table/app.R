library(shiny)
library(googleVis)
library(shinydashboard)
library(tidyverse)


######### WRANGLING #############

df2015 <- read_csv("C:/Users/ensie/Box Sync/UCSB/Winter 2018/ESM 244/Shiny/2015.csv")
df2016 <- read_csv("C:/Users/ensie/Box Sync/UCSB/Winter 2018/ESM 244/Shiny/2016.csv")
df2017 <- read_csv("C:/Users/ensie/Box Sync/UCSB/Winter 2018/ESM 244/Shiny/2017.csv")

happy15 <- df2015 %>% 
  select("Country", "Rank", "GDP", "Health", "Trust", "Generosity")

happy16 <- df2016 %>% 
  select("Country", "Rank", "GDP", "Health", "Trust", "Generosity")

happy17 <- df2017 %>% 
  select("Country", "Rank", "GDP", "Health", "Trust", "Generosity")

Year15 <- rep(2015, length(happy15$Country))
Year16 <- rep(2016, length(happy16$Country))
Year17 <- rep(2017, length(happy17$Country))

happy15$Year <- Year15
happy16$Year <- Year16
happy17$Year <- Year17

happy_all <- rbind(happy15, happy16, happy17)
happy_all$Year <- as.character(happy_all$Year)

happy_1516 <- rbind(happy15, happy16)

mini <- head(happy_all)
options <- mini$Country

#################

)
#fluidRow(
#  checkboxGroupInput(inputId = "Region", "Select Region(s) of Interest:", choiceNames = "Regions", 
#                     inline = TRUE),
#  sliderInput(inputId = "Year", "Year", min = 2015, max = 2016),
#  radioButtons(inputId = "VariableX", label = "Select X-axis:"),
#  radioButtons(inputId = "VariableY", label = "Select Y-axis:"),
#  plotOutput(outputId = "bubble")

)

),

ui <- fluidPage(
   
   # Application title
   titlePanel("Happiness Table"),
   
   sidebarLayout(
      sidebarPanel(
         selectInput("Country",
                     "Select a Country:", mini$Country, multiple = FALSE)
      ),
      
      # Show a table of 2015 countries
      mainPanel(
         tableOutput("data")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$data <- renderTable({
      # generate table based on input$Country from ui.R
     
     mini[input$Country, drop = FALSE]
   })
}


# Run the application 
shinyApp(ui = ui, server = server)

