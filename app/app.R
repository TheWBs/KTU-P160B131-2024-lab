library(shiny)
library(ggplot2)
library(tidyverse)
library(jsonlite)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Veiklos kodas 702200"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput("Įmonė",
                     "Pasirinkite įmonę",choices = NULL)),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  data = readRDS('../data/702200.rds')
  updateSelectizeInput(session, "Įmonė", choices = data$name, server = T)
  output$plot = renderPlot(
    data%>%
      filter(name == input$Įmonė)%>%
      ggplot(aes(x=ym(month),y=avgWage))+geom_point()+
      geom_line()+
      theme_classic()+
      labs(x="Mėnesis", y="Vidutinis atlyginimas")
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
