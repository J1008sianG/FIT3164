#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyr)

library(datasets)
data(iris)

ui <- fluidPage(
  
  titlePanel("Classification Model"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 2,
      style = paste0("height: 90vh; overflow-y: auto;"),
      
      fileInput("file1", "Upload XML File",
                multiple = FALSE,
                accept = c(".xml")),
      
      actionButton(inputId='ab1', label="Learn More", 
                   icon = icon("th"), 
                   onclick ="window.open('http://google.com', '_blank')"),
      
      actionButton(inputId='ab2', label="Predict")
    ),
    mainPanel(
      
      textOutput("text"),
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("plot1"), plotlyOutput("plot2"))
      ),
      HTML('<hr style="color: purple;">'),
      
      tabsetPanel(
        tabPanel("Suggested Articles", 
                 DT::dataTableOutput("outputtable")
        )
      )
      
    )
  )
)

server <- function(input, output) {
  
  output$text <- renderText("Characteristics of the labelling")
  
  output$plot1 <- renderPlotly({
    plot_ly(iris, x = ~~Sepal.Width, y = ~Sepal.Length, type = 'scatter', mode = 'markers') %>% layout(title = "Keyword Appearences")
  })
  
  output$plot2 <- renderPlotly({
    USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
    data <- USPersonalExpenditure[,c('Categorie', 'X1960')]
    
    fig <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie')
    fig <- fig %>% layout(title = 'Density of the related keywords',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  
  
  output$outputtable <- DT::renderDataTable(
    data.frame(iris)
  )
  
}


shinyApp(ui, server)
