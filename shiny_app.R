#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
source("scripts/3_shiny_data.R")

server <- function(input, output) {
  # Reactive expression to return the selected dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "Overall metrics for all classes" = class_all,
           "Overall metrics for neutral class" = class_all_neutral,
           "Overall metrics for positive class" = class_all_positive,
           "Overall metrics for negative class" = class_all_negative,
           "Overall accuracy for all classes" = overall_all)
  })

  # Show the selected dataset
  output$view <- renderTable({
    datasetInput()
  })
    
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Help text per dataset
  # output$helpText <- renderText({
  #   switch(input$dataset,
  #          "Overall metrics for all classes" = "Text for this all",
  #          "Overall metrics for neutral class" = "Text for this neutral",
  #          "Overall metrics for positive class" = "Text for this positive",
  #          "Overall metrics for negative class" = "Text for this negative",
  #          "Overall accuracy for all classes" = "Text for this accuracy")
  # })
}

## User Interface side of the app
ui <- fluidPage(
  titlePanel("Confusion Matrix App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Input: Selector for choosing dataset
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("Overall metrics for all classes",
                              "Overall metrics for neutral class",
                              "Overall metrics for positive class",
                              "Overall metrics for negative class",
                              "Overall accuracy for all classes")),
      # Get help text per dataset
      # textOutput("helpText")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        
        tabPanel("View", tableOutput("view")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)

# Run the Shiny App
shinyApp(ui = ui, server = server)