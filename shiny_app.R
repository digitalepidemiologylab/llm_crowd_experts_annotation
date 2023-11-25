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
  
  datasetInput_full <- reactive({
    switch(input$dataset_full,
           "Overall metrics for all classes " = class_all_agree,
           "Overall metrics for neutral class " = class_all_neutral_agree,
           "Overall metrics for positive class " = class_all_positive_agree,
           "Overall metrics for negative class " = class_all_negative_agree,
           "Overall accuracy for all classes " = overall_all_agree)
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
  
  # Show the selected dataset
  output$view_full <- renderTable({
    datasetInput_full()
  })
  
  # Generate a summary of the dataset
  output$summary_full <- renderPrint({
    dataset_full <- datasetInput_full()
    summary(dataset_full)
  })
  
  # Help text per dataset
  output$helpText <- renderText({
    switch(input$dataset,
           "Overall metrics for all classes" = "Performance metrics of GPT and Amazon Mturk annotation in comparison with the EPFL experts classification. \n Tweets with the same stance assigned by at least three out of the four EPFL experts were considered tweets with partial agreement",
           "Overall metrics for neutral class" = "Performance metrics of GPT and Amazon Mturk annotation in comparison with the EPFL experts classification for the neutral tweets. \n Tweets with the same stance assigned by at least three out of the four EPFL experts were considered tweets with partial agreement",
           "Overall metrics for positive class" = "Performance metrics of GPT and Amazon Mturk annotation in comparison with the EPFL experts classification for the positive tweets. \n Tweets with the same stance assigned by at least three out of the four EPFL experts were considered tweets with partial agreement",
           "Overall metrics for negative class" = "Performance metrics of GPT and Amazon Mturk annotation in comparison with the EPFL experts classification for the negative tweets. \n Tweets with the same stance assigned by at least three out of the four EPFL experts were considered tweets with partial agreement",
           "Overall accuracy for all classes" = "Overall accuracy of GPT and Amazon Mturk annotation in comparison with the EPFL experts classification. \n Tweets with the same stance assigned by at least three out of the four EPFL experts were considered tweets with partial agreement")
  })
  
  output$helpText_full <- renderText({
    switch(input$dataset_full,
           "Overall metrics for all classes " = "Performance metrics of GPT and Amazon Mturk annotation in comparison with the EPFL experts classification. \n Tweets with the same stance assigned by all four EPFL experts were considered tweets with full agreement",
           "Overall metrics for neutral class " = "Performance metrics of GPT and Amazon Mturk annotation in comparison with the EPFL experts classification for the neutral tweets. \n Tweets with the same stance assigned by all four EPFL experts were considered tweets with full agreement",
           "Overall metrics for positive class " = "Performance metrics of GPT and Amazon Mturk annotation in comparison with the EPFL experts classification for the positive tweets. \n Tweets with the same stance assigned by all four EPFL experts were considered tweets with full agreement",
           "Overall metrics for negative class " = "Performance metrics of GPT and Amazon Mturk annotation in comparison with the EPFL experts classification for the negative tweets. \n Tweets with the same stance assigned by all four EPFL experts were considered tweets with full agreement",
           "Overall accuracy for all classes " = "Overall accuracy of GPT and Amazon Mturk annotation in comparison with the EPFL experts classification. \n Tweets with the same stance assigned by all four EPFL experts were considered tweets with full agreement")
  })
}

## User Interface side of the app
ui <- navbarPage(
  title = "Confusion Matrix App",
  
  tabPanel("GPT & Mturk performance in tweets with partial agreement",
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
       textOutput("helpText")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        
        tabPanel("View", tableOutput("view")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
),

tabPanel("GPT & Mturk performance in tweets with full agreement",
         sidebarLayout(
           sidebarPanel(
             selectInput("dataset_full", "Choose a dataset:", 
                         choices = c("Overall metrics for all classes ",
                                     "Overall metrics for neutral class ",
                                     "Overall metrics for positive class ",
                                     "Overall metrics for negative class ",
                                     "Overall accuracy for all classes ")),
             # Get help text per dataset
             textOutput("helpText_full")
           ),
           
           mainPanel(
             tabsetPanel(
               tabPanel("View", tableOutput("view_full")),
               tabPanel("Summary", verbatimTextOutput("summary_full"))
             )
           )
         )
)
)


# Run the Shiny App
shinyApp(ui = ui, server = server)