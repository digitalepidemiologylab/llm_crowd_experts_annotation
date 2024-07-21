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
source("scripts/4_shiny_data.R")

server <- function(input, output) {
  # Reactive expression to return the selected dataset
  datasetInput_describe <- reactive({
    switch(input$dataset_describe,
           "Summary of stance by EPFL experts" = shiny_epfl,
           "Summary of stance by Amazon Mturk workers" = shiny_mturk,
           "Summary of stance by GPT" = shiny_gpt,
           "Summary of stance by Mistral (7B)" = shiny_mistral,
           "Summary of stance by Mixtral (8x7B)" = shiny_mixtral,
           "Summary of stance by Llama3 (8B)" = shiny_llama,
           "Summary of stance by Llama3 (70B)" = shiny_llama_70b)
  })
  
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
  output$view_describe <- renderDT({
    datasetInput_describe()},
    filter = "top",
    options = list(pageLength = 16)
  )
  
  # Show the selected dataset
  output$view <- renderDT({
    datasetInput()},
    filter = "top",
    options = list(pageLength = 25))
    
  # # Generate a summary of the dataset
  # output$summary <- renderTable({
  #   datasetInput()
  # })
  
  # Show the selected dataset
  output$view_full <- renderDT({
    datasetInput_full()},
  filter = "top",
  options = list(pageLength = 25))
  
  # Generate a summary of the dataset
  # output$summary_full <- renderTable({
  #   datasetInput_full()
  # })
  
  # Help text per dataset
  output$helpText_describe <- renderText({
    switch(input$dataset_describe,
           "Summary of stance by EPFL experts" = "Distribution of stance depending on agreement reached by EPFL experts. Tweets with the same stance assigned by at least three out of the four EPFL experts were considered tweets with partial agreement",
           "Summary of stance by Amazon Mturk workers" = "Distribution of stance depending on agreement reached by Amazon Mturk workers. Tweets with the same stance assigned by at least 75% of the Amazon Mturk workers classifying each tweet were considered tweets with partial agreement",
           "Summary of stance by GPT" = "Distribution of stance provided by GPT versions 3.5 and 4 and depending on the prompt provided to GPT and experts' agreement",
           "Summary of stance by Mistral (7B)" = "Distribution of stance provided by Mistral (7B) depending on the prompt provided to Mistral (7B) and experts' agreement",
           "Summary of stance by Mixtral (8x7B)" = "Distribution of stance provided by Mixtral (8x7B) depending on the prompt provided to Mixtral (8x7B) and experts' agreement",
           "Summary of stance by Llama3 (8B)" = "Distribution of stance provided by Llama3 (8B) depending on the prompt provided to Llama3 (8B) and experts' agreement",
           "Summary of stance by Llama3 (70B)" = "Distribution of stance provided by Llama3 (70B) depending on the prompt provided to Llama3 (70B) and experts' agreement")
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
  
  tabPanel("Distribution of stance by each method",
           # Sidebar with a slider input for number of bins 
           sidebarLayout(
             sidebarPanel(
               # Input: Selector for choosing dataset
               selectInput("dataset_describe", "Choose a dataset:", 
                           choices = c("Summary of stance by EPFL experts",
                                       "Summary of stance by Amazon Mturk workers",
                                       "Summary of stance by GPT",
                                       "Summary of stance by Mistral (7B)",
                                       "Summary of stance by Mixtral (8x7B)",
                                       "Summary of stance by Llama3 (8B)",
                                       "Summary of stance by Llama3 (70B)")),
               # Get help text per dataset
               textOutput("helpText_describe")
             ),
             
             # Main panel for displaying outputs
             mainPanel(
               tabsetPanel(
                 
                 tabPanel("View", DT::dataTableOutput("view_describe"))
               )
             )
           )
  ),
  
  tabPanel("Performance of Mturk and LLMs (tweets with partial agreement)",
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
        
        tabPanel("View", DT::dataTableOutput("view"))
      )
    )
  )
),

tabPanel("Performance of Mturk and LLMs (tweets with full agreement)",
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
               tabPanel("View", DT::dataTableOutput("view_full"))
             )
           )
         )
)
)


# Run the Shiny App
shinyApp(ui = ui, server = server)
