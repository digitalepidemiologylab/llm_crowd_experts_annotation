#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    "Visualization of different annotations",
    tabPanel("Select Database",
             sidebarLayout(
               sidebarPanel(
                 selectInput("database", "Choose a Database:",
                             choices = c("GPT annotations", 
                                         "EPFL annotations", 
                                         "Mturk annotations")),
                 actionButton("show_data", "Show Data")
                 
               ),
               mainPanel(
                 textOutput("selected_database"),
                 reactableOutput("data_table")
               )
             )
    ),
    tabPanel("Compare annotations",
             sidebarLayout(
               sidebarPanel(),
               mainPanel(
                 downloadButton("download_merged_data", "Download Merged Data"),
                 reactableOutput("merged_data_table")
               )
             )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Store the selected database in a reactive variable
  selected_database <- reactive({
    input$database
  })
  
  # Display the selected database name
  output$selected_database <- renderText({
    paste("Selected database: ", selected_database())
  })
  
  # Simulated database data (replace with your actual data)
    database_data <- list(
    "GPT annotations" = read_csv("data/gpt_annotations_simple.csv") %>% 
      group_by(prompt, sentiment_gpt) %>% 
      tally() %>% 
      ungroup()  %>% 
      mutate(prompt = round(prompt, digits = 0)),
    "EPFL annotations" = read_csv("data/epfl_annotations_simple.csv") %>% 
      group_by(stance_epfl) %>% 
      tally() %>% 
      ungroup() %>% 
      mutate(percentage = round(n/sum(n) *100, digits = 1)),
    "Mturk annotations" = read_csv("data/mturk_annotations_simple.csv") %>% 
      group_by(answer_tag_mturk) %>% 
      tally() %>% 
      ungroup() %>% 
      mutate(percentage = round(n/sum(n)*100, digits = 1))
  )
    # Display the selected database's data
    output$data_table <- renderReactable({
      if (!is.null(selected_database())) {
        reactable(database_data[[selected_database()]], filterable = TRUE)
      }
    })
    
    # Merge the three datasets
    merged_data <- reactive({
      database_data_2 <- list(
        "GPT annotations raw" = read_csv("data/gpt_annotations_simple.csv") %>% 
          mutate(id_tweet = as.integer(id_tweet)),
        "EPFL annotations raw" = read_csv("data/epfl_annotations_simple.csv") ,
        "Mturk annotations raw" = read_csv("data/mturk_annotations_simple.csv") 
      )
      
      merged_data <- merge(database_data_2[["GPT annotations raw"]],
                           database_data_2[["EPFL annotations raw"]],
                           database_data_2[["Mturk annotations raw"]],
                           by = "id_tweets")  # Replace with your actual merge logic
      return(merged_data)
      
      
    })
    
    # Display the merged data using reactable in the second tab
    output$merged_data_table <- renderReactable({
      if (!is.null(merged_data())) {
        reactable(merged_data())
      }
    })
    
    # Allow users to download the merged data
    output$download_merged_data <- downloadHandler(
      filename = function() {
        "merged_data.csv"  # Set the download file name
      },
      content = function(file) {
        # Write the merged data to a CSV file
        write.csv(merged_data(), file, row.names = FALSE)
      }
    )
}



shinyApp(ui = ui, server=server)
