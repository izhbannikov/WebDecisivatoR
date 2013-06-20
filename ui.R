library(shiny)
library(shinyIncubator)
library(decisivatoR)

# Function shinyUI() determines user interface
shinyUI(
  pageWithSidebar(
    headerPanel(
      "decisivatoR"
    ),
    sidebarPanel(
      fileInput(inputId = "file1", label="Upload your own data set:",accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      checkboxInput("format", "Nexus format", FALSE),
      br(),
      radioButtons("tree_type", "Tree type:",
                   list("Unrooted" = "unrooted",
                        "Rooted" = "rooted")),
      checkboxInput("fix_dataset", "Make decisive", FALSE)
    ),
    
    mainPanel(
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      h4("Data matrix"),
      tableOutput("data_matrix"),
      
      h4("Decisive matrix"),
      HTML("<div name=\"decisive_matrix\" id=\"decisive_matrix\" class=\"shiny-html-output\"></div>")
      
      
    )
  )
)