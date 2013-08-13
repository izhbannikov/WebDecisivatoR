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
      selectInput("format", "Input file format:",
                  list("CSV" = "csv", "Nexus" = "nex")
                  ),
      br(),
      radioButtons("tree_type", "Tree type:",
                   list("Unrooted" = "unrooted",
                        "Rooted" = "rooted")),
      checkboxInput("fix_dataset", "Make decisive", FALSE),
      br(),
      conditionalPanel(
        condition = "input.fix_dataset == true",
        downloadButton('downloadList', 'Download suggested list')
      ),
      br(),
      helpText(a("Test data",href="https://www.dropbox.com/sh/17srv29a2pfzcqg/907vSpcO6k",target="_blank")),
      helpText(a("GitHub", href="https://github.com/izhbannikov/decisivatoR",target="_blank"))    
    ),
    
    mainPanel(
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      h4("Data matrix"),
      tableOutput("data_matrix"),
      
      h4("Decisive matrix"),
      HTML("<div name=\"decisive_matrix\" id=\"decisive_matrix\" class=\"shiny-html-output\"></div>"),
      
      h4("Suggested list"),
      tableOutput("suggested_list")
    )
  )
)