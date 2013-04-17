# function shinyServer() determines the layout
# and updates variables
shinyServer(function(input, output) {
  values <- reactiveValues()
  
  output$data_matrix <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header=T)
    values[["data"]] <- data
    
    taxon_matrix <- values[["data"]]
    
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    inFile <- input$file1
    if (is.null(inFile))
      return("No input file provided")
    
    if(input$tree_type == "unrooted") {
      if(input$fix_dataset == TRUE) {
        res <- isdecisive(filename=inFile$datapath,fflag=T)
      } else {
        res <- isdecisive(filename=inFile$datapath)
      }
        
    }
    else 
    {
      if(input$fix_dataset == TRUE) {
        res <- isdecisive(inFile$datapath,unrooted=F,fflag=T)
      }else {
        res <- isdecisive(inFile$datapath,unrooted=F)
      }
    }
    
    if(input$fix_dataset == TRUE) {
      print(res[[2]])
      print("Fixed dataset")
      print(res[[1]])
    } else {
      print(res[[2]])
    }
    
  })
    
})