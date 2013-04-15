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
    
    
    taxon_matrix <- values[["data"]]
    
    X <- levels(taxon_matrix[,1])
    ans <- data.frame(taxon_matrix[,2:length(taxon_matrix)])
    S <- lapply(seq_len(ncol(ans)), function(i) ans[,i])
    S <- lapply( seq_len(length(S)), function(i) lapply( seq_len(length(S[[i]])), function(j) if ( S[[i]][j] == 1 ) { S[[i]][j] <- X[j] } else { S[[i]][j] <- "NA"  } ) )
    S <- lapply( seq_len(length(S)), function(i) S[[i]] <- S[[i]][which(S[[i]] != "NA")]  )
    S <- lapply( seq_len(length(S)), function(i) unlist(S[[i]]))
    
    if(input$tree_type == "unrooted") {
      res <- isdecisive(taxa=X,s=S)
    }
    else 
    {
      res <- isdecisive(taxa=X,s=S,unrooted=F)
    }
    
    
    
    print(res)
  })
    
})