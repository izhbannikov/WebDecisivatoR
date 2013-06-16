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
      values[["decisive_data"]] <- res[[1]]
    } else {
      print(res[[2]])
    }
    
  })
  
  output$decisive_matrix <- renderText({
    df <- values[["data"]]
    df2 <- values[["decisive_data"]]
    
    df_row <- ''
    df_rows <- ''
    
    cn <- colnames(df)
    for(i in seq(1,dim(df)[2])) {
      df_row <- paste(df_row,cell_html(cn[i],flag=F),sep='')
    }
    df_rows <- paste(df_rows,row_html(df_row),sep='')
    df_row <- ''
    for(i in seq(1,dim(df2)[1])) {
      df_row <- paste(df_row,cell_html(df[i,1],flag=F),sep='')
      for(j in seq(1,dim(df2)[2])) {
        if(df[i,j+1] != df2[i,j]) {
          df_row <- paste(df_row,cell_html(df2[i,j],flag=T),sep='')
        } else {
          df_row <- paste(df_row,cell_html(df2[i,j]),sep='')
        }
        
      }
      df_rows <- paste(df_rows,row_html(df_row),sep='')
      df_row <- ''
    }
    
    full_table <- paste0('<table border=1>', df_rows, '</table>')
  })
  
  radio_html <- function(radio_name, radio_value, radio_text) {
    paste0('<input type="radio" name="', 
           radio_name, '" value="', radio_value, '">', radio_text)
  }
  
  cell_html <- function(table_cell,flag=F) {
    if (flag) {
      paste0('<td BGCOLOR="#ffff00" width=27 height=20 align=\"center\">',table_cell, '</td>')
    } else {
      paste0('<td width=27 height=20 align=\"center\">',table_cell, '</td>')
    }
  }
  
  row_html <- function(table_row) {
    paste0('<tr>', table_row, '</tr>')
  }
    
})