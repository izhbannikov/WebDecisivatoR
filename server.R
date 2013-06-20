# function shinyServer() determines the layout
# and updates variables
shinyServer(function(input, output) {
  values <- reactiveValues()
  
  output$data_matrix <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    if(input$format) {
      data <- read_nexus(inFile$datapath)
    }else {
      data <- read.csv(inFile$datapath, header=T)
    }
    values[["data"]] <- data
    
    if(length(values[["data"]]) != 0) {
      taxon_matrix <- values[["data"]]
    } 
    
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    inFile <- input$file1
    if (is.null(inFile))
      return("No input file provided")
    
    values[["decisive_data"]] <- NULL
    
    if(input$tree_type == "unrooted") {
      if(input$fix_dataset == TRUE) {
        if(input$format) {
          res <- isdecisive(filename=inFile$datapath,fflag=T,format="nexus")
        } else {
          res <- isdecisive(filename=inFile$datapath,fflag=T)
        }
      } else {
        if(input$format) {
          res <- isdecisive(filename=inFile$datapath,format="nexus")
        } else {
          res <- isdecisive(filename=inFile$datapath)
        }
      }
        
    }
    else 
    {
      if(input$fix_dataset == TRUE) {
        if(input$format) {
          res <- isdecisive(inFile$datapath,unrooted=F,fflag=T,format="nexus")
        } else {
          res <- isdecisive(inFile$datapath,unrooted=F,fflag=T)
        }
      }else {
        if(input$format) {
          res <- isdecisive(inFile$datapath,unrooted=F,format="nexus")
        } else {
          res <- isdecisive(inFile$datapath,unrooted=F)
        }
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
    if(length(values[["decisive_data"]]) != 0) {
      df <- values[["data"]]
      df2 <- values[["decisive_data"]]
      
      df_row <- ''
      df_rows <- ''
      
      cn <- colnames(df)
      for(i in seq(1,dim(df)[2])) {
        df_row <- paste(df_row,paste('<td width=27 height=25 align=\"center\"><strong>',cn[i], '</strong></td>',sep=''),sep='')
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
    } 
    
    
  })
  
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
  
  read_nexus <- function(filename) {
    library(ape)
    
    ans<-read.nexus.data(filename)
    
    t<-read.delim(filename,sep='\n')
    genes <- matrix(nrow=1, ncol=3)
    for(i in seq(1,dim(t)[1])) {
      if(length(grep('CHARSET', t$X.NEXUS[i])) > 0)  {
        if( (length(grep('coding', t$X.NEXUS[i])) > 0) || (length(grep('noncoding', t$X.NEXUS[i])) > 0) )  next
        
        trw <- strsplit(toString(t$X.NEXUS[i]),' ')
        pos <- strsplit(trw[[1]][4],'-')
        genes<-rbind( genes, c( trw[[1]][2],pos[[1]][1],substring(pos[[1]][2],1,nchar(pos[[1]][2])-1) ) )
        
      }
    }
    
    genes <- data.frame(genes[(2:dim(genes)[1]),])
    
    data_matrix <- matrix(0,nrow=length(ans),ncol=dim(genes)[1])
    colnames(data_matrix) <- genes[,1]
    rownames(data_matrix) <- names(ans)
    
    for(i in seq(1,length(ans))) {
      for(j in seq(1,dim(genes)[1])) {
        str <- paste(ans[i][as.numeric(as.character(genes$X2[j])):as.numeric(as.character(genes$X3[j]))],collapse='')
        aaa<-gregexpr('-',str)
        if( (length(aaa[[1]])/(as.numeric(as.character(genes$X3[j])) - as.numeric(as.character(genes$X2[j])))) <= 0.1) {
          data_matrix[i,j] <- 1
        }
      }
    }
    
    out <- data_matrix
  }
  
    
})