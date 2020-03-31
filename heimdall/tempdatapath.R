
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", label=NULL
        ),
        actionButton("ggbutton","upload")
      ),
      mainPanel(
        tableOutput("contents")
      )
    )
  )
  
  server <- function(input, output) {
    fbUploaddf<<-data.frame("name"=c(1),"size"=c(1),"type"=c(1),"datapath"=c(1))
    fbUploaddf<<-fbUploaddf[-1,]
    
    observeEvent(input$file1,{
      if(NROW(fbUploaddf)==0)
        fbUploaddf<<-input$file1
      else
        fbUploaddf<<-rbind(fbUploaddf,input$file1)
      df <- input$file1
      path <- df$datapath 
      file.copy(path, paste("uploads/",df$name,sep=""))
      filelist<-list.files("uploads")
      for(i in 0:length(filelist))
        filelist[i]<-paste(gsub("/","//",getwd()),"//uploads//",filelist[i],sep="")
      
      fileConn<-file("data/fbupload.txt")
      writeLines(filelist, fileConn)
      close(fileConn)
      
      output$contents <- renderTable({fbUploaddf})
    })
  
  
  
  observeEvent(input$ggbutton,{
      do.call(file.remove, list(list.files("uploads", full.names = TRUE)))  
  })
  }
  
  shinyApp(ui, server)