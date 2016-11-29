

output$DOWNLOADLIST = renderUI({
  
  selectInput("downloadList","Columns to download", choices = sort(names(RAW())), selected = NULL, multiple = TRUE,width = "1000px")
})

output$downloadData = downloadHandler(
  filename = function(){
    paste("data.csv")
    },
    content = function(con){
      
      if(input$useSelected == 1){
        File = RAW()[,input$downloadList]
      } else {
        File = raw()[,input$downloadList]
        
      }
      
      write.csv(File,con,row.names = FALSE)
    }
)