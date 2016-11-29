boxplot_data = eventReactive({
  input$ok |input$reset | input$delete
},{
  req(input$x,input$y,input$facet_1,input$facet_2)
  withProgress(message = "Drawing boxplot 1",{
    
  X = raw_deleted()
  print(dim(X))
  set.seed(123)
  
  if( input$facet_1 == "None" & input$facet_2 == "None"){
    p = boxplot_0_facet(X, input$x, input$y,INDEX=RED())
  } else if(input$facet_1 != "None" & input$facet_2 == "None"){
    p = boxplot_1_facet(X, input$x, input$y, input$facet_1,INDEX=RED())
  } else if(input$facet_1 != "None" & input$facet_2 != "None"){
    p = boxplot_2_facet(X, input$x, input$y, input$facet_1, input$facet_2,INDEX=RED())
  } else {
    NULL
  }
  })
  p
})


boxplot_delete_data = eventReactive({
  input$ok |input$reset | input$delete
  
},{
  req(input$x,input$y,input$facet_1,input$facet_2)
  withProgress(message = "Drawing boxplot 2",{
    
  X = raw()
  set.seed(123)
  if( input$facet_1 == "None" & input$facet_2 == "None"){
    p = boxplot_0_facet(X, input$x, input$y,INDEX = !vals$keeprows, mode=1)
  } else if(input$facet_1 != "None" & input$facet_2 == "None"){
    p = boxplot_1_facet(X, input$x, input$y, input$facet_1,INDEX = !vals$keeprows, mode=1)
  } else if(input$facet_1 != "None" & input$facet_2 != "None"){
    p = boxplot_2_facet(X, input$x, input$y, input$facet_1, input$facet_2,INDEX = !vals$keeprows, mode=1)
  } else {
    NULL
  }
  })
  p
})


output$boxplot_delete = renderPlot({
  boxplot_delete_data()
})

output$BOXPLOT_DELETE = renderUI({
  plotOutput("boxplot_delete",width = input$boxplot_width, height = input$boxplot_height, brush = "brush_1")
})

output$boxplot_ggplot = renderPlot({
  boxplot_data()
})

output$boxplot_plotly = renderPlotly({
  ggplotly(boxplot_data())
})

output$BOXPLOT = renderUI({
#   
#   if(input$ok==0){
#     return(    
#       plotOutput("boxplot_ggplot",width = input$boxplot_width, height = 0)
#     )
#   }
  
  if(input$plotType =="ggplot2"){
    plotOutput("boxplot_ggplot",width = input$boxplot_width, height = input$boxplot_height)
  } else {
    plotlyOutput("boxplot_plotly")
  }

})



