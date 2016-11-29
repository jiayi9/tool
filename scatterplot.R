
scatter_data = eventReactive({
  input$ok |input$reset | input$delete
  
  },{
    
  req(input$scatter_x,input$scatter_y,input$facet_1,input$facet_2)
  withProgress(message = "Drawing scatterplot 1",{
    
  X = raw_deleted()
  
  if( input$facet_1 == "None" & input$facet_2 == "None"){
    p = scatter_0_facet(X, input$scatter_x, input$scatter_y,INDEX=RED())
  } else if(input$facet_1 != "None" & input$facet_2 == "None"){
    p = scatter_1_facet(X, input$scatter_x, input$scatter_y, input$facet_1,INDEX=RED())
  } else if(input$facet_1 != "None" & input$facet_2 != "None"){
    p = scatter_2_facet(X, input$scatter_x, input$scatter_y, input$facet_1, input$facet_2,INDEX=RED())
  } else {
    NULL
  }
  })
  p
})

scatter_delete_data = eventReactive({
  input$ok |input$reset | input$delete
  
},{
  req(input$scatter_x,input$scatter_y,input$facet_1,input$facet_2)
  withProgress(message = "Drawing scatterplot 2",{
    
  X = raw()
  set.seed(123)
  if( input$facet_1 == "None" & input$facet_2 == "None"){
    p = scatter_0_facet(X, input$scatter_x, input$scatter_y,mode=1,INDEX = !vals$keeprows)
  } else if(input$facet_1 != "None" & input$facet_2 == "None"){
    p = scatter_1_facet(X, input$scatter_x, input$scatter_y, input$facet_1,mode=1,INDEX = !vals$keeprows)
  } else if(input$facet_1 != "None" & input$facet_2 != "None"){
    p = scatter_2_facet(X, input$scatter_x, input$scatter_y, input$facet_1, input$facet_2,mode=1,INDEX = !vals$keeprows)
  } else {
    NULL
  }
  })
  p
})

output$scatter_delete = renderPlot({
  scatter_delete_data()  
})

output$SCATTER_DELETE = renderUI({
  plotOutput("scatter_delete",width = input$scatter_width, height = input$scatter_height, brush = "brush_1")
})


output$scatter_ggplot = renderPlot({
  scatter_data()
})

output$scatter_plotly = renderPlotly({
  ggplotly(scatter_data())
})

output$SCATTER = renderUI({
  
  if(input$plotType =="ggplot2"){
      plotOutput("scatter_ggplot", height = input$scatter_height, width = input$scatter_width)
  } else {
    plotlyOutput("scatter_plotly")
  }
  
})



