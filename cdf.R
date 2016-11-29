
cdf_data = eventReactive({
  input$ok |input$reset | input$delete
  
},{
  req(input$y, input$x, input$facet_1,input$facet_2)
  X = raw_deleted()
  
  if( input$facet_1 == "None" & input$facet_2 == "None"){
    p = cdf_0_facet(X, input$x, input$y )
  } else if(input$facet_1 != "None" & input$facet_2 == "None"){
    p = cdf_1_facet(X, input$x, input$y, input$facet_1)
  } else if(input$facet_1 != "None" & input$facet_2 != "None"){
    p = cdf_2_facet(X, input$x, input$y, input$facet_1, input$facet_2)
  } else {
    NULL
  }
  p
})


output$cdf_ggplot = renderPlot({
  cdf_data()
})

output$cdf_plotly = renderPlotly({
  ggplotly(cdf_data())
})

output$CDF = renderUI({
  
#   if(input$plotType =="ggplot2"){
#     plotOutput("cdf_ggplot",width = input$cdf_width, height = input$cdf_height)
#   } else {
#     plotlyOutput("cdf_plotly")
#   }
  plotOutput("cdf_ggplot",width = input$cdf_width, height = input$cdf_height)
  
})



