


output$PID_1 = renderUI({
  selectInput("pid_1","PID",choices = PIDS)
})


output$FILTERS = renderUI({
  selectInput("filters","Filters",choices = ATTR_NAMES(),multiple = TRUE)
})
observe({
  
  filter_names = input$filters
  n = length(filter_names)
  
  X = RAW()
  
  lapply(1:n,function(x){
    output[[paste0("FILTER_",x)]] = renderUI({
      req(input$filters)
      
#       selectInput(paste0("filter_",x),
#                   paste0(filter_names[x]),
#                   choices = unique(X[,filter_names[x]]),
#                   multiple = TRUE,
#                   selected = unique(X[,filter_names[x]])
#       )
      
#         dropdownButton(
#           label = paste0(filter_names[x]), status ="default",width =50,
#           div(class = "dynamicSI" ,
#             actionButton(inputId = paste0("filter_","_all",x), label = "(Un)select all",
#                          class="btn btn-primary btn-sm",
#                          data = paste0("filter_",x),
#                          name = paste(filter_names[x])
#                          ),
#             checkboxGroupInput(paste0("filter_",x),"",choices = sort(unique(X[,filter_names[x]])),selected = unique(X[,filter_names[x]]))
#           )
#         )
      
#         div(class = "dynamicSI" , 
#             actionButton(inputId = paste0("filter_","_all",x), label = "(Un)select all",
#                          class="btn btn-primary btn-sm",
#                          data = paste0("filter_",x),
#                          name = paste(filter_names[x])
#             ),
#             checkboxGroupInput(paste0("filter_",x),"",choices = sort(unique(X[,filter_names[x]])),selected = unique(X[,filter_names[x]]))
#         )
      
          
              dropdownButton(
                label = paste0(filter_names[x]), status ="default",width =50,
#                   div(class = "dynamicSI" ,
#                       
#                     actionButton(inputId = paste0("filter_","_all",x), label = "(Un)select all",
#                                  class="btn btn-primary btn-sm",
#                                  data = paste0("filter_",x),
#                                  name = paste(filter_names[x])
#                                  )
#                   )
#                   ,
                    checkboxGroupInput(paste0("filter_",x),"",choices = sort(unique(X[,filter_names[x]])),selected = unique(X[,filter_names[x]]))
                )
              
            
    })
  })
  
  output$FILTER_GROUP = renderUI({
    div(class="dynamicSI",
        
      lapply(1:n, function(i){
        uiOutput(paste0("FILTER_",i))
      })
    
    )
  })
  
})


observeEvent(input$lastSelect, {
  
  if (!is.null(input$lastSelectId)) {
    cat("lastSelectId:", input$lastSelectId, "\n")
    cat("lastSelectName:", input$lastSelectName, "\n")
  }  
  
  X = RAW()
  # selectInput id
  Filter = input$lastSelectId
  # column name of dataset, (label on select input)
  NAME = input$lastSelectName
  choices = sort(unique(X[,NAME]))
  
  if (length(input[[Filter]]) == 0) {
    # in corresponding selectInput has no elements selected
    updateCheckboxGroupInput(
      session = session, inputId = Filter, selected = as.character(choices)
      
    )
  } else {
    # has at least one element selected

    updateCheckboxGroupInput(
      session = session, inputId = Filter, selected = ""
      
    )
  }
  
})



INDEX = reactive({
  filter_names = input$filters
  n = length(filter_names)
  X = RAW()
  INDEX_N = lapply(1:n,function(i){
    X[,filter_names[i]] %in% input[[paste0("filter_",i)]]
  })
  
  X = Reduce("&",INDEX_N)
  if(is.null(filter_names)) X = req(TRUE,nrow(RAW()))
  
  validate(need(sum(X)>0,"No data selected."))
  X
})



output$Y = renderUI({
  choices = PARA_NAMES()
  selectInput("y","Boxplot Y",choices,multiple = FALSE,selected = choices[1])
})

output$X = renderUI({
  choices = ATTR_NAMES()
  selectInput("x","Boxplot X",choices)
})

output$SCATTER_X = renderUI({
  choices = PARA_NAMES()
  selectInput("scatter_x","Scatter X ",choices,multiple = FALSE,selected = choices[1])
})

output$SCATTER_Y = renderUI({
  choices = PARA_NAMES()
  selectInput("scatter_y","Scatter Y ",choices,multiple = FALSE,selected = choices[2])
})

output$FACET_1 = renderUI({
  choices = c("None",ATTR_NAMES())
  selectInput("facet_1","Facet 1",choices,selected = "None")
})

output$FACET_2 = renderUI({
  choices = c("None",ATTR_NAMES())
  selectInput("facet_2","Facet 2",choices,selected = "None")
})

output$LAYER = renderUI({
  choices = ATTR_NAMES()
  if("EFA_HEAD" %in% choices) default = "EFA_HEAD"
  if("EFA_DRIVE" %in% choices) default = "EFA_DRIVE"
  if("EFA" %in% choices) default = "EFA"
  if("EFA_STATUS" %in% choices) default = "EFA_STATUS"
  if("STATUS" %in% choices) {default = "STATUS"}
  if("EFA_RESULT" %in% choices) default = "EFA_RESULT"
  
  if(is.null(default)| length(default)==0| default=="") {default = choices[1]}
  
  selectInput("layer","Target",choices,selected = default)
})

output$RED = renderUI({
  req(input$layer)
  choices = sort(unique( raw()[,input$layer]))
  default = character()
  if("F" %in% choices) default = c(default,"F")
  if("FAIL" %in% choices) default = c(default,"FAIL")
  if("FAILURE" %in% choices) default = c(default,"FAILURE")
  if("Fail" %in% choices) default = c(default,"Fail")
  if("Failure" %in% choices) default = c(default,"Failure")
  
#  if(length(default)==0) default = setdiff(choices,"N/A")
  dropdownButton(
    label = "Define Failure", status ="default",width =50,
    checkboxGroupInput("red","",choices = choices,selected = NULL)
  )
#  selectInput("red","Red Points",choices,multiple = TRUE)
})

RED = reactive({
   R = raw_deleted()[,input$layer] %in% input$red
#   print(R)
   R
})


output$info = renderText({
  if(input$delete){
    x = paste(sum(vals$keeprows,na.rm = TRUE),"rows in chart.")
  } else{
    x = ""
  }
  paste0(
    nrow(RAW())," rows, ", ncol(RAW())," cols in raw data. ",
    nrow(raw())," rows in selected data. ",
    x
  )

})