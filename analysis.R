


output$BY = renderUI({
  choices = c("None",ATTR_NAMES())
  selectInput("by","Group",choices, selected = choices[1])
})

output$ATTRIBUTE = renderUI({
  choices = ATTR_NAMES()
  selectInput("attribute","Attribute",choices,selected = choices[2])
})

# output$TARGET = renderUI({
#   choices = ATTR_NAMES()
#   if("EFA_HEAD" %in% choices) default = "EFA_HEAD"
#   if("EFA_DRIVE" %in% choices) default = "EFA_DRIVE"
#   if("EFA" %in% choices) default = "EFA"
#   if("EFA_STATUS" %in% choices) default = "EFA_STATUS"
#   if("STATUS" %in% choices) {default = "STATUS"}
#   if("EFA_RESULT" %in% choices) default = "EFA_RESULT"
#   
#   if(is.null(default)| length(default)==0| default=="") {default = choices[1]}
#   
#   selectInput("target","Target",choices,selected = default)
#   
# })

# output$FAIL_DEFINE = renderUI({
#   req(input$target)
#   choices = sort(unique( raw_deleted()[,input$target]))
#   default = character()
#   if("F" %in% choices) default = c(default,"F")
#   if("FAIL" %in% choices) default = c(default,"FAIL")
#   if("FAILURE" %in% choices) default = c(default,"FAILURE")
#   if("Fail" %in% choices) default = c(default,"Fail")
#   if("Failure" %in% choices) default = c(default,"Failure")
#   
#   #  if(length(default)==0) default = setdiff(choices,"N/A")
# #   dropdownButton(
# #     label = "Define failures", status ="default",width =50,
# #     checkboxGroupInput("fail_define","",choices = choices,selected = NULL)
# #   )
#   selectInput("fail_define","Define failures",choices,multiple = TRUE,selected = default)
# })



FAIL = reactive({
#  R = raw_deleted()[,input$target] %in% input$fail_define
  R = RED()
#  print(R)
  R = ifelse(R,"F","P")
  R
})


######################### new table  #############################

TABU = reactive({
  library(scales)
  
  req(input$by,input$attribute,FAIL())
  
  X = raw_deleted()
  Factor = X[,input$attribute]
  Status = FAIL()
  sig = input$sig
#  cutoff = nrow(X)*0.05
  cutoff = round(nrow(X)*input$min_prop)
  
  if(input$by == "None"){
    D = data.frame(Factor, Status)
    
    M = D %>% group_by(Factor) %>% dplyr::summarise(
      
      Qty = length(Status),
      Fail = sum(Status == "F"),
      Rate = round(sum(Status == "F")/length(Status),5)
      #    Pvalue = round( chisq_test(Attribute, Status, nrow(X)*0.05), 3)
      
      
    )
    R = data.frame(Group = input$by, M, Pvalue = round( chisq_test(Factor, Status, cutoff,sig=sig), 3))
    

  } else {
    Group = X[,input$by]
    D = data.frame(Group, Factor, Status)
    
    M = D %>% group_by(Group, Factor) %>% dplyr::summarise(
      
      Qty = length(Status),
      Fail = sum(Status == "F"),
      Rate = round(sum(Status == "F")/length(Status),5)
      
      #    Pvalue = round( chisq_test(Attribute, Status, nrow(X)*0.05), 3)
      
    )
    
    P = D %>% group_by(Group) %>% dplyr::summarise(
      
      Pvalue = round( chisq_test(Factor, Status, cutoff, sig = sig), 3)
      
    )
    
    R = merge(M, P, by="Group")
    
  }
  R$Group = tabu_vector_adjust(as.character(R$Group))
  
  for(i in 1:nrow(R)){ if(R$Group[i]=="") R$Pvalue[i] ="" }
  R$Pvalue = as.numeric(R$Pvalue)
  
#  R$Rate = as.numeric(str_replace(as.character(R$Rate), "^0\\.", "."))
#  R$Rate = str_replace(as.character(R$Rate), "^0\\.", ".")
  
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }
  
  R$Rate = percent(R$Rate)
  
  R$Rate = ifelse(R$Rate =="0.00%", "",R$Rate)
  
  R
  
  
})

output$TABU = renderTable({
  TABU()
})

output$TABU_2 = DT::renderDataTable({
    R = TABU()
    datatable(R, 

              class = "table",
              caption = htmltools::tags$caption(
                style = 'caption-side: bottom; text-align: center;',
                '', htmltools::em('P value of Chisq test by Group')
              ),
              rownames = FALSE,
              #selection ='single',
              options = list(                paging = FALSE,
                                             ordering = FALSE,
                                             filtering = FALSE,
                                             searching =FALSE,
                                             info=FALSE)
    )%>% 
      formatStyle(
        #paste0("RPI"),
        #c("RPI","HSI","CSI","RPI","RPI_2","HSI_2","CSI_2"),
        "Pvalue",
        color = styleInterval(c(0.05),c("red","black"))
        
      )
      
})


output$TABU_2_UI = renderUI({
  DT::dataTableOutput("TABU_2",width = input$table_width)
})

# observe({
#   req(input$target)
#   choices = sort(unique( raw_deleted()[,input$target]))
#   
#   if(input$allNoneNA){
#     updateSelectInput(session = session,inputId = "fail_define",selected = setdiff(choices,"N/A") )
#   } else{
#     updateSelectInput(session = session,inputId = "fail_define",selected = "" )
#   }
# })

observe({
  req(input$layer)
  choices = sort(unique( raw_deleted()[,input$layer]))
  
  if(input$allNoneNA){
    updateSelectInput(session = session,inputId = "red",selected = setdiff(choices,"N/A") )
  } else{
    #updateSelectInput(session = session,inputId = "red",selected = "" )
  }
})
# 
# pvalue_table = reactive({
#   
#   req(input$by,input$attribute,FAIL())
#   
#   X = raw_deleted()
#   Attribute = X[,input$attribute]
#   Status = FAIL()
#   
#   if(input$by=="None"){
#     M = data.frame(By ="None", Pvalue = round( chisq_test(Attribute, Status, nrow(X)*0.05), 3))
#     return(M)
#   }
#   
#   By = X[,input$by]
#   
#   D = data.frame(By = By, Attribute = Attribute, Status = Status)
# 
#   M = D %>% group_by(By) %>% dplyr::summarise(
# #    Pvalue = round( chisq_test(Attribute, Status, nrow(X)*0.05), 3)
#     Pvalue = round( chisq_test(Attribute, Status, nrow(X)*0.05), 3)
#     
#   )  
#   M
#   
# })
# 
# 
# output$PVALUE_TABLE_2 = renderTable({ pvalue_table() })
# output$PVALUE_TABLE = renderDataTable({
#   R = pvalue_table()
#   datatable(R, 
#             
#             caption = htmltools::tags$caption(
#               style = 'caption-side: bottom; text-align: center;',
#               '', htmltools::em('P value of Chisq test by Group')
#             ),
#             #selection ='single',
#             options = list(                paging = FALSE,
#                                            ordering = FALSE,
#                                            filtering = FALSE,
#                                            searching =FALSE,
#                                            info=FALSE)
#   )%>% 
#     formatStyle(
#       #paste0("RPI"),
#       #c("RPI","HSI","CSI","RPI","RPI_2","HSI_2","CSI_2"),
#       "Pvalue",
#       color = styleInterval(c(0.05),c("red","black"))
#       
#     )
# })


output$crosstab = renderPlot({
  X = raw_deleted()
  
  if(input$by=="None"){
    D = data.frame(Attribute = X[,input$attribute], Status = FAIL())
    D = data.frame(table(D))
    p = ggplot(D,aes(Status,Attribute))+    
      geom_point(aes(size = Freq), colour = "green") + 
      theme_bw() + xlab("FAIL / PASS") + ylab(input$attribute) +
      geom_text(aes(label = Freq))+ggtitle("Contigency Table")
    return(p)
  }
  
  
  D = data.frame(By = X[,input$by], Attribute = X[,input$attribute], Status = FAIL())
  
  D = data.frame(table(D))

  ggplot(D,aes(Status,Attribute))+
    geom_point(aes(size = Freq), colour = "green") + 
    theme_bw() + xlab("FAIL / PASS") + ylab(input$attribute) +
    geom_text(aes(label = Freq)) +
    facet_grid(. ~ By,scales = "free",space = "free")+ggtitle("Contigency Table")
  
})

output$CROSSTAB = renderUI({
  X = raw_deleted()
  
  Attribute = X[,input$attribute]
  n = length(unique(Attribute))
  plotOutput("crosstab",height = 20*n+80)
})