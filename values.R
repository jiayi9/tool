

PID_3 = eventReactive(input$confirmPID,{
  input$pid_3
})


ATTR_NAMES = reactive({
  X = RAW()
  NAMES = names(X)
  N = nrow(X)
  uniqueValues = sapply(X, function(x) length(unique(x)) )
  missingProps = sapply(X, function(x) sum(is.na(x))/N  )
  
#   print(uniqueValues)
#   print(missingProps)
  
  INDEX = (uniqueValues >0) & (missingProps<1)
  R = NAMES[INDEX]
  if(input$attr_use_list){
    R = R[R %in% ATTR_list]
  }
  R
})


PARA_NAMES = reactive({
  X = RAW()
  NAMES = names(X)
  isNumeric = sapply(X, is.numeric)
  
  uniqueValues = sapply(X, function(x) length(unique(x)) )
  missingProps = sapply(X, function(x) sum(is.na(x))/nrow(X)  )
  
  INDEX = (uniqueValues >0) & (missingProps<1)
  
  R = NAMES[isNumeric & INDEX]
  R
})



vals = reactiveValues(keeprows = TRUE)


observeEvent(input$delete,{
  Res=brushedPoints(raw(),input$brush_1,allRows = TRUE)
  vals$keeprows = !(Res$selected_  |   !vals$keeprows)
  vals$keeprows[is.na(vals$keeprows)] = FALSE
})



observeEvent(input$reset,{
  vals$keeprows = TRUE
})

