RAW = reactive({
  
  withProgress(message = "Loading data",{
  
  if(input$fileSourceType =="Local Upload"){
    req(input$file1)
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    ext = tools::file_ext(inFile)[1]
    print(ext)
    if(ext =="csv"){
      R = read.csv(inFile$datapath, header=TRUE, sep=",",stringsAsFactors=FALSE)
    } else if( ext == "rda"){
      R = readRDS(inFile$datapath)
    } else if( ext == "sas7bdat") {
      R = haven::read_sas(inFile$datapath)
    } else {
      stop("Upoaded file format unsupported")
    }
  } else if(input$fileSourceType =="PID List"){
    req(input$pid_1)
    udv_job = input$pid_1
    Results<-paste(ResultsPath, udv_job, sep = "/")
    
    sasFile <- file.path(Results, "all.sas7bdat")
    rdaFile <- file.path(Results, "all.rda")
    
    validate(need(file.exists(rdaFile)|file.exists(sasFile),"This PID does not contain a rda or sas file."))
    
    
    if(file.exists(rdaFile)){
      R = readRDS(rdaFile)
    } else if(file.exists(sasFile)){
      R =  rxImport(inData = sasFile, rowsPerRead = 200000)
    } else {
      NULL
    }
    
  } else{
    udv_job = PID_3()
    Results<-paste(ResultsPath, udv_job, sep = "/")
    
    sasFile <- file.path(Results, "all.sas7bdat")
    rdaFile <- file.path(Results, "all.rda")
    
    validate(need(file.exists(rdaFile)|file.exists(sasFile),"This PID does not contain a rda or sas file."))

    if(file.exists(rdaFile)){
      R = readRDS(rdaFile)
    } else if(file.exists(sasFile)){
      R =  rxImport(inData = sasFile, rowsPerRead = 200000)
    } else{
      NULL
    }

  }
  cat("-------------\n")
  print(paste("RAW:", nrow(R),ncol(R)))
  cat("-------------\n")
  
  })
  
  R[,names(R) %in% ATTR_list][] = lapply(R[,names(R) %in% ATTR_list], as.character)
  R[,names(R) %in% ATTR_list][is.na(R[,names(R) %in% ATTR_list])] = "N/A"

  R$DRIVE_SERIAL_NUM = R$DRIVE_SN
  R
  
})

output$L = renderPrint({
  list(dim(RAW()),table(INDEX()),ATTR_NAMES(),PARA_NAMES())
})



raw = reactive({
  R = RAW()[INDEX(),]
  R
})

raw_deleted = eventReactive(input$ok|input$reset|input$delete,{
  R = raw()[vals$keeprows,]
  R
})



