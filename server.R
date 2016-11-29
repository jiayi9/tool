InsPack <- function(pack) { 
  if (!pack %in% rownames(installed.packages())) { 
    print(paste("installing",pack)) 
    install.packages(pack) 
  } else {
    print(paste(pack," already installed")) 
  }
}

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
options(shiny.maxRequestSize=100*1024^2)


server <- function(input, session,output) {
  
  source("global.R",local = TRUE)
  source("FUNCTIONS.R",local = TRUE)
  source("control.R",local = TRUE)
  
  source("values.R",local = TRUE)
  source("data.R",local = TRUE)
  
  source("boxplot.R",local = TRUE)
  source("scatterplot.R",local = TRUE)
  source("cdf.R",local = TRUE)
  source("download.R",local = TRUE)
  source("analysis.R",local = TRUE)
  
  output$info_table = renderTable({
    D = raw_deleted()
    xname = input$x
    yname = input$y
    f1 = input$facet_1
    f2 = input$facet_2
    
    if(input$facet_1=="None" & input$facet_2 =="None"){

      X = data.frame(x = D[,xname],y=D[,yname])
      
#      X = na.omit(X)
      
      S = X %>% group_by(x) %>% dplyr::summarise(
        MEAN = mean(y,na.rm = TRUE),
        SD = sd(y,na.rm = TRUE),
        N = length(y)
      )
      names(S)[1] = xname
      
    } else if(input$facet_1!="None" & input$facet_2 =="None"){
      
      X = data.frame(x = D[,xname],y=D[,yname], f = D[,f1])
#      X = na.omit(X)
      
      S = X %>% group_by(f,x) %>% dplyr::summarise(
        MEAN = mean(y,na.rm = TRUE),
        SD = sd(y,na.rm = TRUE)
      )
      names(S)[1:2] = c(f1,xname)
      
    } else if(input$facet_1!="None" & input$facet_2 !="None"){
      
      X = data.frame(x = D[,xname],y=D[,yname], f1 = D[,f1],f2 = D[,f2])
#      X = na.omit(X)
      
      S = X %>% group_by(f2,f1,x) %>% dplyr::summarise(
        MEAN = mean(y,na.rm = TRUE),
        SD = sd(y,na.rm = TRUE)
      )
      names(S)[1:3] = c(f2,f1,xname)
    } else {
      S = NULL
    }  
    S
  })
  
  output$pvalue = renderText({
    x = raw_deleted()[,input$x]
    y = raw_deleted()[,input$y]
    pvalue = robust_anova_p(x,y)
    paste("Boxplot ANOVA P-value:", pvalue,".")
  })
  
  output$pvalue_2 = renderText({
    x = as.character(raw_deleted()[,input$x])
    y = raw_deleted()[,input$y]

    pvalue =tryCatch({
      round(kSamples::ad.test(y~x)$ad[5],4)
      #round(kSamples::ad.test(x,y)$ad[5])
    },error = function(e){1})
    
    
    
    paste("CDF K-sample KS P-value:", pvalue,".")
  })
}




