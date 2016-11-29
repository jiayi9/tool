tabu_vector_adjust = function(x){
  
  if(length(x)==1) return(x)
  
  n = length(x)
  R = character(n)
  R[1] = x[1]
  for(i in 2:n){
    if(x[i] == x[i-1]){
      R[i] == ""
    } else {
      R[i] = x[i]
    }
  }
  R
  
}



chisq_test = function(x,y,n,sig=3){
  
  M = data.frame(x=x,y=y,stringsAsFactors = FALSE)
  M = M[!is.na(x),]
  M = M[x!="N/A",]
  x = M[,1]
  y = M[,2]
  
  
  library(dplyr)
  temp = M %>% group_by(x) %>% dplyr::summarise(sum = sum(y=="F"))    
  
  #>=3 group  
  big1 = temp$x[temp$sum>=sig]
  
  #>= n group
  big2 = names(table(x))[table(x)>=n]
  
  big_group = union(big1,big2)  
  
  M2 = M[M$x %in% big_group,]
  R = 400
  
  R = tryCatch({
    chisq.test(M2$x,M2$y)$p.value
  },
  warning = function(w){
    suppressWarnings({  chisq.test(M2$x,M2$y)$p.value  })
  },
  error = function(e){
    401
  }
  )
  
  if(is.na(R)) R = 401
  
  return(R)
}


cdf_0_facet = function(D, xname, yname){
  if( !all(c(xname,yname) %in% names(D))) {stop("Name not in Data")}
  
  x = as.character(D[,xname])
  y = D[,yname]
  
  X = data.frame(Group= x, y = y)

  p = ggplot(X, aes(color=Group,x=y))+ theme_bw()+
    stat_ecdf() +
    xlab(xname)+ylab(yname)+
    theme(
          axis.text.x = element_text(angle = 90, hjust = 0),
          plot.title = element_text( colour = "blue",size=rel(1.4))
    )
  p 
}

cdf_1_facet = function(D, xname, yname, facet_name){
  library(dplyr)
  set.seed(123)
  if( !all(c(xname,yname,facet_name) %in% names(D))) {stop("Name not in Data")}
  
  X = data.frame(Group = as.character(D[,xname]), y = D[,yname], f = as.character(D[,facet_name]))
  

  TT = paste(facet_name)
  
  p = ggplot(X, aes(x=y,color=Group))+ theme_bw()+
    stat_ecdf() +
    
    facet_grid(.~f) +
    xlab(xname)+ylab(yname)+ggtitle(TT)+
    theme(
          axis.text.x = element_text(angle = 90, hjust = 0),
          plot.title = element_text( colour = "blue",size=rel(1.4))
    )
  p
  
}


cdf_2_facet = function(D, xname, yname, facet_name_1, facet_name_2){
  library(dplyr)
  set.seed(123)
  if( !all(c(xname,yname,facet_name_1,facet_name_2) %in% names(D))) {stop("Name not in Data")}
  
  X = data.frame(Group= as.character(D[,xname]), y = D[,yname], 
                 f1 = as.character(D[,facet_name_1]), f2 = as.character(D[,facet_name_2]))
  

  TT = paste(facet_name_1, " > ", facet_name_2)
  p = ggplot(X, aes(color=Group,x=y))+theme_bw()+
    stat_ecdf() +
    
    facet_grid(.~f1+f2) +
    xlab(xname)+ylab(yname)+ggtitle(TT)+
    theme(
          axis.text.x = element_text(angle = 90, hjust = 0),
          plot.title = element_text( colour = "blue",size=rel(1.4))
    )
  p
}
#############


boxplot_0_facet = function(D, xname, yname, pvalue=0.05, theme=0,INDEX=rep(FALSE,nrow(D)), mode = 0){
  set.seed(123)
  if( !all(c(xname,yname) %in% names(D))) {stop("Name not in Data")}
  
  x = as.character(D[,xname])
  y = D[,yname]
  
  X = data.frame(x= x, y = y)
  p = robust_anova_p(x,y)
  
#  ann_text = data.frame(xx=1.5, yy=max(y,na.rm = TRUE), p = p, Label=paste("P:",round(p,3)),isSig = p<pvalue)
  ann_text = data.frame(xx=-Inf, yy=Inf, p = p, Label=paste("P:",round(p,3)),isSig = p<pvalue)
  
  print(ann_text)
  
#   p = ggplot(X, aes(x=x,y=y))+ geom_boxplot(outlier.shape = NA) +theme_bw()+
#     geom_jitter(width=0.3,aes(color=INDEX)) + 
#     geom_text(data = ann_text,aes(label =Label,x=xx,y=yy,color=isSig))+
#     scale_color_manual(values =c('FALSE'='black', 'TRUE'='red'),guide="none")+
#     xlab(xname)+ylab(yname)+
#     theme(legend.position="none",
#           axis.text.x = element_text(angle = 90, hjust = 0),
#           plot.title = element_text( colour = "blue",size=rel(1.4))
#     )
  p = ggplot(D, aes_string(x=xname,y=yname))+ geom_boxplot(outlier.shape = NA) +theme_bw()+
#    geom_text(data = ann_text,aes(label =Label,x=xx,y=yy,color=isSig),hjust=0,vjust=1.5)+
    scale_color_manual(values =c('FALSE'='black', 'TRUE'='red'),guide="none")+
    xlab(xname)+ylab(yname)+
    theme(legend.position="none",
          axis.text.x = element_text(angle = 90, hjust = 0),
          plot.title = element_text( colour = "blue",size=rel(1.4))
    )
  if(mode==1){
    p = p + geom_jitter(width=0.3,aes(color=INDEX   )) +
      scale_color_manual(values =c('FALSE'='black', 'TRUE'='grey'),guide="none")
  } else {
    p = p + geom_jitter(width=0.3,aes(color=INDEX,size=INDEX, order=INDEX))
  }
  p
}

boxplot_1_facet = function(D, xname, yname, facet_name, pvalue=0.05, theme=0,INDEX=rep(FALSE,nrow(D)),mode=0){
  library(dplyr)
  set.seed(123)
  if( !all(c(xname,yname,facet_name) %in% names(D))) {stop("Name not in Data")}
  
  X = data.frame(x= as.character(D[,xname]), y = D[,yname], f = as.character(D[,facet_name]))
  
  print(head(X))
  ann_text = X %>% group_by(f) %>% dplyr::summarise(
#     xx=1.5,
#     yy=1,
    xx = -Inf,
    yy = Inf,
    p = robust_anova_p(x,y),
    Label = paste("P:",round(robust_anova_p(x,y),3)),
    isSig = robust_anova_p(x,y)< pvalue
  )
  print(ann_text)
  
  names(ann_text)[1] = facet_name
  
#  ann_text$yy = max(X$y,na.rm = TRUE)
  TT = paste(facet_name)
  p = ggplot(D, aes_string(x=xname,y=yname))+ geom_boxplot(outlier.shape = NA) +theme_bw()+
    facet_grid(reformulate(facet_name,response=".")) +
#    geom_text(data = ann_text,aes(label =Label,x=xx,y=yy,color=isSig),hjust=0,vjust=1.5)+
    scale_color_manual(values =c('FALSE'='black', 'TRUE'='red'),guide="none")+
    xlab(xname)+ylab(yname)+ggtitle(TT)+
    theme(legend.position="none",
          axis.text.x = element_text(angle = 90, hjust = 0),
          plot.title = element_text( colour = "blue",size=rel(1.4))
    )
  if(mode==1){
    p = p + geom_jitter(width=0.3,aes(color=INDEX)) +
      scale_color_manual(values =c('FALSE'='black', 'TRUE'='grey'),guide="none")
  } else {
    p = p + geom_jitter(width=0.3,aes(color=INDEX,size=INDEX))
  }
  p
  
}


boxplot_2_facet = function(D, xname, yname, facet_name_1, facet_name_2, pvalue=0.05, theme=0,INDEX=rep(FALSE,nrow(D)),mode=0){
  library(dplyr)
  set.seed(123)
  if( !all(c(xname,yname,facet_name_1,facet_name_2) %in% names(D))) {stop("Name not in Data")}
  
  X = data.frame(x= as.character(D[,xname]), y = D[,yname], 
                 f1 = as.character(D[,facet_name_1]), f2 = as.character(D[,facet_name_2]))
  
  ann_text = X %>% group_by(f1,f2) %>% dplyr::summarise(
#     xx=1.5,
#     yy=1,
    xx = -Inf,
    yy= Inf,
    p = robust_anova_p(x,y),
    Label = paste("P:",round(robust_anova_p(x,y),3)),
    isSig = robust_anova_p(x,y)< pvalue
  )
  
  names(ann_text)[1:2] = c(facet_name_1,facet_name_2)
#  ann_text$yy = max(X$y,na.rm = TRUE)
  TT = paste(facet_name_1, " > ", facet_name_2)
  p = ggplot(D, aes_string(x=xname,y=yname))+ geom_boxplot(outlier.shape = NA) +theme_bw()+
    facet_grid(reformulate(c(facet_name_1,facet_name_2),response=".")) +
#    geom_text(data = ann_text,aes(label =Label,x=xx,y=yy,color=isSig),hjust=0,vjust=1.5)+
    scale_color_manual(values =c('FALSE'='black', 'TRUE'='red'),guide="none")+
    xlab(xname)+ylab(yname)+ggtitle(TT)+
    theme(legend.position="none",
          axis.text.x = element_text(angle = 90, hjust = 0),
          plot.title = element_text( colour = "blue",size=rel(1.4))
    )
  if(mode==1){
    p = p + geom_jitter(width=0.3,aes(color=INDEX)) +
      scale_color_manual(values =c('FALSE'='black', 'TRUE'='grey'),guide="none")
  } else {
    p = p + geom_jitter(width=0.3,aes(color=INDEX,size=INDEX))
  }
  p 
}


scatter_0_facet = function(D, xname, yname, sigCor=0.5, ellipse=TRUE, theme=0,INDEX=rep(FALSE,nrow(D)), mode = 0){
  if( !all(c(xname,yname) %in% names(D))) {stop("Name not in Data")}
  
  x = D[,xname]
  y = D[,yname]
  
  X = data.frame(x= x, y = y)
  Cor = robust_corr(x,y)
  
  ann_text = data.frame(xx=-Inf, yy=-Inf, Cor = Cor, Label=paste("pho:",Cor),isSig = abs(Cor)>sigCor)
  
  p = ggplot(D, aes_string(x=xname,y=yname))+ theme_bw()+
    geom_text(data = ann_text,aes(label =Label,x=xx,y=yy,color=isSig),hjust=0,vjust=-1)+
    xlab(xname)+ylab(yname)+
    theme(legend.position="none",
          axis.text.x = element_text(angle = 90, hjust = 0),
          plot.title = element_text( colour = "blue",size=rel(1.4))
    )
  if(ellipse){
    p = p + stat_ellipse(type="norm", linetype=2,level = 0.99,color="red")
  }
  if(mode==1){
    p = p + geom_point(aes(color=INDEX)) +
      scale_color_manual(values =c('FALSE'='black', 'TRUE'='grey'),guide="none")
  } else {
    p = p + geom_point(aes(color=INDEX,size=INDEX))+
      scale_color_manual(values =c('FALSE'='black', 'TRUE'='red'),guide="none")
  }
  p 
}



scatter_1_facet = function(D, xname, yname, facet_name, sigCor=0.5, ellipse=TRUE, theme=0,INDEX=rep(FALSE,nrow(D)), mode = 0){
  library(dplyr)
  set.seed(123)
  if( !all(c(xname,yname,facet_name) %in% names(D))) {stop("Name not in Data")}
  
  X = data.frame(x= D[,xname], y = D[,yname], f = as.character(D[,facet_name]))
  
  ann_text = X %>% group_by(f) %>% dplyr::summarise(
    xx=-Inf,#quantile(x,0.05),
    yy=-Inf,#quantile(y,0.05),
    p = robust_corr(x,y),
    Label = paste("pho:",round(robust_corr(x,y),3)),
    isSig = abs(round(robust_corr(x,y),3)) > sigCor
  )
  
  names(ann_text)[1] = facet_name
  print(ann_text)
  
  
  TT = paste(facet_name)
  
  p = ggplot(D, aes_string(x=xname,y=yname))+ theme_bw()+
#    geom_point(aes(color=INDEX,size=INDEX)) +
    geom_text(data = ann_text,aes(label =Label,x=xx,y=yy,color=isSig),hjust=0,vjust=-1)+
    facet_grid(reformulate(facet_name,response=".")) +
    scale_color_manual(values =c('FALSE'='black', 'TRUE'='red'),guide="none")+
    xlab(xname)+ylab(yname)+ggtitle(TT)+
    theme(legend.position="none",
          axis.text.x = element_text(angle = 90, hjust = 0),
          plot.title = element_text( colour = "blue",size=rel(1.4))
    )
  if(ellipse){
    p = p + stat_ellipse(type="norm", linetype=2,level = 0.99,color="red")
  }
  if(mode==1){
    p = p + geom_point(aes(color=INDEX)) +
      scale_color_manual(values =c('FALSE'='black', 'TRUE'='grey'),guide="none")
  } else {
    p = p + geom_point(aes(color=INDEX,size=INDEX))+
      scale_color_manual(values =c('FALSE'='black', 'TRUE'='red'),guide="none")
  }
  p 
  
}



scatter_2_facet = function(D, xname, yname, facet_name_1, facet_name_2, sigCor=0.5, ellipse=TRUE, theme=0,INDEX=rep(FALSE,nrow(D)), mode = 0){
  library(dplyr)
  set.seed(123)
  if( !all(c(xname,yname,facet_name_1,facet_name_2) %in% names(D))) {stop("Name not in Data")}
  
  X = data.frame(x= D[,xname], y = D[,yname], 
                 f1 = as.character(D[,facet_name_1]), f2 = as.character(D[,facet_name_2]))
  
  ann_text = X %>% group_by(f1,f2) %>% dplyr::summarise(
    xx=-Inf,#quantile(x,0.05),
    yy=-Inf,#quantile(y,0.05),
    p = robust_corr(x,y),
    Label = paste("pho:",round(robust_corr(x,y),2)),
    isSig = abs(robust_corr(x,y))>sigCor
  )
  
  names(ann_text)[1:2] = c(facet_name_1,facet_name_2)
  
  TT = paste(facet_name_1, " > ", facet_name_2)
  p = ggplot(D, aes_string(x=xname,y=yname))+ theme_bw()+
#    geom_point(aes(color=INDEX,size=INDEX)) +
    facet_grid(reformulate(c(facet_name_1,facet_name_2),response=".")) +
    geom_text(data = ann_text,aes(label =Label,x=xx,y=yy,color=isSig),hjust=0,vjust=-1)+
    scale_color_manual(values =c('FALSE'='black', 'TRUE'='red'),guide="none")+
    xlab(xname)+ylab(yname)+ggtitle(TT)+
    theme(legend.position="none",
          axis.text.x = element_text(angle = 90, hjust = 0),
          plot.title = element_text( colour = "blue",size=rel(1.4))
    )
  
  if(ellipse){
    p = p + stat_ellipse(type="norm", linetype=2,level = 0.99,color="red")
  }
  if(mode==1){
    p = p + geom_point(aes(color=INDEX)) +
      scale_color_manual(values =c('FALSE'='black', 'TRUE'='grey'),guide="none")
  } else {
    p = p + geom_point(aes(color=INDEX,size=INDEX))+
      scale_color_manual(values =c('FALSE'='black', 'TRUE'='red'),guide="none")
  }
  p
}


extremeDetectIndex = function(X,n){
  MU = sapply(X,function(x) mean(x,na.rm = TRUE))
  SD = sapply(X,function(x) sd(x,na.rm = TRUE))
  UCL = MU + SD*n
  LCL = MU - SD*n
  
  R = data.frame(lapply(1:ncol(X), function(i) {
    
    X[,i] > UCL[i] | X[,i] <LCL[i]
    
  }))
  
  names(R) = names(X)
  R = do.call(OR,R)
  R[is.na(R)] = FALSE
  R
}


toChar = function(X,NAMES){
  NAME_1 = NAMES[NAMES %in% names(X)]
  NAME_2 = setdiff(NAMES,NAME_1)
  cat("------------\n")
  print(paste(NAME_1, "are in data and are converted to character"))
  print(paste(NAME_2, "are not in data"))
  
  for( i in NAME_1)    X[,i] =as.character(X[,i])
  
  X
}

# sapply(mtcars,class)
# X = toChar(mtcars, c("cyl","ppp"))
# sapply(X,class)



dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;font-size:x-small")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown",
    style="font-size:x-small;width:165px;"
#    style="font-size:small;width:135px"
    
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    #style = "height = 25px",
    #br(),
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
    
  )
}











print_summary = function(X,TT = ""){
  
  X = data.frame(
             Title = TT,
             Nrow = nrow(X), 
             Ncol = ncol(X), 
             has.all.na = sum(sapply(X,function(x) all(is.na(x))))>0,
             Numeric = sum(sapply(X,is.numeric)),
             Character = sum(sapply(X,is.character)),
             Factor = sum(sapply(X,is.factor))
  )
  print(X)
  cat("\n---------------\n")
}

robust_chisq_p = function(x1,x2){
  R =     
    tryCatch({
      chisq.test(x1,x2)$p.value
    }, warning = function(w){
      suppressWarnings({
        chisq.test(x1,x2)$p.value
      })
    },
    error = function(e){
      404
    })
  
  R = ifelse(is.null(R),404,R)
  round(R,4)  
}

robust_anova_p = function(x,y){
  
  R =     
    tryCatch({
    fit = aov(y ~ x)
    summary(fit)[[1]][[1,"Pr(>F)"]]
  }, warning = function(w){
    suppressWarnings({
      fit = aov(y ~ x)
      summary(fit)[[1]][[1,"Pr(>F)"]]
    })
  },
  error = function(e){
    404
  })
  
  R = ifelse(is.null(R),404,R)
  round(R,4)  
  
}

robust_corr = function(x,y){
  
  R =     
    tryCatch({
      cor(x,y,use = "complete.obs")
    }, warning = function(w){
      suppressWarnings({
        cor(x,y,use = "complete.obs")
      })
    },
    error = function(e){
      404
    })
  
  R = ifelse(is.null(R),404,R)
  round(R,3)  
  
}

robust_bartlett_p = function(x,y){
  
  R =     
    tryCatch({
      bartlett.test(y ~ x)$p.value
      
    }, warning = function(w){
      suppressWarnings({
        bartlett.test(y ~ x)$p.value
        
      })
    },
    error = function(e){
      404
    })
  
  round(R,3)  
  
}



