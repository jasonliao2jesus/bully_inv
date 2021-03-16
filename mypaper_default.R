library(esc)
library(tidyverse)
library(brglm2)
library("rms")
library(ggpubr)
library("pROC")
library(caret)
library(mice)
library(glmnet)
library(e1071)
library(randomForest)
library(kknn)
library(mgcv)
library(rms)
library(Information)

## dependent variables should not be ordered.
# model1.imp$data$

sourced_files <- c("model_mypaper.R", "plot_mypaper.R","predicted_mypaper.R","creat_unadj_OR_df.R", "creat_adj_OR_mypaper.R")


for(i in sourced_files){
  do.call(source,list(i))
}


mypaper <- function(...) UseMethod("mypaper" )




# select independent variables+ dependent variables
mypaper.default<- function(ind_var, dep_var="firstyrbully2",additional_var=NULL,  data=dataset, 
                         imputation.mis.data=F, 
                         span=1, group=12, plot=T,
                         validate.boot40= T, val.B=40, confusion.matrix= T, cut.off=0.5,
                         p.threshhold= 0.05, correct= F )
{
# 檢查挑選變數是否存在於data
  allvars <- c(ind_var, dep_var, additional_var)
  
  existvars <-   allvars %in% colnames(data)
  absentvars <- !existvars 
  if( any(absentvars)
  ){
    
    stop(paste( allvars[absentvars],": absent in data."))
    
  }
  
data = data[,allvars]

##  exclude missing data or imputation


var_na <- NULL
prop.na.v <- NULL

for (i in ind_var ){
  if(any(is.na(data[,i]))){
    
    var_na <- c(var_na,i)
    prop.na <- sum(is.na(data[,i]))/length(data[,i])
    prop.na.v <- c(prop.na.v, prop.na)
    #      data <- data[!is.na(data[,i]),]
    
  }
  
}

if(is.null(var_na)){
  print("no missing data")
} else{
  print(paste("missing data in column of",
              paste(var_na, collapse = "& "), 
              "with ratio of",
              paste( round(prop.na.v,digits = 3)*100, "%",collapse = "& " ),
              sep = " ")
  )
}



if(imputation.mis.data){
  
  tempData <- mice(data, m=5)
  data <- mice::complete(tempData)
  print("missing data imputed!!")
  
}else{
  print("cast rows with missing data away!!")
  for(i in var_na){
    data <- data[!is.na(data[,i]),]
  }
  

  
}


mypaper.arg <- list(ind_var=ind_var, dep_var=dep_var, data=data, 
                    imputation.mis.data=imputation.mis.data, 
                    span=span, group=group, plot=plot,
                    validate.boot40= validate.boot40, val.B=val.B, confusion.matrix= confusion.matrix, cut.off=cut.off,
                    p.threshhold= p.threshhold,correct= correct  )


class(mypaper.arg) <- c("mypaper", "list")

model <- model.mypaper(mypaper.arg, lasso = T)

mypaper.arg <- list(ind_var=ind_var, dep_var=dep_var, data=data, 
                    imputation.mis.data=imputation.mis.data, 
                    span=span, group=group, plot=plot,
                    validate.boot40= validate.boot40, val.B=val.B, confusion.matrix= confusion.matrix, cut.off=cut.off,
                    p.threshhold= p.threshhold,correct= correct, model=model )

class(mypaper.arg) <- c("mypaper", "list")

return(mypaper.arg)

}

