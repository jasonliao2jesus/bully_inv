sourced_files <- c("creat_adj_OR_df.R","creat_unadj_OR_df.R")


for(i in sourced_files){
do.call(source,list(i))
}

creat_table2 <- function(ind_var, dep_var="firstyrbully2", data=dataset, 
                         imputation.mis.data=F, 
                         span=1, group=12, plot=T,
                         validate.boot40= T, val.B=40, confusion.matrix= T, cut.off=0.5,
                          p.threshhold= 0.05 )
{ 
  # select independent variables+ dependent variables
  

  data = data[,c(ind_var, dep_var)]
  
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
  
  
  ##
  
  
  dep_var_arg_unadj <- list(ind_var, dep_var ,data)
  dep_var_arg_adj <- list(ind_var, dep_var ,data 
                         # span=span, group=group, plot = plot, 
                        #  confusion.matrix=confusion.matrix, cut.off = cut.off,
                        #  p.threshhold=p.threshhold, 
                         # validate.boot40 = validate.boot40, val.B= val.B
                          #boot.cal.plot= F
                          )
  adj_OR_dep_var <- do.call(creat_adj_OR_df, dep_var_arg_adj)
  unadj_OR_dep_var<- do.call(creat_unadj_OR_df,dep_var_arg_unadj)
  OR_dep_var_arg <- list(unadj_OR_dep_var,adj_OR_dep_var,by="Var_level")
  OR2_dep_var <-do.call(dplyr::full_join, OR_dep_var_arg)
  return(OR2_dep_var)
}

