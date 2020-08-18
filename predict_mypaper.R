### require or.prev
predict.mypaper <- function(x,traindata=NULL,testdata= NULL, method=c("glm","svm","lasso"), ...){
  
  
  
  
  corrected.c <- NULL
  
  calibration.auc <- NULL
  
  predicted_pro_2 <- NULL
  
  model.data <-x$model$GLM 
  model.formula <- model.data$formula
  if(is.null(traindata)){
    if(method=="glm"){model <- x$model$GLM}
    if(method=="svm"){model <- x$model$SVM}
    if(method=="lasso"){model <- x$model$lasso.glm
                        mod.matrix <-  x$model$model.matrix 
             }
  }else{
  
  if(method=="glm"){ model <- glm(model.formula, data = traindata, family = binomial(), method = "brglmFit")}
  if(method=="svm"){model  <-e1071::svm(model.formula, data=traindata, probability=TRUE, scale=FALSE, 
                                        kernal="radial basis", epsilon=0.5, cost=0.09 )
                    }
  if(method=="lasso"){
    
    
    dep_var_level2 <- levels(x$data[,x$dep_var])[[2]]
    
    dep_var_0or1 <-  ifelse(traindata[,x$dep_var] == dep_var_level2,1,0 )
    
    ind_var_str <- paste(c(as.character(x$ind_var)
                           #,must.have.var #"firstrses_standardized"
    ), collapse = "+ ")
    fmla <- as.formula( paste( x$dep_var,"~", ind_var_str))
    
    
    mod.matrix <- model.matrix(fmla,  traindata)
  
    cv.glm.out <- cv.glmnet(x=mod.matrix, y= dep_var_0or1,
                            weights = NULL, offset = NULL, lambda = NULL,
                            type.measure = "auc",
                            nfolds = 10, foldid = NULL, alignment = c("lambda","fraction"), grouped = TRUE, keep = FALSE, parallel = FALSE,
                            gamma = c(0, 0.25, 0.5, 0.75, 1), relax = FALSE, trace.it = 0)
    
    model <- glmnet(x=mod.matrix, y= dep_var_0or1, family = "binomial", alpha = 1, lambda = cv.glm.out$lambda.1se)
    }
    
  
  }
 
  
  
  # below: prepare testdata--------------------   
  dep_var <- x$dep_var
  ind_var <- x$ind_var

  if(is.null(testdata)){testdata= x$data}
  
  #testdata = testdata[,c(ind_var, dep_var)]
    
  
  # above: prepare testdata-------------------------------------------
  #dep_varname <- as.name(dep_var) 
  
  
  dep_var_level2 <- levels(x$data[,dep_var])[[2]]
  
  dep_var_0or1 <-  ifelse(testdata[,dep_var] == dep_var_level2,1,0 )
  
  #data1<-mutate(data1, dep_var_0 = dep_var_0or1)
  
  if(method=="glm"){
    predicted_pro_2 <- predict(model,newdata= testdata, type = "response") 
  }
  if(method=="svm"){
    predicted_svm <- predict(model, newdata = testdata, probability=T)
    predicted_svm <- attr(predicted_svm, "probabilities")[,2]
    predicted_pro_2 <- predicted_svm
  }
  if(method=="lasso"){
    
    mod.matrix <-  model.matrix(fmla,  testdata)
    predicted_lasso <- predict(model, newx= mod.matrix ,type="response",s=model$lambda)
    predicted_lasso <- as.numeric(predicted_lasso)
    predicted_pro_2 <- predicted_lasso
  }
    
    
    
    
    predicted <- data.frame( Predicted = predicted_pro_2, Observed = dep_var_0or1)                     
  
  
  
  class(predicted) <- c("mypaper", "data.frame")
  
  return(predicted)
}
