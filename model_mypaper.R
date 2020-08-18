
model <- function(x){ UseMethod("model")}

model.mypaper <- function(x, traindata=x$data, glm.method="brglmFit", lasso= T,...){
  
  ind_var= x$ind_var
  
  var_df_firsryrbully <- var_univar_analyses(ind_var= ind_var, y=x$dep_var, data_frame = x$data)
  
  sig_df <-  var_df_firsryrbully%>%filter(p_value<x$p.threshhold)
  sig_var <- sig_df$Var
  
  
  dep_var= x$dep_var

  sig_var_str <- paste(c(as.character(sig_var)
                         #,must.have.var #"firstrses_standardized"
  ), collapse = "+ ")
  sig_fmla <- as.formula( paste( dep_var,"~",sig_var_str))
  
  glm_model <-  glm(sig_fmla, family = binomial(),   method =  glm.method,
                    data = traindata)
  lrm_model <- lrm(sig_fmla, model=T, x=TRUE, y=TRUE, data =  traindata)
  
  svm_model <- e1071::svm(sig_fmla, data=traindata, probability=TRUE, scale=FALSE, 
                          kernal="radial basis", epsilon=0.5, cost=0.09 
                          )
  
  if(lasso){
    dep_var_level2 <- levels(traindata[,dep_var])[[2]]
    
    dep_var_0or1 <-  ifelse(traindata[,dep_var] == dep_var_level2,1,0 )
    
    ind_var_str <- paste(c(as.character(ind_var)
                           #,must.have.var #"firstrses_standardized"
    ), collapse = "+ ")
    fmla <- as.formula( paste( dep_var,"~", ind_var_str))
    message(paste(fmla))
    
    mod.matrix <- model.matrix(fmla, traindata)
    
    cv.glm.out <- cv.glmnet(x=mod.matrix, y= dep_var_0or1,
                            type.measure = "default")
    
    glm.out <- glmnet(x=mod.matrix, y= dep_var_0or1, family = "binomial", alpha = 1, lambda = cv.glm.out$lambda.1se)
    
    
    
    model.ls <- list(GLM=glm_model, LRM= lrm_model, SVM=svm_model, model.formula= sig_fmla, 
                     lasso.glm=glm.out, model.matrix=mod.matrix)
    
  }else{
    model.ls <- list(GLM=glm_model, LRM= lrm_model, SVM=svm_model, model.formula= sig_fmla)
    
  }
  
}


