

model <- function(x){ UseMethod("model")}

model.mypaper <- function(x, traindata=x$data, ind_var=NULL, dep_var=NULL, #glm.method="brglmFit", 
                          lasso= T, selected_ind_var=NULL, alpha= 1){
  
  
  if(is.null(dep_var)){dep_var= x$dep_var}
  if(is.null(ind_var)){ind_var= x$ind_var}
  
  var_str <- paste(c(as.character(ind_var)), collapse = "+ ")
  global_fmla <- as.formula( paste( dep_var,"~",var_str))
  
  var_df_firsryrbully <- var_univar_analyses(ind_var= ind_var, y=dep_var, data_frame = x$data)
  
  sig_df <-  var_df_firsryrbully%>%dplyr::filter(p_value<x$p.threshhold)
  sig_var <- sig_df$Var
  sig_var <-unique(sig_var) 
  
  
  
  
  sig_var_str <- paste(c(as.character(sig_var)
                         #,must.have.var #"firstrses_standardized"
  ), collapse = "+ ")
  sig_fmla <- as.formula( paste( dep_var,"~",sig_var_str))
  
  
  global_model <-  glm(global_fmla, family = binomial(),  # method =  glm.method,
                       data = traindata, x=T, y=T)
  
  glm_model <-  glm(sig_fmla, family = binomial(),  # method =  glm.method,
                    data = traindata)
  lrm_model <- lrm(sig_fmla, model=T, x=TRUE, y=TRUE, data =  traindata)
  
  svm_model <- e1071::svm(sig_fmla, data=traindata, probability=TRUE, scale=FALSE, 
                          kernal="radial basis",gamma=0.125, epsilon=0.125, cost= 0.25
                          )
  rf_model <- randomForest::randomForest(sig_fmla, data=traindata, ntree=500, nodesize=3, mtry=2 )
  
  knn_model <- kknn::kknn(sig_fmla, train = traindata, test = traindata, k=25)
 # knn_model <- kknn::kknn(sig_fmla,  train = traindata, test = traindata)
  
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
    
    glm.out <- glmnet(x=mod.matrix, y= dep_var_0or1, family = "binomial", alpha = alpha, lambda = cv.glm.out$lambda.1se)
    
    
    
    model.ls <- list(global_model=global_model, GLM=glm_model, LRM= lrm_model, SVM=svm_model,RF= rf_model,KNN= knn_model, model.formula= sig_fmla, 
                     lasso.glm=glm.out, model.matrix=mod.matrix)
    
  }else{
    model.ls <- list(global_model=global_model, GLM=glm_model, LRM= lrm_model, SVM=svm_model,RF= rf_model,KNN= knn_model, model.formula= sig_fmla)
    
  }
  
}

# tunning for the svm model
#d <- tune(svm, model1.imp$model$model.formula, data = model1.imp$data, ranges = list(gamma = 2^(-1:1), cost = 2^(-2:2), epsilon=2^(-2:2)),
 #         tunecontrol = tune.control(sampling = "bootstrap"), nboot=20)

# tunning for the random forest model

#e <- tune.randomForest(model1.imp$model$model.formula, tunecontrol= tune.control(sampling = "bootstrap", nboot=20),  
 #                      data =model1.imp$data , nodesize = 1:5,
  #                     mtry = 2:5, ntree = 500)
# tunning for the knn model
#knn.boot <- e1071::tune.knn( x= model1.imp$model$model.matrix, y=model1.imp$data[,model1.imp$dep_var], k=1:100, tunecontrol= tune.control(sampling = "boot"))


