### require or.prev
predict.mypaper <- function(x,traindata=NULL,testdata= NULL, method=c("glm","svm","lasso","randomforest", "knn"), kernal="linear", OOB=T,k=25,logit=F, ...){
  
  
  
  
  corrected.c <- NULL
  
  calibration.auc <- NULL
  
  predicted_pro_2 <- NULL
  
  model.data <-x$model$GLM 
  model.formula <- model.data$formula
  
  if(method=="lasso" ){

    dep_var_level2 <- levels(x$data[,x$dep_var])[[2]]
    dep_var_0or1 <-  ifelse(traindata[,x$dep_var] == dep_var_level2,1,0 )
    ind_var_str <- paste(c(as.character(x$ind_var)), collapse = "+ ")
  
    fmla <- as.formula( paste( x$dep_var,"~", ind_var_str))
  }
  
  
  if(is.null(traindata)){
    if(method=="glm"){model <- x$model$GLM}
    if(method=="svm"){model <- x$model$SVM}
    if(method=="randomforest"){model <- x$model$RF}
    if(method=="lasso"){model <- x$model$lasso.glm
                        mod.matrix <-  x$model$model.matrix 
                        }
    if(method=="knn"){model <- x$model$KNN}
  }else{
  
  if(method=="glm"){ model <- glm(model.formula, data = traindata, family = binomial(), method = "brglmFit")}
  if(method=="svm"){model  <-e1071::svm(model.formula, data=traindata, probability=TRUE, scale=FALSE, 
                                        kernal="radial basis",gamma=0.125,  epsilon=0.125, cost=0.25 )
  }  #hyperparameter after tunning: kernal="radial basis",gamma=0.125,  epsilon=0.125, cost=0.25
    
  if(method=="randomforest"){ model<- randomForest::randomForest(model.formula, data=traindata, ntree = 500)}  
  if(method=="lasso"){
    

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
  testdata_rf <- testdata
  if(is.null(testdata)){testdata= x$data}

  #testdata = testdata[,c(ind_var, dep_var)]
    
  
  # above: prepare testdata-------------------------------------------
  #dep_varname <- as.name(dep_var) 
  
  
  dep_var_level2 <- levels(x$data[,dep_var])[[2]]
  
  dep_var_0or1 <-  ifelse(testdata[,dep_var] == dep_var_level2,1,0 )
  
  #data1<-mutate(data1, dep_var_0 = dep_var_0or1)
  
  if(method=="glm"){
   if(logit){ predicted_pro_2 <- predict(model,newdata= testdata, type = "link")
   }else{predicted_pro_2 <- predict(model,newdata= testdata, type = "response")}
  }
  if(method=="svm"){
    predicted_svm <- predict(model, newdata = testdata, probability=T)
    predicted_svm <- attr(predicted_svm, "probabilities")[,2]
    predicted_pro_2 <- predicted_svm
  }
  if(method=="randomforest"){
    if(is.null(testdata_rf)){
      if(OOB){
        message(paste("out-of-bag auc"))
        predicted_rf <- predict(model, type="prob")
      }else{
        message(paste("not out-of-bag but original auc"))
        predicted_rf <- predict(model, newdata= x$data,
                                type="prob")
      }
      
      
       
    }else{
      message(paste("not out-of-bag but original auc"))
    predicted_rf <- predict(model, newdata= testdata_rf, 
                            type="prob")
    
    }
    predicted_rf <- as.numeric(predicted_rf[,2])
    predicted_pro_2 <- predicted_rf
    }
  if(method=="lasso"){
    
    mod.matrix <-  model.matrix(fmla,  testdata)
    predicted_lasso <- predict(model, newx= mod.matrix ,type="response",s=model$lambda)
    predicted_lasso <- as.numeric(predicted_lasso)
    predicted_pro_2 <- predicted_lasso
  }
    
  if(method=="knn"){model <- kknn::kknn(x$model$model.formula, train = x$data, test = testdata, k=k)
         predicted_pro_2 <-  model$prob[,2]
      }  
    
    
    predicted <- data.frame( Predicted = predicted_pro_2, Observed = dep_var_0or1)                     
  
  
  
  class(predicted) <- c("mypaper", "data.frame")
  
  return(predicted)
}

#predglm <- predict.mypaper(model3.imp, traindata=model3.imp$data, method="glm")
#predsvm <- predict.mypaper(model3.imp, traindata=model3.imp$data, method="svm")
#predlasso <- predict.mypaper(model3.imp, traindata=model3.imp$data, method="lasso")
#data.test <- cbind(dataset_selected_imp,OBS=predglm$Observed, GLM=predglm$Predicted, SVM= predsvm$Predicted, lasso= predlasso$Predicted)


# tunning for the svm model
#d <- tune(svm, model1.imp$model$model.formula, data = model1.imp$data, ranges = list(gamma = 2^(-1:1), cost = 2^(-2:2), epsilon=2^(-2:2)),
 #         tunecontrol = tune.control(sampling = "bootstrap"), nboot=20)

#d <- tune(svm, model3.s$model$model.formula, data = model3.s$data, ranges = list(gamma = 2^(-2:2), cost = 2^(-2:2), epsilon=2^(-2:2)),
 #         tunecontrol = tune.control(sampling = "bootstrap"), nboot=20)
# tunning for the random forest model

#e <- tune.randomForest(model1.imp$model$model.formula, tunecontrol= tune.control(sampling = "bootstrap", nboot=20),  
 #                      data =model1.imp$data , nodesize = 1:5,
  #                     mtry = 2:5, ntree = 500)
# tunning for the knn model
#knn.boot <- e1071::tune.knn( x= model1.imp$model$model.matrix, y=model1.imp$data[,model1.imp$dep_var], k=1:100, tunecontrol= tune.control(sampling = "boot"))













