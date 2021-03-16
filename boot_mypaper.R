


auc.mypaper <- function(traindata,testdata,  method="glm",x=model1.imp,plot=F,OOB=F, k=25,suppressMessages=T, ...){
  
  
  #%>%filter(!is.na(q5_a)))
  
#  if(method=="randomforest"){
    #testdata <- ifelse(OOB, NULL, testdata)
    predicted <- predict.mypaper(x, traindata, testdata = testdata , correct=F, method = method, OOB = OOB) 
   
 # }else{
  #  predicted <- predict.mypaper(x, traindata, testdata = testdata, correct=F, method = method, k=k,OOB) 
  #}
  
  
  y <-  predicted$Observed
  p <- predicted$Predicted
  
#  data <- cbind(y=y, p=p )
  
  
  
  
  
 roc.test <- roc(y, p, plot = plot, quiet = suppressMessages)
  auc.test <- roc.test$auc
  auc.95CI <- ci.auc(roc.test, method= "delong")
  auc.95CI <- unclass(auc.95CI)
  
  
  
  loess.calibrate <- loess(y~p )
  
  P.calibrate <- predict(loess.calibrate, newdata=p)
  ICI <- mean(abs(P.calibrate-p))
  E50 <- median(abs(P.calibrate-p))
  E90 <- quantile(abs(P.calibrate-p), probs = 0.9)
  Emax <- max(abs(P.calibrate-p))
  cal.in <- c(ICI=ICI, E50=E50, E90=E90, Emax=Emax)
  
  # discrimiation:AUC and its confidence interval   
  
  
  
  # discrimiation:AUC and its confidence interval   
  #roc.test <- roc(y, p, plot = F)
  if(plot){ plot(roc.test)}
  auc.test <- roc.test$auc
  
  calibration.auc <- c(   cal.in, 
                          AUC=auc.test, AUC.5CI=auc.95CI[1], AUC.95CI=auc.95CI[3])
  
  return(calibration.auc)
  
  
} 



boot2.mypaper <- function(data = model1.s$data, method=c("glm","lasso", "svm","randomforest"),func=auc.mypaper, model = model1.s, OOB = F, R = 40)
{
  method=match.arg(method)
  l <- list()
  tests <- c(0)
  apps <- c(0)
  for(i in 1:R){
    #traindata <-bootsamp[,i] 
    obs.boot <- sample(x = 1:nrow(data), size = nrow(data), replace = T)
    
    traindata <- data[obs.boot, ]
    #testdata <- notin.traindata
    # fit the model on bootstrap sample
    arg.test <- list( traindata=traindata, testdata=data, method=method, x=model)
    arg.app <-  list(traindata=traindata, testdata=traindata, method=method, x=model)
    
    test <- do.call(func, arg.test)
    app <- do.call(func, arg.app)
    #pred <- as.data.frame(pred)
    tests <- tests+test
    # apply model to original data
    apps <- apps+app
    #cat('Processing the', x, 'th sample of', R,'\n')
    svMisc::progress(100*i/R)
  }
   
    optimism= (apps-tests)/R
  
     names(optimism) <- c("ICI","E50","E90","Emax", "AUC","AUC5%CI","AUC95%CI")
  
  #corrected <- apply(l, MARGIN = 2, mean)
  # apply model to bootstrap data
  # arg1 <- list(model=model, data=data, indices= obs.boot, method=method)
  #pred <- do.call(func, arg)
  
  #prob2 = predict(logit.boot, type='response', data.boot)
  #pred2 = prediction(prob2, data.boot$credit)
  #auc2 = performance(pred2,"auc")@y.values[[1]][1]
  #B[i, 2] = auc2
  
  if(method=="randomforest"){
  original <- auc.mypaper(traindata= NULL,  testdata = NULL,   method=method,x=model, OOB = OOB)
    
  }else{  original <- auc.mypaper(traindata=NULL, testdata=data,  method=method,x=model)
  }
  
  corrected= original-optimism
  
  calib <- data.frame( original=original,corrected=corrected, optimism=optimism, apparent=apps/R, average.test=tests/R)
  writeLines("\n")
  return(calib)
}



boot632.mypaper <- function(model=model3.imp,data=model3.imp$data, func=auc.mypaper, R=40, 
                            method= c("glm", "svm", "lasso","randomforest","knn"),k=25,
                            OOB=T)
{
  method=match.arg(method)

  df.test <- data.frame()

  l <- list()
  
  for(i in 1:R){
    obs.boot <- sample(x = 1:nrow(data), size = nrow(data), replace = T)
    in.obs.boot <- 1:nrow(data) %in% obs.boot
    notin.obs.boot <-  !in.obs.boot
    notin.traindata <- data[notin.obs.boot,]
    
    traindata <- data[obs.boot, ]
    testdata <- notin.traindata
    # fit the model on bootstrap sample
    arg.test <- list(  x=model, traindata=traindata, testdata=testdata, method=method, k=k)
    
    pred.test <- do.call(func, arg.test)
   

    df.test <- rbind(df.test, pred.test)
    svMisc::progress(i)
    #pred <- as.data.frame(pred)
    
    # apply model to original data
    
    
    
  }
  df.test <<-df.test 
  if(method=="randomforest"){
    original <- auc.mypaper(traindata= data,  testdata = NULL,   method=method,x=model, OOB = OOB, k=k)
    
  }else{  original <- auc.mypaper(traindata=data, testdata=data,  method=method,x=model, k=k)
  }
  
  
  l[["test"]] <- df.test
  test <- apply(l[["test"]], MARGIN = 2, mean)
  corrected <- 0.368*original+ 0.632*test
  
  
  calib <- data.frame( original=original, test=test, corrected= corrected)
 # calib <- mutate(data=calib,   corrected= original-optimism)
  rownames(calib) <- c("ICI","E50","E90","Emax", "AUC","AUC5%CI","AUC95%CI")
  writeLines("\n")
  return(calib)
}



auc.glm <- auc.mypaper(traindata = model1$data, testdata= dataset_sec_yr_testing_imp, method="glm", x= model1)
auc.svm <- auc.mypaper(traindata = model1$data, testdata= dataset_sec_yr_testing_imp, method="svm", x= model1)
auc.rf <- auc.mypaper(traindata = model1$data, testdata= dataset_sec_yr_testing_imp, method="randomforest", x= model1)
auc.lasso <- auc.mypaper(traindata = model1$data, testdata= dataset_sec_yr_testing_imp, method="lasso", x= model1)

sum.auc <- data.frame(glm=auc.glm, )




## GAM apparent calibration plot



##GAM bootstrap calibration plot, 


bootplot.mypaper <- function(model=model3.imp,data=model3.imp$data, R=40, 
                            method= c("glm", "svm", "lasso","randomforest","knn"),
                            OOB=T)
  {
  
  
  method=match.arg(method)
  
  #df.test <- data.frame()
  sy.opt <- c(0)
  #l  <- list()
  b <- data.frame(Predicted=seq(0.01,0.99, length.out = 50))
  for(i in 1:R){
    
    obs.boot <- sample(x = 1:nrow(data), size = nrow(data), replace = T)
    #in.obs.boot <- 1:nrow(data) %in% obs.boot
    #notin.obs.boot <-  !in.obs.boot
   # notin.traindata <- data[notin.obs.boot,]
    
    traindata <- data[obs.boot, ]
    
    #testdata <- notin.traindata
    # fit the model on bootstrap sample
    pred.app <- predict.mypaper(model, traindata=traindata, testdata=data, method=method)
    pred.boot <- predict.mypaper(model, traindata=traindata, testdata=traindata, method=method)
    
    
    gammod.app <- gam(formula = Observed ~ s(Predicted) ,data = pred.app,family=binomial())
    gammod.boot <- gam(formula = Observed ~ s(Predicted) ,data = pred.boot, family=binomial() )
    
    y.app <- predict.gam(gammod.app,b, type="response"  )
    y.boot <- predict.gam(gammod.boot,b, type="response"  )
  
    y.opt <- y.boot- y.app
    
    sy.opt <- sy.opt+y.opt
    
 
   svMisc::progress(100*i/R)
   # cat('Processing the', x, 'th sample of', R,'\n')
    
  }
  optimism <- sy.opt/R
  #optimism <<-optimism 
  
  
  # original calibration curve

   if(method=="randomforest"){
     if(OOB){
     pred.orig <- predict.mypaper(model, traindata = model$data, testdata = NULL, method=method, OOB = OOB)
     }else
       pred.orig <- predict.mypaper(model, traindata = model$data, testdata = model$data, 
                                    method=method, OOB = OOB)
  }else{  
     pred.orig <- predict.mypaper(model, traindata = model$data, testdata = model$data, method=method, OOB = OOB)
     
  }

  gammod.orig <- gam(formula = Observed ~ s(Predicted) ,data = pred.orig, family =  binomial() )
  

  y.orig <- predict.gam(gammod.orig,b, type="response"  )
  mse <-  round( sum((y.orig-b)^2)/50, digits = 4)
  mae <- round(  sum(abs(y.orig-b))/50, digits = 4)
  
  d <- cbind(b, original=y.orig, correct=y.orig-optimism)
  plot1 <-  plot_calibration_c(data1=pred.orig, plottitle = "A",#%>%filter(!is.na(q5_a)), #  如果model_1含q5_a, 要把dataset_selected 中q5_a==NA去掉 
                               #  因為model也去掉了q5_a==NA...
                               cOutcome=2,  # firstyrbully_1or0 是 dataset_selected[,3] 
                               predRisk=pred.orig$Predicted,
                               groups=model$group, rangeaxis=c(0,1), span=model$span)
  cal.plot <- plot1+
    geom_smooth(data=d, aes(x=Predicted, y=original), col="blue", method = "gam", se=FALSE)+
    geom_smooth(data=d,aes(x=Predicted, y=correct), col="red", method = "gam", se= FALSE)+
    #geom_smooth(data= pred.orig, aes(x=Predicted, y=Observed),col="blue", se=TRUE)+
    geom_rug(data=pred.orig,  aes(x= Predicted), stat = "identity",
             position = "identity",alpha=0.2, sides = "t")+ 
    annotate("text", x = 0.2, y = 0.8, label = paste("Mean squared error =", mse,"\nMean absolute error =", mae))+
    annotate("segment", x=0, xend=1, y=0, yend=1)+ ylim(c(0,1))
     
  
  return(cal.plot)
     

}


###




bootcoef.mypaper <- function(model=model3.imp,data=model3.imp$data, R=40, 
                            method= "glm")
{
  
  
  df.test <- data.frame()
  
  l <- list()
  ind_var <-  model$ind_var
  dep_var <- model$dep_var
  
  ind_var_str <- paste(c(as.character(ind_var)), collapse = "+ ")
  fmla <- as.formula( paste( dep_var,"~", ind_var_str))
  
  for(i in 1:R){
    obs.boot <- sample(x = 1:nrow(data), size = nrow(data), replace = T)
    
    traindata <- data[obs.boot, ]
   
    # fit the model on bootstrap sample
    
    
    
   
    
    df.test <- rbind(df.test, pred.test)
    svMisc::progress(i)
    #pred <- as.data.frame(pred)
    
    # apply model to original data
    
    
    
  }
  df.test <<-df.test 
  if(method=="randomforest"){
    original <- auc.mypaper(traindata= data,  testdata = NULL,   method=method,x=model, OOB = OOB, k=k)
    
  }else{  original <- auc.mypaper(traindata=data, testdata=data,  method=method,x=model, k=k)
  }
  
  
  l[["test"]] <- df.test
  test <- apply(l[["test"]], MARGIN = 2, mean)
  corrected <- 0.368*original+ 0.632*test
  
  
  calib <- data.frame( original=original, test=test, corrected= corrected)
  # calib <- mutate(data=calib,   corrected= original-optimism)
  rownames(calib) <- c("ICI","E50","E90","Emax", "AUC","AUC5%CI","AUC95%CI")
  writeLines("\n")
  return(calib)
}
