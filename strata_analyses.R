

strata.mypaper <- function(model=model3.s, testdata=model3.s$data, method="glm", cutoff=c(0.1, 0.5))
{

data0 <- dataset



pred <- predict.mypaper(model, NULL, testdata=testdata,method=method)
predcut <- cut2(pred$Predicted, cutoff)
testdata$predcut <- predcut
cali <- auc1.mypaper(traindata = NULL, method=method, x=model, testdata = testdata)

strata_results <- list()


for (i in levels(testdata$predcut))
        {
data <-testdata%>%filter(predcut==i)

calimed <- auc1.mypaper(traindata = model$data, method=method, x=model, testdata = data)

strata_results[[i]] <-round(calimed, digits = 3)
 
}

strata_table <- table(testdata$predcut, testdata[,model$dep_var])

strata_results[["table"]] <- strata_table
return(strata_results)
} 

stratalist.mypaper <- function(model=model3.s, traindata=NULL, testdata=model3.s$data, cutoff=c(0.1, 0.5))
{
      glm <- stracal.mypaper(traindata=NULL,x=model, testdata=testdata, method="glm", cutoff=cutoff)
              
      lasso <- stracal.mypaper(traindata=NULL,x=model, testdata=testdata, method="lasso", cutoff=cutoff)
      
      svm  <-stracal.mypaper(traindata=NULL,x=model, testdata=testdata, method="svm", cutoff=cutoff)
      
      randomforest  <-stracal.mypaper(traindata=NULL,x=model, testdata=testdata, method="randomforest", cutoff=cutoff)
      
    lowrisk <- cbind(glm=glm[[1]], lasso=lasso[[1]], svm=svm[[1]], 
                     RF=randomforest[[1]])
    mediumrisk <- cbind(glm=glm[[2]], lasso=lasso[[2]], svm=svm[[2]], 
                        RF=randomforest[[2]])
    highrisk <- cbind(glm=glm[[3]], lasso=lasso[[3]], svm=svm[[3]], 
                      RF= randomforest[[3]])
    total <- cbind(glm=glm[[4]], lasso=lasso[[4]], svm=svm[[4]], 
                   RF= randomforest[[4]])
    lowrisk <- round(lowrisk, digits = 3)
    mediumrisk <- round(mediumrisk, digits = 3)
    highrisk <- round(highrisk, digits = 3)
    total <- round(total, digits = 3)
    
    stratalist <- list(lowrisk=lowrisk, mediumrisk=mediumrisk, highrisk=highrisk, total=total)
      
}





#strata <- t(as.data.frame.list(strata_results))[,1:4]
#rownames(strata) <- levels(testdata$predcut)
#strata_df <- data.frame()
#colnames(strata_df) <- names(cali)
#for(j in  names(strata_results))
#{       
# strata_df <- rbind( strata_df, strata_results[[i]])
#}
#strata_df <- rbind(strata_df, cali)

 
 meth="randomforest"
 cutoff=c(0.1, 0.5)
 pred <- predict.mypaper(model1.s, traindata=model1.s$data, method=meth)
 predcut <- cut2(pred3$Predicted, cutoff)
 data0 <- dataset
 data0$predcut <- predcut
 datalow <-data0%>%filter(predcut==levels(predcut)[1])
 datamed <- data0%>%filter(predcut==levels(predcut)[2])
 datahigh <- data0%>%filter(predcut==levels(predcut)[3])
 
 calimed <- auc.mypaper(traindata = model1.s$data, method=meth, x=model1.s, testdata = datamed)
 calihigh <- auc.mypaper(traindata = model1.s$data, method=meth, x=model1.s, testdata = datahigh)
 calilow <- auc.mypaper(traindata = model1.s$data, method=meth, x=model1.s, testdata = datalow)
 rbind(calilow, calimed, calihigh)
 
 
 
 
 
 auc1.mypaper <- function(traindata,testdata,  method="glm",x=model1.imp,plot=F,OOB=F, k=25,suppressMessages=T, ...){
   
   
   #%>%filter(!is.na(q5_a)))
   
   #  if(method=="randomforest"){
   #testdata <- ifelse(OOB, NULL, testdata)
   predicted <- predict.mypaper(x, traindata, testdata = testdata , correct=F, method = method, OOB = OOB) 
   
   # }else{
   #  predicted <- predict.mypaper(x, traindata, testdata = testdata, correct=F, method = method, k=k,OOB) 
   #}
   
   
   y <-  predicted$Observed
   p <- predicted$Predicted
 
   loess.calibrate <- loess(y~p )
   
   P.calibrate <- predict(loess.calibrate, newdata=p)
   ICI <- mean(abs(P.calibrate-p))
   E50 <- median(abs(P.calibrate-p))
   E90 <- quantile(abs(P.calibrate-p), probs = 0.9)
   Emax <- max(abs(P.calibrate-p))
   cal.in <- c(ICI=ICI, E50=E50, E90=E90, Emax=Emax)
   
   
   
   
   
   
   
   return(cal.in)
   
   
 } 
 
 
 
 
 stracal.mypaper <- function(traindata,testdata,  method="glm",x=model1.imp,cutoff= c(0.1, 0.5),...){
         
         
         #%>%filter(!is.na(q5_a)))
         
         #  if(method=="randomforest"){
         #testdata <- ifelse(OOB, NULL, testdata)
         predicted <- predict.mypaper(x, traindata, testdata = testdata , correct=F, method = method, OOB = F) 
         
         # }else{
         #  predicted <- predict.mypaper(x, traindata, testdata = testdata, correct=F, method = method, k=k,OOB) 
         #}
         
         
         y <-  predicted$Observed
         p <- predicted$Predicted
         
         roc.test <- roc(y, p, plot = F, quiet = T)
         auc.test <- roc.test$auc
         auc.95CI <- ci.auc(roc.test, method= "delong")
         auc.95CI <- unclass(auc.95CI)
         
         
         loess.calibrate <- loess(y~p )
         
         P.calibrate <- predict(loess.calibrate, newdata=p)
         ICI <- mean(abs(P.calibrate-p))
         E50 <- median(abs(P.calibrate-p))
         E90 <- quantile(abs(P.calibrate-p), probs = 0.9)
         Emax <- max(abs(P.calibrate-p))
         total.cal <- c(ICI=ICI, E50=E50, E90=E90, Emax=Emax, AUC=auc.test, AUC.5CI=auc.95CI[1], AUC.95CI=auc.95CI[3])
         
         
         
         predcut <- cut2(p, cutoff)
          p_data <- data.frame( p=p, predcut=predcut )
          
          stra_cal <- list()
          
         for(i in levels(p_data$predcut)){
           stradata=p_data%>%filter(predcut==i)
           strap <- stradata$p
         
           
           P.calibrate <- predict(loess.calibrate, newdata=strap)
           ICI <- mean(abs(P.calibrate-strap))
           E50 <- median(abs(P.calibrate-strap))
           E90 <- quantile(abs(P.calibrate-strap), probs = 0.9)
           Emax <- max(abs(P.calibrate-strap))
           cal.in <- c(ICI=ICI, E50=E50, E90=E90, Emax=Emax)
          stra_cal[[i]] <- cal.in  
           
         }
        
          stra_cal[["total"]] <- total.cal
         
         
 
         
         
         return(stra_cal)
         
         
 } 
 
 stratalist1.mypaper <- function(model=model3.s, traindata=NULL, testdata=model3.s$data, cutoff=c(0.1, 0.5))
 {
   glm <- stracal.mypaper(traindata=NULL,x=model, testdata=testdata, method="glm", cutoff=cutoff)
   
   lasso <- stracal.mypaper(traindata=NULL,x=model, testdata=testdata, method="lasso", cutoff=cutoff)
   
   svm  <-stracal.mypaper(traindata=NULL,x=model, testdata=testdata, method="svm", cutoff=cutoff)
   
   randomforest  <-stracal.mypaper(traindata=NULL,x=model, testdata=testdata, method="randomforest", cutoff=cutoff)
   
   lowrisk <- cbind(glm=glm[[1]], #svm=svm[[1]], 
                    RF=randomforest[[1]])
   mediumrisk <- cbind(glm=glm[[2]], lasso=lasso[[1]], svm=svm[[1]], 
                       RF=randomforest[[2]])
   highrisk <- cbind(glm=glm[[3]], lasso=lasso[[2]], svm=svm[[2]], 
                     RF= randomforest[[3]])
   total <- cbind(glm=glm[[4]], lasso=lasso[[3]], svm=svm[[3]], 
                  RF= randomforest[[4]])
   lowrisk <- round(lowrisk, digits = 3)
   mediumrisk <- round(mediumrisk, digits = 3)
   highrisk <- round(highrisk, digits = 3)
   total <- round(total, digits = 3)
   
   stratalist <- list(lowrisk=lowrisk, mediumrisk=mediumrisk, highrisk=highrisk, total=total)
   
 }
 