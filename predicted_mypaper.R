

# required by predicted.mypaper
or.prev <- function(testdata= dataset_sec_yr_testing, traindata=dataset,dep_var= "firstyrbully2"){
  test_tab_df <-  table(testdata[,dep_var])
  train_tab_df <- table(traindata[,dep_var])
  prev <- test_tab_df[[2]]/sum(test_tab_df)
  prev0 <- train_tab_df[[2]]/sum(train_tab_df)
  or.prev <-  (prev/(1-prev))/(prev0/(1-prev0))  
  
  
}


### require or.prev
predicted.mypaper <- function(x,testdata= NULL, correct= F, imputation.mis.data=T, seed=123, method=c("glm","svm","lasso","all"),message=T, ...){
  
  set.seed(seed)
  
  
  corrected.c <- NULL
  
  calibration.auc <- NULL
  
  predicted_pro_2 <- NULL
  
  model_GLM=x$model$GLM
  model_SVM <- x$model$SVM
  model_lasso <- x$model$lasso
  mod.matrix <- x$model$model.matrix

  
  
  dep_var <- x$dep_var
  ind_var <- x$ind_var
  
  testdata = testdata[,c(ind_var, dep_var)]
  
  
  # below: prepare testdata----------------------------
  
  ##  exclude missing data or imputation
  
  if(is.null(testdata)){data1=model_GLM$data}else{
  var_na <- NULL
  prop.na.v <- NULL
  
  for (i in colnames(testdata) ){
    if(any(is.na(testdata[,i]))){
      
      var_na <- c(var_na,i)
      prop.na <- sum(is.na(testdata[,i]))/length(testdata[,i])
      prop.na.v <- c(prop.na.v, prop.na)
      #      data <- data[!is.na(data[,i]),]
      
    }
    
  }
  
  if(is.null(var_na)){
    if(message){print("no missing data in testdata")}
  } else{
    if(message){print(paste("testdata: missing data in column of",
                paste(var_na, collapse = "& "), 
                "with ratio of",
                paste( round(prop.na.v,digits = 3)*100, "%",collapse = "& " ),
                sep = " ")
    )
      }
  
    if(imputation.mis.data ){
      
      tempData <- mice(testdata, m=5, printFlag = FALSE)  
      testdata <- mice::complete(tempData)
      if(message){print("missing data in testdata imputed!!")}
      
    }else{
      
      if(message){ print("cast rows with missing data away!!")
      for(i in var_na){
        testdata <- testdata[!is.na(testdata[,i]),]
      }
      }
    }
    
  }
  
  data1=testdata
  }
  
  
  # above: prepare testdata---------------------------------------------
  
  
  
  
  dep_var <- names(model_GLM$model)[[1]]
  
  dep_varname <- as.name(dep_var) 
  
  
  dep_var_level2 <- levels(data1[,dep_var])[[2]]
  
  dep_var_0or1 <-  ifelse(data1[,dep_var] == dep_var_level2,1,0 )
  
  #data1<-mutate(data1, dep_var_0 = dep_var_0or1)
  
  if(method=="glm"){
  predicted_pro_2 <- predict(model_GLM,newdata= data1, type = "response") 
  }
  if(method=="svm"){
    predicted_svm <- predict(model_SVM, newdata = data1, probability=T)
    predicted_svm <- attr(predicted_svm, "probabilities")[,2]
    predicted_pro_2 <- predicted_svm
  }
  if(method=="lasso"){
    predicted_lasso<- predict(model_lasso, newx= mod.matrix ,type="response",s=model_lasso$lambda)
    predicted_lasso <- as.numeric(predicted_lasso)
    predicted_pro_2 <- predicted_lasso
  }
  
  
  
  

  
  if(method=="all"){
    
    predicted_glm <- predict(model_GLM,newdata= data1, type = "response") 
    predicted_svm <- predict(model_SVM, newdata = data1, probability=T)
    predicted_svm <- attr(predicted_svm, "probabilities")[,2]
    predicted_lasso<- predict(model_lasso, newx= mod.matrix ,type="response",s=model_lasso$lambda)
    predicted_lasso <- as.numeric(predicted_lasso)
    
    predicted <- data.frame(Observed=dep_var_0or1, 
                            Predicted_glm=predicted_glm,
                            Predicted_svm=predicted_svm,
                            Predicted_lasso=predicted_lasso)
    }else{
           if(correct){
                or.prev <- or.prev(testdata = testdata, traindata = x$data, dep_var = dep_var )
                predicted_pro_2= 1/(((1/predicted_pro_2)-1)*(1/or.prev)+1)
              }
      
      
      
      
      
      predicted <- data.frame( Predicted = predicted_pro_2, Observed = dep_var_0or1)                     
                            }
  
  
  class(predicted) <- c("mypaper", "data.frame")
  
  return(predicted)
}






validate.mypaper <-  function(x,...){
  model.lrm=x$model$LRM
  data= x$data
  
  if(x$validate.boot40 ){ 
    val.boot <- NULL
    
    
    
    # bootstrap-correct internal validation   
    val.boot <- validate(model.lrm, B=val.B)
    #val.boot <- unclass(val.boot)
    #cal.boot <- calibrate(model, method="boot", B= val.B)
    auc.org <- (1+val.boot[1,1])/2
    corrected.c <- (1+val.boot[1,5])/2
    cal.in.large <- val.boot[3,5]
    Nagelkerke.R2 <- val.boot[2,1]
    Bootstrap.corrected.Nagelkerke.R2 <- val.boot[2,5]
    
    val.boot.index <- round(c( Nagelkerke.R2, 
                               Bootstrap.corrected.Nagelkerke.R2,
                               auc.org,
                               corrected.c, 
                               cal.in.large ), digits = 3)
    
    names(val.boot.index) <- c("Nagelkerke R2", 
                               "Bootstrap-corrected Nagelkerke R2",
                               "AUC",
                               "Bootstrap-corrected AUC", 
                               "Calibration-in-large" )
    
    val.boot.index <- data.frame(Validation_index= val.boot.index)
    names(val.boot.index)[colnames(val.boot.index) == "Validation_index"] <- x$dep_var
    return(val.boot.index)
    
  }
  
}


calibrate.mypaper <- function(x, testdata= NULL, correct= F,method="glm",  HLtest= T,...){
  
  if(!is.null(testdata)){
    
    predicted <- predicted.mypaper(x, testdata = testdata, correct=correct, method = method)
    print(paste("external validation and calibration of testdata with prevalence correction: model building with ", method))
  }else{
    predicted <- predicted.mypaper(x, method = method)
    print(paste("internal validation and calibration; model building with ", method))
  }
  
  y <- predicted$Observed
  p <- predicted$Predicted
  groups= x$group
  
  matres <- matrix(NA, nrow = groups, ncol = 5)
  sor <- order(p)
  p <- p[sor]
  y <- y[sor]
  
  groep <- cut2(p, g = groups)
  total <- tapply(y, groep, length)
  predicted <- round(tapply(p, groep, sum), 3)
  observed <- tapply(y, groep, sum)
  meanpred <- round(tapply(p, groep, mean), 3)
  meanobs <- round(tapply(y, groep, mean), 3)
  matres <- cbind(total, meanpred, meanobs, predicted, 
                  observed
                  #bias= ifelse(meanpred>meanobs, "overestimated", "underestimated")
  )
  matres <- as.data.frame(matres)
  matres <- mutate(matres, bias= ifelse(meanpred>meanobs, "overestimated", "underestimated"))
  
  contr <- ((observed - predicted)^2)/(total * meanpred * 
                                         (1 - meanpred))
  chisqr <- sum(contr)
  
  df <- (groups - 2)
  pval <- 1 - pchisq(chisqr, df)
  
  ### calibration index 
  
  loess.calibrate <- loess(y~p )
  P.calibrate <- predict(loess.calibrate, newdata=p)
  ICI <- mean(abs(P.calibrate-p))
  E50 <- median(abs(P.calibrate-p))
  E90 <- quantile(abs(P.calibrate-p), probs = 0.9)
  Emax <- max(abs(P.calibrate-p))
  cal.in <- round(c(ICI=ICI, E50=E50, E90=E90, Emax=Emax), digits = 3)
  cal.in <- data.frame(cal.in)
  names(cal.in)[names(cal.in) == "cal.in"] <- x$dep_var
  # discrimiation:AUC and its confidence interval   
  roc.test <- roc(y, p, plot = F)
  auc.test <- roc.test$auc
  auc.95CI <- ci.auc(roc.test, method= "delong")
  auc.95CI <- unclass(auc.95CI)
  
  
  
  dis.in <- c( AUC.95CI= paste( round(auc.test, digits = 3), 
                                paste(  "(95% CI",
                                        paste(round(auc.95CI[1], digits = 3),
                                              sep =  "~", 
                                              round(auc.95CI[3], digits = 3)
                                        ),
                                        sep = " ",
                                        ")"),
                                sep=" "
  )
  )
  
  if(!HLtest){
  calibration.auc <- list( 
                          Chi_square = round(chisqr,3), 
                          df = df, 
                          p_value_HLtest = round(pval, 3), 
                          calibration.index = cal.in, 
                          discrimination.index = dis.in)
  }else{
    calibration.auc <- list(Table_HLtest = matres, 
                            Chi_square = round(chisqr,3), 
                            df = df, 
                            p_value_HLtest = round(pval, 3), 
                            calibration.index = cal.in, 
                            discrimination.index = dis.in)
  }
  
  
  return(calibration.auc)
  
  
}  


confusionMatrix.mypaper <- function(x, testdata= NULL,activated=T,  correct= F, cut.off=0.3,imputation.testdata=T, ...)
  {
  predicted <- NULL
  
  if(!is.null(testdata)){
    predicted <- predicted.mypaper(x, testdata = testdata, correct=correct, imputation.mis.data = imputation.testdata)
    print("external validation and calibration of testdata with prevalence correction")
  }else{
    predicted <- predicted.mypaper(x)
    print("internal validation and calibration")
  }
  
  cut.off <- ifelse(is.na(cut.off), x$cut.off, cut.off)
  y <- predicted$Observed
  p <- predicted$Predicted
  p1 <- ifelse(p < cut.off,0,1)

if(x$confusion.matrix|activated){ 
  
  conf_matrix<-table(p1,y)
  conf.matrix <- caret::confusionMatrix(  conf_matrix, positive = "1")
  conf.matrix <-  list(cut.off.prob=  cut.off, confusion.matrix=conf.matrix)
  }else{ print("confusion matrix is inactivated")}
  return(conf.matrix)
}




suppressMessages()







plot_calibration_c <-   function (data1, cOutcome, predRisk, groups, rangeaxis, plottitle, 
                                  xlabel, ylabel, filename, fileplot, plottype, span=0.8
                                  
                                  ) 
{
  if (missing(groups)) {
    groups <- 10
  }
  #if (missing(plottitle)) {
  # plottitle <- "Calibration Plot"
  #}
  if (missing(xlabel)) {
    xlabel <- "Predicted Probability"
  }
  if (missing(ylabel)) {
    ylabel <- "Observed Probability"
  }
  if (missing(rangeaxis)) {
    rangeaxis <- c(0, 1)
  }
  p = predRisk
  y = data1[, cOutcome]
  if (length(unique(y)) != 2) {
    stop(" The specified outcome is not a binary variable.\n")
  }
  else {
    matres <- matrix(NA, nrow = groups, ncol = 5)
    sor <- order(p)
    p <- p[sor]
    y <- y[sor]
    
    groep <- cut2(p, g = groups)
    total <- tapply(y, groep, length)
    predicted <- round(tapply(p, groep, sum), 3)
    observed <- tapply(y, groep, sum)
    meanpred <- round(tapply(p, groep, mean), 3)
    meanobs <- round(tapply(y, groep, mean), 3)
    matres <- cbind(total, meanpred, meanobs, predicted, 
                    observed
                    #bias= ifelse(meanpred>meanobs, "overestimated", "underestimated")
    )
    matres <- as.data.frame(matres)
    matres <- mutate(matres, bias= ifelse(meanpred>meanobs, "overestimated", "underestimated"))
    matres <- mutate(matres, Type= paste("Grouped observations\n(", groups, " groups)"))
    #matres[bias] <- factor(matres[,bias], levels = c("overestimated", "underestimated"))
    
    
    
    
    plotc <-  ggplot(data =matres )+ # geom_smooth(aes(x= meanpred, y=meanobs), span= span)+
      geom_point(aes(x= meanpred, y=meanobs, shape= Type #,col=bias
                    )
                )+
      xlab(xlabel)+ylab(ylabel) +annotate("segment", x=0, xend = 1, y=0, yend=1)+
      xlim(c(0,1))+ylim(c(0,1)) + theme_pubr(base_size = 10)+ grids(axis = "xy")
  }
  
  return(plotc)
  
}  


#

obs.boot <- sample(x = 1:nrow(dataset_selected_imp), size = nrow(dataset_selected_imp), replace = T)
in.obs.boot <- 1:nrow(dataset_selected_imp) %in% obs.boot
notin.obs.boot <-  !in.obs.boot
notin.traindata <- dataset[notin.obs.boot,]

auc.mypaper <- function(traindata,testdata,  method="glm",x=model1.imp, ...){

 
  
  
  predicted <- predict.mypaper(x, traindata=traindata, testdata = testdata, correct=F, method = method) #%>%filter(!is.na(q5_a)))
  
  
  
  
  y <-  predicted$Observed
  p <- predicted$Predicted
  
  data <- cbind(y=y, p=p )
  
  
  
  
  
  roc.test <- roc(data[,"y"], data[,"p"], plot = F)
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
  roc.test <- roc(y, p, plot = F)
  auc.test <- roc.test$auc

  calibration.auc <- c(   cal.in, 
                          AUC=auc.test, AUC.5CI=auc.95CI[1], AUC.95CI=auc.95CI[3])
  
  return(calibration.auc)
  
  
} 

boot.mypaper <- function(data=model3.imp$data, statistic=auc.mypaper, 
                         R=100,sim="parametric", method=c("glm", "svm", "lasso","all"), 
                         x=model3.imp,...){
  method=match.arg(method)
  
  message(class(method))
  a <- boot(data, statistic , R=R, sim = sim, stype = "i", method=method, x=x)
  
  b <- as.data.frame(a$t)
  names(b) <- c("ICI","E50","E90","Emax", "AUC","AUC5%CI","AUC95%CI")
  original <- a$t0
  corrected <- apply(b, MARGIN = 2, mean)
  optimism <- original - corrected
  l <- cbind.data.frame(original= original, corrected=corrected, optimism=optimism)
  return(l)
}



boot.mypaper <- function(data=model3.imp$data, statistic=auc.mypaper, 
                         R=100,sim="parametric", method=c("glm", "svm", "lasso","all"), 
                         x=model3.imp,...){
  method=match.arg(method)
  
  message(class(method))
  a <- boot(data, statistic , R=R, sim = sim, stype = "i", method=method, x=x)
  
  b <- as.data.frame(a$t)
  names(b) <- c("ICI","E50","E90","Emax", "AUC","AUC5%CI","AUC95%CI")
  original <- a$t0
  corrected <- apply(b, MARGIN = 2, mean)
  optimism <- original - corrected
  l <- cbind.data.frame(original= original, corrected=corrected, optimism=optimism)
  return(l)
}


boot2.mypaper <- function(model=model3.imp,data=model3.imp$data, func=auc.mypaper, R=40, method= c("glm", "svm", "lasso","all"))
{
  method=match.arg(method)
  l <- data.frame()
  original <- auc.mypaper(traindata=data, testdata=data,  method=method,x=model)
  for(i in 1:R){
    obs.boot <- sample(x = 1:nrow(data), size = nrow(data), replace = T)
    in.obs.boot <- 1:nrow(data) %in% obs.boot
    notin.obs.boot <-  !in.obs.boot
    notin.traindata <- data[notin.obs.boot,]
    
    traindata <- data[obs.boot, ]
    testdata <- notin.traindata
    # fit the model on bootstrap sample
    arg1 <- list( traindata=traindata, testdata=testdata, method=method, x=model)
    pred <- do.call(func, arg1)
    #pred <- as.data.frame(pred)
    
    # apply model to original data
      
      l <- rbind(l,pred)
      
  }
  message(dim(l))
  
      colnames(l) <- c("ICI","E50","E90","Emax", "AUC","AUC5%CI","AUC95%CI")
      
      corrected <- apply(l, MARGIN = 2, mean)
    # apply model to bootstrap data
   # arg1 <- list(model=model, data=data, indices= obs.boot, method=method)
    #pred <- do.call(func, arg)
    
    #prob2 = predict(logit.boot, type='response', data.boot)
    #pred2 = prediction(prob2, data.boot$credit)
    #auc2 = performance(pred2,"auc")@y.values[[1]][1]
    #B[i, 2] = auc2

  optimism= original-corrected
  
  calib <- data.frame( original=original,corrected=corrected, optimism=optimism)
  return(calib)
}

method=c("glm", "svm", "lasso")
R=2000
data= dataset_selected_imp

bootval.mod1.glm <- boot2.mypaper(model=model1.imp,data, func = auc.mypaper, R=R, method = "glm")
bootval.mod1.svm <- boot2.mypaper(model=model1.imp,data, func = auc.mypaper, R=R, method = "svm")
bootval.mod1.lasso <- boot2.mypaper(model=model1.imp,data, func = auc.mypaper, R=R, method = "lasso")

bootval.mod3.glm <- boot2.mypaper(model=model3.imp,data, func = auc.mypaper, R=R, method = "glm")
bootval.mod3.svm <- boot2.mypaper(model=model3.imp,data, func = auc.mypaper, R=R, method = "svm")
bootval.mod3.lasso <- boot2.mypaper(model=model3.imp,data, func = auc.mypaper, R=R, method = "lasso")
