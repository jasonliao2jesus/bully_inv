

# required by predicted.mypaper
or.prev <- function(testdata= dataset_sec_yr_testing, traindata=dataset,dep_var= "firstyrbully2"){
  test_tab_df <-  table(testdata[,dep_var])
  train_tab_df <- table(traindata[,dep_var])
  prev <- test_tab_df[[2]]/sum(test_tab_df)
  prev0 <- train_tab_df[[2]]/sum(train_tab_df)
  or.prev <-  (prev/(1-prev))/(prev0/(1-prev0))  
  
  
}


### require or.prev
predicted.mypaper <- function(x,testdata= NULL, correct= F, imputation.mis.data=T, seed=123, 
                              method=c("glm","svm","lasso","randomforest","knn"),OOB=T, k=25, 
                              message=T, ...){
  
  set.seed(seed)
  
  
  corrected.c <- NULL
  
  calibration.auc <- NULL
  
  predicted_pro_2 <- NULL
  
  model_GLM=x$model$GLM
  model_SVM <- x$model$SVM
  model_lasso <- x$model$lasso
  model_rf <- x$model$RF
  

  
  
  dep_var <- x$dep_var
  ind_var <- x$ind_var
  
  testdata = testdata[,c(ind_var, dep_var)]
  testdata_rf <- testdata
  
  
  # below: prepare testdata----------------------------
  
  ##  exclude missing data or imputation
  
  if(is.null(testdata)){data1=x$data}else{
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
    ind_var_str <- paste(c(as.character(ind_var)
                           #,must.have.var #"firstrses_standardized"
    ), collapse = "+ ")
    fmla <- as.formula( paste( dep_var,"~", ind_var_str))
    mod.matrix <- model.matrix(fmla, data1)
    message(fmla)
    predicted_lasso<- predict(model_lasso, newx= mod.matrix ,type="response",s=model_lasso$lambda)
    predicted_lasso <- as.numeric(predicted_lasso)
    predicted_pro_2 <- predicted_lasso
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
      predicted_pro_2 <- predicted_rf
      
    }else{
      message(paste("not out-of-bag but original auc: newdata= "))
      predicted_rf <- predict(model_rf, newdata= x$data, 
                              type="prob")
      predicted_rf <- as.numeric(predicted_rf[,2])
      predicted_pro_2 <- predicted_rf
      }
    
    }
  if(method=="knn"){
    if(is.null(testdata)){testdata=x$data}
    model <- kknn(x$model$model.formula, train = x$data, test = testdata, k=k)
  predicted_pro_2 <-  model$prob[,2]
  }  

  
  if(method=="all"){
    
    predicted_glm <- predict(model_GLM,newdata= data1, type = "response") 
    predicted_svm <- predict(model_SVM, newdata = data1, probability=T)
    predicted_svm <- attr(predicted_svm, "probabilities")[,2]
    predicted_lasso<- predict(model_lasso, newx= mod.matrix ,type="response",s=model_lasso$lambda)
    predicted_lasso <- as.numeric(predicted_lasso)
    predicted_rf <- predict(model_rf, newdata= testdata, type="prob")
    predicted_rf <- as.numeric(predicted_rf[,2])
    
    predicted <- data.frame(Observed=dep_var_0or1, 
                            Predicted_glm=predicted_glm,
                            Predicted_svm=predicted_svm,
                            Predicted_rf=predicted_rf,
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
    
    predicted <- predict.mypaper(x, traindata = x$data, testdata = testdata, correct=correct, method = method)
    print(paste("external validation and calibration of testdata with prevalence correction: model building with ", method))
  }else{
    predicted <- predict.mypaper(x, traindata = x$data, method = method)
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


confusionMatrix.mypaper <- function(x, testdata= NULL,activated=T,method="glm",  correct= F, cut.off=0.3,imputation.testdata=T, ...)
  {
  predicted <- NULL
  
  if(!is.null(testdata)){
    predicted <- predict.mypaper(x, traindata=x$data, testdata = testdata, method= method, #correct=correct #, imputation.mis.data = imputation.testdata
                                 )
    print("external validation and calibration of testdata with prevalence correction")
  }else{
    predicted <- predict.mypaper(x,traindata=x$data, testdata = x$data, method = method)
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









plot_calibration_c <-   function (data1, cOutcome, predRisk, groups, rangeaxis, plottitle, 
                                  xlabel, ylabel, filename, fileplot, plottype, span=1
                                  
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
    ylabel <- "Observed/Actual Probability"
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
    matres <- mutate(matres, Type= "Grouped observations")
    #matres[bias] <- factor(matres[,bias], levels = c("overestimated", "underestimated"))
    
    
    
    
    plotc <-  ggplot(data =matres )+  #geom_smooth(aes(x= meanpred, y=meanobs), span= span)+
      geom_point(aes(x= meanpred, y=meanobs, shape= Type #,col=bias
                    )
                )+
      xlab(xlabel)+ylab(ylabel) +annotate("segment", x=0, xend = 1, y=0, yend=1)+
      xlim(c(0,1))+ylim(c(0,1)) + theme_pubr(base_size = 12)+ grids(axis = "xy")
  }
  
  return(plotc)
  
} 








#
