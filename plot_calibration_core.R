
plot_calibration_core <-   function (data1, cOutcome, predRisk, groups, rangeaxis, plottitle, 
                                     xlabel, ylabel, filename, fileplot, plottype, span=0.8, cut.off= 0.5,
                                     
                                     confusion.matrix=F, val.boot.index=val.boot.index) 
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
    #matres[bias] <- factor(matres[,bias], levels = c("overestimated", "underestimated"))
    
    
    
    
   plotc <-  ggplot(data =matres )+ # geom_smooth(aes(x= meanpred, y=meanobs), span= span)+
      geom_point(aes(x= meanpred, y=meanobs #,col=bias
                     )
                 )+
      xlab(xlabel)+ylab(ylabel) +annotate("segment", x=0, xend = 1, y=0, yend=1)+
      xlim(c(0,1))+ylim(c(0,1)) + theme_pubr(base_size = 10)+ grids(axis = "xy")
      
      
      #plot(matres[, 2], matres[, 3], main = plottitle, xlab = xlabel, 
      # ylab = ylabel, pch = 16, ps = 2, xlim = rangeaxis, 
      # ylim = rangeaxis, cex.lab = 1.2, cex.axis = 1.1, 
      #las = 1)
      #plotc <- plotc +lines(x = c(0, 1), y = c(0, 1)) 
   contr <- ((observed - predicted)^2)/(total * meanpred * 
                                          (1 - meanpred))
   chisqr <- sum(contr)
   df <- (groups - 2)
   pval <- 1 - pchisq(chisqr, df)
   lines(x = c(0, 1), y = c(0, 1))
   if (missing(plottype)) {
     plottype <- "jpg"
   }
   if (!missing(fileplot)) 
     savePlot(filename = fileplot, type = plottype, device = dev.cur())
   if (!missing(filename)) 
     write.table(matres, file = filename, row.names = TRUE, 
                 sep = "\t", dec = ",")
# calibration index   
   loess.calibrate <- loess(y~p )
   P.calibrate <- predict(loess.calibrate, newdata=p)
   ICI <- mean(abs(P.calibrate-p))
   E50 <- median(abs(P.calibrate-p))
   E90 <- quantile(abs(P.calibrate-p), probs = 0.9)
   Emax <- max(abs(P.calibrate-p))
   cal.in <- round(c(ICI=ICI, E50=E50, E90=E90, Emax=Emax), digits = 3)
   cal.in <- data.frame(cal.in)
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
                  
   calibration.auc <<- list(Table_HLtest = matres, 
                            Chi_square = round(chisqr,3), 
                            df = df, 
                            p_value = round(pval, 3), 
                            calibration.index = cal.in, 
                            discrimination.index = dis.in, 
                            val.boot.index= val.boot.index)
   
   p1 <- ifelse(p < cut.off,0,1)
   
  if(confusion.matrix){ 
    
    conf_matrix<-table(p1,y)
    conf.matrix <- caret::confusionMatrix(  conf_matrix, positive = "1")
    conf.matrix <<-  list(cut.off.prob= cut.off, confusion.matrix=conf.matrix)
  }
   
  } 
  calibration.auc <<- calibration.auc
  return(plotc)
}


