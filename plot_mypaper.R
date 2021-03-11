

plot.mypaper <- function(x ,testdata=NULL , correct=F, method="glm",OOB=T, k=25,groups=NULL, boot=NULL,font_size=14, ...
                         # must.have.var=c("firstrses_standardized")
)
{

  corrected.c <- NULL
  predicted_pro_2 <- NULL
  
if(is.null(testdata)){  
  data1=x$data
  predicted <- predicted.mypaper(x, method = method)
  print("internal validation and calibration")

 }else{ 
  data1=testdata
  predicted <- predicted.mypaper(x,  testdata = testdata, correct=correct, method = method, OOB=OOB, k=k)
  print("external validation and calibration of testdata with prevalence correction")

 }
    

  model <- x$model$GLM
  model.lrm <-  x$model$LRM

  
  y <- predicted$Observed
  p <- predicted$Predicted
  
  
  dep_var <- x$dep_var
  data1<-mutate(data1, dep_var_0 = y)
  
  # specify range of x-axis and y-axis
  rangeaxis <- c(0,1)
  # specify number of groups for Hosmer-Lemeshow test
  

  
  cOutcome <- match(as.character(quote(dep_var_0)), names(data1))
  

  if(is.null(groups)){groups= x$group}
    
    # Calibration plot#
    plot1 <-  plot_calibration_c(data1=data1, plottitle = plottitle,#%>%filter(!is.na(q5_a)), #  如果model_1含q5_a, 要把dataset_selected 中q5_a==NA去掉 
                                 #  因為model也去掉了q5_a==NA...
                                 cOutcome=cOutcome,  # firstyrbully_1or0 是 dataset_selected[,3] 
                                 predRisk=p,
                                 groups=groups, rangeaxis=rangeaxis, span=x$span
                                 
    )
    if(is.null(boot)){boot= x$validate.boot40}
    
    if(boot ){
      
      cal<- NULL
      # bootstrap-corrected calibration
      
      cal <- rms::calibrate(model.lrm , #predy = c(seq(0, 1, by = 0.01)), 
                       method="boot", B=x$val.B) # usually B=200 or 300
      
      cal.attr <- attributes(cal)
      
      predicted <- cal.attr$predicted
      p <- cal[, "predy"]
      p.app <- cal[, "calibrated.orig"]
      p.cal <- cal[, "calibrated.corrected"]
      if (length(predicted)) {
        s <- !is.na(p + p.cal)
        err <- predicted - approx(p[s], p.cal[s], xout = predicted, 
                                  ties = mean)$y
        cat("\nn=", n <- length(err), "   Mean absolute error=", 
            round(mae <- mean(abs(err), na.rm = TRUE), 3), "   Mean squared error=", 
            round(mean(err^2, na.rm = TRUE), 5), "\n0.9 Quantile of absolute error=", 
            round(quantile(abs(err), 0.9, na.rm = TRUE), 3), 
            "\n\n", sep = "")
       # if (subtitles) 
        #  title(sub = paste("Mean absolute error=", round(mae, 
         #                                                 3), " n=", n, sep = ""), cex.sub = cex.subtitles, 
          #      adj = 1)
      }
      
      
      cal.attr.df <- data.frame(predy=cal.attr$predicted)
      
      
      cal.df <- as.data.frame( unclass(cal))
     #cal.df$ideal <-cal.df$predy 
      
      cal.df.gather <- gather(data=cal.df, key="Type", value="Predicted_Probability",  calibrated.orig, 
                              calibrated.corrected)
      ###
      
      plot1 <- plot1+ 
        geom_line(data= cal.df.gather,   aes(x=predy, y=Predicted_Probability, col = Type), size=1.1)+
        geom_rug(data=cal.attr.df,  aes(x= predy), stat = "identity",
                 position = "identity",alpha=0.2, sides = "t") + 
        scale_shape_discrete(name=" ",  labels=paste("Grouped/Actual observations\n(", x$group, "groups)"),
                             )+
        scale_color_discrete(name=" ", 
                            breaks= c("calibrated.orig", "calibrated.corrected"), 
                            labels= c("Apparent", paste("Bias-corrected\n(Bootstrap repetition = ", 
                                                        x$val.B,
                                                        ")",
                                                        sep = "")
                                      )
                            )+
        theme_bw(base_size = font_size)
      
    }
    
    # calibration.auc <<- list(calibration.auc, optimism.corrected.c=corrected.c) 
    return(plot1)
    
 
  
}



calplot.mypaper <- function(x ,testdata=NULL , correct=F, method="glm",OOB=T, k=25,groups=NULL,...
                         # must.have.var=c("firstrses_standardized")
)
{
  
  corrected.c <- NULL
  predicted_pro_2 <- NULL
  
  if(is.null(testdata)){  
    data1=x$data
    predicted <- predicted.mypaper(x, method = method)
    print("internal validation and calibration")
    
  }else{ 
    data1=testdata
    predicted <- predicted.mypaper(x,  testdata = testdata, correct=correct, method = method, OOB=OOB, k=k)
    print("external validation and calibration of testdata with prevalence correction")
    
  }
  
  
  model <- x$model$GLM
  model.lrm <-  x$model$LRM
  
  
  y <- predicted$Observed
  p <- predicted$Predicted
 
  
  
  dep_var <- x$dep_var
  data1<-mutate(data1, dep_var_0 = y)
  
  # specify range of x-axis and y-axis
  rangeaxis <- c(0,1)
  # specify number of groups for Hosmer-Lemeshow test
  
  
  
  cOutcome <- match(as.character(quote(dep_var_0)), names(data1))
  
  
  if(is.null(groups)){groups= x$group}
  
  # Calibration plot#
  plot1 <-  plot_calibration_c(data1=data1, plottitle = plottitle,#%>%filter(!is.na(q5_a)), #  如果model_1含q5_a, 要把dataset_selected 中q5_a==NA去掉 
                               #  因為model也去掉了q5_a==NA...
                               cOutcome=cOutcome,  # firstyrbully_1or0 是 dataset_selected[,3] 
                               predRisk=p,
                               groups=groups, rangeaxis=rangeaxis, span=x$span
                               
  )
 
  
  # calibration.auc <<- list(calibration.auc, optimism.corrected.c=corrected.c) 
  return(plot1)
  
  
  
}



