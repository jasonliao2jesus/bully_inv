

plot.mypaper <- function(x ,testdata=NULL , correct=F, method="glm", boot40=F, ...
                         # must.have.var=c("firstrses_standardized")
)
{
  validate.boot40
  corrected.c <- NULL
  predicted_pro_2 <- NULL
  
if(is.null(testdata)){  
  data1=x$data
  predicted <- predicted.mypaper(x, method = method)
  print("internal validation and calibration")

 }else{ 
  data1=testdata
  predicted <- predicted.mypaper(x, testdata = testdata, correct=correct, method = method)
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
  
  groups=x$group
  
  cOutcome <- match(as.character(quote(dep_var_0)), names(data1))
  


    
    # Calibration plot#
    plot1 <-  plot_calibration_c(data1=data1, plottitle = plottitle,#%>%filter(!is.na(q5_a)), #  如果model_1含q5_a, 要把dataset_selected 中q5_a==NA去掉 
                                 #  因為model也去掉了q5_a==NA...
                                 cOutcome=cOutcome,  # firstyrbully_1or0 是 dataset_selected[,3] 
                                 predRisk=p,
                                 groups=x$group, rangeaxis=rangeaxis, span=x$span
                                 
    )
    
    if(x$validate.boot40& boot40 ){
      
      cal<- NULL
      # bootstrap-corrected calibration
      
      cal <- rms::calibrate(model.lrm , #predy = c(seq(0, 1, by = 0.01)), 
                       method="boot", B=x$val.B) # usually B=200 or 300
      
      cal.attr <- attributes(cal)
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
        scale_shape_discrete(name=" ")+
        scale_color_discrete(name=" ", 
                            breaks= c("calibrated.orig", "calibrated.corrected"), 
                            labels= c("Apparent", "Bias-corrected"))
        
    }
    
    # calibration.auc <<- list(calibration.auc, optimism.corrected.c=corrected.c) 
    return(plot1)
    
 
  
}

