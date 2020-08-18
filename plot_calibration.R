source("plot_calibration_core.R")


or.prev <- function(testdata= dataset_sec_yr_testing, traindata=dataset,dep_var= "firstyrbully2"){
  test_tab_df <-  table(testdata[,dep_var])
  train_tab_df <- table(traindata[,dep_var])
  prev <- test_tab_df[[2]]/sum(test_tab_df)
  prev0 <- train_tab_df[[2]]/sum(train_tab_df)
  or.prev <-  (prev/(1-prev))/(prev0/(1-prev0))  
  
  
}

boot.cal <- function(model, B=40,... ) 
{
  
  cal <- calibrate(model.lrm.boot, #predy = c(seq(0, 1, by = 0.01)), 
                   method="boot", B=val.B) # usually B=200 or 300
  
  cal.attr <- attributes(cal)
  cal.attr.df <- data.frame(predy=cal.attr$predicted)
  
  
  cal.df <- as.data.frame( unclass(cal))
  cal.df$ideal <-cal.df$predy 
  
  cal.df.gather <- gather(data=cal.df, key="Type", value="Predicted_Probability",  calibrated.orig, 
                          calibrated.corrected)
  boot.cal.df <- list(cal.df.gather, cal.attr.df)
}


##
plot_calibration <- function(data1=dataset, model=model_2, groups=12, 
                              correct=T,span=0.8, svm.mod=F, 
                             confusion.matrix=T, cut.off= 0.5, 
                             validate.boot40 = T, val.B=40, #, machine=c("lrm","glm")
                             plottitle= "Calibration Plot: Model 1")
  
{  corrected.c <- NULL
  
  calibration.auc <- NULL
  
  predicted_pro_2 <- NULL
  
  if(class(model)[1]=="lrm")
    { 
    fo <- model$sformula
    data0 <- model$model 
    
    model <- glm( formula= fo,  data = data0, family=binomial, na.action=na.omit)
    
    
  }
  
  if(!svm.mod){
   
    if(is.null(data1)){data1=model$data}
    
    dep_var <- names(model$model)[[1]]
    
    dep_varname <- as.name(dep_var) 
   # dep_var1 <-  enquo(dep_varname)
    
    dep_var_level2 <- levels(data1[,dep_var])[[2]]
    
    dep_var_0or1 <-  ifelse(data1[,dep_var] == dep_var_level2,1,0 )
    
    data1<-mutate(data1, dep_var_0 = dep_var_0or1)
    
    or.prev <- or.prev(testdata = data1, traindata = model$data, dep_var = dep_var )
    
    predicted_pro_2 <- predict(model,newdata= data1, type = "response") 
    predicted_c= 1/(((1/predicted_pro_2)-1)*(1/or.prev)+1)
    
    
  }else{
    
    #if(is.null(data1)){data1=model$data}
    
    
    
    dep_varname <- model$call$formula[[2]]
    dep_var <-  deparse( dep_varname)
    #dep_var1 <-  enquo(dep_varname)
    
    dep_var_level2 <- levels(data1[,dep_var])[[2]]
    
    dep_var_0or1 <-  ifelse(data1[,dep_var] == dep_var_level2,1,0 )
    
    data1<-mutate(data1, dep_var_0 = dep_var_0or1)
    
    #or.prev <- or.prev(testdata = data1, traindata = model$data, dep_var = dep_var )
    
    predicted_pro_2 <- predict(model,newdata= data1,  probability=TRUE) 
    predicted_pro_2 <- attr(predicted_pro_2, "probabilities")[,2]
    
  }

  


  #
  
 
  
# specify range of x-axis and y-axis
rangeaxis <- c(0,1)
# specify number of groups for Hosmer-Lemeshow test

groups=groups

 cOutcome <- match(as.character(quote(dep_var_0)), names(data1))

 
 # validation with bootstraping  B=2000
 if(validate.boot40 & !svm.mod){ 
   data0=model$data
   fo <- model$formula
   model.lrm <- lrm(fo, x=TRUE, y=TRUE, data =  data0)
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
                              "Bootstrap-corrected Nagelkerke.R2",
                              "AUC",
                              "Bootstrap-corrected AUC", 
                              "Calibration-in-large" )
   
   val.boot.index <- data.frame(val.boot.index)
   #calibration.auc[["optimism.corrected.c"]] <- corrected.c

   # bootstrap-corrected calibration
   
   cal <- calibrate(model.lrm , #predy = c(seq(0, 1, by = 0.01)), 
                    method="boot", B=val.B) # usually B=200 or 300
   
   cal.attr <- attributes(cal)
   cal.attr.df <- data.frame(predy=cal.attr$predicted)
   
   
   cal.df <- as.data.frame( unclass(cal))
   cal.df$ideal <-cal.df$predy 
   
   cal.df.gather <- gather(data=cal.df, key="Type", value="Predicted_Probability",  calibrated.orig, 
                           calibrated.corrected)
   #boot.cal.df <- list(cal.df.gather, cal.attr.df)

      } else{ val.boot.index= "bootstrap resampling inactivated or the the model is not glm"}
  
 
 

if(!correct|svm.mod)
{
# Calibration plot#
 plot1 <-  plot_calibration_core(data1=data1, plottitle = plottitle,#%>%filter(!is.na(q5_a)), #  如果model_1含q5_a, 要把dataset_selected 中q5_a==NA去掉 
                #  因為model也去掉了q5_a==NA...
                cOutcome=cOutcome,  # firstyrbully_1or0 是 dataset_selected[,3] 
                predRisk=predicted_pro_2,
                groups=groups, rangeaxis=rangeaxis, span=span, 
                confusion.matrix = confusion.matrix, cut.off = cut.off , 
                val.boot.index = val.boot.index
              )
 if(validate.boot40 & !svm.mod){
 plot1 <- plot1+ 
    geom_line(data= cal.df.gather,   aes(x=predy, y=Predicted_Probability, col = Type), size=1.1)+
   geom_rug(data=cal.attr.df, mapping = aes(x= predy), stat = "identity",
            position = "identity",alpha=0.2, sides = "t") 
    }
 
# calibration.auc <<- list(calibration.auc, optimism.corrected.c=corrected.c) 
return(plot1)
 
}else{
  plot2 <-  plot_calibration_core(data1=data1,  plottitle = plottitle,#%>%filter(!is.na(q5_a)), #  如果model_1含q5_a, 要把dataset_selected 中q5_a==NA去掉 
                                  #  因為model也去掉了q5_a==NA...
                                  cOutcome=cOutcome,  # firstyrbully_1or0 是 dataset_selected[,3] 
                                  predRisk=predicted_c,
                                  groups=groups, rangeaxis=rangeaxis, span = span, 
                                  confusion.matrix = confusion.matrix, cut.off = cut.off , 
                                  val.boot.index = val.boot.index)
  if(validate.boot40 & !svm.mod){
    plot2 <- plot2+ 
      geom_line(data= cal.df.gather,   aes(x=predy, y=Predicted_Probability, col = Type), size=1.1)+
      geom_rug(data=cal.attr.df, mapping = aes(x= predy), stat = "identity",
               position = "identity",alpha=0.2, sides = "t") 
  }
 # calibration.auc <<- list(calibration.auc, optimism.corrected.c = corrected.c) 
  
  return(plot2)
}
 #calibration.auc <<- list(calibration.auc, optimism.corrected.c = corrected.c)
 #return(plot2)

}




# the data frame for the Calibration plot
selected_variables <- c("firstyrbully2","victim_3group", "firstyrbully_1or0", "bully_3group","bully_inv",
                        "rec_bully_1or0","rec_bully_act_1or0",
                        "firstyrvic2","rec_victim_firstyr",
                        "rec_bully_firstyr","rec_bully_firstyr_act", "physicalbully", "verbalbully", "relationalbully", "otherbully",
                        "physicalvictim", "verbalvictim", "relationalvictim", 
                        
                        
                        
                        "q3_a","q4_a","q5_a","q6_a","q7_a","q8_a", "q10_a", "school_PR","q22_a",
                        "q68_a","AUDIT_C_4_a", "AUDIT_C_a", "PHQ9_1yr_new", "q31_0_a", "q22_0_a", 
                        "parent1_MDSS", "teacher_MDSS",
                        "peer1", "firstrses", "firstrses_standardized",
                        "bis1_total", "bis1_standardized", 
                        
                        "bis1_inability_to_plan", "bis1_novelty_seeking", "bis1_lack_of_selfcontrol",
                        "bis1_inability_to_plan_standardized", "bis1_novelty_seeking_standardized", "bis1_lack_of_selfcontrol_standardized"
)
dataset_selected <- dataset[,selected_variables]


#try 

plot_model_2 <-  plot_calibration(data1 = dataset_selected%>%filter(!is.na(q5_a)) , model = model_2 , 
                                  groups = 15, correct=F,span=0.8, svm.mod=F, validate.boot40=F, B=1000,
                                  plottitle = "Calibration Plot: Model 1"
                                  )


plot_model_21 <-  plot_calibration(data1 = dataset_selected%>%filter(!is.na(q5_a)),model = model_21, 
                                   groups = 15, correct=F,span=0.8, svm.mod=F, validate.boot40=F, B=1000,
                                   plottitle = "Calibration Plot: Model 3"
                                   )


plot_model_2_rec <-  plot_calibration(data1 = dataset_selected%>%filter(!is.na(q5_a)),  model = model_2_rec,
                                      groups = 15 ,correct=F,span=0.8, svm.mod=F, validate.boot40=F, B=1000,
                                      plottitle = "Calibration Plot: Model 2"
                                      )

plot_model_s <-   plot_calibration(data1 = dataset_selected%>%filter(!is.na(q5_a)),  model = model_s,
                                   groups = 15 ,correct=F,span=0.8, svm.mod=T, validate.boot40=F, B=1000,
                                   plottitle = "Calibration Plot: Model 2"
)



