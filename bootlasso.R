
##LASSO取出來的變數

bootlasso <- function(bootsamp, func=lrm.mypaper,model=model1,...){
  
 # c#oef <- data.frame()
  fmla <- c()
  
  
  global_model <- lrm.mypaper(traindata = model$data, model=model, method = "all")
  coef.orig <- coef(global_model)
  coefsd.orig <- sqrt(diag(vcov(global_model)))
  coef <- data.frame(rbind(coef.orig[-1]))
  global_levels <- names(global_model$coefficients)[-1]
  
  
  selected_levels <- global_levels[model$model$lasso.glm$beta@i]
  selected_levels_coef <- model$model$lasso.glm$beta@x
  names(selected_levels_coef) <- selected_levels
  
  R= ncol(bootsamp)
  
  
  for(i in 1:R){
    svMisc::progress(100*i/R)
    traindata <-as.data.frame(bootsamp[,i]) 
    lasso_model <- lrm.mypaper(traindata, model=model,method="lasso")
    
    if(lasso_model$beta@i[1]==0){
      resample_levels_coef <- rep(NA, length.out=length(global_levels))
      names(resample_levels_coef) <-  global_levels
      
    }else{
    resample_levels<-c(global_levels[lasso_model$beta@i])
    resample_levels_coef <-c(lasso_model$beta@x)
    names(resample_levels_coef) <- resample_levels
    }
    coef <- bind_rows(coef,resample_levels_coef, .id="id")
    
    #fmla <- c(fmla, deparse(lasso_model$sformula, width.cutoff = 500)
    #)
    
  }
  
  #預備計算model frequency
  coef <- coef[,c(-1,-2)][-1,]
  
 
  coef<- coef[, global_levels]
  coef.boot.na0 <- coef
  for(i in colnames(coef)){  
    coef.boot.na0[,i] <- gtools::na.replace(coef[,i], 0)
  }
  selected_var <- !coef%>%is.na
  freq <- apply(selected_var, 2, sum)/R
  freq_order <- sort(freq, decreasing = T)
  
  BIF_vars <- c()
  for(i in names(freq_order)){
    for(j in model$ind_var){
      if(any(grepl( j , i )
      )
      ){ 
        BIF_vars <- c(BIF_vars, j)
      }
    }
  }
  BIF_vars <- unique(BIF_vars)
  
  
 
  boot_coef_median <- apply(coef.boot.na0[,names(freq_order)], 2, median)
  boot_coef_pr2.5 <- apply(coef.boot.na0[,names(freq_order)], 2, quantile, probs=0.025)
  boot_coef_pr97.5 <- apply(coef.boot.na0[,names(freq_order)], 2, quantile, probs=0.975)
  
  
  coef.orig <- coef.orig[names(freq_order)]
  coef.boot.na0 <- coef.boot.na0[,names(freq_order)]
  
  d1 <- cbind.data.frame(global_coef = round( coef.orig, digits = 3), global_sd = round( coefsd.orig[-1], digits = 3 ),  
                         BIF=freq_order, 
                         boot_coef_median = round(boot_coef_median, digits = 3),  
                         PR2.5 = round( boot_coef_pr2.5, digits = 3) , 
                         PR97.5 = round( boot_coef_pr97.5, digits = 3 ))
  d1 <- cbind(var=rownames(d1), d1)
  
  
  d2 <- cbind.data.frame(selected_model_coef= round( selected_levels_coef, digits = 3))
  d2 <- cbind(var=rownames(d2), d2)
  
  d <- full_join(d1,d2, by="var") 
  d <- d[, c(1, 2, 3, 4, 8, 5, 6, 7)] 
  
  
  bootlasso <- list(coef=coef , coef.boot.na0= coef.boot.na0, global_coef=coef.orig, 
                    freq_order=freq_order, BIF_vars=BIF_vars, bsi_summary=d
                    )
  return(bootlasso)
}
