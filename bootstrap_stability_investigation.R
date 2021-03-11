source("fastbw1.R")
source("bootlasso.R")







# bootstrap 取樣

bootsamp <- function(data=model1.imp$data, R=100){
  data.m <-as.matrix(data)
  ndata <- nrow(data.m)
  bsamp <-  replicate(R, data[sample.int(ndata, replace = T),])
  
}

bootsamp <- function(data=model1.imp$data, R=100,  m=1, replace=T){
  
  data.m <-as.matrix(data)
  ndata <- m*nrow(data.m)
  bsamp <-  replicate(R, data[sample.int(nrow(data.m), size =  ndata, replace = replace),])
  
}




bootglm <- function(bootsamp, func=lrm.mypaper,model,method=c("p", "bwAIC", "bwP"),p=0.05,...){
  
  
  
  coef <- data.frame()
  fmla <- c()
  glm.orig <- lrm.mypaper(traindata = model$data, model = model, method="all")
    
  coef.orig <- coef(glm.orig)
  coefsd.orig <- sqrt(diag(vcov(glm.orig)))
     #the coefficient's sd of the original model
  
  glm.se <-  lrm.mypaper(traindata = model$data, model = model, method=method)
  coef.se <- coef(glm.se)
  coefsd.se <- sqrt(diag(vcov(glm.se)))
  coefsd.se <- coefsd.se  
  
  #ind_var <- model$ind_var
  R= ncol(bootsamp)
  cat("                     create models from the ", R ," resamples --            ")
  if(method=="p"){  
   for(i in 1:R){
     
    svMisc::progress(100*i/R)
    traindata <-bootsamp[,i] 
    glm_model <- lrm.mypaper(traindata, model=model,method, p=p )
    
    coefglm <- coef(glm_model)
    #coefglm <- coefglm[-1]
    coef <- bind_rows(coef,coefglm, .id="id")
   
      fmla <- c(fmla, deparse(glm_model$sformula, width.cutoff = 500)
                )
              
      }#預備計算model frequency
  }else{
  
   for(i in 1:R){
    svMisc::progress(100*i/R)
    traindata <-bootsamp[,i] 
    glm_model <- lrm.mypaper(traindata, model=model,method, p=p )
    
    coefglm <- coef(glm_model)
    
    if(is.matrix(coef(glm_model)) ) {
      coefnames <-  rownames(coefglm)
      coefglm <- as.vector(coefglm)
      names(coefglm) <-coefnames 
    }
    #coefglm <- coefglm[-1]
    coef <- bind_rows(coef,coefglm, .id="id")
      fmla.rt <-paste(c(as.character(glm_model$names.kept)), collapse = "+ ")
      fmla <-  c(fmla, paste( model$dep_var,"~", fmla.rt))
    }
  }
  
  
  
 coef <- coef[,c(-1,-2)]

  coef.fmla <-cbind(coef, fmla)
  
  missing_var <- na.omit(names(coef.orig)[!names(coef.orig)%in%colnames(coef)])
  coef[,missing_var] <- NA
  coef<- coef[, names(coef.orig)]
  coef.boot.na0 <- coef
  
  #coef <- coef
 # colnames(coef) <- sum1[,1]
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
  
  
  for(i in colnames(coef)){  
    coef.boot.na0[,i] <- gtools::na.replace(coef[,i], 0)
  }
  
  boot_coef_mean <- apply(coef.boot.na0[,names(freq_order)], 2, mean)
  
    boot_coef_median <- apply(coef.boot.na0[,names(freq_order)], 2, median)
    boot_coef_pr2.5 <- apply(coef.boot.na0[,names(freq_order)], 2, quantile, probs=0.025)
    boot_coef_pr97.5 <- apply(coef.boot.na0[,names(freq_order)], 2, quantile, probs=0.975)
    

    coef.orig <- coef.orig[names(freq_order)]
    coefsd.orig <- coefsd.orig[names(freq_order)]
    coef.boot.na0 <- coef.boot.na0[,names(freq_order)]
    
    relative_bias <- c()
   for (i in names(freq_order)) {
     relative_bias[i] <- boot_coef_mean[i]/(coef.orig[i]*freq_order[i])-1 
   }
   
    
    
    sum_d <- c()
    for(i in colnames(coef.boot.na0)){
      d <- coef.boot.na0[,i]-coef.orig[i]
      
      d_sqr <- sum(d^2)
      rmsd <- sqrt(d_sqr/R)
      
      sum_d <-  c(sum_d,rmsd)
      
    }
    names(sum_d) <- names(freq_order)
    rmsd <- sum_d/coefsd.orig
    
    model_freq <- dplyr::count(coef.fmla, fmla)
    model_freq <- model_freq[order(model_freq$n,decreasing = TRUE),]
    model_freq <- mutate(model_freq, freq= n/R)
    model_freq <- cbind(model_freq, cumulative_freq= round(cumsum(model_freq$freq), digits = 3))
  #  model_freq<<-model_freq    
    cat("\n                     evaluate models created from resamples  --                  ")
    fmla <- NULL
    aic <- c()
    auc.seq <- c()
    Emax.seq <- c()
    dep_var_level2 <- levels(model$data[,model$dep_var])[[2]]
    for(i in 1:nrow(model_freq)) {
   
      svMisc::progress(100*i/nrow(model_freq))
      
     if( grepl("~ $", model_freq$fmla[i]))  # find the formula "dependent_variable ~ "s and replance them with "dependent_variable ~ 1"s
         { fmla <-  as.formula(paste(grep("~ $",model_freq$fmla[i], value = T),1))
     }else{
       fmla <-  as.formula(as.character(model_freq$fmla[i]))  
        }
    
    model_f <-  glm(formula = fmla, family = binomial(), data =  model$data)
    p<- predict(model_f, newdata=model$data, type = "response")
    y <-  ifelse(model$data[,model$dep_var] == dep_var_level2,1,0 )

    roc1 <- roc(y, p, plot=FALSE, quiet = T) 
    
    auc.seq <- c(auc.seq, roc1$auc)
    
    loess.calibrate <- loess(y~p )
    if( any( is.na(loess.calibrate$fitted ))){
      Emax.seq <- c(Emax.seq, NA ) 
    }else{
    P.calibrate <- predict(loess.calibrate, newdata=p)
    Emax <- max(abs(P.calibrate-p))
    Emax.seq <- c(Emax.seq, Emax)
    }
    
  }
    model_freq <- cbind(model_freq, AUC= round(auc.seq, digits = 3), Emax=round(Emax.seq, digits = 3) )
    
    d1 <- cbind.data.frame(global_coef = round( coef.orig, digits = 3), global_sd = round( coefsd.orig, digits = 3 ),  
                           BIF=freq_order, 
                           boot_coef_median = round(boot_coef_median, digits = 3),  
                           PR2.5 = round( boot_coef_pr2.5, digits = 3) , 
                           PR97.5 = round( boot_coef_pr97.5, digits = 3 ) , 
                           relative_bias = round(100*relative_bias, digits = 1),
                           RMSD = round( rmsd, digits = 3))
    d1 <- cbind(var=rownames(d1), d1)
    
    
    d2 <- cbind.data.frame(selected_model_coef= round( coef.se, digits = 3), selected_model_coef_sd= round(coefsd.se, digits = 3))
    d2 <- cbind(var=rownames(d2), d2)
   
    d <- full_join(d1,d2, by="var")   # join the d2 ( the coeficient table of the selected model)
    d <- d[, c(1, 2, 3, 4, 10, 11, 5, 6, 7, 8, 9)]  # adjust the bsi summary table: change the order of columns
  
    rownames(d) <- NULL
  bootglm <- list(coef.orig=coef.orig,coef.orig.sd=coefsd.orig, coef.df=coef.fmla,coef.df.na0= coef.boot.na0, selected_var=selected_var, boot_inclusion_freq= freq_order, 
                  BIF_vars=BIF_vars, 
                  boot_coef_median= boot_coef_median, boot_coef_pr2.5=boot_coef_pr2.5,boot_coef_pr97.5=boot_coef_pr97.5,
                  rmsd=rmsd, relative_bias=relative_bias, model_freq= model_freq, method=method, global_model_coef= coef.orig, 
                  global_model_coef_sd= coefsd.orig, selected_model_coef= coef.se, selected_model_coef_sd= coefsd.se, 
                  bsi_summary=d)
  cat("\n                       ")
  return(bootglm)
  
}


lrm.mypaper <- function(traindata, model=model1.imp, method=c("p", "bwAIC", "bwP","all","lasso"), p=0.05)
  {
  
  
  
  ind_var <- model$ind_var
  dep_var= model$dep_var
  
  if(method=="p"){
  sorted <- sort_var(model$ind_var,data_frame = model$data)
  cat_var <- sorted[[1]]
  num_var <- sorted[[2]]
  
  
  cat_include <- c()
  num_include <- c()
  
  for(j in cat_var){
    
    table2_q3_a <-table(traindata[[j]], traindata[[dep_var]])
    
    for (i in 1:(nlevels(traindata[[j]])-1))  {
      chisq_q3_a <-   chisq.test(table2_q3_a[c(1, i+1),],correct=TRUE)
      if(chisq_q3_a$p.value<p){
        cat_include <- c(cat_include, j)
      }
      
    }
  }
  
  for(k in num_var){
    
    t.test_xy <-t.test(traindata[[k]]~ traindata[[dep_var]], var.equal=TRUE)
    if(t.test_xy$p.value < p){
      num_include <- c(num_include, k)
    }
    
  }
  
  sig_var <- c(cat_include, num_include)
  sig_var <-unique(sig_var) 
  
  sig_var_str <- paste(c(as.character(sig_var)
                         #,must.have.var #"firstrses_standardized"
  ), collapse = "+ ")
  
  fmla <- as.formula( paste( dep_var,"~",sig_var_str))
  glm_model <-  lrm(fmla, x=TRUE, y=TRUE ,  # method =  glm.method,
                    data = traindata) 
  }
  else{
    ind_var_str <- paste(c(as.character(ind_var)
                           #,must.have.var #"firstrses_standardized"
    ), collapse = "+ ")
    
    fmla <- as.formula( paste( dep_var,"~", ind_var_str))
  }
  
  if(method=="all"){

    glm_model <-  lrm(fmla, x=TRUE, y=TRUE, maxit=1000,  # method =  glm.method,
                      data = traindata)
  
  }
  if(method=="bwAIC"){
    mod.test <- lrm(fmla, data = traindata, maxit=1000 #, x=TRUE, y=TRUE
                    )

    glm_model <- fastbw1(mod.test, rule = "aic")
  }
  if(method=="bwP"){
    mod.test <- lrm(fmla, data = traindata )
    glm_model <- fastbw1(mod.test, rule = "p")
  }
  if(method=="lasso"){
  
    dep_var_level2 <- levels(traindata[,dep_var])[[2]]
    
    dep_var_0or1 <-  ifelse(traindata[,dep_var] == dep_var_level2,1,0 )  
    
  mod.matrix <- model.matrix(fmla, traindata)
  #message(class(mod.matrix))
  cv.glm.out <- cv.glmnet(x=mod.matrix, y= dep_var_0or1,
                          type.measure = "default")
  
  glm_model <- glmnet(x=mod.matrix, y= dep_var_0or1, family = "binomial", alpha = 1, lambda = cv.glm.out$lambda.1se)
    
  }
  
   
  # knn_model <- kknn::kknn(sig_fmla,  train = traindata, test = traindata)
  return(glm_model)
  
}





names(model1$model$global_model$coefficients)[-1][model1$model$lasso.glm$beta@i]


plot.bootglm <- function(bootglm, level.name=NULL){

 
  
BIF <- bootglm$boot_inclusion_freq
BIF <- sort(BIF, decreasing = TRUE)
coef.median <- bootglm$boot_coef_median
coef.df <- bootglm$coef.df
coef.df.na0 <- bootglm$coef.df.na0
coef.rmsd <- bootglm$rmsd
coef.mean <- coef.mean[names(BIF)]
coef.df <-  coef.df[, names(BIF)]
coef.df.na0 <-  coef.df.na0[, names(BIF)]
coef.rmsd <- coef.rmsd[names(BIF)]


l <- list()
for (i in colnames(coef.df.na0)) {
if(is.null( level.name[i])){
  temp <- ggplot()+ geom_histogram(data=coef.df.na0,aes(x= .data[[i]], y=..count..), bins = 50)+
    geom_histogram(data=coef.df , aes(x=.data[[i]], y=..count..), fill="red", bins=50)+
    xlab(i)+
    ylim(c(0,nrow(coef.df)))+
    annotate("text",x=median(coef.df.na0[[i]],na.rm = TRUE ), y=nrow(coef.df)/1.5,
             label= paste0( "BIF = ", round(BIF[[i]], digits = 3), "\n", 
                            "median beta =", round(coef.median[[i]], digits = 3), "\n",
                            "rmsd beta =", round(coef.rmsd[[i]], digits = 3)
             )
    )+
    theme_pubr(base_size = 10)+ grids(axis = "xy")
}else{
  temp <- ggplot()+ geom_histogram(data=coef.df.na0,aes(x= .data[[i]], y=..count..), bins = 50)+
    geom_histogram(data=coef.df , aes(x=.data[[i]], y=..count..), fill="red", bins=50)+
    xlab(i)+ xlab(level.name[i])+
    ylim(c(0,nrow(coef.df)))+
    annotate("text",x=median(coef.df.na0[[i]],na.rm = TRUE ), y=nrow(coef.df)/1.5,
             label= paste0( "BIF = ", round(BIF[[i]], digits = 3), "\n", 
                            "median beta =", round(coef.median[[i]], digits = 3), "\n",
                            "rmsd beta =", round(coef.rmsd[[i]], digits = 3)
             )
    )+
    theme_pubr(base_size = 10)+ grids(axis = "xy")
  
}


l[[i]] <- temp
temp <- NULL
}
return(l)
}

do.call(ggarrange, l)

level.name <- c("Victimization: 1-2 times", "Victimization: frequent", "Impulsivity: BIS", "Father's unemployment", 
                "AUDIT-C: >=4", "Gender: male", "Self-esteem: RSES", "School PR", 
                "PHQ-9: 6-10","PHQ-9: 11-15", "PHQ-9: >15", "MDSS", "Needy family: yes", "Living with parents: No", "Smoking: yes", 
                "Mother's unemployment")
names(level.name) <- names(bsi.aic$boot_inclusion_freq)

aic <- bootglm(a, method="bwAIC",model=model1.imp)


cul.all <- c()
cul <- 0
for(i in 1:20){
  cul <- cul+ k$freq[i]
  cul.all <- c(cul.all, cul)
}


