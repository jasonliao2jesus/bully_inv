source("bootstrap_stability_investigation.R")
source("bootlasso.R")


f <- function(bootsamp=a2.i, model= model2.imp, start.end=NULL, maxit=100){
  d <- data.frame(i=NULL, glmm=NULL, lrmm=NULL)
  
  fmla <- model$model$global_model$formula
  if(is.null(start.end)){
    start.end <- 1:ncol(bootsamp)
  }
  cat("\n\n\n")
  for(i in start.end){
    
    svMisc::progress(100*(order(start.end)[start.end==i])/length(start.end))
    #assign("last.warning", NULL, envir=baseenv())
    model_2 <- lrm(data= as.data.frame(bootsamp[,i]), formula = fmla, maxit=maxit)
    model_2_glm <-  glm(data= as.data.frame( bootsamp[,i]), formula = fmla, family = binomial(), method="detect_separation")
    a <-   c(i, glmm= model_2_glm$separation,lrmm=model_2$fail )
    d<- rbind(d, a)
    #if(is.null(model_2$Design)){ print(paste( "lrm",i))}
    #if(length(warnings())>0){print(i)}
    #if(model_2_glm$separation){print(paste("glm", i))}
  }
   colnames(d) <- c("model", "glmm_separation", "lrm_nonconvergence")
  return(d)
}

table(k$glmm_separation, k$lrm_nonconvergence)
set.seed(2020.10)
# example: model 1
a.i <- bootsamp(data=model1.imp$data, R=2000)

#bsi2.bwp <- bootglm(bootsamp = a2, model=model2, method = "bwP", p=0.05)
bsi.i.aic <- bootglm(bootsamp = a.i, model=model1.imp, method = "bwAIC", p=0.05)
bsi.i.p <- bootglm(bootsamp = a.i, model=model1.imp, method = "p", p=0.05)
bsi.i.lasso <- bootlasso(a.i, model = model1.imp)


a2.i <- bootsamp(data=model2.imp$data, R=2000)
#b2 <- bootsamp(data=model2$data, R=200)
#bsi2.bwp <- bootglm(bootsamp = a2, model=model2, method = "bwP", p=0.05)
bsi2.i.aic <- bootglm(bootsamp = a2.i, model=model2.imp, method = "bwAIC", p=0.05)
bsi2.i.p <- bootglm(bootsamp = a2.i, model=model2.imp, method = "p", p=0.05)
bsi2.i.lasso <- bootlasso(a2.i, model = model2.imp)

#example: model 3
act3 <- bootsamp(data=model3.act$data, R=2000)

bsiact3.aic <- bootglm(bootsamp = act3, model=model3.act, method = "bwAIC", p=0.05)
bsiact3.p <- bootglm(bootsamp = act3, model=model3.act, method = "p", p=0.05)
bsiact3.lasso <- bootlasso(bootsamp = act3, model=model3.act)


summary.bsi <- function(bsi="bsi",n=NULL, aic=T ){
  
  bsip <- eval(parse(text=paste(bsi,n, ".p$bsi_summary", sep = "")))
  lv_order <- bsip$var
  
  if(aic){
    bsiaic <- eval(parse(text=paste(bsi,n, ".aic", "$bsi_summary", sep = "")))
    lv <- match(lv_order, bsiaic$var)
  }
  bsilasso <- eval(parse(text=paste(bsi,n, ".lasso", "$bsi_summary", sep = "")))
  
  
  
  lvlasso <- match(lv_order[-1], bsilasso$var)
  
  if(aic){
    bsi1 <- cbind(bsip[,c(1:9)],
                  bsiaic[lv,][,4:11],
                  rbind(rep(NA,length.out=5), bsilasso[lvlasso,][,4:8])
    )
  }else{
    bsi1 <- cbind(bsip[,c(1:9)],
                  
                  rbind(rep(NA,length.out=5), bsilasso[lvlasso,][,4:8])
    )
  }
  
  return(bsi1)
}





bsi1 <- summary.bsi(n=NULL, aic = T)
write.table(bsi1, "bsi1.csv",sep = ",")
bsi1i <-summary.bsi(n=".i", aic = T)
write.table(bsi1i, "bsi1i.csv",sep = ",")

bsi2 <- summary.bsi(n="2", aic = F)
write.table(bsi2, "bsi2.csv",sep = ",")
bsi2i <- summary.bsi(n="2.i", aic = F)
write.table(bsi2i, "bsi2i.csv",sep = ",")


bsiact3 <- summary.bsi(n="act3", aic = T)
write.table(bsiact3, "bsi3.csv",sep = ",")
bsi3i <- summary.bsi(n="3.i", aic = T)
write.table(bsi3i, "bsi3i.csv",sep = ",")


level.name <- c("Intercept", "Active victimization: 1-2 times", "Active victimization: >1-2 times",
                "Relational victimization: 1-2 times", "Relational victimization: >1-2 times","Impulsivity: BIS",
                
                "AUDIT-C: >=4", "PHQ-9: 6-10","PHQ-9: 11-15", 
                "PHQ-9: >15", "MDSS","Gender: male", 
                "Smoking: yes", "Father's unemployment", "School PR", 
                "Mother's unemployment", "Living with parents: No",  "Self-esteem: RSES", 
                "Needy family: yes" 
)
names(level.name) <- names(bsiact3.p$boot_inclusion_freq)
names(bsiact3.p$boot_inclusion_freq) <- names(level.name)

a.test <- bootsamp(data=model1.imp$data, R=1000, subsampling = T, m=0.5)
bsi.test <- bootglm(bootsamp = a.test, model=model1.imp, method = "bwAIC", p=0.05)
