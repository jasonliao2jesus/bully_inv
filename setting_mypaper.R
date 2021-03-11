
#setting independent variables
vars_vector_all <- c("victim_3group_act","victim_3group_rel","q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                      "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                      "q22_0_a", "firstrses_standardized","bis1_standardized")


vars_vector<-c("q3_a","school_PR","q8_a","q7_a","q6_a","q5_a", #"q5.a",
               "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
               "q22_0_a", "firstrses_standardized","bis1_standardized")
add_vector <- c("bully_3group", "bully_3group_act", "firstyrbully2", "firstyr_bully2_act", "rec_bully_firstyr",
                "relationalb_3group", "physicalb_3group", "verbalb_3group")


vars_vector_victim_s<-c("victim_3group",
                        "bis1_standardized")


vars_vector_victim_a_s<-c("victim_3group_act",  "victim_3group_rel",
                          "bis1_standardized")

vars_v <- c("bully_3group", "q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                 "q68_a","PHQ9_1yr_new","AUDIT_C_a",
                 "q22_0_a", "firstrses_standardized","bis1_standardized")

#####
## predicting models: model and model 2.  missing data: excluded
set.seed(2020.10)
span=0.8
group= 20
validate.boot40= T
val.B= 2000
confusion.matrix= T
p.threshhold= 0.05
imputation=T
cut.off=0.5
correct= F


firstyrbully21_arg <-list(ind_var=vars_vector_victim_s, dep_var = "firstyrbully2", additional_var= add_vector,  data=dataset,  # additional_var ㄤ跑计
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix,
                          cut.off=cut.off,
                          p.threshhold= p.threshhold, 
                          correct= correct)

firstyrbully22_arg <-list(ind_var=vars_vector_victim_a_s, dep_var = "firstyr_bully2_act",additional_var= add_vector, data=dataset, 
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix, 
                          cut.off=cut.off,
                          p.threshhold= p.threshhold, 
                          correct= correct)

rec_b1_arg <- list(ind_var=vars_vector_victim_a_s, dep_var = "rec_bully_firstyr", additional_var= add_vector, data= dataset, 
                   imputation.mis.data=imputation,
                   span=span, group=group,
                   validate.boot40= validate.boot40, 
                   val.B=val.B, 
                   confusion.matrix= confusion.matrix,
                   cut.off=cut.off,
                   p.threshhold= p.threshhold,
                   correct= correct)

model1.s <- do.call(mypaper, firstyrbully21_arg)
model3.s<- do.call(mypaper, firstyrbully22_arg)
model2.s <-  do.call(mypaper,rec_b1_arg)

sum1 <- summary(model1.s)
sum2 <- summary(model2.s)
sum3 <- summary(model3.s)
tab3<- cbind( sum1[,1],
              Bullies =sum1[,c(2,3,4)], 
              Frequent_bullies= sum2[,c(2,3,4)],
              Active_bullies=sum3[,c(2,3,4)])

plot1.s <- plot(model1.s)
plot2.s <- plot(model2.s)
plot3.s <- plot(model3.s)
plot.s.all <- ggarrange(
  plot1.s,plot3.s,
  labels = c(
    "1.", "2."
  ),label.y = 1.015, legend = "right", common.legend = T, nrow = 1, 
  font.label = list(size=12, face="plain")
)

fig.2 <- annotate_figure(
  plot.s.all, top = text_grob("Calibration plot: before and after bootstrap-resampling bias correction", 
                            face = "plain", size = 12, hjust=0.45, vjust = 0.27),
  fig.lab = "Figure 2", fig.lab.face = "plain", 
  fig.lab.size = 12, fig.lab.pos= "top.left"
)
model3.s$ind_var

val.mod1.s <- validate.mypaper(model1.s)
val.mod2.s <- validate.mypaper(model2.s)
val.mod3.s <- validate.mypaper(model3.s)
val.all.s <- cbind( val.mod1.s, val.mod2.s, val.mod3.s)

cal.mod1.s <- calibrate.mypaper(model1.s,HLtest= F)
cal.mod2.s <- calibrate.mypaper(model2.s,HLtest= F)
cal.mod3.s <- calibrate.mypaper(model3.s,HLtest= F)
cal.all.s <-  cbind(cal.mod1.s$calibration.index,cal.mod2.s$calibration.index, cal.mod3.s$calibration.index)
auc.all.s <-  cbind(cal.mod1.s$discrimination.index,cal.mod2.s$discrimination.index, cal.mod3.s$discrimination.index)
index.all.s <- rbind(val.all.s, cal.all)


con.matrix1.s <- confusionMatrix(model1.s, cut.off = 0.5)
con.matrix2.s <- confusionMatrix(model2.s, cut.off = 0.5)
con.matrix3.s <- confusionMatrix(model3.s, cut.off = 0.5)

modeva <- function(models=model1.s, boot=T, R=500){
  bootglm <-  boot2.mypaper(data = models$data, 
                         method="glm", model = models, OOB = F, R = R)
  bootlasso <- boot2.mypaper(data = models$data, 
                           method="lasso", model = models, OOB = F, R = R)
  bootsvm <-  boot2.mypaper(data = models$data, 
                          method="svm", model = models, OOB = F, R = R)
  bootrf <- boot2.mypaper(data = models$data, 
                       method="randomforest", model = models, OOB = F, R = R)
  aucglm <- round(bootglm[,c(1,2)], digits = 3)
  auclasso <- round(bootlasso[,c(1,2)], digits = 3)
  aucsvm <- round(bootsvm[,c(1,2)], digits = 3)
  aucrf <- round(bootrf[,c(1,2)], digits = 3)

  d <- list(glm=aucglm, lasso=auclasso, svm=aucsvm, rf=aucrf
            )

  return(d)
}

performance.mod1.s <- modeva(R=2000)
write.table(performance.mod1.s, "test.csv",sep = ",") 
performance.mod3.s <- modeva(models = model3.s, R=2000)

write.table(performance.mod3.s, "test2.csv",sep = ",") 
performance.mod1r.s <- modeva(models=model1r.s, R=2000)
performance.mod3r.s <- modeva(models = model3r.s, R=2000)
####
vars_vector_victim<-c("victim_3group","q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                      "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                      "q22_0_a", "firstrses_standardized","bis1_standardized")
vars_vector_victim_act<-c("victim_3group_act","q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                          "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                          "q22_0_a", "firstrses_standardized","bis1_standardized")
set.seed(2020.10)
span=0.8
group= 20
validate.boot40= T
val.B= 2000
confusion.matrix= T
p.threshhold= 0.05
imputation=F
cut.off=0.5
correct= F




firstyrbully21_arg <-list(ind_var=c(vars_vector_victim,"a"), dep_var = "firstyrbully2", additional_var= add_vector,  data=dataset,  # additional_var ㄤ跑计
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix,
                          cut.off=cut.off,
                          p.threshhold= p.threshhold, 
                          correct= correct)

firstyrbully22_arg <-list(ind_var=vars_vector_victim_act, dep_var = "firstyr_bully2_act",additional_var= add_vector, data=dataset, 
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix, 
                          cut.off=cut.off,
                          p.threshhold= p.threshhold, 
                          correct= correct)

rec_b1_arg <- list(ind_var=vars_vector_victim, dep_var = "rec_bully_firstyr", additional_var= add_vector, data= dataset, 
                   imputation.mis.data=imputation,
                   span=span, group=group,
                   validate.boot40= validate.boot40, 
                   val.B=val.B, 
                   confusion.matrix= confusion.matrix,
                   cut.off=cut.off,
                   p.threshhold= p.threshhold,
                   correct= correct)

model1 <- do.call(mypaper, firstyrbully21_arg)
model3<- do.call(mypaper, firstyrbully22_arg)
model2 <-  do.call(mypaper,rec_b1_arg)

sum1 <- summary(model1.s)
sum2 <- summary(model2.s)
sum3 <- summary(model3.s)
tab3<- cbind( sum1[,1],
              Bullies =sum1[,c(2,3,4)], 
              Frequent_bullies= sum2[,c(2,3,4)],
              Active_bullies=sum3[,c(2,3,4)])

sum1 <- summary(model1)
sum2 <- summary(model2)
sum3 <- summary(model3)
tab3<- cbind( sum1[,1],
            Bullies =sum1[,c(2,3,4)], 
            Frequent_bullies= sum2[,c(2,3,4)],
            Active_bullies=sum3[,c(2,3,4)])
write.table(tab3, "table3.csv",sep = ",")

write.table(bsi.aic$bsi_summary, "bsiaic.csv",sep = ",")
write.table(bsi.p$bsi_summary, "bsip.csv",sep = ",")

plot1 <- plot(model1)
plot2 <- plot(model2)
plot3 <- plot(model3)


val.mod1 <- validate.mypaper(model1)
val.mod2 <- validate.mypaper(model2)
val.mod3 <- validate.mypaper(model3)
val.all <- cbind( val.mod1, val.mod2, val.mod3)

cal.mod1 <- calibrate.mypaper(model1,HLtest= F)
cal.mod2 <- calibrate.mypaper(model2,HLtest= F)
cal.mod3 <- calibrate.mypaper(model3,HLtest= F)
cal.all <-  cbind(cal.mod1$calibration.index,cal.mod2$calibration.index, cal.mod3$calibration.index)
auc.all <-  cbind(cal.mod1$discrimination.index,cal.mod2$discrimination.index, cal.mod3$discrimination.index)
index.all <- rbind(val.all, cal.all)


write.table(index.all, "model_index.csv",sep = ",")
write.table(auc.all, "auc.csv",sep = ",")

con.matrix1 <- confusionMatrix(model1)
con.matrix2 <- confusionMatrix(model2)
con.matrix3 <- confusionMatrix(model3)

plot.all <- ggarrange(
  plot1, plot2, plot3,
  labels = c(
    "A", "B","C"
  ),label.y = 1.015, legend = "right", common.legend = T, nrow = 1
)

fig_3 <- annotate_figure(
  plot.all, top = text_grob("Calibration plot: before and after bootstrap-resampling bias correction", 
                            face = "plain", size = 12, hjust=0.45, vjust = 0.27),
  fig.lab = "Figure 3", fig.lab.face = "bold", 
  fig.lab.size = 13, fig.lab.pos= "top.left"
)

## 
## predicting models: model and model 2.  missing data: imputated
set.seed(125)
span=0.8
group= 10
validate.boot40= F
val.B= 2000
confusion.matrix= T
p.threshhold= 0.05
imputation=T
cut.off=0.5
correct= F

vars_vector_victim<-c("victim_3group","q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                      "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                      "q22_0_a", "firstrses","bis1_total")
vars_vector_victim_act<-c("victim_3group_act","victim_3group_rel","q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                      "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                      "q22_0_a", "firstrses_standardized","bis1_total")

firstyrbully21_arg <-list(ind_var=vars_vector_victim, dep_var = "firstyrbully2", additional_var= add_vector,  data=dataset,  # additional_var ㄤ跑计
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix,
                          cut.off=cut.off,
                          p.threshhold= p.threshhold, 
                          correct= correct)

firstyrbully22_arg <-list(ind_var=vars_vector_victim_act, dep_var = "firstyr_bully2_act",additional_var= add_vector, data=dataset, 
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix, 
                          cut.off=cut.off,
                          p.threshhold= p.threshhold, 
                          correct= correct)

rec_b1_arg <- list(ind_var=vars_vector_victim, dep_var = "rec_bully_firstyr", additional_var= add_vector, data= dataset, 
                   imputation.mis.data=imputation,
                   span=span, group=group,
                   validate.boot40= validate.boot40, 
                   val.B=val.B, 
                   confusion.matrix= confusion.matrix,
                   cut.off=cut.off,
                   p.threshhold= p.threshhold,
                   correct= correct)



model1.woe <- do.call(mypaper, firstyrbully21_arg)
model3.woe<- do.call(mypaper, firstyrbully22_arg)
model2.woe <-  do.call(mypaper,rec_b1_arg)


sum1.imp <- summary(model1.imp)
sum2.imp <- summary(model2.imp)
sum3.imp <- summary(model3.imp)
tab3.imp<- cbind( #sum1.imp[,1],
              #Bullies =sum1.imp[,c(2,3,4)], 
              #Frequent_bullies= sum2.imp[,c(2,3,4)],
              Active_bullies=sum3.imp[,c(2,3,4)])
write.table(tab3.imp, "table3imp.csv",sep = ",")


plot1.imp <- plot(model1.imp)
plot2.imp <- plot(model2.imp)
plot3.imp <- plot(model3.imp)


val.mod1.imp <- validate.mypaper(model1.imp)
val.mod2.imp <- validate.mypaper(model2.imp)
val.mod3.imp <- validate.mypaper(model3.imp)
val.all.imp <- cbind( val.mod1.imp, val.mod2.imp, val.mod3.imp)

cal.mod1.imp <- calibrate.mypaper(model1.imp,HLtest= F)
cal.mod2.imp <- calibrate.mypaper(model2.imp,HLtest= F)
cal.mod3.imp <- calibrate.mypaper(model3.imp,HLtest= F)
cal.all.imp <-  cbind(cal.mod1.imp$calibration.index,cal.mod2.imp$calibration.index, cal.mod3.imp$calibration.index)
auc.all.imp <-  cbind(cal.mod1.imp$discrimination.index,cal.mod2.imp$discrimination.index, cal.mod3.imp$discrimination.index)
index.all.imp <- rbind(val.all.imp, cal.all.imp)


write.table(index.all.imp, "model_index_imp.csv",sep = ",")
write.table(auc.all.imp, "auc_imp.csv",sep = ",")

con.matrix1.imp <- confusionMatrix(model1.imp)
con.matrix2.imp <- confusionMatrix(model2.imp)
con.matrix3.imp <- confusionMatrix(model3.imp)

plot.all.imp <- ggarrange(
  plot1.imp,  plot3.imp,
  labels = c(
    "A", "C"
  ),label.y = 1.015, legend = "right", common.legend = T, nrow = 1
)

fig.2 <- annotate_figure(
  plot.all.imp, top = text_grob("Calibration plot: before and after bootstrapp-resamping correction", 
                            face = "plain", size = 12, hjust=0.45, vjust = 0.27),
  bottom = text_grob(paste("Groups =", sep = "", model1$group ), color = "blue",
                     hjust = 1, x = 1, face = "italic", size = 12),
  fig.lab = "Figure 2.", #fig.lab.face = "bold", 
  fig.lab.size = 12, fig.lab.pos= "top.left"
)





##

set.seed(125)
span=0.8
group= 20
validate.boot40= T
val.B= 2000
confusion.matrix= T
p.threshhold= 0.05
imputation=T
cut.off=0.5
correct= F

test_arg <- list(ind_var=bsi.aic$BIF_vars[1:2], dep_var = "firstyrbully2", additional_var= add_vector,  data=dataset,  # additional_var ㄤ跑计
                 imputation.mis.data=imputation,
                 span=span, group=group,
                 validate.boot40= validate.boot40, 
                 val.B=val.B, 
                 confusion.matrix= confusion.matrix,
                 cut.off=cut.off,
                 p.threshhold= p.threshhold, 
                 correct= correct)

model1.impt <- do.call(mypaper, test_arg)

calibrate.mypaper(model1.impt)
val1r <- validate.mypaper(model1.impt)
plot1r <- plot.mypaper(model1.impt)

##
set.seed(125)
span=0.8
group= 20
validate.boot40= T
val.B= 2000
confusion.matrix= T
p.threshhold= 0.05
imputation=T
cut.off=0.5
correct= F

test_arg <- list(ind_var=bsi2.aic$BIF_vars[1:2], dep_var = "rec_bully_firstyr", additional_var= add_vector,  data=dataset,  # additional_var ㄤ跑计
                 imputation.mis.data=imputation,
                 span=span, group=group,
                 validate.boot40= validate.boot40, 
                 val.B=val.B, 
                 confusion.matrix= confusion.matrix,
                 cut.off=cut.off,
                 p.threshhold= p.threshhold, 
                 correct= correct)

model2.impt <- do.call(mypaper, test_arg)

calibrate.mypaper(model2.impt)
val2r <- validate.mypaper(model2.impt)
plot2r <-  plot.mypaper(model2.impt)

##
set.seed(125)
span=0.8
group= 20
validate.boot40= T
val.B= 2000
confusion.matrix= T
p.threshhold= 0.05
imputation=T
cut.off=0.5
correct= F

test_arg <- list(ind_var=bsi3.p$BIF_vars[1:2], dep_var = "firstyr_bully2_act", additional_var= add_vector,  data=dataset,  # additional_var ㄤ跑计
                 imputation.mis.data=imputation,
                 span=span, group=group,
                 validate.boot40= validate.boot40, 
                 val.B=val.B, 
                 confusion.matrix= confusion.matrix,
                 cut.off=cut.off,
                 p.threshhold= p.threshhold, 
                 correct= correct)

model3.impt <- do.call(mypaper, test_arg)

calibrate.mypaper(model3.impt)
val3r <- validate.mypaper(model3.impt)
plot3r <- plot.mypaper(model3.impt)
##
set.seed(125)
span=0.8
group= 20
validate.boot40= T
val.B= 2000
confusion.matrix= T
p.threshhold= 0.05
imputation=F
cut.off=0.5
correct= F

firstyrbully21_arg <-list(ind_var=vars_vector_all, dep_var = "firstyrbully2",additional_var= add_vector, data=dataset, 
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix, 
                          cut.off=cut.off,
                          p.threshhold= p.threshhold, 
                          correct= correct)
model1.act <- do.call(mypaper, firstyrbully21_arg)



firstyrbully23_arg <-list(ind_var=vars_vector_all, dep_var = "firstyr_bully2_act",additional_var= add_vector, data=dataset, 
                          imputation.mis.data=T,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix, 
                          cut.off=cut.off,
                          p.threshhold= p.threshhold, 
                          correct= correct)
model3.act <- do.call(mypaper, firstyrbully23_arg)


rec_b1_arg <- list(ind_var=vars_vector_all, dep_var = "rec_bully_firstyr", additional_var= add_vector, data= dataset, 
                   imputation.mis.data=imputation,
                   span=span, group=group,
                   validate.boot40= validate.boot40, 
                   val.B=val.B, 
                   confusion.matrix= confusion.matrix,
                   cut.off=cut.off,
                   p.threshhold= p.threshhold,
                   correct= correct)
model2.act <- do.call(mypaper, rec_b1_arg)

################3

##
set.seed(125)
span=0.8
group= 20
validate.boot40= T
val.B= 2000
confusion.matrix= T
p.threshhold= 0.05
imputation=T
cut.off=0.5
correct= F

firstyrbully21_arg <-list(ind_var=vars_vector_all, dep_var = "firstyrbully2",additional_var= add_vector, data=dataset, 
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix, 
                          cut.off=cut.off,
                          p.threshhold= p.threshhold, 
                          correct= correct)
model1i.act <- do.call(mypaper, firstyrbully21_arg)




firstyrbully23_arg <-list(ind_var=vars_vector_all, dep_var = "firstyr_bully2_act",additional_var= add_vector, data=dataset, 
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix, 
                          cut.off=cut.off,
                          p.threshhold= p.threshhold, 
                          correct= correct)
model3i.act <- do.call(mypaper, firstyrbully23_arg)


rec_b1_arg <- list(ind_var=vars_vector_all, dep_var = "rec_bully_firstyr", additional_var= add_vector, data= dataset, 
                   imputation.mis.data=imputation,
                   span=span, group=group,
                   validate.boot40= validate.boot40, 
                   val.B=val.B, 
                   confusion.matrix= confusion.matrix,
                   cut.off=cut.off,
                   p.threshhold= p.threshhold,
                   correct= correct)
model2i.act <- do.call(mypaper, rec_b1_arg)

#
firstyrbully21_arg <-list(ind_var=vars_vector_victim_s, dep_var = "firstyrbully2", additional_var= add_vector,  data=model1$data,  # additional_var ㄤ跑计
                        imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix,
                          cut.off=cut.off,
                          p.threshhold= p.threshhold, 
                          correct= correct)

firstyrbully22_arg <-list(ind_var=vars_vector_victim_a_s, dep_var = "firstyr_bully2_act",additional_var= add_vector, data=model3$data, 
                          imputation.mis.data=F,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix, 
                          cut.off=cut.off,
                          
                          correct= correct)
model1r.s <- do.call(mypaper, firstyrbully21_arg)
model3r.s <- do.call(mypaper, firstyrbully22_arg)

#
firstyrvic_arg <-list(ind_var=vars_v, dep_var = "firstyrvic2", additional_var= NULL,  data=dataset,  # additional_var ㄤ跑计
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix,
                          cut.off=cut.off,
                          p.threshhold= p.threshhold, 
                          correct= correct)
modelv <- do.call(mypaper, firstyrvic_arg)

# Augmented BE
cri="alpha"
bsi1 <- abe.boot(model1.imp$model$global_model, data = model1.imp$data, type.boot = "bootstrap", exact = T, type.factor = "factor", num.boot = 2000, criterion = cri, alpha = 0.157)
bsi2 <- abe.boot(model2.imp$model$global_model, data = model2.imp$data, type.boot = "bootstrap", exact = T, type.factor = "factor", num.boot = 2000, criterion =  cri, alpha=0.157)
bsi3 <- abe.boot(model3.act$model$global_model, data = model3.act$data, type.boot = "bootstrap", exact = T, type.factor = "factor", num.boot = 2000, criterion = cri, alpha=0.157)



bsisum <- function(bsi=bsi1){
  bsisum <-summary(bsi)
  estimate <- data.frame(t(bsisum$var.coefs$'tau='))
  modelfrq <- data.frame(bsisum$model.rel.frequencies)
  colnames(modelfrq) <- c("model", "Freq")
  BIFtau <- data.frame(t(bsisum$var.rel.frequencies))
  BIF <- BIFtau[,1]
   names(BIF)<- rownames(BIFtau)
  bsisum <- cbind(BIF, estimate)
  bsisum <- list(bsi= bsisum[order(bsisum$BIF, decreasing = T),], modelfrq=modelfrq)
  return(bsisum)
}
        
bsi1sum <- bsisum(bsi1)
bsi2sum <- bsisum(bsi2)
bsi3sum <- bsisum(bsi3)          

