

#setting independent variables
vars_vector_victim<-c("victim_3group","q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                      "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                      "q22_0_a", "firstrses_standardized","bis1_standardized")
vars_vector_victim_act<-c("victim_3group_act","q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                          "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                          "q22_0_a", "firstrses_standardized","bis1_standardized")
vars_vector<-c("q3_a","school_PR","q8_a","q7_a","q6_a","q5_a", #"q5.a",
               "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
               "q22_0_a", "firstrses_standardized","bis1_standardized")
add_vector <- c("bully_3group", "bully_3group_act", "firstyrbully2", "firstyr_bully2_act", "rec_bully_firstyr",
                "relationalb_3group", "physicalb_3group", "verbalb_3group")

vars_vector_victim_s<-c("victim_3group","q3_a","q5_a",#"q5.a",
                      "q68_a","AUDIT_C_4_a",
                      "bis1_standardized")

## predicting models: model and model 2.  missing data: excluded
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

test_arg <- list(ind_var=vars_vector_victim_s, dep_var = "firstyrbully2", additional_var= add_vector,  data=dataset,  # additional_var 加上其他變數
                 imputation.mis.data=imputation,
                 span=span, group=group,
                 validate.boot40= validate.boot40, 
                 val.B=val.B, 
                 confusion.matrix= confusion.matrix,
                 cut.off=cut.off,
                 p.threshhold= p.threshhold, 
                 correct= correct)
  


firstyrbully21_arg <-list(ind_var=vars_vector_victim, dep_var = "firstyrbully2", additional_var= add_vector,  data=dataset,  # additional_var 加上其他變數
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

model.test <- do.call(mypaper, test_arg)

sum1 <- summary(model1)
sum2 <- summary(model2)
sum3 <- summary(model3)
tab3<- cbind( sum1[,1],
            Bullies =sum1[,c(2,3,4)], 
            Frequent_bullies= sum2[,c(2,3,4)],
            Active_bullies=sum3[,c(2,3,4)])
write.table(tab3, "table3.csv",sep = ",")


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

fig.5 <- annotate_figure(
  plot.all, top = text_grob("Calibration plot: before and after bootstrap-resampling bias correction", 
                            face = "plain", size = 12, hjust=0.45, vjust = 0.27),
  fig.lab = "Figure 5", fig.lab.face = "bold", 
  fig.lab.size = 13, fig.lab.pos= "top.left"
)

## 
## predicting models: model and model 2.  missing data: imputated
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


firstyrbully21_arg <-list(ind_var=vars_vector_victim, dep_var = "firstyrbully2", additional_var= add_vector,  data=dataset,  # additional_var 加上其他變數
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

model1.imp <- do.call(mypaper, firstyrbully21_arg)
model3.imp<- do.call(mypaper, firstyrbully22_arg)
model2.imp <-  do.call(mypaper,rec_b1_arg)

sum1.imp <- summary(model1.imp)
sum2.imp <- summary(model2.imp)
sum3.imp <- summary(model3.imp)
tab3.imp<- cbind( sum1.imp[,1],
              Bullies =sum1.imp[,c(2,3,4)], 
              Frequent_bullies= sum2.imp[,c(2,3,4)],
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
  plot1.imp, plot2.imp, plot3.imp,
  labels = c(
    "A", "B","C"
  ),label.y = 1.015, legend = "right", common.legend = T, nrow = 1
)

fig.5.imp <- annotate_figure(
  plot.all.imp, top = text_grob("Calibration plot: before and after bootstrapp-resamping correction", 
                            face = "plain", size = 12, hjust=0.45, vjust = 0.27),
  bottom = text_grob(paste("Groups =", sep = "", model1$group ), color = "blue",
                     hjust = 1, x = 1, face = "italic", size = 10),
  fig.lab = "Figure 5", fig.lab.face = "bold", 
  fig.lab.size = 13, fig.lab.pos= "top.left"
)



set.seed(125)
span=0.8
group= 20
validate.boot40= T
val.B= 40
confusion.matrix= T
p.threshhold= 0.05
imputation=F
cut.off=0.5
correct= F


##




