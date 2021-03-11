

#setting independent variables
vars_vector_victim<-c("victim_3group","q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
               "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
               "q22_0_a", "firstrses_standardized","bis1_standardized")
vars_vector_victim_act<-c("victim_3group_act","q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                      "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                      "q22_0_a", "firstrses_standardized","bis1_standardized")
vars_vector<-c("q3_a","school_PR","q8_a","q7_a","q6_a","q5_a", #"q5.a",
                     # "q68_a",#"PHQ9_1yr_new","AUDIT_C_4_a",
                      "q22_0_a", "firstrses_standardized","bis1_standardized")
# setting working data frames : 
#  goal: comparison group= non-involved group 

datat2=dataset


dataset_table2<-datat2%>%filter(firstyrpurev=="킠")
dataset_table3<-datat2%>%filter(firstyr_pureb=="no")
dataset_bv_vs_noninvolved<-datat2%>%filter(firstyrpureb=="킠" & firstyrpurev=="킠")
dataset_pureb<-datat2%>%filter(firstyrvic2=="킠")
dataset_purev<-datat2%>%filter(firstyr_bully2=="no") 
dataset_table2_rec<-datat2%>%filter(firstyrpurev=="킠")
dataset_table3_rec<-datat2%>%filter(firstyrpureb=="킠")






#create arguments for all comparison(bullies vs. non-involved; victims vs non-involved;... etc.  )


firstyrbully2_arg <-list(ind_var=vars_vector, dep_var = "firstyrbully2", data= dataset_table2,  imputation.mis.data=imputation,
                         span=span, group=group,
                         validate.boot40= validate.boot40, 
                         val.B=val.B, 
                         confusion.matrix= confusion.matrix,
                         cut.off=cut.off,
                         p.threshhold= p.threshhold, 
                         correct= correct)
firstyrvic2_arg <- list(ind_var=vars_vector, dep_var = "firstyrvic2",data= dataset_table3,  imputation.mis.data=imputation,
                        span=span, group=group,
                        validate.boot40= validate.boot40, 
                        val.B=val.B, 
                        confusion.matrix= confusion.matrix,
                        cut.off=cut.off,
                        p.threshhold= p.threshhold, 
                        correct= correct)
bv_arg <- list(ind_var=vars_vector, dep_var = "firstyrbv", data=dataset_bv_vs_noninvolved,  imputation.mis.data=imputation,
               span=span, group=group,
               validate.boot40= validate.boot40, 
               val.B=val.B, 
               confusion.matrix= confusion.matrix,
               cut.off=cut.off,
               p.threshhold= p.threshhold, 
               correct= correct)
dataset$physica
pureb_arg <- list(ind_var=vars_vector, dep_var = "firstyrbully2", data=dataset_pureb,  imputation.mis.data=imputation,
                  span=span, group=group,
                  validate.boot40= validate.boot40, 
                  val.B=val.B, 
                  confusion.matrix= confusion.matrix,
                  cut.off=cut.off,
                  p.threshhold= p.threshhold, 
                  correct= correct)
purev_arg <- list(ind_var=vars_vector, dep_var = "firstyrvic2", data=dataset_purev,  imputation.mis.data=imputation,
                  span=span, group=group,
                  validate.boot40= validate.boot40, 
                  val.B=val.B, 
                  confusion.matrix= confusion.matrix,
                  cut.off=cut.off,
                  p.threshhold= p.threshhold, 
                  correct= correct)
rec_b_arg <- list(ind_var=vars_vector, dep_var = "rec_bully_firstyr",data= dataset_table2_rec,  imputation.mis.data=imputation,
                  span=span, group=group,
                  validate.boot40= validate.boot40, 
                  val.B=val.B, 
                  confusion.matrix= confusion.matrix,
                  cut.off=cut.off,
                  p.threshhold= p.threshhold, 
                  correct= correct)
rec_v_arg <- list(ind_var=vars_vector, dep_var = "rec_victim_firstyr",data= dataset_table3_rec,  imputation.mis.data=imputation,
                  span=span, group=group,
                  validate.boot40= validate.boot40, 
                  val.B=val.B, 
                  confusion.matrix= confusion.matrix,
                  cut.off=cut.off,
                  p.threshhold= p.threshhold, 
                  correct= correct)

modb <- do.call(mypaper, firstyrbully2_arg)
modv<- do.call(mypaper, firstyrvic2_arg)
modbv <- do.call(mypaper, bv_arg)
modpb <- do.call(mypaper, pureb_arg)
modpv <- do.call(mypaper, purev_arg)
modrb <-  do.call(mypaper,rec_b_arg)
modrv <-  do.call(mypaper,rec_v_arg)

sumb <- summary(modb)
sumv <- summary(modv)
sumbv <- summary(modbv)
sumpb <- summary(modpb)
sumpv <- summary(modpv)
sumrb <- summary(modrb)

table2 <-  list(bullies= sumb[,c(1,2,3)],
                victims= sumv[,c(2,3)],
                bully_victims= sumbv[,c(2,3)],
                pure_bullies= sumpb[,c(2,3)],
                pure_victims= sumpv[,c(2,3)],
                repeated_bullies= sumrb[,c(2,3)])
write.table(table2, "table2imp.csv",sep = ",")



## create data

OR2_firstyrbully <- do.call(creat_table2, firstyrbully2_arg)
OR2_firstyrvic  <- do.call(creat_table2, firstyrvic2_arg)
OR2_bv <- do.call(creat_table2, bv_arg)
OR2_pureb <- do.call(creat_table2, pureb_arg)
OR2_purev <- do.call(creat_table2,purev_arg)
OR2_rec_b <- do.call(creat_table2, rec_b_arg)
OR2_rec_v <- do.call(creat_table2,rec_v_arg)



table2 <-  list(bullies= OR2_firstyrbully,
                victims= OR2_firstyrvic[,c(2,3)],
                bully_victims= OR2_bv[,c(2,3)],
                pure_bullies= OR2_pureb[,c(2,3)],
                pure_victims= OR2_purev[,c(2,3)],
                repeated_bullies= OR2_rec_b[,c(2,3)],
                repeated_victims= OR2_rec_v[,c(2,3)])




write.table(table2, "table2.csv",sep = ",")


table2imp <-  list(bullies= OR2_firstyrbully,
                   victims= OR2_firstyrvic[,c(2,3)],
                   bully_victims= OR2_bv[,c(2,3)],
                   pure_bullies= OR2_pureb[,c(2,3)],
                   pure_victims= OR2_purev[,c(2,3)],
                   repeated_bullies= OR2_rec_b[,c(2,3)],
                   repeated_victims= OR2_rec_v[,c(2,3)])




write.table(table2imp, "table2imp.csv",sep = ",")


## predicting models: model and model 2.  missing data: excluded
set.seed(125)
span=0.8
group= 20
validate.boot40= T
val.B= 2000
confusion.matrix= F
p.threshhold= 0.05
imputation=T
cut.off=0.5
correct= F


firstyrbully21_arg <-list(ind_var=vars_vector_victim, dep_var = "firstyrbully2", data=dataset, 
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix,
                          cut.off=cut.off,
                          p.threshhold= p.threshhold, 
                          correct= correct)
OR21_firstyrbully <- do.call(creat_table2, firstyrbully21_arg)
plot.a <-  plot.model.glm
calibration.auc.mod1 <- calibration.auc
confusion.matrix.mod1 <- conf.matrix



firstyrbully22_arg <-list(ind_var=vars_vector_victim_act, dep_var = "firstyr_bully2_act", dataset, 
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix, 
                          cut.off=cut.off,
                          p.threshhold= p.threshhold, 
                          correct= correct)
OR21_firstyrbully_act <- do.call(creat_table2, firstyrbully22_arg)
plot.c <-  plot.model.glm
calibration.auc.mod3 <- calibration.auc
confusion.matrix.mod3 <- conf.matrix



rec_b1_arg <- list(ind_var=vars_vector_victim, dep_var = "rec_bully_firstyr", dataset, 
                   imputation.mis.data=imputation,
                   span=span, group=group,
                   validate.boot40= validate.boot40, 
                   val.B=val.B, 
                   confusion.matrix= confusion.matrix,
                   cut.off=cut.off,
                   p.threshhold= p.threshhold,
                   correct= correct)
OR21_rec_b <- do.call(creat_table2, rec_b1_arg)
plot.b <-  plot.model.glm
calibration.auc.mod2 <- calibration.auc
confusion.matrix.mod2 <- conf.matrix




table3 <-  list(bullies= OR21_firstyrbully[,c(1,2,3)],
                act_bullies=OR21_firstyrbully_act[,c(2,3)],
                repeated_bullies= OR21_rec_b[,c(2,3)])

a <- cbind( table3$bullies[1],
            Bullies =table3$bullies[,c(2,3)], 
            Frequent_bullies=table3$repeated_bullies[,c(1,2)],
            Active_bullies=table3$act_bullies[,c(1,2)])

write.table(a, "table3.csv",sep = ",")

tab.mod.eval <-  list(bullies = list( calibration.auc.mod1[["discrimination.index"]],
                                      calibration.auc.mod1[["val.boot.index"]] ,
                                      calibration.auc.mod1[["calibration.index"]]
                                      ),
                      frequent.bullies = list( calibration.auc.mod2[["discrimination.index"]],
                                           calibration.auc.mod2[["val.boot.index"]] ,
                                          calibration.auc.mod2[["calibration.index"]]
                                 ),
                      active.bullies = list( calibration.auc.mod3[["discrimination.index"]],
                                             calibration.auc.mod3[["val.boot.index"]] ,
                                             calibration.auc.mod3[["calibration.index"]] 
                                             )
                    )


                      
                      

plot.all <- ggarrange(
  plot.a, plot.b, plot.c,
  labels = c(
    "A", "B","C"
  ),label.y = 1.015, legend = "right", common.legend = T, nrow = 1
)

fig.5 <- annotate_figure(
  plot.all, top = text_grob("Calibration plot: before and after bootstrapp-resamping correction", 
                         face = "plain", size = 12, hjust=0.45, vjust = 0.27
  ),
  fig.lab = "Figure 5", fig.lab.face = "bold", 
  fig.lab.size = 13, fig.lab.pos= "top.left"
)

##  missing data filled with multiple imputation by chained equations. 
seed(124)
span=0.8
group= 20
validate.boot40= T
val.B= 40
confusion.matrix= T
p.threshhold= 0.05
imputation=T
cut.off=0.5


firstyrbully21_arg <-list(ind_var=vars_vector_victim, dep_var = "firstyrbully2", data=dataset, 
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix,
                          cut.off=cut.off,
                          p.threshhold= p.threshhold)
OR21_firstyrbully <- do.call(creat_table2, firstyrbully21_arg)
plot.bully <-  plot.model.glm
calibration.auc.mod1 <- calibration.auc
confusion.matrix.mod1 <- conf.matrix
mod1 <- model.lrm.boot
boot.plot.actbully <- boot.cal.plot(mod1,B=val.B)
plot.a <- plot.bully +  geom_line(data=boot.cal.df[[1]],   aes(x=predy, y=Predicted_Probability, col = Type,
), size=1)+ grids(axis = "xy")+ 
  geom_rug(data=boot.cal.df, mapping = aes(x= predy), stat = "identity",
                                         position = "identity",alpha=0.2, sides = "t") 


firstyrbully22_arg <-list(ind_var=vars_vector_victim_act, dep_var = "firstyr_bully2_act", dataset, 
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix, 
                          cut.off=cut.off,
                          p.threshhold= p.threshhold)
OR21_firstyrbully_act <- do.call(creat_table2, firstyrbully22_arg)
plot.actbully <-  plot.model.glm
calibration.auc.mod3 <- calibration.auc
confusion.matrix.mod3 <- conf.matrix
mod3 <- model.lrm.boot
boot.plot.actbully <- boot.cal.plot(mod3,B=val.B)
plot.c <- plot.actbully +  geom_line(data=boot.cal.df,   
                                     aes(x=predy, y=Predicted_Probability, col = Type,), 
                                     size=1)+ grids(axis = "xy")+
  geom_rug(data=boot.cal.df, mapping = aes(x= predy), stat = "identity",
           position = "identity",alpha=0.2, sides = "t") 

rec_b1_arg <- list(ind_var=vars_vector_victim, dep_var = "rec_bully_firstyr", dataset, 
                   imputation.mis.data=imputation,
                   span=span, group=group,
                   validate.boot40= validate.boot40, 
                   val.B=val.B, 
                   confusion.matrix= confusion.matrix,
                   cut.off=cut.off,
                   p.threshhold= p.threshhold)
OR21_rec_b <- do.call(creat_table2, rec_b1_arg)
plot.recbully <-  plot.model.glm
calibration.auc.mod2 <- calibration.auc
confusion.matrix.mod2 <- conf.matrix
mod2 <- model.lrm.boot
boot.plot.recbully <- boot.cal.plot(mod2,B=val.B)
plot.b <- plot.recbully + geom_line(data=boot.cal.df,   
                                    aes(x=predy, y=Predicted_Probability, col = Type), 
                                    size=1)+ grids(axis = "xy")+  
          geom_rug(data=boot.cal.df, mapping = aes(x= predy), stat = "identity",
           position = "identity",alpha=0.2, sides = "t") 





table3 <-  list(bullies= OR21_firstyrbully[,c(1,2,3)],
                act_bullies=OR21_firstyrbully_act[,c(2,3)],
                repeated_bullies= OR21_rec_b[,c(2,3)])

b <- cbind( table3$bullies[1],
            Bullies =table3$bullies[,c(2,3)], 
            Frequent_bullies=table3$repeated_bullies[,c(1,2)],
            Active_bullies=table3$act_bullies[,c(1,2)])

write.table(b, "table3_imp.csv",sep = ",")

tab.mod.eval.imp <-  list(bullies = list( calibration.auc.mod1[["discrimination.index"]],
                                      calibration.auc.mod1[["val.boot.index"]] ,
                                      calibration.auc.mod1[["calibration.index"]]
),
frequent.bullies = list( calibration.auc.mod1[["discrimination.index"]],
                         calibration.auc.mod1[["val.boot.index"]] ,
                         calibration.auc.mod1[["calibration.index"]]
),
active.bullies = list( calibration.auc.mod1[["discrimination.index"]],
                       calibration.auc.mod1[["val.boot.index"]] ,
                       calibration.auc.mod1[["calibration.index"]] 
)
)





plot.all.imp <- ggarrange(
  plot.a, plot.b, plot.c,
  labels = c(
    "A", "B","C"
  ),label.y = 1.015, legend = "right", common.legend = T, nrow = 1
)

fig.5.imp <- annotate_figure(
  plot.all.imp, top = text_grob("Calibration plot: before and after bootstrapp-resamping correction", 
                            face = "plain", size = 12, hjust=0.45, vjust = 0.27
  ),
  fig.lab = "Figure 5", fig.lab.face = "bold", 
  fig.lab.size = 13, fig.lab.pos= "top.left"
)




