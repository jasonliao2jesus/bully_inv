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





dataset_sec_yr_testing_selected <- dataset_sec_yr_testing[,selected_variables]

# NULL model for the Calibration plot
model_1 <-  glm(formula = firstyrbully2 ~ 1,
                                   
                                    
                                 family = binomial(), data =  dataset_selected%>%filter(!is.na(q5_a))
                )

# model 2: p<0.05
model_2 <- glm(formula = firstyrbully2 ~ victim_3group+q3_a + 
                 school_PR+ q5_a+q68_a+
                PHQ9_1yr_new+ AUDIT_C_4_a+ firstrses_standardized+ q22_0_a+
                bis1_standardized, 
              family = binomial(), data =  dataset_selected%>%filter(!is.na(q5_a))
              )
# model 3  +RSES
model_3 <- glm(formula = firstyrbully2 ~ victim_3group+q3_a +
                 PHQ9_1yr_new+ AUDIT_C_4_a+ firstrses_standardized+
                 bis1_standardized, 
               family = binomial(), data =  dataset_selected%>%filter(!is.na(q5_a)))

#model 4: 去掉PHQ9
model_4 <- glm(formula = firstyrbully2 ~ victim_3group+q3_a +q5_a+ AUDIT_C_4_a+
                 bis1_standardized, 
               family = binomial(), data =  dataset_selected)
# model 5: 去掉性別
model_5 <- glm(formula = firstyrbully2 ~ victim_3group +q5_a+ AUDIT_C_4_a+
                 bis1_standardized, 
               family = binomial(), data =  dataset_selected)


model_2_rec <- glm(formula= rec_bully_firstyr~ victim_3group+
                     q3_a+ firstrses+ 
                     AUDIT_C_4_a+ bis1_standardized, family=binomial(), data=dataset_selected)
model_1_rec <- glm(formula= rec_bully_firstyr~1,family=binomial(), data=dataset_selected)







# figure S4
dom_model_2 <- dominanceanalysis::dominanceAnalysis(model_2)
plot(dom_model_2, fit.function="r2.m",  which.graph='conditional')

#VIF
vif_model_2 <- vif(model_2)
write.table(vif_model_2, "vif.csv",sep = ",")

# the predicted probabilities calculated according to the model (length共708組數據)
predicted_pro_1 <- predict(model_1,type="response")
predicted_pro_2 <- predict(model_2, type = "response")
predicted_pro_3 <- predict(model_3, type = "response")
predicted_pro_4 <- predict(model_4, type = "response")
predicted_pro_5 <- predict(model_5, type = "response")

predicted_pro_2_logit <- predict(model_2, type = "link")
#
predicted_pro_2_rec <-  predict(model_2_rec, type = "response")
predicted_pro_1_rec <- predict(model_1_rec, type ="response")

# specify range of x-axis and y-axis
rangeaxis <- c(0,1)
# specify number of groups for Hosmer-Lemeshow test
groups <- 12



# Calibration plot
plotCalibration(data=dataset_selected%>%filter(!is.na(q5_a)), #  如果model_1含q5_a, 要把dataset_selected 中q5_a==NA去掉 
                                                            #  因為model也去掉了q5_a==NA...
                cOutcome=3,  # firstyrbully_1or0 是 dataset_selected[,3] 
                predRisk=predicted_pro_2,
                groups=groups, rangeaxis=rangeaxis)


plotCalibration(data=dataset_selected, #  如果model_1含q5_a, 要把dataset_selected 中q5_a==NA去掉 
                #  因為model也去掉了q5_a==NA...
                cOutcome=6,  # firstyrbully_1or0 是 dataset_selected[,3] 
                predRisk=predicted_pro_2_rec,
                groups=groups, rangeaxis=rangeaxis)





#
# obtain predicted risks

pred1<-data.frame(predicted_pro_1)
pred2<-data.frame(predicted_pro_2)
pred3<-data.frame(predicted_pro_3)
pred4<-data.frame(predicted_pro_4)
pred5<-data.frame(predicted_pro_5)

pred2_logit <- data.frame(predicted_pro_2_logit)

pred6 <- data.frame(predicted_pro_1_rec)
pred7 <- data.frame(predicted_pro_2_rec)

#pred2_rec<-data.frame(predicted_pro_2_rec)

data1<-cbind(dataset_selected%>%filter(!is.na(q5_a)), cbind(pred1, pred2, pred3, pred4, pred5))
names(data1)


data_rec<-cbind(dataset_selected, cbind(pred6, pred7))
names(data_rec)

ggplot(data1, aes(pred2_logit, bis1_total))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() 

# default: two-sided test
roc1 <- roc(data1$firstyrbully_1or0, data1$predicted_pro_1)
roc2 <- roc(data1$firstyrbully_1or0, data1$predicted_pro_2)
roc3 <- roc(data1$firstyrbully_1or0, data1$predicted_pro_3)
roc4 <- roc(data1$firstyrbully_1or0, data1$predicted_pro_4)
roc5 <- roc(data1$firstyrbully_1or0, data1$predicted_pro_5)

roc_rec <- roc(data_rec$rec_bully_1or0, data_rec$predicted_pro_2_rec)
roc_rec_null <- roc(data_rec$rec_bully_1or0, data_rec$predicted_pro_1_rec)

roc.test(roc1, roc2, method="delong")
roc.test(roc1, roc3, method="delong")
roc.test(roc1, roc4, method="delong")
roc.test(roc2, roc3, method="delong")
roc.test(roc2, roc4, method="delong")
roc.test(roc3, roc4, method="delong")
roc.test(roc2, roc5, method="delong")

roc.test(roc4, roc5, method="delong")

roc.test(roc_rec, roc_rec_null, method="delong")

ci.auc(roc1, method="delong")  # AUC的95%信賴區間
ci.auc(roc2, method="delong")
ci.auc(roc3, method="delong")  # AUC的95%信賴區間
ci.auc(roc4, method="delong")
ci.auc(roc5, method="delong")



# specify labels for the groups without and with the outcome of interest
labels <- c("No bullying", ">1-2times")
# produce discrimination box plot 
plotDiscriminationBox(data=dataset_selected%>%filter(!is.na(q5_a)), 
                      cOutcome=3, 
                      predrisk=predicted_pro_2, 
                      labels=labels)






#下面是 Validation跟Resampling calibration，尚未寫成文章

# model 2: p<0.05
model_2 <- lrm( firstyrbully2 ~ victim_3group+q3_a + school_PR+q5_a+
                 PHQ9_1yr_new+ q68_a+AUDIT_C_4_a+ firstrses+
                 bis1_total, 
               x=TRUE, y=TRUE, data =  dataset_selected)
# model 3  +RSES
model_3 <- lrm( firstyrbully2 ~ victim_3group+q3_a +q5_a+ school_PR+
                 PHQ9_1yr_new+ AUDIT_C_4_a+ firstrses+
                 bis1_total, 
                x=TRUE, y=TRUE, data =  dataset_selected)

#model 4: 去掉PHQ9
model_4 <- lrm(firstyrbully2 ~ victim_3group+q3_a +q5_a+ AUDIT_C_4_a+
                 bis1_total, 
               x=TRUE, y=TRUE, data =  dataset_selected)
# model 5: 去掉性別 父親工作
model_5 <- lrm(firstyrbully2 ~ victim_3group + q5_a+ AUDIT_C_4_a+
                 bis1_standardized, 
               x=TRUE, y=TRUE, data =  dataset_selected)



model_2_rec <- lrm( rec_bully_1or0 ~ victim_3group+q3_a +  AUDIT_C_4_a+ firstrses+
                      bis1_total, 
                    x=TRUE, y=TRUE, data =  dataset_selected)



set.seed(1)
a <- validate(model_2, B=2000)  # (1+Dxy)/2=AUC
# AUC1=(index.orig+1)/2=AUC of original data, 
# AUC2=(training+1)/2= AUC of 2000 resampling
# AUC3=(test+1)/2 ....


#number of repetitions. For method="crossvalidation", 
#is the number of groups of omitted observations
#default number of repetitions: 40
# K crossvalidation
#set.seed(1)
#validate(model_2, xval=10, B=40)

set.seed(1)
a <- validate(model_2, B=2000) 
cal <- calibrate(model_2, method="boot", B=2000) # usually B=200 or 300
plot(cal)

set.seed(2)
a_rec <- validate(model_2_rec, B=2000)  
cal_rec <- calibrate(model_2_rec, method="boot", B=2000) # usually B=200 or 300
plot(cal_rec)




cal_risk <- function( sex="男",school_level=1,father_job="沒有", 
                      smoking="否", PHQ9_level=1, AUDIT_C_4="<4",
                      MDSS= 20,
                      victim_3group_level= 1, firstrses_standardized=25, bis1_standardized=0  ,model=model_2, data=dataset)
{
  
  
  data_test <- data.frame(q3_a= sex, school_PR=levels(data$school_PR)[[school_level]], q5_a= father_job, 
             q68_a= smoking, PHQ9_1yr_new= levels(data$PHQ9_1yr_new)[[PHQ9_level]], AUDIT_C_4_a=  AUDIT_C_4,
             q22_0_a= MDSS,
             victim_3group=  levels(data$victim_3group)[[victim_3group_level]], firstrses_standardized=firstrses_standardized, bis1_standardized=bis1_standardized)
  
  predict(model,data_test, type="response")
  
  
}

  
  data.frame(q3_a="男", school_PR="<70", q5_a="沒有", 
                        q68_a="沒有", PHQ9_1yr_new="6-10", AUDIT_C_4_a="<4",
                        q22_0_a= 20,
                        victim_3group="1or2_times", firstrses=25, bis1_standardized=60)

