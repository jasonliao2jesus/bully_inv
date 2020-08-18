# the data frame for the Calibration plot
selected_variables <- c("firstyrbully2","victim_3group", "firstyrbully_1or0", 
                        "bully_3group","bully_inv",
                        "rec_bully_1or0","rec_bully_act_1or0",
                        "firstyrvic2","rec_victim_firstyr","victim_3group_act","firstyr_bully2_act", "firstyrbully_1or0_act",
                        "rec_bully_firstyr", "rec_bully_firstyr_act", 
                        "physicalbully", "verbalbully", "relationalbully", "otherbully",
                        "physicalvictim", "verbalvictim", "relationalvictim",  
                        "physicalb_3group","verbalb_3group","relationalb_3group",
                       # "bully_sec_3group","victim_sec_3group","secyr_bully2","secyr_vic2",
                        
                        
                        "q3_a","q4_a","q5_a","q6_a","q7_a","q8_a", "q10_a", "school_PR","q22_a",
                        "q68_a","AUDIT_C_4_a", "AUDIT_C_a", "PHQ9_1yr_new", "q31_0_a", "q22_0_a", 
                        "parent1_MDSS", "teacher_MDSS",
                        "peer1", "firstrses", "firstrses_standardized",
                        "bis1_total", "bis1_standardized", 
                        
                        "bis1_inability_to_plan", "bis1_novelty_seeking", "bis1_lack_of_selfcontrol",
                        "bis1_inability_to_plan_standardized", "bis1_novelty_seeking_standardized", "bis1_lack_of_selfcontrol_standardized"
)
dataset_selected <- dataset[,selected_variables]


# the data frame for the Calibration plot
selected_variables <- c("firstyrbully2","victim_3group", 
                        
                        
                        "q3_a",#"q5_a",
                        "q68_a","AUDIT_C_4_a",  "PHQ9_1yr_new", "q22_0_a", 
                       
                        "bis1_total"
)
dataset_selected <- dataset[,selected_variables]

# the data frame for the Calibration plot
selected_variables <- c("firstyrbully2","victim_3group", "firstyrbully_1or0", 
                        "bully_3group",#"bully_inv",
                        #"rec_bully_1or0","rec_bully_act_1or0",
                        "firstyrvic2","rec_victim_firstyr",
                        "rec_bully_firstyr", #"rec_bully_firstyr_act", 
                        "physicalbully", "verbalbully", "relationalbully", "otherbully",
                        "physicalvictim", "verbalvictim", "relationalvictim",  
                        #"physicalb_3group","verbalb_3group","relationalb_3group",
                        #"bully_sec_3group","victim_sec_3group",
                        #"secyr_bully2","secyr_vic2",
                        
                        
                        "q3_a","q4_a","q5_a","q6_a","q7_a","q8_a", "q10_a", "school_PR","q22_a",
                        "q68_a","AUDIT_C_4_a", "AUDIT_C_a", "PHQ9_1yr_new", "q31_0_a", "q22_0_a", 
                        "parent1_MDSS", "teacher_MDSS",
                        "peer1", "firstrses", "firstrses_standardized",
                        "bis1_total", "bis1_standardized", 
                        
                        "bis1_inability_to_plan", "bis1_novelty_seeking", "bis1_lack_of_selfcontrol",
                        "bis1_inability_to_plan_standardized", "bis1_novelty_seeking_standardized", "bis1_lack_of_selfcontrol_standardized"
)



dataset_sec_yr_testing_selected <- dataset_sec_yr_testing[,selected_variables]

# NULL model for the Calibration plot
model_1 <-  glm(formula = firstyrbully2 ~ 1,
                                   
                                    
                                 family = binomial(), data =  dataset_selected_imp
                )

# model 2: p<0.05
model_2 <- glm(formula = firstyrbully2 ~ victim_3group+
                 q3_a + 
                 q5_a+
                 q68_a+
                PHQ9_1yr_new+ 
                 AUDIT_C_4_a+ 
              
                bis1_standardized, 
              family = binomial(),data =  dataset_selected_imp #%>%filter(!is.na(q5_a))
              )


              
# model 3  +RSES
model_3 <- glm(formula = firstyrbully2 ~ victim_3group+q3_a + school_PR+ q5_a+q68_a+
                 PHQ9_1yr_new+ AUDIT_C_4_a+q22_0_a+ firstrses_standardized+
                 bis1_standardized, 
               family = binomial(), data =  dataset_selected_imp
               )

#model 4: 少bis
model_4 <- glm(formula = firstyrbully2 ~ victim_3group+q3_a +q5_a+q68_a+
                 PHQ9_1yr_new+ AUDIT_C_4_a+ q22_0_a, 
               family = binomial(), data =  dataset_selected_imp
               )
# model 5: 只剩victim bis
model_5 <- glm(formula = firstyrbully2 ~ victim_3group + 
                 bis1_standardized, 
               family = binomial(), data =  dataset_selected_imp
               )
# model 6



model_21 <- glm(formula = firstyr_bully2_act ~ victim_3group_act+
                 q3_a + 
                 school_PR+ 
                 q5_a+
                 q68_a+
                 PHQ9_1yr_new+ 
                 AUDIT_C_4_a+ 
                 q22_0_a+
                 bis1_standardized, 
               family = binomial(), method = "brglmFit",
               data =  dataset #%>%filter(!is.na(q5_a))
)




model_2_rec <- glm(formula= rec_bully_firstyr~ victim_3group+
                     q3_a+  
                     AUDIT_C_4_a+ bis1_standardized, family=binomial(), data=dataset)
model_1_rec <- glm(formula= rec_bully_firstyr~1,family=binomial(), data=dataset_selected)


model=model_2
predicted_pro_2 <- predict(model, newdata=dataset_selected_imp, type = "response")
pred2<-data.frame(predicted_pro_2)
data1<-cbind(dataset_selected_imp, cbind( pred2))
roc2 <- roc(data1$firstyrbully_1or0, data1$predicted_pro_2)



# calibration
plotCalibration(data=dataset_selected_imp, #  如果model_1含q5_a, 要把dataset_selected 中q5_a==NA去掉 
                #  因為model也去掉了q5_a==NA...
                cOutcome=12,  # firstyrbully_1or0 是 dataset_selected[,3] 
                predRisk=pred_pro_s,
                groups=12, rangeaxis=c(0,1))






# figure S4
dom_model_2 <- dominanceanalysis::dominanceAnalysis(model_2)
plot(dom_model_2, fit.function="r2.m",  which.graph='conditional')

#VIF
vif_model_2 <- vif(model_2)
write.table(vif_model_2, "vif.csv",sep = ",")

# the predicted probabilities calculated according to the model (length共708組數據)
predicted_pro_1 <- predict( model_1, newdata=dataset_selected_imp, type="response")
predicted_pro_2 <- predict(model_2, newdata=dataset_selected_imp, type = "response")
predicted_pro_3 <- predict(model_3, newdata=dataset_selected_imp, type = "response")
predicted_pro_4 <- predict(model_4, newdata=dataset_selected_imp, type = "response")
predicted_pro_5 <- predict(model_5, newdata=dataset_selected_imp, type = "response")
predicted_pro_21 <- predict(model_21, newdata=dataset_selected_imp, type = "response")
predicted_pro_rec<- predict(model_2_rec, newdata=dataset_selected_imp, type = "response")
predicted_lasso<- predict(glm.out, newx= x,type="response",s=0.03662)

# specify range of x-axis and y-axis
rangeaxis <- c(0,1)
# specify number of groups for Hosmer-Lemeshow test
groups <- 12

cOutcome <- match("firstyrbully_1or0", selected_variables)

# Calibration plot



plotCalibration(data=dataset_selected_imp, #  如果model_1含q5_a, 要把dataset_selected 中q5_a==NA去掉 
                                                            #  因為model也去掉了q5_a==NA...
                cOutcome=3,  # firstyrbully_1or0 是 dataset_selected[,3] 
                predRisk= predicted_lasso,
                groups=groups, rangeaxis=rangeaxis)


plotCalibration(data=dataset_selected, #  如果model_1含q5_a, 要把dataset_selected 中q5_a==NA去掉 
                #  因為model也去掉了q5_a==NA...
                cOutcome=6,  # firstyrbully_1or0 是 dataset_selected[,3] 
                predRisk=predicted_pro_2_rec,
                groups=groups, rangeaxis=rangeaxis)

plotCalibration(data=dataset_sec_yr_testing, #  如果model_1含q5_a, 要把dataset_selected 中q5_a==NA去掉 
                #  因為model也去掉了q5_a==NA...
                cOutcome=48,  # firstyrbully_1or0 是 dataset_selected[,3] 
                predRisk=predicted_sec,
                groups=groups, rangeaxis=rangeaxis)

Y= data1$rec_bully_1or0
P= data1$predicted_pro_rec

loess.calibrate <- loess(Y~P )
 P.calibrate <- predict(loess.calibrate, newdata=P)
 ICI <- mean(abs(P.calibrate-P))
 E50 <- median(abs(P.calibrate-P))
 E90 <- quantile(abs(P.calibrate-P))
 Emax <- max(abs(P.calibrate-P))
 cal.in <- c(ICI=ICI, E50=E50, E90=E90, Emax=Emax)
 
# obtain predicted risks

pred1<-data.frame(predicted_pro_1)
pred2<-data.frame(predicted_pro_2)
pred3<-data.frame(predicted_pro_3)
pred4<-data.frame(predicted_pro_4)
pred5<-data.frame(predicted_pro_5)
pred21<-data.frame(predicted_pro_21)
predlasso<-data.frame(predicted_lasso)
pred_rec <- data.frame(predicted_pro_rec)


x <- model.matrix(firstyrbully2 ~ victim_3group+
                    q3_a + 
                    school_PR+ 
                    q5_a+
                    q6_a+
                    q7_a+
                    q8_a+
                  
                    q68_a+
                    PHQ9_1yr_new+ 
                    AUDIT_C_4_a+ 
                    q22_0_a+
                    bis1_standardized+
                    firstrses_standardized, dataset_selected_imp)
library(glmnet)
yp <- predicted.mypaper(model1.imp)  
cv.glm.out <- cv.glmnet(x, yp$Observed, weights = NULL, offset = NULL, lambda = NULL,
                        type.measure = "auc",
                      nfolds = 10, foldid = NULL, alignment = c("lambda","fraction"), grouped = TRUE, keep = FALSE, parallel = FALSE,
                        gamma = c(0, 0.25, 0.5, 0.75, 1), relax = FALSE, trace.it = 1)
glm.out <- glmnet(x, yp$Observed, family = "binomial", alpha = 1, lambda = cv.glm.out$lambda.1se)
predicted_lasso<- predict(glm.out, newx= x,type="response",s=glm.out$lambda)
predicted_lasso <- as.numeric(predicted_lasso)


lasso.boot <- bootLasso(x, y, B = 500, type.boot = "residual", alpha = 0.05, 
          cv.method = "cv", nfolds = 10, foldid, cv.OLS = FALSE, tau = 0, 
          parallel = FALSE, standardize = TRUE, intercept = TRUE, 
          parallel.boot = FALSE, ncores.boot = 1)




pred2_logit <- data.frame(predicted_pro_2_logit)

pred6 <- data.frame(predicted_pro_1_rec)
pred7 <- data.frame(predicted_pro_2_rec)

#pred2_rec<-data.frame(predicted_pro_2_rec)

data1<-cbind(dataset_selected_imp, cbind(pred1, pred2, pred3, pred4, pred5, predlasso))
names(data1)

data2 <- cbind(dataset_sec_yr_testing, cbind(pred_test))


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
roc21 <- roc(data1$firstyr_bully2_act, data1$predicted_pro_21)
rocs <- roc(data1$firstyr_bully2_act, data1$pred_pro_s)



roc_test <- roc(data2$firstyrbully_1or0,data2$predicted_test) 

roc_rec <- roc(data1$rec_bully_1or0, data1$predicted_pro_2_rec)
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
ci.auc(roc21, method="delong")


r=c(0.34, 0.3, 0.38)
round(exp((2*r*pi/sqrt(1-r^2))/sqrt(3)), digits = 2)

d=c(0.0139, 0.3, 0.38)
round(exp((d*pi/sqrt(3)), digits = 2))

#下面是 Validation跟Resampling calibration，尚未寫成文章

# model 2: p<0.05
model_2 <- lrm( firstyrbully2 ~ victim_3group+
                  q3_a + 
                  q5_a+
                  q68_a+
                  PHQ9_1yr_new+ 
                  AUDIT_C_4_a+ 
                  q22_0_a+
                  bis1_standardized, model = T, 
               x=TRUE, y=TRUE, data =  dataset)
# model 3  +RSES
model_3 <- lrm( firstyrbully2 ~ victim_3group+q3_a +q5_a+ school_PR+
                 PHQ9_1yr_new+ AUDIT_C_4_a+ 
                 bis1_standardized, model = T, 
                x=TRUE, y=TRUE, data =  dataset)

#model 4: 去掉PHQ9
model_4 <- lrm(firstyrbully2 ~ victim_3group+q3_a +q5_a+ AUDIT_C_4_a+
                 bis1_total, model = T, 
               x=TRUE, y=TRUE, data =  dataset)
# model 5: 去掉性別 父親工作
model_5 <- lrm(firstyrbully2 ~ victim_3group*
                 bis1_standardized, model = T, 
                data =  dataset)

model_21 <- lrm(firstyr_bully2_act ~ victim_3group_act+
                  q3_a + 
                  school_PR+ 
                  q5_a+
                  q68_a+
                  PHQ9_1yr_new+ 
                  AUDIT_C_4_a+ 
                   q22_0_a+
                  bis1_standardized, model = T, 
               x=TRUE, y=TRUE, data =  dataset)


model_2_rec <- lrm( rec_bully_firstyr ~ victim_3group+q3_a +  AUDIT_C_4_a+ 
                      bis1_standardized, model = T,  
                    x=TRUE, y=TRUE, data =  model2.imp$data)



set.seed(1)
# (1+Dxy)/2=AUC
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
a <- validate(model_2, B=200) 
cal <- calibrate(model_2, method="boot", B=200) # usually B=200 or 300
plot(cal)

set.seed(2)
a_rec <- validate(model_2_rec, B=200)  
cal_rec <- calibrate(model_2_rec, method="boot", B=200) # usually B=200 or 300
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

 
  

