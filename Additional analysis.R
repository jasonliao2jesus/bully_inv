# model 1: bully: all relevent variables
dataset_sec_glm_binomial <-  glm(formula = firstyrbully2 ~ victim_3group+q3_a + q5_a + school_PR + 
                                   PHQ9_1yr_new +AUDIT_C_4_a + 
                                   bis1_total, 
                                 family = binomial(), data =  dataset)
summary(dataset_sec_glm_binomial)
exp(coef(dataset_sec_glm_binomial))
pR2(dataset_sec_glm_binomial)

# model 1: bully: best performance
dataset_sec_glm_binomial <-  glm(formula = firstyrbully2 ~ victim_3group+
                                   q3_a+q5_a+AUDIT_C_4_a+
                                   bis1_total, 
                                 family = binomial(), data 
                                 =  dataset)
summary(dataset_sec_glm_binomial)
exp(coef(dataset_sec_glm_binomial))
pR2(dataset_sec_glm_binomial)

# model 1: Firth correction
dataset_sec_glm_binomial_firth <-  glm(formula = firstyrbully2 ~ victim_3group+q3_a + q5_a + school_PR + 
                                         PHQ9_1yr_new +AUDIT_C_4_a + 
                                         teacher_MDSS+parent1_MDSS+peer1  + 
                                         bis1_standardized, 
                                       family =  binomial(), method =  "brglmFit", data =  dataset)
# 將上述model odds ratio 視覺化 依順序排列
ggcoef(dataset_sec_glm_binomial_firth, exponentiate = TRUE , conf.int = TRUE,
       conf.level = 0.95, exclude_intercept = FALSE,
       vline = TRUE, vline_intercept = "auto", vline_color = "gray50",
       vline_linetype = "dotted", vline_size = 1, errorbar_color = "gray25",
       errorbar_height = 0.3, errorbar_linetype = "solid", errorbar_size = 0.5,
       sort = c("ascending")) #+ scale_y_discrete("1","2","3","4","5","6","7","8","9","10","intercept")

# Predict the probability (p) of bullying perpetration
  probabilities <- predict(dataset_sec_glm_binomial, type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

  # Select only numeric predictors
  mydata <- dataset_selected %>%
  dplyr::select_if(is.numeric) 
  predictors <- colnames(mydata)
  # Bind the logit and tidying the data for plot
  mydata <- mydata %>%
    mutate(logit = log(probabilities/(1-probabilities))) %>%
    gather(key = "predictors", value = "predictor.value", -logit)

  ggplot(mydata, aes(logit, predictor.value)) +
    geom_smooth(method = "loess") + 
    geom_point( size = 0.5, alpha = 0.5)+
    theme_bw() + 
    facet_wrap(~predictors, scales = "free_y")

# find variable importance
importance <- varImp(dataset_sec_glm_binomial, scale=FALSE)
importance

anova(dataset_sec_glm_binomial, test="Chisq")

glm.diag.plots(dataset_sec_glm_binomial)
 
#model 2:  bully BIS subscale 
 dataset_sub <-  glm(formula = firstyrbully2 ~ victim_3group+q3_a + q5_a + q22_a+ school_PR + 
                                    PHQ9_1yr_new +AUDIT_C_4_a + 
                                    parent1_MDSS + teacher_MDSS+ peer1 + firstrses + 
                          bis1_inability_to_plan+bis1_lack_of_selfcontrol+ bis1_novelty_seeking, 
                          family = binomial(), data = dataset_selected_imp)
 summary(dataset_sub)
 exp(coef(dataset_sub))
 pR2(dataset_sub) 
 
# model 3: victim 
 dataset_table3_glm_binomial <-  glm(formula = firstyrvic2 ~ bully_3group+q3_a+q5_a+q7_a+ q8_a+school_PR +
                                       q68_a + PHQ9_1yr_new  +AUDIT_C_4_a+
                                     parent1_MDSS+ teacher_MDSS+peer1+firstrses+
                                       bis1_total, 
                                     family = binomial(), data = dataset_selected_imp)
 summary(dataset_table3_glm_binomial)
 exp(coef(dataset_table3_glm_binomial))
 pR2(dataset_table3_glm_binomial)
 
# model 4
 dataset_table3_glm_binomial <-  glm(formula = firstyrvic2 ~ bully_3group+q3_a+q5_a+q7_a+ q8_a+school_PR +
                                       q68_a + PHQ9_1yr_new  +AUDIT_C_4_a+
                                     parent1_MDSS+ teacher_MDSS+peer1+firstrses+
                                       bis1_inability_to_plan+bis1_lack_of_selfcontrol+ bis1_novelty_seeking, 
                                     family = binomial(), data = dataset_selected_imp)
 summary(dataset_table3_glm_binomial)
 exp(coef(dataset_table3_glm_binomial))
 pR2(dataset_table3_glm_binomial)

 # model 1 exclude passive bullying
 
 dataset_sec_glm_binomial <-  glm(formula = firstyrbully2 ~ victim_3group+q3_a + q5_a + q22_a+ school_PR + 
                                    PHQ9_1yr_new +AUDIT_C_4_a + 
                                    parent1_MDSS + teacher_MDSS+ peer1 + firstrses + 
                                    bis1_total, 
                                  family = binomial(), data =  dataset_sec_yr_testing_imp%>%filter(otherbully=="無"& relationalbully=="無"))
 summary(dataset_sec_glm_binomial)
 exp(coef(dataset_sec_glm_binomial))
 pR2(dataset_sec_glm_binomial)

#recursive partition analysis 
firstyrbully<- rpart(firstyrbully2~ firstyrvic2+ q3_a+q5_a+school_PR +q22_a+ q68_a + PHQ9_1yr_new  +AUDIT_C_4_a+
                     parent1_MDSS+teacher_MDSS+peer1+firstrses+bis1_total, data= dataset)
plot(firstyrbully, uniform=TRUE, margin=0.1, branch= 0.5, compress=TRUE)
text(firstyrbully, use.n= TRUE)

#check multicollinearity if vif （Variance inflation factor, VIF，大於10有共線性問題）
vif(dataset_sec_glm_binomial)


# k-fold cross validation: internal
cv.glm(dataset_selected_imp, dataset_sec_binomial, cost= function(r, pi = 0) mean(abs(r-pi) > 0.5), K=10)$delta

# k-fold cross validation: external
cv.glm(dataset_sec_yr_testing_imp, dataset_table2_glm_binomial, cost=function(r, pi = 0) mean(abs(r-pi) > 0.5), K=10)$delta
# accuracy 1yr sensitivity and specificity
threshold=0.5
predicted_values<-ifelse(predict(dataset_sec_glm_binomial,type="response")>threshold,1,0)
actual_values<-dataset_sec_glm_binomial$y
conf_matrix<-table(predicted_values,actual_values)
caret::confusionMatrix(conf_matrix, positive = "1")


#sensitivity(conf_matrix)
#specificity(conf_matrix)
#accuracy <- (conf_matrix[1,1]+conf_matrix[2,2])/(conf_matrix[1,1]+conf_matrix[2,2]+conf_matrix[2,1]+conf_matrix[1,2])
#accuracy

predicted_prob<- predict(dataset_sec_glm_binomial, type="response")
predicted_prob<-sort (predicted_prob, decreasing = TRUE) 
predicted_values_sort<- ifelse(predicted_prob>threshold,1,0)


 
# accuracy 2y
 threshold=0.3
 dataset_sec_yr_testing <- mutate(dataset_sec_yr_testing, y = ifelse(firstyrbully2=="有", 1, 0))
 predicted_values<-ifelse(predict(dataset_sec_glm_binomial,type="response",dataset_sec_yr_testing)>threshold,1,0)
 actual_values<-dataset_sec_yr_testing$y
 conf_matrix<-table(predicted_values,actual_values)
 caret::confusionMatrix(  conf_matrix, positive = "1")

threshold=0.36
dataset_sec_yr_testing <- mutate(dataset_sec_yr_testing, y = ifelse(firstyrbully2=="有", 1, 0))
predicted_values<-ifelse(predict(dataset_sec_glm_binomial,type="response",dataset_sec_yr_testing)>threshold,1,0)
actual_values<-dataset_sec_yr_testing$y
conf_matrix<-table(predicted_values,actual_values)
caret::confusionMatrix(  conf_matrix, positive = "1")
#

dataset_nopurev <- dataset%>%filter(firstyrvic=="無")
dataset_sec_yr_testing_nopurev <- dataset_sec_yr_testing%>%filter(firstyrvic2=="無")


# ROC and AUC 1st yr
plot.new()
prob <- predict(model_2, type="response")
dataset_selected$prob <- prob
g <- roc(firstyrbully2~prob, data=dataset_selected%>%filter(!is.na(q5_a)), percent=TRUE,
         
         ci=TRUE, print.auc=TRUE)
ciobj<- ci.se(g, # CI of sensitivity
               
               specificities=seq(0, 100, 5)) # over a select set of specificities

# plot as a blue shape

#pROC::auc(g)  
 # ROC and AUC: 2nd yr 
 prob2 <- predict(model_2_rec, type="response")
 dataset_selected$prob2 <- prob2
 g_2 <- roc(rec_bully_firstyr~prob, data=dataset_selected, percent=TRUE,
            
            ci=TRUE, print.auc=TRUE)
 ciobj_2 <- ci.se(g_2, # CI of sensitivity
                
                specificities=seq(0, 100, 5)) # over a select set of specificities
 plot.roc(g_2, add=FALSE, reuse.auc=TRUE, of="thresholds", # compute AUC (of threshold)
          
          thresholds= 0.5, # select the (best) threshold
          
          print.thres= 0.5)
 plot(ciobj_2, type="shape", col=rgb(0,0.5,0.88,alpha=0.9)) # plot as a blue shape
 plot.roc(g, add=TRUE, reuse.auc=TRUE, of="thresholds", # compute AUC (of threshold)
          
          thresholds= 0.5, # select the (best) threshold
          
          print.thres= 0.5
          )
 plot(ciobj, type="shape", col=rgb(1,1,0,alpha=0.55)) 
 
 pROC::auc(g_2)
 
 

 
 
# dataset_null
 dataset_null <-  glm(formula = firstyrbully2 ~ 1, 
                      family = binomial(), data = dataset_selected)
 

 # g 第一年與null model比較
 prob <- predict(dataset_null,dataset_selected, type="response")
 dataset$selected$prob <- prob
 g_null <- roc(firstyrbully2~prob, data=dataset_selected) 
 
 testobj_1 <- roc.test( g, g_null, boot.n=1000) 
 
 
 # g 第二年與null model比較 
 prob <- predict(dataset_null, type="response")
 dataset_sec_yr_testing$prob <- prob
 g_2_null <- roc(firstyrbully2~prob, data=dataset_sec_yr_testing)
 
 testobj_2 <- roc.test( g_2, g_2_null, boot.n=1000)  
 
 
 
 text(100, 80, labels=paste("p-value =", format.pval(testobj_1$p.value,eps = 0.000001)), adj=c(0, .5))
 
 text(40, 70, labels=paste("p-value =", format.pval(testobj_2$p.value,eps = 0.000001)), adj=c(0, .5))
 
 
 
 
 
 #accuracy null hypothesis 2: victim only predict bullying perpetration, 
 dataset_b_v <-  glm(formula = firstyrbully2 ~ victim_3group, 
                     family = binomial(), data = dataset_selected)
 summary(dataset_b_v)
 exp(coef(dataset_b_v))
 pR2(dataset_b_V)
 
 threshold=0.5
 predicted_values<-ifelse(predict(dataset_b_v,type="response")>threshold,1,0)
 actual_values<-dataset_b_v$y
 conf_matrix<-table(predicted_values,actual_values)
 conf_matrix
 sensitivity(conf_matrix)
 specificity(conf_matrix)
 
 threshold=0.5
 predicted_values<-ifelse(predict(dataset_b_v,type="response",dataset_sec_yr_testing_imp)>threshold,1,0)
 actual_values<-dataset_b_v$y
 conf_matrix<-table(predicted_values,actual_values)
 conf_matrix
 sensitivity(conf_matrix)
 specificity(conf_matrix)
 
 prob <- predict(dataset_b_v, type="response")
 dataset_selected_imp$prob <- prob
 g_sub <- roc(firstyrbully2~prob, data=dataset_selected_imp)
 plot(g_sub)
 auc(g_sub)
 
 prob <- predict(dataset_b_v, type="response")
 dataset_sec_yr_testing_imp$prob <- prob
 g_2_b_v <- roc(firstyrbully2~prob, data=dataset_sec_yr_testing_imp)
 plot(g_2_b_v)
 auc(g_2_b_v)
 
 # accuracy bis sub
 threshold=0.5
 predicted_values<-ifelse(predict(dataset_sub,type="response")>threshold,1,0)
 actual_values<-dataset_sub$y
 conf_matrix<-table(predicted_values,actual_values)
 conf_matrix
 sensitivity(conf_matrix)
 specificity(conf_matrix)
 
 prob <- predict(dataset_sub, type="response")
 dataset_selected_imp$prob <- prob
 g_sub <- roc(firstyrbully2~prob, data=dataset_selected_imp)
 plot(g_sub)
 auc(g_sub)
 
 prob <- predict(dataset_sub, type="response")
 dataset_sec_yr_testing_imp$prob <- prob
 g_sub_2 <- roc(firstyrbully2~prob, data=dataset_sec_yr_testing_imp)
 plot(g__sub_2)
 auc(g_sub_2)
 
 #accuracy: external validation  去掉relational bully
 prob <- predict(dataset_sec_glm_binomial, dataset_no_relationalbully,type="response")
 dataset_no_relationalbully$prob <- prob
 g_2 <- roc(firstyrbully2~prob, data=dataset_no_relationalbully)
 
 plot(g_2)
 auc(g_2)
 

# build testing dataset
testing_variables2 <- c(
                         "firstyrbully2", "firstyrvic2",
                         "secondyrbully2","secondvic2", "bully_sec_3group", "victim_sec_3group", "rec_victim_secyr", "rec_bully_secyr", "secondyrpurev", "secondyrpureb",
                         "secyr_bully2" ,"secyr_vic2", "secyr_pureb", "secyr_purev","secyr_bv",
                         "physicalbully_sec", "verbalbully_sec", "relationalbully_sec", "otherbully_sec",
                         
                         "q3_b","q4_b","q5_b","q6_b","q7_b","q8_b", "q10_b", "school_PR","q22_b",
                         "q68_b","AUDIT_C_4_b", "AUDIT_C_b", "PHQ9_2yr", "q31_0_b", "q22_0_b", "parent2_MDSS", "teacher2_MDSS","peer2", "secondtrses", 
                         "bis2_total", "bis2_standardized", 
                         "bis2_inability_to_plan", "bis2_novelty_seeking", "bis2_lack_of_selfcontrol",
                         "bis2_inability_to_plan_standardized", "bis2_novelty_seeking_standardized", "bis2_lack_of_selfcontrol_standardized"
)

dataset_sec_yr_testing <- dataset_sec_yr[,testing_variables2]

dataset_sec_yr_testing<- dataset_sec_yr_testing%>% dplyr::rename(
  "pastyrbully2"="firstyrbully2","pastyrvic2"="firstyrvic2",
  "firstyrbully2"="secondyrbully2","firstyrvic2"="secondvic2", "bully_3group"="bully_sec_3group", "victim_3group"="victim_sec_3group", 
  "rec_victim_firstyr"="rec_victim_secyr", "rec_bully_firstyr"="rec_bully_secyr", "firstyrpurev"="secondyrpurev", "firstyrpureb"="secondyrpureb",
                    "firstyr_bully2"= "secyr_bully2","firstyr_vic2"="secyr_vic2", "firstyr_pureb"="secyr_pureb", 
  "firstyr_purev"="secyr_purev","firstyr_bv"="secyr_bv",
  "physicalbully"="physicalbully_sec", "verbalbully"="verbalbully_sec", "relationalbully"="relationalbully_sec", "otherbully"="otherbully_sec",
                                                                
            "q3_a"="q3_b","q4_a"="q4_b","q5_a"="q5_b","q6_a"="q6_b","q7_a"="q7_b","q8_a"="q8_b", "q10_a"="q10_b", "school_PR"="school_PR","q22_a"="q22_b",
            "q68_a"="q68_b","AUDIT_C_4_a"="AUDIT_C_4_b", "AUDIT_C_a"="AUDIT_C_b", "PHQ9_1yr_new"="PHQ9_2yr", "q31_0_a"="q31_0_b", "q22_0_a"="q22_0_b",
            "parent1_MDSS"= "parent2_MDSS","teacher_MDSS"="teacher2_MDSS",  "peer1"="peer2", "firstrses"="secondtrses", 
                "bis1_total"="bis2_total", "bis1_standardized"="bis2_standardized", 
                 "bis1_inability_to_plan"="bis2_inability_to_plan", "bis1_novelty_seeking"="bis2_novelty_seeking", "bis1_lack_of_selfcontrol"="bis2_lack_of_selfcontrol",
                  "bis1_inability_to_plan_standardized"= "bis2_inability_to_plan_standardized",
         "bis1_novelty_seeking_standardized"="bis2_novelty_seeking_standardized", "bis1_lack_of_selfcontrol_standardized"="bis2_lack_of_selfcontrol_standardized")

dataset_sec_yr_testing <- mutate(dataset_sec_yr_testing, bully_inv= ifelse(firstyr_bv=="yes", "3_bv", ifelse(firstyr_pureb=="yes", "4_pureb", ifelse(firstyr_purev=="yes", "2_purev", "1_not_inv"))))

dataset_sec_yr_testing<-mutate(dataset_sec_yr_testing, firstyrbully_1or0= ifelse(firstyrbully2=="有",1,0 ))

tempData <- mice(dataset_sec_yr_testing, m=5)
dataset_sec_yr_testing_imp <- mice::complete(tempData)

dataset_sec_yr_table2_testing<-dataset_sec_yr_testing_imp%>%filter(firstyr_purev=="no")
dataset_sec_yr_table3_testing<-dataset_sec_yr_testing_imp%>%filter(firstyr_pureb=="no")
dataset_sec_yr_bv_vs_noninvolved_testing<-dataset_sec_yr_testing_imp%>%filter(firstyr_pureb=="no" & firstyr_purev=="no")
dataset_sec_yr_pureb_testing<-dataset_sec_yr_testing_imp%>%filter(firstyr_vic2=="no")
dataset_sec_yr_purev_testing<-dataset_sec_yr_testing_imp%>%filter(firstyr_bully2=="no")
dataset_sec_yr_v_testing<-dataset_sec_yr_testing_imp%>%filter(firstyr_vic2=="yes")
dataset_sec_yr_b_testing<-dataset_sec_yr_testing_imp%>%filter(firstyr_bully2=="yes")

dataset_sec_yr_testing_ordinal_imp <- dataset_sec_yr_testing_imp
dataset_sec_yr_testing_ordinal_imp$bully_3group <- ordered(dataset_sec_yr_testing_ordinal_imp$bully_3group, levels= c("no_bullying", "1or2_times", "repeated_bullying"))
dataset_sec_yr_testing_ordinal_imp$victim_3group <- ordered(dataset_sec_yr_testing_ordinal_imp$victim_3group, levels= c("not_being_bullied", "1or2_times", "repeated_being_bullied"))

#active bullying testing dataset

testing_variables2_act <- c(
  "firstyr_bully2_act", "firstyrvic2",
  "secyr_bully2_act","secondvic2", "bully_sec_3group_act", "victim_sec_3group", "rec_victim_secyr", "rec_bully_secyr_act", "secondyrpurev", 
 "secyr_vic2",  "secyr_purev","secyr_bv",
  "physicalbully_sec", "verbalbully_sec", "relationalbully_sec", "otherbully_sec",
  
  "q3_b","q4_b","q5_b","q6_b","q7_b","q8_b", "q10_b", "school_PR","q22_b",
  "q68_b","AUDIT_C_4_b", "AUDIT_C_b", "PHQ9_2yr", "q31_0_b", "q22_0_b", "parent2_MDSS", "teacher2_MDSS","peer2", "secondtrses", 
  "bis2_total", "bis2_standardized", 
  "bis2_inability_to_plan", "bis2_novelty_seeking", "bis2_lack_of_selfcontrol",
  "bis2_inability_to_plan_standardized", "bis2_novelty_seeking_standardized", "bis2_lack_of_selfcontrol_standardized"
)

dataset_sec_yr_testing_act <- dataset_sec_yr[,testing_variables2_act]

dataset_sec_yr_testing_act<- dataset_sec_yr_testing_act%>% dplyr::rename(
  "pastyrbully2"="firstyr_bully2_act","pastyrvic2"="firstyrvic2",
  "firstyrvic2"="secondvic2", "bully_3group_act"="bully_sec_3group_act", "victim_3group"="victim_sec_3group", 
  "rec_victim_firstyr"="rec_victim_secyr", "rec_bully_firstyr"="rec_bully_secyr_act", "firstyrpurev"="secondyrpurev", 
  "firstyr_bully2_act"= "secyr_bully2_act","firstyr_vic2"="secyr_vic2", 
  "firstyr_purev"="secyr_purev",
  
  
  "q3_a"="q3_b","q4_a"="q4_b","q5_a"="q5_b","q6_a"="q6_b","q7_a"="q7_b","q8_a"="q8_b", "q10_a"="q10_b", "school_PR"="school_PR","q22_a"="q22_b",
  "q68_a"="q68_b","AUDIT_C_4_a"="AUDIT_C_4_b", "AUDIT_C_a"="AUDIT_C_b", "PHQ9_1yr_new"="PHQ9_2yr", "q31_0_a"="q31_0_b", "q22_0_a"="q22_0_b",
  "parent1_MDSS"= "parent2_MDSS","teacher_MDSS"="teacher2_MDSS",  "peer1"="peer2", "firstrses"="secondtrses", 
  "bis1_total"="bis2_total", "bis1_standardized"="bis2_standardized", 
  "bis1_inability_to_plan"="bis2_inability_to_plan", "bis1_novelty_seeking"="bis2_novelty_seeking", "bis1_lack_of_selfcontrol"="bis2_lack_of_selfcontrol",
  "bis1_inability_to_plan_standardized"= "bis2_inability_to_plan_standardized",
  "bis1_novelty_seeking_standardized"="bis2_novelty_seeking_standardized", "bis1_lack_of_selfcontrol_standardized"="bis2_lack_of_selfcontrol_standardized")

#dataset_sec_yr_testing <- mutate(dataset_sec_yr_testing, bully_inv= ifelse(firstyr_bv_act=="yes", "2_bv", ifelse(firstyr_pureb_act=="yes", "1_pureb", ifelse(firstyr_purev=="yes", "3_purev", "4_not_inv"))))

dataset_selected_imp$first

tempData <- mice(dataset_sec_yr_testing_act, m=5)
dataset_sec_yr_testing_act_imp <- mice::complete(tempData)

# training data and testing data
smp_size <- floor(0.75 * nrow(mtcars))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

train <- dataset[train_ind, ]
test <- dataset[-train_ind, ]

#

