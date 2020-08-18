
##SVM
model_s <- e1071::svm(formula= firstyrbully2 ~ victim_3group+
                        q3_a + 
                        school_PR+ 
                        q5_a+
                        q68_a+
                        PHQ9_1yr_new+ 
                        AUDIT_C_4_a+ 
                        q22_0_a+
                        bis1_standardized, data=dataset_selected_imp, probability=TRUE, scale=FALSE, kernal="radial basis", epsilon=0.5, cost=0.09
)
predicted_s <- predict(object= model_s, newdata=dataset_selected_imp, probability=TRUE)

pred_pro_s <- attr(predicted_s, "probabilities")[,2]

preds1<-data.frame(pred_pro_s)
data1<-cbind(data1, cbind( preds1))
#data1$predicted_s<- factor(data1$predicted_s, levels= c("無", "有"), ordered = T)


rocs <- roc(data1$firstyrbully2, data1$pred_pro_s)





####SVM model for active bullying
model_s <- e1071::svm(formula= firstyr_bully2_act ~ victim_3group_act+
                        q3_a + 
                        school_PR+ 
                        q5_a+
                        q68_a+
                        PHQ9_1yr_new+ 
                        AUDIT_C_4_a+ 
                        q22_0_a+
                        bis1_standardized, data=dataset_selected_imp, probability=TRUE, scale=FALSE, epsilon = 0.5, kernal="radial basis")
predicted_s <- predict(object =  model_s, newdata =dataset_selected_imp, probability=TRUE)

pred_pro_s <- attr(predicted_s, "probabilities")[,2]

preds<-data.frame(pred_pro_s)
data1<-cbind(dataset_selected_imp,#%>%filter(!is.na(q5_a)), 
             cbind( preds))
#data1$predicted_s<- factor(data1$predicted_s, levels= c("無", "有"), ordered = T)


rocs <- roc(data1$firstyr_bully2_act, data1$pred_pro_s)


# 
set.seed(125)
fitControl <-  trainControl(method="boot", number = 10,
                            savePredictions = TRUE,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)

svmFit <- train( firstyr_bully2_act  ~ victim_3group_act+
                  q3_a + 
                  school_PR+ 
                  q5_a+
                  q68_a+
                  PHQ9_1yr_new+ 
                  AUDIT_C_4_a+ 
                  q22_0_a+
                  bis1_standardized, data=dataset_selected_imp, 
                 method= "svmRadial", 
                 trControl=fitControl, preProc=c("center", "scale"),
                 metric="ROC",tuneGrid = expand.grid(C = seq(0, 2, length = 20), sigma = 0.1357843, )



glmFit <- train( firstyrbully2  ~ victim_3group+
                   q3_a + 
                   school_PR+ 
                   q5_a+
                   q68_a+
                   PHQ9_1yr_new+ 
                   AUDIT_C_4_a+ 
                  
                   q22_0_a+
                   bis1_standardized, data=dataset_selected_imp, 
                 method= "glm", 
                 trControl=fitControl, preProc=c("center", "scale"),
                 metric="ROC")
glmFit2 <- train( firstyr_bully2_act  ~ victim_3group_act+
                     q3_a + 
                     school_PR+ 
                     q5_a+
                     q68_a+
                     PHQ9_1yr_new+ 
                     AUDIT_C_4_a+ 
                     
                     q22_0_a+
                     bis1_standardized, data=dataset_selected_imp, 
                 method= "glm", 
                 trControl=fitControl, preProc=c("center", "scale"),
                 metric="ROC")



rpartFit <- train( firstyrbully2  ~ victim_3group+
                   q3_a + 
                   school_PR+ 
                   q5_a+
                   q68_a+
                   PHQ9_1yr_new+ 
                   AUDIT_C_4_a+ 
                   q22_0_a+
                   bis1_standardized, data=dataset_selected_imp, 
                 method= "rpart", 
                 trControl=fitControl, preProc=c("center", "scale"),
                 metric="ROC")

resamps <- resamples(list(SVM = svmFit, GLM = glmFit, RPA= rpartFit))
##

svmFit <- train( firstyr_bully2_act  ~ victim_3group_act+
                   q3_a + 
                   school_PR+ 
                   q5_a+
                   q68_a+
                   PHQ9_1yr_new+ 
                   AUDIT_C_4_a+ 
                   q22_0_a+
                   bis1_standardized, data=dataset_selected_imp, 
                 method= "svmRadial", 
                 trControl=fitControl, preProc=c("center", "scale"),
                 metric="ROC")

glmFit <- train( firstyr_bully2_act  ~ victim_3group_act+
                   q3_a + 
                   school_PR+ 
                   q5_a+
                   q68_a+
                   PHQ9_1yr_new+ 
                   AUDIT_C_4_a+ 
                   q22_0_a+
                   bis1_standardized, data=dataset_selected_imp, 
                 method= "glm", 
                 trControl=fitControl, preProc=c("center", "scale"),
                 metric="ROC")

rpartFit <- train( firstyr_bully2_act  ~ victim_3group_act+
                     q3_a + 
                     school_PR+ 
                     q5_a+
                     q68_a+
                     PHQ9_1yr_new+ 
                     AUDIT_C_4_a+ 
                     q22_0_a+
                     bis1_standardized, data=dataset_selected_imp, 
                   method= "rpart", 
                   trControl=fitControl, preProc=c("center", "scale"),
                   metric="ROC")

resamps <- resamples(list(SVM = svmFit, GLM = glmFit, RPA= rpartFit))


x <- evalm(glmFit)
y <- evalm(svmFit)
z <- evalm(rpartFit)


tree <- rpart( firstyrbully2  ~ victim_3group+
    q3_a + 
    school_PR+ 
    q5_a+
    q68_a+
    PHQ9_1yr_new+ 
    AUDIT_C_4_a+ 
    q22_0_a+
    bis1_standardized, data=dataset_selected_imp, cp=0.2)

