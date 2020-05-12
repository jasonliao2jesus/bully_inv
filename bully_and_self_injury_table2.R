source("rearranged_dataset.R")


# Major tasks: 
#table2 defnie first year bullying group+ non-involved group
dataset_table2<-dataset%>%filter(firstyrpurev=="無")

tempData_table2 <- dataset_temp%>%filter(firstyrpurev=="無")

#table2 with chi-square test: q3_a, school_PR, reside_with parents, needy family q7_a, mother_no_job q6_a, father_no_job q5_a. parents to talk to: q10_a   
table2_q3_a <-table(dataset_table2$q3_a, dataset_table2$firstyrbully2)
chisq_q3_a <-  chisq.test(table2_q3_a ,correct=TRUE )
esc_q3_a <-  esc_chisq(chisq = chisq_q3_a$statistic, totaln = nrow(dataset), es.type = "or")
q3_a <- paste(round( esc_q3_a$es,digits = 2), "(",round( esc_q3_a$ci.lo,digits = 2),"-",round( a$ci.hi, digits = 2),")",sep = "")


#



#
table2_q3_a <-table(dataset_table2$q3_a, dataset_table2$firstyrbully2)
table2_school_PR <-table(dataset_table2$school_PR, dataset_table2$firstyrbully2)
table2_q4_a <- table(dataset_table2$q4_a, dataset_table2$firstyrbully2)
table2_q8_a <-table(dataset_table2$q8_a, dataset_table2$firstyrbully2)
table2_q7_a <-table(dataset_table2$q7_a, dataset_table2$firstyrbully2)
table2_q6_a <-table(dataset_table2$q6_a, dataset_table2$firstyrbully2)
table2_q5_a <-table(dataset_table2$q5_a, dataset_table2$firstyrbully2)


table2_q10_a <-table(dataset_table2$q10_a, dataset_table2$firstyrbully2)
table2_PHQ9_4group <-table(dataset_table2$PHQ9_1yr_new, dataset_table2$firstyrbully2)
table2_PHQ9_10_2group <-table(dataset_table2$PHQ9_1yr_2, dataset_table2$firstyrbully2)
table2_AUDIT_C_4_a <-table(dataset_table2$AUDIT_C_4_a, dataset_table2$firstyrbully2)
table2_q68_a <-table(dataset_table2$q68_a, dataset_table2$firstyrbully2)





chisq_q3_a <-  chisq.test(table2_q3_a ,correct=TRUE )
chisq_school_PR <- chisq.test( table2_school_PR ,correct=T )
chisq_q4_a <- chisq.test( table2_q4_a ,correct=T )
chisq_q8_a <- chisq.test( table2_q8_a ,correct=T )
chisq_q7_a <- chisq.test( table2_q7_a ,correct=T )
chisq_q6_a <- chisq.test( table2_q6_a,correct=T  )
chisq_q5_a <- chisq.test( table2_q5_a,correct=T )
chisq_q10_a <- chisq.test( table2_q10_a , correct = T)
chisq_PHQ9_4group <- chisq.test( table2_PHQ9_4group , correct = T)
chisq_PHQ9_10_2group <- chisq.test( table2_PHQ9_10_2group, correct = T )
chisq_AUDIT_C_4_a <- chisq.test( table2_AUDIT_C_4_a , correct=T)
chisq_q68_a <- chisq.test( table2_q68_a,correct=T )


esc_q3_a <-  esc_chisq(chisq = chisq_q3_a$statistic, totaln = nrow(dataset), es.type = "or")
esc_q4_a <-  esc_chisq(chisq = chisq_q4_a$statistic, totaln = nrow(dataset), es.type = "or")
esc_q5_a <-  esc_chisq(chisq = chisq_q5_a$statistic, totaln = nrow(dataset), es.type = "or")
esc_q6_a <-  esc_chisq(chisq = chisq_q6_a$statistic, totaln = nrow(dataset), es.type = "or")
esc_q7_a <-  esc_chisq(chisq = chisq_q7_a$statistic, totaln = nrow(dataset), es.type = "or")
esc_q8_a <-  esc_chisq(chisq = chisq_q8_a$statistic, totaln = nrow(dataset), es.type = "or")
esc_q10_a <-  esc_chisq(chisq = chisq_q10_a$statistic, totaln = nrow(dataset), es.type = "or")
esc_school_PR <-  esc_chisq(chisq = chisq_school_PR$statistic, totaln = nrow(dataset), es.type = "or")


paste(round( esc_q3_a$es,digits = 2), "(",round( esc_q3_a$ci.lo,digits = 2),"-",round( a$ci.hi, digits = 2),")",sep = "")


table2_q22_a <-table(dataset_table2$q22_a, dataset_table2$firstyrbully2)
table2_q22_3_a <-table(dataset_table2$q22_3_a, dataset_table2$firstyrbully2)
table2_q22_4_a <-table(dataset_table2$q22_4_a, dataset_table2$firstyrbully2)
chisq.test(table2_q22_a)
chisq.test(table2_q22_3_a)
chisq.test(table2_q22_4_a)

or_q3_a <- fisher.test(table2_q3_a)
or_school_PR <- fisher.test(table2_school_PR)
or_q8_a <- fisher.test(table2_q8_a)
or_q7_a <- fisher.test(table2_q7_a)
or_q6_a <- fisher.test(table2_q6_a)
or_q5_a <- fisher.test(table2_q5_a)
or_q68_a <- fisher.test(table2_q68e_a)
or_PHQ9_4group_6_10 <- fisher.test(table2_PHQ9_4group[c(-3,-4),])
or_PHQ9_4group_11_14 <- fisher.test(table2_PHQ9_4group[c(-2,-4),])
or_PHQ9_4group_15 <- fisher.test(table2_PHQ9_4group[c(-2,-3),])
or_AUDIT_C_4_a <-  fisher.test(table2_AUDIT_C_4_a)


paste0(c(round(or_q3_a$estimate,digit=2),"(",round(or_q3_a$conf.int,digit=2),")"), collapse="")
paste0(c(round(or_school_PR$estimate,digit=2),"(",round(or_school_PR$conf.int,digit=2),")"), collapse="")
paste0(c(round(or_q8_a$estimate,digit=2),"(",round(or_q8_a$conf.int,digit=2),")"), collapse="")
paste0(c(round(or_q7_a$estimate,digit=2),"(",round(or_q7_a$conf.int,digit=2),")"), collapse="")
paste0(c(round(or_q6_a$estimate,digit=2),"(",round(or_q6_a$conf.int,digit=2),")"), collapse="")
paste0(c(round(or_q5_a$estimate,digit=2),"(",round(or_q5_a$conf.int,digit=2),")"), collapse="")
paste0(c(round(or_q68_a$estimate,digit=2),"(",round(or_q68_a$conf.int,digit=2),")"), collapse="")
paste0(c(round(or_PHQ9_4group_6_10$estimate,digit=2),"(",round(or_PHQ9_4group_6_10$conf.int,digit=2),")"), collapse="")
paste0(c(round(or_PHQ9_4group_11_14$estimate,digit=2),"(",round(or_PHQ9_4group_11_14$conf.int,digit=2),")"), collapse="")
paste0(c(round(or_PHQ9_4group_15$estimate,digit=2),"(",round(or_PHQ9_4group_15$conf.int,digit=2),")"), collapse="")
paste0(c(round(or_AUDIT_C_4_a$estimate,digit=2),"(",round(or_AUDIT_C_4_a$conf.int,digit=2),")"), collapse="")
#2 sample t test for victims: MDSS_1, peer_MDSS_1, rses,
t_q22_0_a <-t.test(dataset_table2$q22_0_a~ dataset_table2$firstyrbully2, var.equal=FALSE)
t_q22_0_a
sd(dataset_table2$q22_0_a[which(dataset_table2$firstyrbully2=="無")])
sd(dataset_table2$q22_0_a[which(dataset_table2$firstyrbully2=="有")])

t_peer1 <-t.test(dataset_table2$peer1~ dataset_table2$firstyrbully2, var.equal=FALSE)
t_peer1
sd(dataset_table2$peer1[which(dataset_table2$firstyrbully2=="無")])
sd(dataset_table2$peer1[which(dataset_table2$firstyrbully2=="有")])

t_firstrses <-t.test(dataset_table2$firstrses~ dataset_table2$firstyrbully2, var.equal=FALSE)
t_firstrses
sd(dataset_table2$firstrses[which(dataset_table2$firstyrbully2=="無")])
sd(dataset_table2$firstrses[which(dataset_table2$firstyrbully2=="有")])
t_bis1 <-t.test(dataset_table2$bis_total~ dataset_table2$firstyrbully2, var.equal=FALSE)
t_bis1
sd(dataset_table2$bis1_total[which(dataset_table2$firstyrbully2=="無")])
sd(dataset_table2$bis1_total[which(dataset_table2$firstyrbully2=="有")])

 t.test(dataset_table2$parent1_MDSS~ dataset_table2$firstyrbully2, var.equal=FALSE)
 t.test(dataset_table2$teacher_MDSS~ dataset_table2$firstyrbully2, var.equal=FALSE)
t.test(dataset_table2$relative_MDSS~ dataset_table2$firstyrbully2, var.equal=FALSE)

t.test(dataset_table2$bis1_attentional~ dataset_table2$firstyrbully2, var.equal=FALSE)
t.test(dataset_table2$bis1_motor_all~ dataset_table2$firstyrbully2, var.equal=FALSE)
t.test(dataset_table2$bis1_nonplanning~ dataset_table2$firstyrbully2, var.equal=FALSE)

t.test(dataset_table2$bis1_attention~ dataset_table2$firstyrbully2, var.equal=FALSE)
t.test(dataset_table2$bis1_cognitive_instability~ dataset_table2$firstyrbully2, var.equal=FALSE)
t.test(dataset_table2$bis1_motor~ dataset_table2$firstyrbully2, var.equal=FALSE)
t.test(dataset_table2$bis1_perseverance~ dataset_table2$firstyrbully2, var.equal=FALSE)
t.test(dataset_table2$bis1_self_control~ dataset_table2$firstyrbully2, var.equal=FALSE)
t.test(dataset_table2$bis1_cognitive_complexity~ dataset_table2$firstyrbully2, var.equal=FALSE)

t.test(dataset_table2$bis1_inability_to_plan~ dataset_table2$firstyrbully2, var.equal=FALSE)
t.test(dataset_table2$bis1_lack_of_selfcontrol~ dataset_table2$firstyrbully2, var.equal=FALSE)
t.test(dataset_table2$bis1_novelty_seeking~ dataset_table2$firstyrbully2, var.equal=FALSE)

d_q22_0_a <- cohen.d(dataset_table2$q22_0_a~ dataset_table2$firstyrbully2)
d_peer1 <- cohen.d(dataset_table2$peer1~ dataset_table2$firstyrbully2)
d_firstrses <- cohen.d(dataset_table2$firstrses~ dataset_table2$firstyrbully2)
d_bis1 <- cohen.d(dataset_table2$bis1_total~ dataset_table2$firstyrbully2)

exp(t_q22_0_a$statistic*2/sqrt(t_q22_0_a$parameter))

paste0(c(round(exp(-d_q22_0_a$estimate*pi/sqrt(3)),digits = 2),"(", round(exp(-d_q22_0_a$conf.int*pi/sqrt(3)),digits = 2),")"),sep="", collapse ="" )
paste0(c(round(exp(-d_peer1$estimate*pi/sqrt(3)),digits = 2),"(", round(exp(-d_peer1$conf.int*pi/sqrt(3)),digits = 2),")"),sep="", collapse ="" )
paste0(c(round(exp(-d_firstrses$estimate*pi/sqrt(3)),digits = 2),"(", round(exp(-d_firstrses$conf.int*pi/sqrt(3)),digits = 2),")"),sep="", collapse ="" )
paste0(c(round(exp(-d_bis1$estimate*pi/sqrt(3)),digits = 2),"(", round(exp(-d_bis1$conf.int*pi/sqrt(3)),digits = 2),")"),sep="", collapse ="" )






#table 2, logistic regression, Chi or t test 有significance才放入

dataset_table2_glm_binomial <-  glm(formula = firstyrbully2 ~ q3_a+ school_PR + q5_a+q68_a + PHQ9_1yr_new  +AUDIT_C_4_a+
                                      q22_0_a +firstrses+bis1_total, family = binomial(), data = dataset_table2)
model_summary <-  summary(dataset_table2_glm_binomial)

a <- exp(coef(dataset_table2_glm_binomial))
b <- exp(confint(dataset_table2_glm_binomial))

a <- cbind(round(a,digits = 2),round(b, digits = 2))    # 自動整理好格式以免累死

a <- cbind(a, paste0(a[,1], "(", a[,2], "-", a[,3],")", 
                    ifelse(model_summary$coefficients[,4]<0.001,"***",
                           ifelse(model_summary$coefficients[,4]<0.01, "**",  
                                  ifelse(model_summary$coefficients[,4]<0.05,"*", 
                                         ifelse(model_summary$coefficients[,4]<0.10,"+",""
                                                )
                                         )
                                  )
                           )
                    )
            )
a <- cbind(var=rownames(a),as.data.frame(a))
a <- a[,c(-2,-3,-4)]
a <- data.frame(a,row.names = NULL)

write.table(a, "output_table2.csv",sep = ",")

#paste0(round(exp(coef(dataset_table2_glm_binomial)),digits = 2),sep="", collapse ="," )
#paste0( round(exp(confint(dataset_table2_glm_binomial)),digits = 2),")",sep="", collapse ="," )



#table 2, logistic regression, selecting significant variables in the initial analysis
dataset_table2_glm_binomial <-  glm(formula = firstyrbully2 ~ q3_a+q5_a+school_PR +q68_a + PHQ9_1yr_new  +AUDIT_C_4_a
                                       +bis1_total, family = binomial(), data = dataset_table2)
summary(dataset_table2_glm_binomial)
exp(coef(dataset_table2_glm_binomial))
exp(confint(dataset_table2_glm_binomial))

dataset_table2_glm_binomial_2 <-  glm(formula = firstyrbully2 ~ q3_a+q5_a+ school_PR +q68_a + PHQ9_1yr_new  +AUDIT_C_4_a+
                                      peer1 + firstrses+bis1_total, family = binomial(), data = dataset_table2)
summary(dataset_table2_glm_binomial_2)

#table 2, stepwised logistic regression,
table2.model <- stepAIC(dataset_table2_glm_binomial,direction = "backward", trace= FALSE)
summary(table2.model)
exp(coef(table2.model))
confint(table2.model)




#table 2, logistic regression, selecting all variables in the initial analysis
dataset_table2_glm_binomial_2 <-  glm(formula = firstyrbully2 ~ q3_a+q4_a+q5_a+q6_a+ q7_a+q8_a+q10_a+school_PR +q68_a + PHQ9_1yr_new  +AUDIT_C_4_a+
                                        +peer1 + firstrses+bis1_standardized, family = binomial(), data = dataset_table2)
summary(dataset_table2_glm_binomial_2)


dataset_table2_vglm <-  vglm(formula = bully_3group ~ q3_a+q5_a+school_PR +q68_a + PHQ9_1yr_new  +AUDIT_C_4_a
                                    +bis1_lack_of_selfcontrol+bis1_inability_to_plan+ bis1_novelty_seeking, family = multinomial(refLevel = 1), data = dataset_table2)
summary(dataset_table2_vglm)







# ordinal regression analysis: data.frame= dataset_temp_ordinal
dataset_table2_vglm_ordinal <-  vglm(formula = bully_3group ~ firstyrvic2+q3_a+ q4_a+q5_a+q6_a+q7_a+q8_a+q10_a+school_PR +q68_a + 
                                       PHQ9_1yr_new  +AUDIT_C_4_a+
                               q22_0_a+peer1 +firstrses+ bis1_standardized, family = cumulative(parallel= FALSE), data = dataset_selected_ordinal)

summary(dataset_table2_vglm_ordinal)

dataset_table2_vglm_ordinal_null <- vglm(formula = bully_3group ~ 1, family = cumulative(parallel= TRUE), data = dataset_selected_ordinal)

lrtest(dataset_table2_vglm_ordinal,dataset_table2_vglm_ordinal_null)
  
dataset_temp_ordinal$q5_a<- factor(dataset_temp_ordinal$q5_a, levels=c("有","沒有","不知道"))
dataset_temp_ordinal$q5_a[is.na(dataset_temp_ordinal$q5_a)]<- "不知道"


confint( dataset_table2_vglm)[bis1_standardized:2]
coef( dataset_table2_vglm)["bis1_standardized:2",](,1)
