
#
#dataset <-mutate(dataset, yr= 100.5357- (q2y_a+(q2m_a)/12))

# 檢查有無安裝packages 若沒有就安裝
want <-c( "foreign","caret", "dplyr", "forcats","e1071",
          "dbplyr", "MASS", "tidyverse", "Hmisc", "rms","PredictABEL", "mediation", "ggplot2", "effsize", "VGAM", 
         "lmtest","pscl","survey", "oddsratio", "mice","car","rpart","caret","rlang",
         "lattice","boot","ROCR","pROC", "brglm2", "Metrics","InformationValue", 
         "dominanceanalysis", "shrink", "Information", "ggrepel", "vctrs", "ggpubr", "rms", "PredictABEL",
         "ggrepel","esc","xlsx")
has <- want %in% rownames(installed.packages())
if(any(!has)) install.packages(want[!has])

# loaded libraries
 

library(caret)
library(dplyr)
library(MASS)
library(tidyverse)
library(Hmisc)
library(mediation)
library(ggplot2)
library(effsize)
library(VGAM)
library(foreign)
library(Information)
library(forcats)

library(mice)
library(oddsratio)
library(survey)
library(lmtest)
library(pscl)
library(car)
#lib#library(ggalluvial)
library(vctrs)
library(ggpubr)
library(grid)
library(gridExtra)
library(GGally)
library(brglm2)
library("rms")
library("PredictABEL")
library("pROC")
library(pscl)
library(esc)
library(ggrepel)
#library(xlsx)
#dataset$rec
# 
input_file <- "1040103 處理第一及第二年(n=733)(Dr. 黃國洋).sav"
output_file <- "rearranged_dataset.csv"
dataset <- read.spss(input_file, to.data.frame= TRUE)


# dependent variables: bullying and victimization: 改切點 

#切點: 一個月二到三次
dataset<- mutate( dataset, q115_1_a_factor3=fct_collapse(q115_1_a, no = c("無","只有一到兩次"), yes = c("一個月二到三次","約一星期一次","一個星期好幾次"))) 
dataset<- mutate( dataset, q115_2_a_factor3=fct_collapse(q115_2_a, no = c("無","只有一到兩次"), yes = c("一個月二到三次","約一星期一次","一個星期好幾次")))
dataset<- mutate( dataset, q115_3_a_factor3=fct_collapse(q115_3_a, no = c("無","只有一到兩次"), yes = c("一個月二到三次","約一星期一次","一個星期好幾次")) ) 
dataset<- mutate( dataset, q115_4_a_factor3=fct_collapse(q115_4_a, no = c("無","只有一到兩次"), yes = c("一個月二到三次","約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_5_a_factor3=fct_collapse(q115_5_a, no = c("無","只有一到兩次"), yes = c("一個月二到三次","約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_6_a_factor3=fct_collapse(q115_6_a, no = c("無","只有一到兩次"), yes = c("一個月二到三次","約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_7_a_factor3=fct_collapse(q115_7_a, no = c("無","只有一到兩次"), yes = c("一個月二到三次","約一星期一次","一個星期好幾次")))  
dataset<- mutate( dataset, q115_8_a_factor3=fct_collapse(q115_8_a, no = c("無","只有一到兩次"), yes = c("一個月二到三次","約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_9_a_factor3=fct_collapse(q115_9_a, no = c("無","只有一到兩次"), yes = c("一個月二到三次","約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_10_a_factor3=fct_collapse(q115_10_a, no = c("無","只有一到兩次"), yes = c("一個月二到三次","約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_11_a_factor3=fct_collapse(q115_11_a, no = c("無","只有一到兩次"), yes = c("一個月二到三次","約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_12_a_factor3=fct_collapse(q115_12_a, no = c("無","只有一到兩次"), yes = c("一個月二到三次","約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_13_a_factor3=fct_collapse(q115_13_a, no = c("無","只有一到兩次"), yes = c("一個月二到三次","約一星期一次","一個星期好幾次"))) 
dataset<- mutate( dataset, q115_14_a_factor3=fct_collapse(q115_14_a, no = c("無","只有一到兩次"), yes = c("一個月二到三次","約一星期一次","一個星期好幾次"))) 

# 切點: 約一星期一次

dataset<- mutate( dataset, q115_1_a_factor4=fct_collapse(q115_1_a, no = c("無","只有一到兩次","一個月二到三次"), yes = c("約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_2_a_factor4=fct_collapse(q115_2_a, no = c("無","只有一到兩次","一個月二到三次"), yes = c("約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_3_a_factor4=fct_collapse(q115_3_a, no = c("無","只有一到兩次","一個月二到三次"), yes = c("約一星期一次","一個星期好幾次")) )

dataset<- mutate( dataset, q115_4_a_factor4=fct_collapse(q115_4_a, no = c("無","只有一到兩次","一個月二到三次"), yes = c("約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_5_a_factor4=fct_collapse(q115_5_a, no = c("無","只有一到兩次","一個月二到三次"), yes = c("約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_6_a_factor4=fct_collapse(q115_6_a, no = c("無","只有一到兩次","一個月二到三次"), yes = c("約一星期一次","一個星期好幾次"))) 
dataset<- mutate( dataset, q115_7_a_factor4=fct_collapse(q115_7_a, no = c("無","只有一到兩次","一個月二到三次"), yes = c("約一星期一次","一個星期好幾次"))) 

dataset<- mutate( dataset, q115_8_a_factor4=fct_collapse(q115_8_a, no = c("無","只有一到兩次", "一個月二到三次"), yes = c("約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_9_a_factor4=fct_collapse(q115_9_a, no = c("無","一個月二到三次","只有一到兩次"), yes = c("約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_10_a_factor4=fct_collapse(q115_10_a, no = c("無","一個月二到三次","只有一到兩次"), yes = c("約一星期一次","一個星期好幾次")) )

dataset<- mutate( dataset, q115_11_a_factor4=fct_collapse(q115_11_a, no = c("無","一個月二到三次","只有一到兩次"), yes = c("約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_12_a_factor4=fct_collapse(q115_12_a, no = c("無","一個月二到三次","只有一到兩次"), yes = c("約一星期一次","一個星期好幾次")) )
dataset<- mutate( dataset, q115_13_a_factor4=fct_collapse(q115_13_a, no = c("無","一個月二到三次","只有一到兩次"), yes = c("約一星期一次","一個星期好幾次"))) 
dataset<- mutate( dataset, q115_14_a_factor4=fct_collapse(q115_14_a, no = c("無","一個月二到三次","只有一到兩次"), yes = c("約一星期一次","一個星期好幾次"))) 



# 切點 只有一到兩次
dataset<- mutate( dataset, q115_1_a_v_factor1=fct_collapse(q115_1_a, no = c("無"), yes = c("約一星期一次","一個星期好幾次","一個月二到三次","只有一到兩次")) )
dataset<- mutate( dataset, q115_2_a_v_factor1=fct_collapse(q115_2_a, no = c("無"), yes = c("約一星期一次","一個星期好幾次","一個月二到三次","只有一到兩次")) )
dataset<- mutate( dataset, q115_3_a_v_factor1=fct_collapse(q115_3_a, no = c("無"), yes = c("約一星期一次","一個星期好幾次","一個月二到三次","只有一到兩次")) )
dataset<- mutate( dataset, q115_4_a_v_factor1=fct_collapse(q115_4_a, no = c("無"), yes = c("約一星期一次","一個星期好幾次","一個月二到三次","只有一到兩次")) )
dataset<- mutate( dataset, q115_5_a_v_factor1=fct_collapse(q115_5_a, no = c("無"), yes = c("約一星期一次","一個星期好幾次","一個月二到三次","只有一到兩次")) )
dataset<- mutate( dataset, q115_6_a_v_factor1=fct_collapse(q115_6_a, no = c("無"), yes = c("約一星期一次","一個星期好幾次","一個月二到三次","只有一到兩次"))) 
dataset<- mutate( dataset, q115_7_a_v_factor1=fct_collapse(q115_7_a, no = c("無"), yes = c("約一星期一次","一個星期好幾次","一個月二到三次","只有一到兩次"))) 

dataset<- mutate( dataset, q115_8_a_factor1=fct_collapse(q115_8_a, no = c("無"), yes = c("約一星期一次","一個星期好幾次","只有一到兩次", "一個月二到三次")) )
dataset<- mutate( dataset, q115_9_a_factor1=fct_collapse(q115_9_a, no = c("無"), yes = c("約一星期一次","一個星期好幾次","一個月二到三次","只有一到兩次")) )
dataset<- mutate( dataset, q115_10_a_factor1=fct_collapse(q115_10_a, no = c("無"), yes = c("約一星期一次","一個星期好幾次","一個月二到三次","只有一到兩次")) )
dataset<- mutate( dataset, q115_11_a_factor1=fct_collapse(q115_11_a, no = c("無"), yes = c("約一星期一次","一個星期好幾次","一個月二到三次","只有一到兩次")) )
dataset<- mutate( dataset, q115_12_a_factor1=fct_collapse(q115_12_a, no = c("無"), yes = c("約一星期一次","一個星期好幾次","一個月二到三次","只有一到兩次")) )
dataset<- mutate( dataset, q115_13_a_factor1=fct_collapse(q115_13_a, no = c("無"), yes = c("約一星期一次","一個星期好幾次","一個月二到三次","只有一到兩次"))) 
dataset<- mutate( dataset, q115_14_a_factor1=fct_collapse(q115_14_a, no = c("無"), yes = c("約一星期一次","一個星期好幾次","一個月二到三次","只有一到兩次"))) 






#rec_bully_verbal, 
# bully type

dataset<- mutate( dataset, physicalbully=ifelse(q115_8_a=="無", "無", "有"))
dataset$physicalbully <-factor(dataset$physicalbully, levels=c("無","有") )

dataset<- mutate( dataset, verbalbully=ifelse(q115_9_a=="無"&q115_10_a=="無", "無", "有"))
dataset$verbalbully <-factor(dataset$verbalbully, levels=c("無","有") )

dataset<- mutate( dataset, relationalbully=ifelse(q115_11_a=="無"&q115_12_a=="無", "無", "有"))
dataset$relationalbully <-factor(dataset$relationalbully, levels=c("無","有") )

dataset<- mutate( dataset, otherbully=ifelse(q115_13_a=="無"&q115_14_a=="無", "無", "有"))
dataset$otherbully <-factor(dataset$otherbully, levels=c("無","有") )

# victim type

dataset<- mutate( dataset, physicalvictim=ifelse(q115_1_a=="無", "無", "有"))
dataset$physicalvictim <-factor(dataset$physicalvictim, levels=c("無","有") )

dataset<- mutate( dataset, verbalvictim=ifelse(q115_2_a=="無"&q115_3_a=="無", "無", "有"))
dataset$verbalvictim <-factor(dataset$verbalvictim, levels=c("無","有") )

dataset<- mutate( dataset, relationalvictim=ifelse(q115_4_a=="無"&q115_5_a=="無", "無", "有"))
dataset$relationalvictim <-factor(dataset$relationalvictim, levels=c("無","有") )

dataset<- mutate( dataset, otherbully=ifelse(q115_13_a=="無"&q115_14_a=="無", "無", "有"))
dataset$otherbully <-factor(dataset$otherbully, levels=c("無","有") )


# recurrent bully
dataset<- mutate( dataset, physicalbully=ifelse(q115_8_a=="無", "無", "有"))
dataset$physicalbully <-factor(dataset$physicalbully, levels=c("無","有") )

dataset<- mutate( dataset, verbalbully=ifelse(q115_9_a=="無"&q115_10_a=="無", "無", "有"))
dataset$verbalbully <-factor(dataset$verbalbully, levels=c("無","有") )

dataset<- mutate( dataset, relationalbully=ifelse(q115_11_a=="無"&q115_12_a=="無", "無", "有"))
dataset$relationalbully <-factor(dataset$relationalbully, levels=c("無","有") )

dataset<- mutate( dataset, otherbully=ifelse(q115_13_a=="無"&q115_14_a=="無", "無", "有"))
dataset$otherbully <-factor(dataset$otherbully, levels=c("無","有") )


dataset<- mutate(dataset, rec_bully_physical= ifelse(q115_8_a_factor3== "yes", "yes","no"))  
dataset<- mutate(dataset, rec_bully_verbal= ifelse(q115_9_a_factor3== "yes"|q115_10_a_factor3=="yes", "yes","no"))  
dataset<- mutate(dataset, rec_bully_relational= ifelse(q115_11_a_factor3== "yes"|q115_12_a_factor3=="yes", "yes","no"))
dataset<- mutate(dataset, rec_bully_other= ifelse(q115_13_a_factor3== "yes"|q115_14_a_factor3=="yes", "yes","no"))  
dataset<- mutate(dataset, rec_bully_firstyr= ifelse(q115_8_a_factor3== "yes"| 
                                                    q115_9_a_factor3== "yes"|
                                                    q115_10_a_factor3=="yes"| 
                                                    q115_11_a_factor3== "yes"|
                                                    q115_12_a_factor3=="yes"|
                                                    q115_13_a_factor3== "yes"|
                                                    q115_14_a_factor3=="yes", "yes","no")) 
dataset$rec_bully_physical <- factor(dataset$rec_bully_physical, levels= c("no", "yes"))
dataset$rec_bully_verbal <- factor(dataset$rec_bully_verbal, levels= c("no", "yes"))
dataset$rec_bully_relational <- factor(dataset$rec_bully_relational, levels= c("no", "yes"))

dataset$rec_bully_firstyr <- factor(dataset$rec_bully_firstyr, levels= c("no", "yes"))

# recurrent victims
dataset<- mutate(dataset, rec_victim_physical= ifelse(q115_1_a_factor3== "yes", "yes","no"))  
dataset<- mutate(dataset, rec_victim_verbal= ifelse(q115_2_a_factor3== "yes"|q115_3_a_factor3=="yes", "yes","no"))  
dataset<- mutate(dataset, rec_victim_relational= ifelse(q115_4_a_factor3== "yes"|q115_5_a_factor3=="yes", "yes","no"))
dataset<- mutate(dataset, rec_victim_other= ifelse(q115_6_a_factor3== "yes"|q115_7_a_factor3=="yes", "yes","no"))  
dataset<- mutate(dataset, rec_victim_firstyr= ifelse(q115_1_a_factor3== "yes"| 
                                                     q115_2_a_factor3== "yes"|
                                                     q115_3_a_factor3=="yes"| 
                                                     q115_4_a_factor3== "yes"|
                                                     q115_5_a_factor3=="yes"|
                                                     q115_6_a_factor3== "yes"|
                                                     q115_7_a_factor3=="yes", "yes","no")) 

dataset$rec_victim_firstyr <- factor(dataset$rec_victim_firstyr, levels= c("no", "yes"))

dataset$rec_victim_physical <- factor(dataset$rec_victim_physical, levels= c("no", "yes"))
dataset$rec_victim_verbal <- factor(dataset$rec_victim_verbal, levels= c("no", "yes"))
dataset$rec_victim_relational <- factor(dataset$rec_victim_relational, levels= c("no", "yes"))



dataset<- mutate(dataset, rec_bv= ifelse(rec_victim_firstyr=="yes"& rec_bully_firstyr=="yes", "yes", "no"))
        dataset$rec_bv <- factor(dataset$rec_bv, levels= c("no", "yes"))
        
# bully  group 1, 2, 3, 4: 1. non involved, "無"     2."只有一到兩次"  3."一個月二到三次"         4."約一星期一次"&"一個星期好幾次"
 

dataset<- mutate(dataset, firstyr_bully2= ifelse(q115_8_a_factor1== "yes"| 
                                                  q115_9_a_factor1== "yes"|q115_10_a_factor1=="yes"| 
                                                 q115_11_a_factor1== "yes"|q115_12_a_factor1=="yes"|
                                                 q115_13_a_factor1== "yes"|q115_14_a_factor1=="yes", "yes","no")) 
        dataset$firstyr_bully2<- factor(dataset$firstyr_bully2, levels= c("no", "yes"))
dataset<- mutate(dataset, bully_group= ifelse(q115_8_a_factor4== "yes"| q115_9_a_factor4== "yes"|q115_10_a_factor4=="yes"| 
                                              q115_11_a_factor4== "yes"|q115_12_a_factor4=="yes"| q115_13_a_factor4== "yes"|
                                                q115_14_a_factor4=="yes", "group4",
                                                      ifelse(rec_bully_firstyr=="yes","group3",
                                                             ifelse(firstyr_bully2=="yes", "group2","group1"))))  
        dataset$bully_group <- factor(dataset$bully_group, levels= c("group1", "group2", "group3", "group4"))
        
dataset <- mutate(dataset, bully_3group=ifelse(bully_group=="group3"|bully_group=="group4", "repeated_bullying", ifelse(bully_group=="group2","1or2_times", "no_bullying")))        
        dataset$bully_3group <- factor(dataset$bully_3group, levels= c("no_bullying", "1or2_times", "repeated_bullying"))    
#   active     
        
        dataset<- mutate(dataset, rec_bully_firstyr_act= ifelse(q115_8_a_factor3== "yes"| 
                                                              q115_9_a_factor3== "yes"|
                                                              q115_10_a_factor3=="yes" 
                                                              , "yes","no")) 
        
        dataset$rec_bully_firstyr_act <- factor(dataset$rec_bully_firstyr_act, levels= c("no", "yes"))        
dataset_sec_yr$bully
        
dataset<- mutate(dataset, firstyr_bully2_act= ifelse(q115_8_a_factor1== "yes"|q115_9_a_factor1== "yes"|q115_10_a_factor1=="yes" 
          , "yes","no")) 

dataset$firstyr_bully2_act<- factor(dataset$firstyr_bully2_act, levels= c("no", "yes"))


dataset<- mutate(dataset, bully_group_act= ifelse(q115_8_a_factor4== "yes"| q115_9_a_factor4== "yes"|q115_10_a_factor4=="yes" 
                                               , "group4",
                                              ifelse(rec_bully_firstyr_act=="yes","group3",
                                                     ifelse(firstyr_bully2_act=="yes", "group2","group1"))))  
dataset$bully_group_act <- factor(dataset$bully_group_act, levels= c("group1", "group2", "group3", "group4"))

dataset <- mutate(dataset, bully_3group_act=ifelse(bully_group_act=="group3"|bully_group_act=="group4", 
                                               "repeated_bullying", 
                                               ifelse(bully_group_act=="group2",
                                                      "1or2_times", "no_bullying"))) 
dataset$bully_3group_act <- factor(dataset$bully_3group_act, levels= c("no_bullying", "1or2_times", "repeated_bullying"))  


        
 # 4 victim group: 
        


dataset<- mutate(dataset, firstyr_vic2= ifelse(q115_1_a_v_factor1== "yes"| 
                                               q115_2_a_v_factor1== "yes"|
                                               q115_3_a_v_factor1== "yes"| 
                                               q115_4_a_v_factor1== "yes"|
                                               q115_5_a_v_factor1== "yes"|
                                               q115_6_a_v_factor1== "yes"|
                                               q115_7_a_v_factor1== "yes", "yes","no")) 
dataset$firstyr_vic2 <- factor(dataset$firstyr_vic2, levels= c("no", "yes")) 

dataset<- mutate(dataset, victim_group= ifelse(q115_1_a_factor4== "yes"| 
                                               q115_2_a_factor4== "yes"|
                                               q115_3_a_factor4== "yes"| 
                                               q115_4_a_factor4== "yes"|
                                               q115_5_a_factor4== "yes"| 
                                               q115_6_a_factor4== "yes"|
                                               q115_7_a_factor4=="yes", "v_group4",
                                                      ifelse(rec_victim_firstyr=="yes","v_group3",
                                                             ifelse(firstyr_vic2=="yes", "v_group2","v_group1"))))  
        dataset$victim_group <- factor(dataset$victim_group, levels= c("v_group1", "v_group2", "v_group3", "v_group4"))
        
        dataset<- mutate( dataset, firstyr_pureb=ifelse(victim_group=="v_group1"& bully_group!="group1", "yes","no") ) 
        dataset$firstyr_pureb <- factor(dataset$firstyr_pureb, levels= c("no", "yes"))
        
        dataset<- mutate( dataset, firstyr_purev=ifelse(victim_group!="v_group1"& bully_group=="group1", "yes","no") ) 
        dataset$firstyr_purev <- factor(dataset$firstyr_purev, levels= c("no", "yes"))
                
        dataset<- mutate( dataset, firstyr_bv=ifelse(victim_group!="v_group1"& bully_group!="group1", "yes","no") ) 
        dataset$firstyr_bv <- factor(dataset$firstyr_bv, levels= c("no", "yes"))
        
        
        dataset<- mutate( dataset, firstyr_pureb_act=ifelse(victim_group=="v_group1"& bully_group_act!="group1", "yes","no") ) 
        dataset$firstyr_pureb_act <- factor(dataset$firstyr_pureb_act, levels= c("no", "yes"))
        

        
        dataset<- mutate( dataset, firstyr_bv_act=ifelse(victim_group!="v_group1"& bully_group_act!="group1", "yes","no") )
        dataset$firstyr_bv_act <- factor(dataset$firstyr_bv_act, levels= c("no", "yes"))

        
dataset  <- mutate(dataset, victim_3group=ifelse(victim_group=="v_group3"|victim_group=="v_group4", "v2_repeated_being_bullied", ifelse(victim_group=="v_group2","v1_1or2_times", "v0_not_being_bullied")))        
dataset$victim_3group <- factor(dataset$victim_3group, levels= c("v0_not_being_bullied", "v1_1or2_times", "v2_repeated_being_bullied"))


#re-arrange and re-prioritize factors

#re-arrange and re-prioritize factors
# PHQ9分群
dataset <- mutate( dataset, PHQ9_1yr_new = ifelse(dataset$q31_0_a<6, "0-5", ifelse(dataset$q31_0_a<11, "6-10", ifelse(dataset$q31_0_a<15, "11-14","15以上"))))

dataset$PHQ9_1yr_new <- factor(dataset$PHQ9_1yr_new, levels=c("0-5","6-10","11-14","15以上"))
# gender, school_PR, reside with parents, needy family 父母就業
dataset$school_PR <- factor(dataset$school_PR, levels=c("PR70-89","PR<70"))
dataset$q3_a <- factor(dataset$q3_a, levels=c("女","男"))
dataset$q8_a <- factor(dataset$q8_a, levels=c("是","否"))
dataset$q7_a <- factor(dataset$q7_a, levels=c("否","是"))
dataset$q5_a <- factor(dataset$q5_a, levels=c("有","沒有"))
dataset$q6_a <- factor(dataset$q6_a, levels=c("有","沒有"))



# 加上MDSS 各分項第一題 主問題
dataset <- mutate(dataset, parent1_talk = ifelse(q11_a=="從未", 1, ifelse(q11_a=="有時", 2, ifelse(q11_a=="經常", 3,4))))
dataset <- mutate(dataset, teacher_talk = ifelse(q20_a=="從未", 1, ifelse(q20_a=="有時", 2, ifelse(q20_a=="經常", 3,4))))
dataset <- mutate(dataset, peer_talk = ifelse(q17_a=="從未", 1, ifelse(q17_a=="有時", 2, ifelse(q17_a=="經常", 3,4))))
dataset <- mutate(dataset, relative_talk = ifelse(q14_a=="從未", 1, ifelse(q14_a=="有時", 2, ifelse(q14_a=="經常", 3,4))))



# MDSS 各分項第一題 次問題
dataset <- mutate(dataset, parent1_would_rather = ifelse(q12_a=="多一點", 1, ifelse(q12_a=="少一點", 2, 3)))
dataset <- mutate(dataset, teacher_would_rather = ifelse(q21_a=="多一點", 1, ifelse(q21_a=="少一點", 2, 3)))
dataset <- mutate(dataset, peer_would_rather = ifelse(q18_a=="多一點", 1, ifelse(q18_a=="少一點", 2, 3)))
dataset <- mutate(dataset, relative_would_rather = ifelse(q15_a=="多一點", 1, ifelse(q15_a=="少一點", 2, 3)))

dataset <- mutate(dataset, parent1_MDSS = parent1_talk+ parent1_would_rather) 
dataset <- mutate(dataset, teacher_MDSS = teacher_talk+ teacher_would_rather)
dataset <- mutate(dataset, peer_MDSS = peer_talk+ peer_would_rather)
dataset <- mutate(dataset, relative_MDSS = relative_talk+ relative_would_rather) 
#BIS subscale
dataset <- mutate(dataset, bis1_a_1= (as.numeric(q84_a)*(-1)+5))
dataset <- mutate(dataset, bis1_a_2= as.numeric(q85_a))
dataset <- mutate(dataset, bis1_a_3= as.numeric(q86_a))
dataset <- mutate(dataset, bis1_a_4= as.numeric(q87_a))
dataset <- mutate(dataset, bis1_a_5= as.numeric(q88_a))  
dataset <- mutate(dataset, bis1_a_6= as.numeric(q89_a))
dataset <- mutate(dataset, bis1_a_7= (as.numeric(q90_a)*(-1)+5))
dataset <- mutate(dataset, bis1_a_8= (as.numeric(q91_a)*(-1)+5))
dataset <- mutate(dataset, bis1_a_9= (as.numeric(q92_a)*(-1)+5))
dataset <- mutate(dataset, bis1_a_10= (as.numeric(q93_a)*(-1)+5))
dataset <- mutate(dataset, bis1_a_11= as.numeric(q94_a))
dataset <- mutate(dataset, bis1_a_12= (as.numeric(q95_a)*(-1)+5))
dataset <- mutate(dataset, bis1_a_13= (as.numeric(q96_a)*(-1)+5))
dataset <- mutate(dataset, bis1_a_14= as.numeric(q97_a))
dataset <- mutate(dataset, bis1_a_15= (as.numeric(q98_a)*0)) #removed
dataset <- mutate(dataset, bis1_a_16= as.numeric(q99_a))
dataset <- mutate(dataset, bis1_a_17= as.numeric(q100_a))
dataset <- mutate(dataset, bis1_a_18= as.numeric(q101_a))
dataset <- mutate(dataset, bis1_a_19= as.numeric(q102_a))
dataset <- mutate(dataset, bis1_a_20= (as.numeric(q103_a)*(-1)+5))

dataset <- mutate(dataset, bis1_a_21= (as.numeric(q104_a)*0))  #removed
dataset <- mutate(dataset, bis1_a_22= as.numeric(q105_a))
dataset <- mutate(dataset, bis1_a_23= (as.numeric(q106_a)*0))  #removed
dataset <- mutate(dataset, bis1_a_24= as.numeric(q107_a))
dataset <- mutate(dataset, bis1_a_25= as.numeric(q108_a))
dataset <- mutate(dataset, bis1_a_26= as.numeric(q109_a))
dataset <- mutate(dataset, bis1_a_27= (as.numeric(q110_a)*0))  #removed
dataset <- mutate(dataset, bis1_a_28= as.numeric(q111_a))
dataset <- mutate(dataset, bis1_a_29= (as.numeric(q112_a)*0))  #removed
dataset <- mutate(dataset, bis1_a_30= (as.numeric(q113_a)*(-1)+5))

dataset <- mutate(dataset, bis1_attention= (bis1_a_5+bis1_a_9+bis1_a_11+ bis1_a_20+bis1_a_28))
dataset <- mutate(dataset, bis1_cognitive_instability= (bis1_a_6+bis1_a_24+bis1_a_26))
dataset <- mutate(dataset, bis1_motor= (bis1_a_2+bis1_a_3+bis1_a_4+ bis1_a_17+bis1_a_19+bis1_a_22+bis1_a_25))
dataset <- mutate(dataset, bis1_perseverance= (bis1_a_16+bis1_a_21+bis1_a_23+ bis1_a_30))
dataset <- mutate(dataset, bis1_self_control= (bis1_a_1+bis1_a_7+bis1_a_8+ bis1_a_12+bis1_a_13+bis1_a_14))
dataset <- mutate(dataset, bis1_cognitive_complexity= (bis1_a_10+bis1_a_15+bis1_a_18+ bis1_a_27+bis1_a_29))

dataset <- mutate(dataset, bis1_attentional= bis1_attention+ bis1_cognitive_instability)
dataset <- mutate(dataset, bis1_motor_all= bis1_motor +bis1_perseverance)
dataset <- mutate(dataset, bis1_nonplanning= bis1_self_control + bis1_cognitive_complexity)

dataset <- mutate(dataset, bis1_behavioral= (bis1_a_6+bis1_a_16+ bis1_a_17+bis1_a_19+bis1_a_21+bis1_a_24+bis1_a_26))
dataset <- mutate(dataset, bis1_cognitive= (bis1_a_1+bis1_a_7+ bis1_a_8+bis1_a_9+bis1_a_12+bis1_a_20))
dataset <- mutate(dataset, bis1_total= (bis1_a_1+bis1_a_2 +bis1_a_3 +bis1_a_4 +bis1_a_5 +bis1_a_6
                                       +bis1_a_7 +bis1_a_8 +bis1_a_9 +bis1_a_10 +bis1_a_11 +bis1_a_12 +bis1_a_13
                                       +bis1_a_14 +bis1_a_15 +bis1_a_16 +bis1_a_17 +bis1_a_18 +bis1_a_19 +bis1_a_20
                                       +bis1_a_21 +bis1_a_22 +bis1_a_23 +bis1_a_24 +bis1_a_25 +bis1_a_26 +bis1_a_27
                                       +bis1_a_28 +bis1_a_29 +bis1_a_30))

dataset <- mutate(dataset, bis1_inability_to_plan= (bis1_a_1+bis1_a_2 
                                        +bis1_a_7 +bis1_a_8 +bis1_a_9 +bis1_a_12  +bis1_a_20 +bis1_a_30))
dataset <- mutate(dataset, bis1_lack_of_selfcontrol= (bis1_a_5+bis1_a_6 +bis1_a_10 +bis1_a_11 +bis1_a_13
                                                    +bis1_a_18 +bis1_a_22 +bis1_a_24 +bis1_a_25  +bis1_a_26 +bis1_a_28))
dataset <- mutate(dataset, bis1_novelty_seeking= (bis1_a_3+bis1_a_4 
                                                    +bis1_a_14 +bis1_a_16 +bis1_a_17 +bis1_a_19  ))

#將numberic variables標準化
q22_0_a_mean <- mean(dataset$q22_0_a)
q22_0_a_sd <- sd(dataset$q22_0_a_)
dataset<- mutate(dataset,q22_0_a_standardized =(q22_0_a-q22_0_a_mean)/q22_0_a_sd)

peer1_mean <- mean(dataset$peer1)
peer1_sd <- sd(dataset$peer1)
dataset<- mutate(dataset,peer1_standardized =(peer1-peer1_mean)/peer1_sd)

firstrses_mean <- mean(dataset$firstrses)
firstrses_sd <- sd(dataset$firstrses)
dataset<- mutate(dataset,firstrses_standardized =(firstrses-firstrses_mean)/firstrses_sd)

q31_0_a_mean <- mean(dataset$q31_0_a)
q31_0_a_sd <- sd(dataset$q31_0_a)
dataset<- mutate(dataset,q31_0_a_standardized =(q31_0_a-q31_0_a_mean)/q31_0_a_sd)

AUDIT_C_a_mean <- mean(dataset$AUDIT_C_a)
AUDIT_C_a_sd <- sd(dataset$AUDIT_C_a)
dataset<- mutate(dataset,AUDIT_C_a_standardized =(AUDIT_C_a-AUDIT_C_a_mean)/AUDIT_C_a_sd)


bis1_mean <- mean(dataset$bis1_total)
bis1_sd <- sd(dataset$bis1_total)
dataset<- mutate(dataset,bis1_standardized =(bis1_total-bis1_mean)/bis1_sd)

bis1_inability_to_plan_mean <- mean(dataset$bis1_inability_to_plan)
bis1_inability_to_plan_sd <- sd(dataset$bis1_inability_to_plan)
dataset<- mutate(dataset,bis1_inability_to_plan_standardized =(bis1_inability_to_plan-bis1_inability_to_plan_mean)/bis1_inability_to_plan_sd)

bis1_lack_of_selfcontrol_mean <- mean(dataset$bis1_lack_of_selfcontrol)
bis1_lack_of_selfcontrol_sd <- sd(dataset$bis1_lack_of_selfcontrol)
dataset<- mutate(dataset,bis1_lack_of_selfcontrol_standardized =(bis1_lack_of_selfcontrol-bis1_lack_of_selfcontrol_mean)/bis1_lack_of_selfcontrol_sd)


bis1_novelty_seeking_mean <- mean(dataset$bis1_novelty_seeking)
bis1_novelty_seeking_sd <- sd(dataset$bis1_novelty_seeking)
dataset<- mutate(dataset,bis1_novelty_seeking_standardized =(bis1_novelty_seeking-bis1_novelty_seeking_mean)/bis1_novelty_seeking_sd)

####第二年
q22_0_b_mean <- mean(dataset$q22_0_b)
q22_0_b_sd <- sd(dataset$q22_0_b_)
dataset<- mutate(dataset,q22_0_b_standardized =(q22_0_b-q22_0_b_mean)/q22_0_b_sd)

peer2_mean <- mean(dataset$peer2)
peer2_sd <- sd(dataset$peer2)
dataset<- mutate(dataset,peer2_standardized =(peer2-peer2_mean)/peer2_sd)

secondtrses_mean <- mean(dataset$secondtrses)
secondtrses_sd <- sd(dataset$secondtrses)
dataset<- mutate(dataset,secondtrses_standardized =(secondtrses-secondtrses_mean)/secondtrses_sd)

q31_0_b_mean <- mean(dataset$q31_0_b)
q31_0_b_sd <- sd(dataset$q31_0_b)
dataset<- mutate(dataset,q31_0_b_standardized =(q31_0_b-q31_0_b_mean)/q31_0_b_sd)

AUDIT_C_b_mean <- mean(dataset$AUDIT_C_b)
AUDIT_C_b_sd <- sd(dataset$AUDIT_C_b)
dataset<- mutate(dataset,AUDIT_C_b_standardized =(AUDIT_C_b-AUDIT_C_b_mean)/AUDIT_C_b_sd)


bis2_mean <- mean(dataset$bis2)
bis2_sd <- sd(dataset$bis2)
dataset<- mutate(dataset,bis2_standardized =(bis2-bis2_mean)/bis2_sd)

bis2_inability_to_plan_mean <- mean(dataset$bis2_inability_to_plan)
bis2_inability_to_plan_sd <- sd(dataset$bis2_inability_to_plan)
dataset<- mutate(dataset,bis2_inability_to_plan_standardized =(bis2_inability_to_plan-bis2_inability_to_plan_mean)/bis2_inability_to_plan_sd)

bis2_lack_of_selfcontrol_mean <- mean(dataset$bis2_lack_of_selfcontrol)
bis2_lack_of_selfcontrol_sd <- sd(dataset$bis2_lack_of_selfcontrol)
dataset<- mutate(dataset,bis2_lack_of_selfcontrol_standardized =(bis2_lack_of_selfcontrol-bis2_lack_of_selfcontrol_mean)/bis2_lack_of_selfcontrol_sd)


bis2_novelty_seeking_mean <- mean(dataset$bis2_novelty_seeking)
bis2_novelty_seeking_sd <- sd(dataset$bis2_novelty_seeking)
dataset<- mutate(dataset,bis2_novelty_seeking_standardized =(bis2_novelty_seeking-bis2_novelty_seeking_mean)/bis2_novelty_seeking_sd)
# self esteem
dataset <-  mutate(dataset, q73_a_code= ifelse(q73_a=="完全不符合",1,ifelse(q73_a== "大部分不符合",2,ifelse(q73_a== "大部分符合",3, 4 ))))
dataset <-  mutate(dataset, q74_a_code= ifelse(q74_a=="完全不符合",4,ifelse(q74_a=="大部分不符合",3,ifelse(q74_a=="大部分符合",2, 1 ))))
dataset <-  mutate(dataset, q75_a_code= ifelse(q75_a=="完全不符合",1,ifelse(q75_a=="大部分不符合",2,ifelse(q75_a=="大部分符合",3, 4 ))))
dataset <-  mutate(dataset, q76_a_code= ifelse(q76_a=="完全不符合",1,ifelse(q76_a=="大部分不符合",2,ifelse(q76_a=="大部分符合",3, 4 ))))
dataset <-  mutate(dataset, q77_a_code= ifelse(q77_a=="完全不符合",4,ifelse(q77_a=="大部分不符合",3,ifelse(q77_a=="大部分符合",2, 1 ))))
dataset <-  mutate(dataset, q78_a_code= ifelse(q78_a=="完全不符合",4,ifelse(q78_a=="大部分不符合",3,ifelse(q78_a=="大部分符合",2, 1 ))))
dataset <-  mutate(dataset, q79_a_code= ifelse(q79_a=="完全不符合",1,ifelse(q79_a=="大部分不符合",2,ifelse(q79_a=="大部分符合",3, 4 ))))
dataset <-  mutate(dataset, q80_a_code= ifelse(q80_a=="完全不符合",4,ifelse(q80_a=="大部分不符合",3,ifelse(q80_a=="大部分符合",2, 1 ))))
dataset <-  mutate(dataset, q81_a_code= ifelse(q81_a=="完全不符合",4,ifelse(q81_a=="大部分不符合",3,ifelse(q81_a=="大部分符合",2, 1 ))))
dataset <-  mutate(dataset, q82_a_code= ifelse(q82_a=="完全不符合",1,ifelse(q82_a=="大部分不符合",2,ifelse(q82_a=="大部分符合",3, 4 ))))

dataset <-  mutate(dataset, firstyr_rses= (q73_a_code+q74_a_code+q75_a_code+q76_a_code+q77_a_code+q78_a_code+q79_a_code+q80_a_code+q81_a_code+q82_a_code))

dataset <-  mutate(dataset, firstyr_rses_pos= (q73_a_code+q75_a_code+q76_a_code+q79_a_code+q82_a_code))
dataset <-  mutate(dataset, firstyr_rses_neg= (firstyr_rses- firstyr_rses_pos))

# classify bully_involvement as 3 classes: bv, pureb, purev)

dataset <- mutate(dataset, bully_inv= ifelse(firstyr_bv=="yes", "3_bv", ifelse(firstyr_pureb=="yes", "4_pureb", ifelse(firstyr_purev=="yes", "2_purev", "1_not_inv"))))

dataset$bully_inv<- factor(dataset$bully_inv, levels= c("1_not_inv", "2_purev","3_bv", "4_pureb"))



dataset <- mutate(dataset, bully_inv_act= ifelse(firstyr_bv_act=="yes", "2_bv", ifelse(firstyr_pureb_act=="yes", "1_pureb", ifelse(firstyr_purev=="yes", "3_purev", "4_not_inv"))))


dataset<- mutate(dataset, bully_inv_more= ifelse(rec_bully_firstyr=="yes"& rec_victim_firstyr=="yes", "7_b2v2", 
                                                 ifelse(rec_bully_firstyr=="yes"&firstyr_vic2=="yes", "6_b2v1", 
                                                        ifelse(rec_bully_firstyr=="yes"&firstyr_vic2=="no", "9_b2v0", 
                                                               ifelse(rec_victim_firstyr=="yes"& firstyr_bully2=="yes", "5_b1v2", 
                                                                      ifelse(rec_victim_firstyr=="yes"& firstyr_bully2=="no", "3_b0v2", 
                                                                             ifelse(firstyr_bully2=="yes"& firstyr_vic2=="yes", "4_b1v1", 
                                                                                    ifelse(firstyr_bully2=="yes"& firstyr_vic2=="no", "8_b1v0", 
                                                                                           ifelse(firstyr_vic2=="yes", "2_b0v1", "1_b0v0")))))))))


dataset<-mutate(dataset, firstyrbully_1or0= ifelse(firstyrbully2=="有",1,0 ))
dataset<- mutate(dataset, rec_bully_1or0= ifelse(rec_bully_firstyr=="yes", 1,0))
dataset <- mutate(dataset, rec_bully_act_1or0=ifelse(rec_bully_firstyr_act=="yes", 1,0))
# create real age
dataset <-mutate(dataset, yr= 100.7- (q2y_a+(q2m_a)/12))
# 一次改所有的次dataset
dataset_table2<-dataset%>%filter(firstyrpurev=="無")
dataset_table3<-dataset%>%filter(firstyr_pureb=="no")
dataset_bv_vs_noninvolved<-dataset%>%filter(firstyr_pureb=="no" & firstyrpurev=="無")
dataset_pureb<-dataset%>%filter(firstyrvic2=="無")
dataset_purev<-dataset%>%filter(firstyr_bully2=="no") 
dataset_table2_rec<-dataset%>%filter(firstyrpurev=="無")
dataset_table3_rec<-dataset%>%filter(firstyrpureb=="無")
dataset_bv_vs_noninvolved<-dataset%>%filter(firstyrpureb=="無" & firstyrpurev=="無")






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




###



###

tempData <- mice(dataset_selected, m=5)
dataset_selected_imp <- mice::complete(tempData)

dataset_table2_imp<-dataset_selected_imp%>%filter(firstyrpurev=="無")
dataset_table3_imp<-dataset_selected_imp%>%filter(firstyr_pureb=="no")
dataset_bv_vs_noninvolved_imp<-dataset_selected_imp%>%filter(firstyr_pureb=="no" & firstyrpurev=="無")
dataset_b_imp<-dataset_selected_imp%>%filter(firstyrbully2=="有")
dataset_v_imp<-dataset_selected_imp%>%filter(firstyrvic2=="有")

dataset_pureb_imp<-dataset_selected_imp%>%filter(firstyrvic2=="無")
dataset_purev_imp<-dataset_selected_imp%>%filter(firstyr_bully2=="no") 

dataset_selected_ordinal_imp <- dataset_selected_imp
dataset_selected_ordinal_imp$bully_3group <- ordered(dataset_selected_ordinal_imp$bully_3group, levels= c("no_bullying", "1or2_times", "repeated_bullying"))
dataset_selected_ordinal_imp$victim_3group <- ordered(dataset_selected_ordinal_imp$victim_3group, levels= c("not_being_bullied", "1or2_times", "repeated_being_bullied"))

# write dataset_selected to rearranged_dataset.csv
write.table(dataset_selected_imp, output_file)





                 