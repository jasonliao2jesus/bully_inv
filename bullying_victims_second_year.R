# dependent variables: bullying and victimization: 改切點 

dataset_sec_yr<- dataset
##切點: 一個月二到三次
#victim
dataset_sec_yr<- mutate( dataset_sec_yr, q116_3_b_factor3=fct_collapse (q116_3_b,  no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )

dataset_sec_yr<- mutate( dataset_sec_yr, q116_4_b_factor3=fct_collapse (q116_4_b,  no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_5_b_factor3=fct_collapse (q116_5_b,  no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_6_b_factor3=fct_collapse (q116_6_b,  no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_7_b_factor3=fct_collapse (q116_7_b,  no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")))  
dataset_sec_yr<- mutate( dataset_sec_yr, q116_8_b_factor3=fct_collapse (q116_8_b,  no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_9_b_factor3=fct_collapse (q116_9_b,  no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_10_b_factor3=fct_collapse(q116_10_b, no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_11_b_factor3=fct_collapse(q116_11_b, no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_12_b_factor3=fct_collapse(q116_12_b, no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_14_b_factor3=fct_collapse(q116_14_b, no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次"))) 
#bully
dataset_sec_yr<- mutate( dataset_sec_yr, q116_36_b_factor3=fct_collapse(q116_36_b, no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_34_b_factor3=fct_collapse(q116_34_b, no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_33_b_factor3=fct_collapse(q116_33_b, no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次"))) 
dataset_sec_yr<- mutate( dataset_sec_yr, q116_32_b_factor3=fct_collapse(q116_32_b, no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次"))) 

dataset_sec_yr<- mutate( dataset_sec_yr, q116_31_b_factor3=fct_collapse(q116_31_b, no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_30_b_factor3=fct_collapse(q116_30_b, no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_29_b_factor3=fct_collapse(q116_29_b, no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )

dataset_sec_yr<- mutate( dataset_sec_yr, q116_28_b_factor3=fct_collapse(q116_28_b, no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_27_b_factor3=fct_collapse(q116_27_b, no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_26_b_factor3=fct_collapse(q116_26_b, no = c("無","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次"))) 
dataset_sec_yr<- mutate( dataset_sec_yr, q116_25_b_factor3=fct_collapse(q116_25_b, no = c("過去六個月來我在校未霸凌其也學生","只有一到二次"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次"))) 


## 切點: 約一星期一次
# vicim
dataset_sec_yr<- mutate( dataset_sec_yr, q116_3_b_factor4=fct_collapse(q116_3_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )

dataset_sec_yr<- mutate( dataset_sec_yr, q116_4_b_factor4=fct_collapse(q116_4_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_5_b_factor4=fct_collapse(q116_5_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_6_b_factor4=fct_collapse(q116_6_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_7_b_factor4=fct_collapse(q116_7_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_8_b_factor4=fct_collapse(q116_8_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_9_b_factor4=fct_collapse(q116_9_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_10_b_factor4=fct_collapse(q116_10_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_11_b_factor4=fct_collapse(q116_11_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_12_b_factor4=fct_collapse(q116_12_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_14_b_factor4=fct_collapse(q116_14_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )


# bully
dataset_sec_yr<- mutate( dataset_sec_yr, q116_36_b_factor4=fct_collapse(q116_36_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_34_b_factor4=fct_collapse(q116_34_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_33_b_factor4=fct_collapse(q116_33_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次"))) 
dataset_sec_yr<- mutate( dataset_sec_yr, q116_32_b_factor4=fct_collapse(q116_32_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次"))) 

dataset_sec_yr<- mutate( dataset_sec_yr, q116_31_b_factor4=fct_collapse(q116_31_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_30_b_factor4=fct_collapse(q116_30_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_29_b_factor4=fct_collapse(q116_29_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )

dataset_sec_yr<- mutate( dataset_sec_yr, q116_28_b_factor4=fct_collapse(q116_28_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_27_b_factor4=fct_collapse(q116_27_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_26_b_factor4=fct_collapse(q116_26_b, no = c("無","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次"))) 
dataset_sec_yr<- mutate( dataset_sec_yr, q116_25_b_factor4=fct_collapse(q116_25_b, no = c("過去六個月來我在校未霸凌其也學生","只有一到二次","一個月二到三次"), yes = c("約一個星期一次","一個星期好幾次"))) 


## 切點 只有一到二次

# vicim
dataset_sec_yr<- mutate( dataset_sec_yr, q116_3_b_factor1=fct_collapse(q116_3_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )

dataset_sec_yr<- mutate( dataset_sec_yr, q116_4_b_factor1=fct_collapse(q116_4_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_5_b_factor1=fct_collapse(q116_5_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_6_b_factor1=fct_collapse(q116_6_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_7_b_factor1=fct_collapse(q116_7_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_8_b_factor1=fct_collapse(q116_8_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_9_b_factor1=fct_collapse(q116_9_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_10_b_factor1=fct_collapse(q116_10_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_11_b_factor1=fct_collapse(q116_11_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_12_b_factor1=fct_collapse(q116_12_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_14_b_factor1=fct_collapse(q116_14_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )


#bully

dataset_sec_yr<- mutate( dataset_sec_yr, q116_36_b_factor1=fct_collapse(q116_36_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_34_b_factor1=fct_collapse(q116_34_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_33_b_factor1=fct_collapse(q116_33_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次"))) 
dataset_sec_yr<- mutate( dataset_sec_yr, q116_32_b_factor1=fct_collapse(q116_32_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次"))) 

dataset_sec_yr<- mutate( dataset_sec_yr, q116_31_b_factor1=fct_collapse(q116_31_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次", "一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_30_b_factor1=fct_collapse(q116_30_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_29_b_factor1=fct_collapse(q116_29_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_28_b_factor1=fct_collapse(q116_28_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_27_b_factor1=fct_collapse(q116_27_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次")) )
dataset_sec_yr<- mutate( dataset_sec_yr, q116_26_b_factor1=fct_collapse(q116_26_b, no = c("無"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次"))) 
dataset_sec_yr<- mutate( dataset_sec_yr, q116_25_b_factor1=fct_collapse(q116_25_b, no = c("過去六個月來我在校未霸凌其也學生"), yes = c("約一個星期一次","一個星期好幾次","一個月二到三次","只有一到二次"))) 







#bullying involvement, method 
# bully type

dataset_sec_yr<- mutate( dataset_sec_yr, physicalbully_sec=ifelse(q116_28_b=="無", "無", "有"))
dataset_sec_yr$physicalbully_sec <-factor(dataset_sec_yr$physicalbully_sec, levels=c("無","有") )

dataset_sec_yr<- mutate( dataset_sec_yr, verbalbully_sec=ifelse(q116_26_b=="無"
                                                                #&q116_32_b=="無" & q116_33_b=="無"
                                                                , "無", "有"))
dataset_sec_yr$verbalbully_sec <-factor(dataset_sec_yr$verbalbully_sec, levels=c("無","有") )

dataset_sec_yr<- mutate( dataset_sec_yr, relationalbully_sec=ifelse(q116_27_b=="無"&q116_29_b=="無", "無", "有"))
dataset_sec_yr$relationalbully_sec <-factor(dataset_sec_yr$relationalbully_sec, levels=c("無","有") )

dataset_sec_yr<- mutate( dataset_sec_yr, 
                         otherbully_sec=
                           ifelse(q116_30_b=="無"&q116_31_b=="無"  #&q116_32_b=="無"&q116_33_b=="無"
                                  , "無", "有"))
dataset_sec_yr$otherbully_sec <-factor(dataset_sec_yr$otherbully_sec, levels=c("無","有") )
  # active vs passive

# victim type

dataset_sec_yr<- mutate( dataset_sec_yr, physicalvictim_sec=ifelse(q116_6_b=="無", "無", "有"))
dataset_sec_yr$physicalvictim_sec <-factor(dataset_sec_yr$physicalvictim_sec, levels=c("無","有") )

dataset_sec_yr<- mutate( dataset_sec_yr, verbalvictim_sec=ifelse(q116_4_b=="無"#&q116_10_b=="無"&q116_11_b=="無"
                                                                 , "無", "有"
                                                                 ))
dataset_sec_yr$verbalvictim_sec <-factor(dataset_sec_yr$verbalvictim_sec, levels=c("無","有") )

dataset_sec_yr<- mutate( dataset_sec_yr, relationalvictim_sec=ifelse(q116_7_b=="無"&q116_5_b=="無", "無", "有"))
dataset_sec_yr$relationalvictim_sec <-factor(dataset_sec_yr$relationalvictim_sec, levels=c("無","有") )

dataset_sec_yr<- mutate( dataset_sec_yr, othervictim_sec=ifelse(q116_8_b=="無"& q116_9_b=="無"& q116_12_b=="無"&q116_14_b=="無", "無", "有"))
dataset_sec_yr$othervictim_sec <-factor(dataset_sec_yr$othervictim_sec, levels=c("無","有") )


# recurrent bully


dataset_sec_yr<- mutate(dataset_sec_yr, rec_bully_physical_secyr= ifelse(q116_28_b_factor3== "yes", "yes","no"))  
dataset_sec_yr<- mutate(dataset_sec_yr, rec_bully_verbal_secyr= ifelse(q116_26_b_factor3== "yes"|q116_32_b_factor3=="yes"|q116_33_b_factor3=="yes", "yes","no"))  
dataset_sec_yr<- mutate(dataset_sec_yr, rec_bully_relational_secyr= ifelse(q116_11_b_factor3== "yes"|q116_12_b_factor3=="yes", "yes","no"))
dataset_sec_yr<- mutate(dataset_sec_yr, rec_bully_other_secyr= ifelse(q116_27_b_factor3== "yes"|q116_29_b_factor3=="yes", "yes","no"))  
dataset_sec_yr<- mutate(dataset_sec_yr, rec_bully_secyr= ifelse(q116_25_b_factor3=="yes"|
  q116_26_b_factor3=="yes"|q116_27_b_factor3=="yes"|q116_28_b_factor3=="yes"|
    q116_29_b_factor3=="yes"|q116_30_b_factor3=="yes"|q116_31_b_factor3=="yes"|
    #q116_32_b_factor3=="yes"| q116_33_b_factor3=="yes"| 
    q116_34_b_factor3=="yes"|
    q116_36_b_factor3=="yes"
  , "yes","no")) 
dataset_sec_yr$rec_bully_physical_secyr <- factor(dataset_sec_yr$rec_bully_physical_secyr, levels= c("no", "yes"))
dataset_sec_yr$rec_bully_verbal_secyr <- factor(dataset_sec_yr$rec_bully_verbal_secyr, levels= c("no", "yes"))
dataset_sec_yr$rec_bully_relational_secyr <- factor(dataset_sec_yr$rec_bully_relational_secyr, levels= c("no", "yes"))

dataset_sec_yr$rec_bully_secyr <- factor(dataset_sec_yr$rec_bully_secyr, levels= c("no", "yes"))

# recurrent victims
dataset_sec_yr<- mutate(dataset_sec_yr, rec_victim_physical_secyr= ifelse(q116_6_b_factor3== "yes", "yes","no"))  
dataset_sec_yr<- mutate(dataset_sec_yr, rec_victim_verbal_secyr= ifelse(q116_4_b_factor3== "yes"|q116_10_b_factor3=="yes"|q116_11_b_factor3=="yes", "yes","no"))  
dataset_sec_yr<- mutate(dataset_sec_yr, rec_victim_relational_secyr= ifelse(q116_5_b_factor3== "yes"|q116_7_b_factor3=="yes", "yes","no"))
dataset_sec_yr<- mutate(dataset_sec_yr, rec_victim_other_secyr= ifelse(q116_8_b_factor3== "yes"|q116_9_b_factor3=="yes"|q116_12_b_factor3=="yes"|q116_14_b_factor3=="yes", "yes","no"))  
dataset_sec_yr<- mutate(dataset_sec_yr, rec_victim_secyr= ifelse(q116_3_b_factor3== "yes"|q116_4_b_factor3== "yes"|
                                                                   q116_5_b_factor3== "yes"|q116_6_b_factor3== "yes"|
                                                                   q116_7_b_factor3=="yes"|q116_8_b_factor3== "yes"|
                                                                   q116_9_b_factor3=="yes"|
                                                                   #q116_10_b_factor3=="yes"|q116_11_b_factor3=="yes"|
                                                                   q116_12_b_factor3=="yes"|
                                                                   q116_14_b_factor3=="yes" , "yes","no")) 

dataset_sec_yr$rec_victim_secyr <- factor(dataset_sec_yr$rec_victim_secyr, levels= c("no", "yes"))

dataset_sec_yr$rec_victim_physical_secyr <- factor(dataset_sec_yr$rec_victim_physical_secyr, levels= c("no", "yes"))
dataset_sec_yr$rec_victim_verbal_secyr <- factor(dataset_sec_yr$rec_victim_verbal_secyr, levels= c("no", "yes"))
dataset_sec_yr$rec_victim_relational_secyr <- factor(dataset_sec_yr$rec_victim_relational_secyr, levels= c("no", "yes"))



dataset_sec_yr<- mutate(dataset_sec_yr, secyr_rec_bv= ifelse(rec_victim_secyr=="yes"& rec_bully_secyr=="yes", "yes", "no"))
dataset_sec_yr$rec_bv <- factor(dataset_sec_yr$secyr_rec_bv, levels= c("no", "yes"))

# bully  group 1, 2, 3, 4: 1. non involved, "無"     2."只有一到二次"  3."一個月二到三次"         4."約一個星期一次"&"一個星期好幾次"


dataset_sec_yr<- mutate(dataset_sec_yr, secyr_bully2= ifelse( q116_25_b_factor1=="yes"|q116_26_b_factor1=="yes"|q116_27_b_factor1=="yes"|q116_28_b_factor1=="yes"|
                                                               q116_29_b_factor1=="yes"|q116_30_b_factor1=="yes"|q116_31_b_factor1=="yes"|
                                                               #q116_32_b_factor1=="yes"| q116_33_b_factor1=="yes"| 
                                                                q116_34_b_factor1=="yes"|
                                                               q116_36_b_factor1=="yes", "yes","no")) 
dataset_sec_yr$secyr_bully2<- factor(dataset_sec_yr$secyr_bully2, levels= c("no", "yes"))

dataset_sec_yr<- mutate(dataset_sec_yr, secondyrbully2= ifelse( q116_25_b_factor1=="yes"|q116_26_b_factor1=="yes"|q116_27_b_factor1=="yes"|q116_28_b_factor1=="yes"|
                                                                q116_29_b_factor1=="yes"|q116_30_b_factor1=="yes"|q116_31_b_factor1=="yes"|
                                                                #q116_32_b_factor1=="yes"| q116_33_b_factor1=="yes"| 
                                                                  q116_34_b_factor1=="yes"|
                                                                q116_36_b_factor1=="yes", "有","無")) 
dataset_sec_yr$secondyrbully2<- factor(dataset_sec_yr$secondyrbully2, levels= c("無", "有"))





dataset_sec_yr<- mutate(dataset_sec_yr, bully_sec_group= ifelse(q116_25_b_factor4=="yes"|q116_26_b_factor4=="yes"|q116_27_b_factor4=="yes"|q116_28_b_factor4=="yes"|
                                                                  q116_29_b_factor4=="yes"|q116_30_b_factor4=="yes"|q116_31_b_factor4=="yes"|
                                                                  #q116_32_b_factor4=="yes"| q116_33_b_factor4=="yes"| 
                                                                  q116_34_b_factor4=="yes"|
                                                                  q116_36_b_factor4=="yes", "group4",
                                                                ifelse(rec_bully_secyr=="yes","group3",
                                                                       ifelse(secyr_bully2=="yes", "group2","group1"))))  
dataset_sec_yr$bully_sec_group <- factor(dataset_sec_yr$bully_sec_group, levels= c("group1", "group2", "group3", "group4"))

dataset_sec_yr  <- mutate(dataset_sec_yr, bully_sec_3group=ifelse(bully_sec_group=="group3"|bully_sec_group=="group4", "repeated_bullying", ifelse(bully_sec_group=="group2","1or2_times", "no_bullying")))        

dataset_sec_yr$bully_sec_3group <- factor(dataset_sec_yr$bully_sec_3group, levels= c("no_bullying", "1or2_times", "repeated_bullying"))

   # active bully
dataset_sec_yr<- mutate(dataset_sec_yr, rec_bully_secyr_act= ifelse(
  q116_26_b_factor3=="yes"|q116_28_b_factor3=="yes"|
    q116_30_b_factor3=="yes"|q116_31_b_factor3=="yes"
  
    #q116_32_b_factor3=="yes"| q116_33_b_factor3=="yes" 
  , "yes","no")) 

dataset_sec_yr<- mutate(dataset_sec_yr, secyr_bully2_act= ifelse(q116_26_b_factor1=="yes"|q116_28_b_factor1=="yes"|
                                                              q116_30_b_factor1=="yes"|q116_31_b_factor1=="yes",
                                                              # q116_32_b_factor1=="yes"| q116_33_b_factor1=="yes"
                                                                "yes","no")) 

dataset_sec_yr$secyr_bully2_act<- factor(dataset_sec_yr$secyr_bully2_act, levels= c("no", "yes"))

dataset_sec_yr<- mutate(dataset_sec_yr, bully_sec_group_act= ifelse(q116_26_b_factor4=="yes"|q116_28_b_factor4=="yes"|
                                                                  q116_30_b_factor4=="yes"|q116_31_b_factor4=="yes",
                                                               #   q116_32_b_factor4=="yes"| q116_33_b_factor4=="yes"
                                                                   "group4",
                                                                ifelse(rec_bully_secyr_act=="yes","group3",
                                                                       ifelse(secyr_bully2_act=="yes", "group2","group1"))))  
dataset_sec_yr$bully_sec_group_act <- factor(dataset_sec_yr$bully_sec_group_act, levels= c("group1", "group2", "group3", "group4"))


dataset_sec_yr  <- mutate(dataset_sec_yr, bully_sec_3group_act=ifelse(bully_sec_group_act=="group3"|bully_sec_group_act=="group4", "repeated_bullying", ifelse(bully_sec_group_act=="group2","1or2_times", "no_bullying")))        
dataset_sec_yr$bully_sec_3group_act <- factor(dataset_sec_yr$bully_sec_3group_act, levels= c("no_bullying", "1or2_times", "repeated_bullying"))

    # passive bullying
  
# 4 victim group: 



dataset_sec_yr<- mutate(dataset_sec_yr, secyr_vic2= ifelse(q116_3_b_factor1== "yes"|q116_4_b_factor1== "yes"|q116_5_b_factor1== "yes"|
                                                             q116_6_b_factor1== "yes"|q116_7_b_factor1=="yes"|
                                                             q116_8_b_factor1== "yes"|q116_9_b_factor1=="yes"|
                                                             #q116_10_b_factor1=="yes"|q116_11_b_factor1=="yes"|
                                                             q116_12_b_factor1=="yes"|q116_14_b_factor1=="yes", "yes","no")) 
dataset_sec_yr$secyr_vic2 <- factor(dataset_sec_yr$secyr_vic2, levels= c("no", "yes")) 

dataset_sec_yr<- mutate(dataset_sec_yr, victim_sec_group= ifelse(q116_3_b_factor4== "yes"|q116_4_b_factor4== "yes"|
                                                                   q116_5_b_factor4== "yes"|q116_6_b_factor4== "yes"|
                                                                   q116_7_b_factor4=="yes"|q116_8_b_factor4== "yes"|
                                                                   q116_9_b_factor4=="yes"|q116_10_b_factor4=="yes",
                                                              #     q116_11_b_factor4=="yes"|q116_12_b_factor4=="yes"|q116_14_b_factor4=="yes"
                                                                 "v_group4",
                                                                 ifelse(rec_victim_secyr=="yes","v_group3",
                                                                        ifelse(secyr_vic2=="yes", "v_group2","v_group1"))))  
dataset_sec_yr$victim_sec_group <- factor(dataset_sec_yr$victim_sec_group, levels= c("v_group1", "v_group2", "v_group3", "v_group4"))

dataset_sec_yr<- mutate( dataset_sec_yr, secyr_pureb=ifelse(victim_sec_group=="v_group1"& bully_sec_group!="group1", "yes","no") ) 
dataset_sec_yr<- mutate( dataset_sec_yr, secyr_purev=ifelse(victim_sec_group!="v_group1"& bully_sec_group=="group1", "yes","no") )  
dataset_sec_yr<- mutate( dataset_sec_yr, secyr_bv=ifelse(victim_sec_group!="v_group1"& bully_sec_group!="group1", "yes","no") ) 
dataset_sec_yr$secyr_pureb <- factor(dataset_sec_yr$secyr_pureb, levels= c("no", "yes"))
dataset_sec_yr$secyr_purev <- factor(dataset_sec_yr$secyr_purev, levels= c("no", "yes"))
dataset_sec_yr$secyr_bv <- factor(dataset_sec_yr$secyr_bv, levels= c("no", "yes"))



dataset_sec_yr <- mutate(dataset_sec_yr, victim_sec_3group=ifelse(victim_sec_group=="v_group3"|victim_sec_group=="v_group4", "v2_repeated_being_bullied", ifelse(victim_sec_group=="v_group2","v1_1or2_times", "v0_not_being_bullied")))        
dataset_sec_yr$victim_sec_3group <- factor(dataset_sec_yr$victim_sec_3group, levels= c("v0_not_being_bullied", "v1_1or2_times", "v2_repeated_being_bullied"))

##victim of active bullying

dataset_sec_yr<- mutate(dataset_sec_yr, rec_victim_secyr_act= ifelse( q116_4_b_factor3== "yes"| 
                                                            q116_5_b_factor3== "yes"|
                                                            q116_6_b_factor3=="yes" 
                                                          , "yes","no")) 

dataset_sec_yr$rec_victim_secyr_act <- factor(dataset_sec_yr$rec_victim_secyr_act, levels= c("no", "yes"))        


dataset_sec_yr<- mutate(dataset_sec_yr, secyr_victim_act= ifelse(q116_4_b_factor1== "yes"|q116_5_b_factor1== "yes"|q116_6_b_factor1=="yes" 
                                                     , "yes","no")) 

dataset_sec_yr$secyr_victim_act<- factor(dataset_sec_yr$secyr_victim_act, levels= c("no", "yes"))


dataset_sec_yr<- mutate(dataset_sec_yr, victim_group_act_2= ifelse(q116_4_b_factor4== "yes"| q116_5_b_factor4== "yes"|q116_6_b_factor4=="yes" 
                                                   , "group4",
                                                   ifelse(rec_victim_secyr_act=="yes","group3",
                                                          ifelse(secyr_victim_act=="yes", "group2","group1"))))  
dataset_sec_yr$victim_group_act_2 <- factor(dataset_sec_yr$victim_group_act_2, levels= c("group1", "group2", "group3", "group4"))

dataset_sec_yr <- mutate(dataset_sec_yr, victim_3group_act_2=ifelse(victim_group_act_2=="group3"|victim_group_act_2=="group4", 
                                                    "v2_repeated_being_bullied", 
                                                    ifelse(victim_group_act_2=="group2",
                                                           "v1_1or2_times", "v0_not_being_bullied"))) 
dataset_sec_yr$victim_3group_act_2 <- factor(dataset_sec_yr$victim_3group_act_2, levels= c("v0_not_being_bullied", "v1_1or2_times", "v2_repeated_being_bullied")) 

## incidence
dataset_sec_yr <- mutate(dataset_sec_yr, less_bully= ifelse(firstyr_bully2== "yes"& secyr_bully2=="no","yes","no" ))
dataset_sec_yr$less_bully <- factor(dataset_sec_yr$less_bully, levels = c("no","yes"))

dataset_sec_yr   <- mutate(dataset_sec_yr, new_bully= ifelse(firstyr_bully2== "no"& secyr_bully2=="yes","yes","no" ))
dataset_sec_yr$new_bully <- factor(dataset_sec_yr$new_bully, levels = c("no","yes"))

#re-arrange and re-prioritize factors

#re-arrange and re-prioritize factors
# PHQ9分群
dataset_sec_yr <- mutate( dataset_sec_yr, PHQ9_2yr = ifelse(dataset_sec_yr$q31_0_b<6, "0-5", ifelse(dataset_sec_yr$q31_0_b<11, "6-10", ifelse(dataset_sec_yr$q31_0_b<15, "11-14","15以上"))))

dataset_sec_yr$PHQ9_2yr <- factor(dataset_sec_yr$PHQ9_2yr, levels=c("0-5","6-10","11-14","15以上"))
# gender, school_PR, reside with parents, needy family 父母就業
dataset_sec_yr$school_PR <- factor(dataset_sec_yr$school_PR, levels=c("PR70-89","PR<70"))
dataset_sec_yr$q3_b <- factor(dataset_sec_yr$q3_b, levels=c("女","男"))
dataset_sec_yr$q8_b <- factor(dataset_sec_yr$q8_b, levels=c("是","否"))
dataset_sec_yr$q7_b <- factor(dataset_sec_yr$q7_b, levels=c("否","是"))
dataset_sec_yr$q5_b <- factor(dataset_sec_yr$q5_b, levels=c("有","沒有"))
dataset_sec_yr$q6_b <- factor(dataset_sec_yr$q6_b, levels=c("有","沒有"))

dataset_sec_yr$q116_24_b<- factor(dataset_sec_yr$q116_24_b, levels=c("替他/她感到遺憾且想協助他/她","替他/她感到有點遺憾","沒有很多感覺","這大概是他/她活該得的"))
dataset_sec_yr$q116_24_b <- ordered(dataset_sec_yr$q116_24_b, levels=c("替他/她感到遺憾且想協助他/她","替他/她感到有點遺憾","沒有很多感覺","這大概是他/她活該得的"))
dataset_sec_yr<- mutate(dataset_sec_yr,  see_bully=dataset_sec_yr$q116_24_b)


# school and individual attitude for bullying involvement
dataset_sec_yr$q116_22_b<- factor(dataset_sec_yr$q116_22_b, levels=c("幾乎總是","常常","偶爾","有時候","幾乎從來沒有"))
#dataset_sec_yr$q116_22_b<- ordered(dataset_sec_yr$q116_22_b, levels=c("幾乎總是","常常","偶爾","有時候","幾乎從來沒有"))
dataset_sec_yr<- mutate(dataset_sec_yr,  classmates_action=dataset_sec_yr$q116_22_b)

dataset_sec_yr$q116_21_b<- factor(dataset_sec_yr$q116_21_b, levels=c("幾乎總是","常常","偶爾","有時候","幾乎從來沒有"))
#dataset_sec_yr$q116_21_b<- ordered(dataset_sec_yr$q116_21_b, levels=c("幾乎總是","常常","偶爾","有時候","幾乎從來沒有"))
dataset_sec_yr<- mutate(dataset_sec_yr,  schoolstaff_action=dataset_sec_yr$q116_21_b)

dataset_sec_yr$q116_41_b<- factor(dataset_sec_yr$q116_41_b, levels=c("從未","很少","有時候","時常","頻繁","非常頻繁"))
#dataset_sec_yr$q116_41_b<- ordered(dataset_sec_yr$q116_41_b, levels=c("從未","很少","有時候","時常","頻繁","非常頻繁"))
dataset_sec_yr<- mutate(dataset_sec_yr,  worry_being_bullied=dataset_sec_yr$q116_41_b)

dataset_sec_yr$q116_42_b<- factor(dataset_sec_yr$q116_42_b, levels=c("沒有或很少","較少","有一些","蠻多的","非常多"))
#dataset_sec_yr$q116_42_b<- ordered(dataset_sec_yr$q116_42_b, levels=c("沒有或很少","較少","有一些","蠻多的","非常多"))
dataset_sec_yr<- mutate(dataset_sec_yr,  tutor_action=dataset_sec_yr$q116_42_b)



#BIS subscale
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_1= (as.numeric(q84_b)*(-1)+5))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_2= as.numeric(q85_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_3= as.numeric(q86_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_4= as.numeric(q87_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_5= as.numeric(q88_b))  
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_6= as.numeric(q89_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_7= (as.numeric(q90_b)*(-1)+5))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_8= (as.numeric(q91_b)*(-1)+5))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_9= (as.numeric(q92_b)*(-1)+5))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_10= (as.numeric(q93_b)*(-1)+5))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_11= as.numeric(q94_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_12= (as.numeric(q95_b)*(-1)+5))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_13= (as.numeric(q96_b)*(-1)+5))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_14= as.numeric(q97_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_15= (as.numeric(q98_b)*0)) #removed
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_16= as.numeric(q99_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_17= as.numeric(q100_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_18= as.numeric(q101_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_19= as.numeric(q102_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_20= (as.numeric(q103_b)*(-1)+5))

dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_21= (as.numeric(q104_b)*0))  #removed
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_22= as.numeric(q105_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_23= (as.numeric(q106_b)*0))  #removed
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_24= as.numeric(q107_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_25= as.numeric(q108_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_26= as.numeric(q109_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_27= (as.numeric(q110_b)*0))  #removed
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_28= as.numeric(q111_b))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_29= (as.numeric(q112_b)*0))  #removed
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_b_30= (as.numeric(q113_b)*(-1)+5))

dataset_sec_yr <- mutate(dataset_sec_yr, bis2_bttention= (bis2_b_5+bis2_b_9+bis2_b_11+ bis2_b_20+bis2_b_28))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_cognitive_instability= (bis2_b_6+bis2_b_24+bis2_b_26))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_motor= (bis2_b_2+bis2_b_3+bis2_b_4+ bis2_b_17+bis2_b_19+bis2_b_22+bis2_b_25))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_perseverance= (bis2_b_16+bis2_b_21+bis2_b_23+ bis2_b_30))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_self_control= (bis2_b_1+bis2_b_7+bis2_b_8+ bis2_b_12+bis2_b_13+bis2_b_14))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_cognitive_complexity= (bis2_b_10+bis2_b_15+bis2_b_18+ bis2_b_27+bis2_b_29))

dataset_sec_yr <- mutate(dataset_sec_yr, bis2_bttentional= bis2_bttention+ bis2_cognitive_instability)
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_motor_bll= bis2_motor +bis2_perseverance)
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_nonplanning= bis2_self_control + bis2_cognitive_complexity)

dataset_sec_yr <- mutate(dataset_sec_yr, bis2_behavioral= (bis2_b_6+bis2_b_16+ bis2_b_17+bis2_b_19+bis2_b_21+bis2_b_24+bis2_b_26))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_cognitive= (bis2_b_1+bis2_b_7+ bis2_b_8+bis2_b_9+bis2_b_12+bis2_b_20))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_total= (bis2_b_1+bis2_b_2 +bis2_b_3 +bis2_b_4 +bis2_b_5 +bis2_b_6
                                                      +bis2_b_7 +bis2_b_8 +bis2_b_9 +bis2_b_10 +bis2_b_11 +bis2_b_12 +bis2_b_13
                                                      +bis2_b_14 +bis2_b_15 +bis2_b_16 +bis2_b_17 +bis2_b_18 +bis2_b_19 +bis2_b_20
                                                      +bis2_b_21 +bis2_b_22 +bis2_b_23 +bis2_b_24 +bis2_b_25 +bis2_b_26 +bis2_b_27
                                                      +bis2_b_28 +bis2_b_29 +bis2_b_30))

dataset_sec_yr <- mutate(dataset_sec_yr, bis2_inability_to_plan= (bis2_b_1+bis2_b_2 
                                                                  +bis2_b_7 +bis2_b_8 +bis2_b_9 +bis2_b_12  +bis2_b_20 +bis2_b_30))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_lack_of_selfcontrol= (bis2_b_5+bis2_b_6 +bis2_b_10 +bis2_b_11 +bis2_b_13
                                                                    +bis2_b_18 +bis2_b_22 +bis2_b_24 +bis2_b_25  +bis2_b_26 +bis2_b_28))
dataset_sec_yr <- mutate(dataset_sec_yr, bis2_novelty_seeking= (bis2_b_3+bis2_b_4 
                                                                +bis2_b_14 +bis2_b_16 +bis2_b_17 +bis2_b_19  ))

####第二年MDSS
# 加上MDSS 各分項第一題 主問題
dataset_sec_yr <- mutate(dataset_sec_yr, parent2_talk = ifelse(q11_b=="從未", 1, ifelse(q11_b=="有時", 2, ifelse(q11_b=="經常", 3,4))))
dataset_sec_yr <- mutate(dataset_sec_yr, teacher2_talk = ifelse(q20_b=="從未", 1, ifelse(q20_b=="有時", 2, ifelse(q20_b=="經常", 3,4))))
dataset_sec_yr <- mutate(dataset_sec_yr, peer2_talk = ifelse(q17_b=="從未", 1, ifelse(q17_b=="有時", 2, ifelse(q17_b=="經常", 3,4))))
dataset_sec_yr <- mutate(dataset_sec_yr, relative2_talk = ifelse(q14_b=="從未", 1, ifelse(q14_b=="有時", 2, ifelse(q14_b=="經常", 3,4))))


# MDSS 各分項第一題 次問題
dataset_sec_yr <- mutate(dataset_sec_yr, parent2_would_rather = ifelse(q12_b=="多一點", 1, ifelse(q12_b=="少一點", 2, 3)))
dataset_sec_yr <- mutate(dataset_sec_yr, teacher2_would_rather = ifelse(q21_b=="多一點", 1, ifelse(q21_b=="少一點", 2, 3)))
dataset_sec_yr <- mutate(dataset_sec_yr, peer2_would_rather = ifelse(q18_b=="多一點", 1, ifelse(q18_b=="少一點", 2, 3)))
dataset_sec_yr <- mutate(dataset_sec_yr, relative2_would_rather = ifelse(q15_b=="多一點", 1, ifelse(q15_b=="少一點", 2, 3)))

dataset_sec_yr <- mutate(dataset_sec_yr, parent2_MDSS = parent2_talk+ parent2_would_rather) 
dataset_sec_yr <- mutate(dataset_sec_yr, teacher2_MDSS = teacher2_talk+ teacher2_would_rather)
dataset_sec_yr <- mutate(dataset_sec_yr, peer2_MDSS = peer2_talk+ peer2_would_rather)
dataset_sec_yr <- mutate(dataset_sec_yr, relative_MDSS = relative2_talk+ relative2_would_rather)

q22_0_b_mean <- mean(dataset_sec_yr$q22_0_b)
q22_0_b_sd <- sd(dataset_sec_yr$q22_0_b_)
dataset_sec_yr<- mutate(dataset_sec_yr,q22_0_b_standardized =(q22_0_b-q22_0_b_mean)/q22_0_b_sd)

peer2_mean <- mean(dataset_sec_yr$peer2)
peer2_sd <- sd(dataset_sec_yr$peer2)
dataset_sec_yr<- mutate(dataset_sec_yr,peer2_standardized =(peer2-peer2_mean)/peer2_sd)

secondtrses_mean <- mean(dataset_sec_yr$secondtrses)
secondtrses_sd <- sd(dataset_sec_yr$secondtrses)
dataset_sec_yr<- mutate(dataset_sec_yr,secondtrses_standardized =(secondtrses-secondtrses_mean)/secondtrses_sd)

q31_0_b_mean <- mean(dataset_sec_yr$q31_0_b)
q31_0_b_sd <- sd(dataset_sec_yr$q31_0_b)
dataset_sec_yr<- mutate(dataset_sec_yr,q31_0_b_standardized =(q31_0_b-q31_0_b_mean)/q31_0_b_sd)

AUDIT_C_b_mean <- mean(dataset_sec_yr$AUDIT_C_b)
AUDIT_C_b_sd <- sd(dataset_sec_yr$AUDIT_C_b)
dataset_sec_yr<- mutate(dataset_sec_yr,AUDIT_C_b_standardized =(AUDIT_C_b-AUDIT_C_b_mean)/AUDIT_C_b_sd)


bis2_mean <- mean(dataset_sec_yr$bis2_total)
bis2_sd <- sd(dataset_sec_yr$bis2_total)
dataset_sec_yr<- mutate(dataset_sec_yr,bis2_standardized =(bis2_total-bis2_mean)/bis2_sd)

bis2_inability_to_plan_mean <- mean(dataset_sec_yr$bis2_inability_to_plan)
bis2_inability_to_plan_sd <- sd(dataset_sec_yr$bis2_inability_to_plan)
dataset_sec_yr<- mutate(dataset_sec_yr,bis2_inability_to_plan_standardized =(bis2_inability_to_plan-bis2_inability_to_plan_mean)/bis2_inability_to_plan_sd)

bis2_lack_of_selfcontrol_mean <- mean(dataset_sec_yr$bis2_lack_of_selfcontrol)
bis2_lack_of_selfcontrol_sd <- sd(dataset_sec_yr$bis2_lack_of_selfcontrol)
dataset_sec_yr<- mutate(dataset_sec_yr,bis2_lack_of_selfcontrol_standardized =(bis2_lack_of_selfcontrol-bis2_lack_of_selfcontrol_mean)/bis2_lack_of_selfcontrol_sd)


bis2_novelty_seeking_mean <- mean(dataset_sec_yr$bis2_novelty_seeking)
bis2_novelty_seeking_sd <- sd(dataset_sec_yr$bis2_novelty_seeking)
dataset_sec_yr<- mutate(dataset_sec_yr,bis2_novelty_seeking_standardized =(bis2_novelty_seeking-bis2_novelty_seeking_mean)/bis2_novelty_seeking_sd)

#
dataset_sec_yr <-  mutate(dataset_sec_yr, q73_b_code= ifelse(q73_b=="完全不符合",1,ifelse(q73_b== "大部分不符合",2,ifelse(q73_b== "大部分符合",3, 4 ))))
dataset_sec_yr <-  mutate(dataset_sec_yr, q74_b_code= ifelse(q74_b=="完全不符合",1,ifelse(q74_b=="大部分不符合",2,ifelse(q74_b=="大部分符合",3, 4 ))))
dataset_sec_yr <-  mutate(dataset_sec_yr, q75_b_code= ifelse(q75_b=="完全不符合",1,ifelse(q75_b=="大部分不符合",2,ifelse(q75_b=="大部分符合",3, 4 ))))
dataset_sec_yr <-  mutate(dataset_sec_yr, q76_b_code= ifelse(q76_b=="完全不符合",1,ifelse(q76_b=="大部分不符合",2,ifelse(q76_b=="大部分符合",3, 4 ))))
dataset_sec_yr <-  mutate(dataset_sec_yr, q77_b_code= ifelse(q77_b=="完全不符合",1,ifelse(q77_b=="大部分不符合",2,ifelse(q77_b=="大部分符合",3, 4 ))))
dataset_sec_yr <-  mutate(dataset_sec_yr, q78_b_code= ifelse(q78_b=="完全不符合",1,ifelse(q78_b=="大部分不符合",2,ifelse(q78_b=="大部分符合",3, 4 ))))
dataset_sec_yr <-  mutate(dataset_sec_yr, q79_b_code= ifelse(q79_b=="完全不符合",1,ifelse(q79_b=="大部分不符合",2,ifelse(q79_b=="大部分符合",3, 4 ))))
dataset_sec_yr <-  mutate(dataset_sec_yr, q80_b_code= ifelse(q80_b=="完全不符合",1,ifelse(q80_b=="大部分不符合",2,ifelse(q80_b=="大部分符合",3, 4 ))))
dataset_sec_yr <-  mutate(dataset_sec_yr, q81_b_code= ifelse(q81_b=="完全不符合",1,ifelse(q81_b=="大部分不符合",2,ifelse(q81_b=="大部分符合",3, 4 ))))
dataset_sec_yr <-  mutate(dataset_sec_yr, q82_b_code= ifelse(q82_b=="完全不符合",1,ifelse(q82_b=="大部分不符合",2,ifelse(q82_b=="大部分符合",3, 4 ))))

dataset_sec_yr <-  mutate(dataset_sec_yr, secondyr_rses= (q73_b_code+q74_b_code+q75_b_code+q76_b_code+q77_b_code+q78_b_code+q79_b_code+q80_b_code+q81_b_code+q82_b_code))

dataset_sec_yr <-  mutate(dataset_sec_yr, secondyr_rses_pos= (q73_b_code+q74_b_code+q76_b_code))
dataset_sec_yr <-  mutate(dataset_sec_yr, secondyr_rses_neg= (secondyr_rses- secondyr_rses_pos))

#
dataset_sec_yr <- mutate(dataset_sec_yr, bully_inv_secyr= ifelse(secyr_bv=="yes", "3_bv", ifelse(secyr_pureb=="yes", "4_pureb", ifelse(secyr_purev=="yes", "2_purev", "1_not_inv"))))
dataset_sec_yr <- mutate(dataset_sec_yr, bully_inv_act_secyr= ifelse(secyr_bv_act=="yes", "2_bv", ifelse(secyr_pureb_act=="yes", "1_pureb", ifelse(secyr_purev=="yes", "3_purev", "4_not_inv"))))



#
dataset <- dataset_sec_yr


# 挑independent_var與dependent_var出來

selected_variables2 <- c("firstyrbully2","firstyrvic2", "bully_3group", "victim_3group", "rec_victim_firstyr", "rec_bully_firstyr", "firstyrpurev", "firstyr_pureb","firstyr_purev", 
                         "firstyr_bully2","firstyr_bv","less_bully", "new_bully", "physicalbully", "verbalbully", "relationalbully", "otherbully",
                         "firstyr_bully2_act","bully_3group_act","bully_group_act","firstyr_pureb_act",
                         #"yr",
                         "q3_a","q4_a","q5_a","q6_a","q7_a","q8_a", "q10_a", "school_PR","q22_a",
                         "q68_a","AUDIT_C_4_a", "AUDIT_C_a", "PHQ9_1yr_new", "q31_0_a", "q22_0_a",  "parent1_MDSS", "teacher_MDSS","peer1", "firstrses", 
                         
                         "bis1_total", "bis1_standardized", 
                         "bis1_inability_to_plan", "bis1_novelty_seeking", "bis1_lack_of_selfcontrol",
                         "bis1_inability_to_plan_standardized", "bis1_novelty_seeking_standardized", "bis1_lack_of_selfcontrol_standardized",
                         
                         
                         
                         "secondyrbully2","secondvic2", "bully_sec_3group", "victim_sec_3group", "rec_victim_secyr", "rec_bully_secyr", "secondyrpurev", "secondyrpureb",
                         "secyr_bully2" ,"secyr_vic2", "secyr_pureb", "secyr_purev","secyr_bv", "new_bully", "physicalbully_sec", "verbalbully_sec", "relationalbully_sec", "otherbully_sec",
                         "bully_sec_group_act", "bully_sec_3group_act", "secyr_bully2_act", "rec_bully_secyr_act",
                          "q3_b","q4_b","q5_b","q6_b","q7_b","q8_b", "q10_b", "school_PR","q22_b","see_bully", "classmates_action", "schoolstaff_action","worry_being_bullied", "tutor_action",
                         "q68_b","AUDIT_C_4_b", "AUDIT_C_b", "PHQ9_2yr", "q31_0_b", "q22_0_b","parent2_MDSS","teacher2_MDSS", "peer2", "secondtrses", 
                         "bis2_total", "bis2_standardized", 
                         "bis2_inability_to_plan", "bis2_novelty_seeking", "bis2_lack_of_selfcontrol",
                         "bis2_inability_to_plan_standardized", "bis2_novelty_seeking_standardized", "bis2_lack_of_selfcontrol_standardized"
)
dataset_sec_yr_selected <- dataset_sec_yr[,selected_variables2]

tempData <- mice(dataset_sec_yr_selected, m=5)
dataset_sec_yr_selected_imp <- mice::complete(tempData)

dataset_sec_yr_table2_imp<-dataset_sec_yr_selected_imp%>%filter(secyr_purev=="no")
dataset_sec_yr_table3_imp<-dataset_sec_yr_selected_imp%>%filter(secyr_pureb=="no")
dataset_sec_yr_bv_vs_noninvolved_imp<-dataset_sec_yr_selected_imp%>%filter(secyr_pureb=="no" & secyr_purev=="no")
dataset_sec_yr_pureb_imp<-dataset_sec_yr_selected_imp%>%filter(secyr_vic2=="no")
dataset_sec_yr_purev_imp<-dataset_sec_yr_selected_imp%>%filter(secyr_bully2=="no")
dataset_sec_yr_v_imp<-dataset_sec_yr_selected_imp%>%filter(secyr_vic2=="yes")
dataset_sec_yr_b_imp<-dataset_sec_yr_selected_imp%>%filter(secyr_bully2=="yes")

dataset_sec_yr_selected_ordinal_imp <- dataset_sec_yr_selected_imp
dataset_sec_yr_selected_ordinal_imp$bully_sec_3group <- ordered(dataset_sec_yr_selected_ordinal_imp$bully_sec_3group, levels= c("no_bullying", "1or2_times", "repeated_bullying"))
dataset_sec_yr_selected_ordinal_imp$victim_sec_3group <- ordered(dataset_sec_yr_selected_ordinal_imp$victim_sec_3group, levels= c("not_being_bullied", "1or2_times", "repeated_being_bullied"))

dataset_sec_yr_selected_ordinal_imp$bully_3group <- ordered(dataset_sec_yr_selected_ordinal_imp$bully_3group, levels= c("no_bullying", "1or2_times", "repeated_bullying"))
dataset_sec_yr_selected_ordinal_imp$victim_3group <- ordered(dataset_sec_yr_selected_ordinal_imp$victim_3group, levels= c("not_being_bullied", "1or2_times", "repeated_being_bullied"))


####

# build testing dataset
testing_variables2 <- c(
  "firstyrbully2", "firstyrvic2",
  "secondyrbully2","secondvic2", "bully_sec_3group", "victim_sec_3group", "rec_victim_secyr", "rec_bully_secyr", "secondyrpurev", "secondyrpureb",
  "secyr_bully2" ,"secyr_vic2", "secyr_pureb", "secyr_purev","secyr_bv", "secyr_bully2_act",
  "physicalbully_sec", "verbalbully_sec", "relationalbully_sec", "otherbully_sec",
  "physicalvictim_sec", "verbalvictim_sec", "relationalvictim_sec",  
  "victim_3group_act_2",
  #"physicalb_3group","verbalb_3group","relationalb_3group",
  
  
  "q3_b","q4_b","q5_b","q6_b","q7_b","q8_b", "q10_b", "school_PR","q22_b",
  "q68_b","AUDIT_C_4_b", "AUDIT_C_b", "PHQ9_2yr", "q31_0_b", "q22_0_b", "parent2_MDSS", "teacher2_MDSS","peer2", "secondtrses", "secondtrses_standardized",
  "bis2_total", "bis2_standardized", 
  "bis2_inability_to_plan", "bis2_novelty_seeking", "bis2_lack_of_selfcontrol",
  "bis2_inability_to_plan_standardized", "bis2_novelty_seeking_standardized", "bis2_lack_of_selfcontrol_standardized"
)

dataset_sec_yr_testing <- dataset_sec_yr[,testing_variables2]

dataset_sec_yr_testing<- dataset_sec_yr_testing%>% dplyr::rename(
  "pastyrbully2"="firstyrbully2","pastyrvic2"="firstyrvic2",
  "firstyrbully2"="secondyrbully2","firstyrvic2"="secondvic2", 
  "firstyr_bully2"= "secyr_bully2","firstyr_vic2"="secyr_vic2",
  "bully_3group"="bully_sec_3group", "victim_3group"="victim_sec_3group", 
  "rec_victim_firstyr"="rec_victim_secyr", "rec_bully_firstyr"="rec_bully_secyr", "firstyrpurev"="secondyrpurev", "firstyrpureb"="secondyrpureb",
   "firstyr_pureb"="secyr_pureb", 
  "firstyr_purev"="secyr_purev","firstyr_bv"="secyr_bv",
  "physicalbully"="physicalbully_sec", "verbalbully"="verbalbully_sec", "relationalbully"="relationalbully_sec", "otherbully"="otherbully_sec",
  "physicalvictim"="physicalvictim_sec", "verbalvictim"="verbalvictim_sec", "relationalvictim"="relationalvictim_sec", 
  "firstyr_bully2_act"="secyr_bully2_act",
  "victim_3group_act"= "victim_3group_act_2",
  
  
  "q3_a"="q3_b","q4_a"="q4_b","q5_a"="q5_b","q6_a"="q6_b","q7_a"="q7_b","q8_a"="q8_b", "q10_a"="q10_b", "school_PR"="school_PR","q22_a"="q22_b",
  "q68_a"="q68_b","AUDIT_C_4_a"="AUDIT_C_4_b", "AUDIT_C_a"="AUDIT_C_b", "PHQ9_1yr_new"="PHQ9_2yr", "q31_0_a"="q31_0_b", "q22_0_a"="q22_0_b",
  "parent1_MDSS"= "parent2_MDSS","teacher_MDSS"="teacher2_MDSS",  "peer1"="peer2", "firstrses"="secondtrses", "firstrses_standardized"="secondtrses_standardized",
  "bis1_total"="bis2_total", "bis1_standardized"="bis2_standardized", 
  "bis1_inability_to_plan"="bis2_inability_to_plan", "bis1_novelty_seeking"="bis2_novelty_seeking", "bis1_lack_of_selfcontrol"="bis2_lack_of_selfcontrol",
  "bis1_inability_to_plan_standardized"= "bis2_inability_to_plan_standardized",
  "bis1_novelty_seeking_standardized"="bis2_novelty_seeking_standardized", "bis1_lack_of_selfcontrol_standardized"="bis2_lack_of_selfcontrol_standardized")



#dataset_sec_yr_testing <- mutate(dataset_sec_yr_testing, bully_inv= ifelse(firstyr_bv=="yes", "3_bv", ifelse(firstyr_pureb=="yes", "4_pureb", ifelse(firstyr_purev=="yes", "2_purev", "1_not_inv"))))
tempData <- mice(dataset_sec_yr_testing, m=5)
dataset_sec_yr_testing_imp <- mice::complete(tempData)






# write dataset_selected to rearranged_dataset.csv
write.table(dataset_sec_yr_selected_temp, output_file)


