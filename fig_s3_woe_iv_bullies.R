library(Information)
source("level_factor_lists.R")


# weight of evidence analysis

#model2


vars_vector_victim<-c("victim_3group","q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                      "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                      "q22_0_a", "firstrses","bis1_total")


vars_names <- c("Victimization", "Gender", "School PR", "Liviing with Parents", "Needy family", "Mother's employment","Father's employment",
                "Smoking habit","PHQ-9", "AUDIT-C", "MDSS", "RSES", "Impulsivity")


names(vars_names) <- vars_vector_victim


names(vars_vector_victim) <- vars_names
names(vars_names) <- vars_vector_victim
names.ls <-list( victim_3group=c("Never", "Once or twice", "Moderate"), 
                 q3_a=c( "Female","Male"), 
                 school_PR=c("PR70-89", "PR<70"),
                 q8_a= c("No","Yes"),
                 q7_a= c("No","Yes"),
                 q6_a= c("Yes","No"),
                 q5_a= c("Yes","No"),#"q5.a",
                 q68_a= c("Yes","No"),
                 PHQ9_1yr_new= c("0-5","6-10","11-14",">15") ,AUDIT_C_4_a= c("<4",">=4")
                 ) 


## model 3 test, 
vars_vector_victim_act<-c("victim_3group","victim_3group_act","victim_3group_rel", "q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                          "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                          "q22_0_a", "firstrses","bis1_total")
vars_names <- c("Victimization","Active Victimization","Relational victimization", "Gender", "School PR", "Liviing with Parents", "Needy family", "Mother's employment","Father's employment",
                "Smoking habit","PHQ-9", "AUDIT-C", "MDSS", "RSES", "Impulsivity")
names(vars_vector_victim_act) <- vars_names
names(vars_names) <- vars_vector_victim_act
names.ls <-list( victim_3group= c("Never", "Once or twice", "Moderate"),
                victim_3group_act=c("Never", "Once or twice", "Moderate"),
                 victim_3group_rel=c("Never", "Once or twice", "Moderate"),
                 q3_a=c( "Female","Male"), 
                 school_PR=c("PR70-89", "PR<70"),
                 q8_a= c("No","Yes"),
                 q7_a= c("No","Yes"),
                 q6_a= c("Yes","No"),
                 q5_a= c("Yes","No"),#"q5.a",
                 q68_a= c("Yes","No"),
                 PHQ9_1yr_new= c("0-5","6-10","11-14",">15"),
                 AUDIT_C_4_a= c("<4",">=4")) 
var.ls <- list( victim_3group_act=c("Never", "Once or twice", "Moderate"), 
                q3_a=c( "Female","Male"), 
                school_PR=c("PR70-89", "PR<70"),
                q8_a= c("No","Yes"),
                q7_a= c("No","Yes"),
                q6_a= c("Yes","No"),
                q5_a= c("Yes","No"),#"q5.a",
                q68_a= c("Yes","No"),
                PHQ9_1yr_new= c("0-5","6-10","11-14",">15"),
                AUDIT_C_4_a= c("<4",">=4")) 







## 畫WOE plot


plotwoe.mypaper <- function(x, names.list= names.ls, variable_names=vars_names,size=7,k=1,titles="A2", ...){  


pred1.test <- predicted.mypaper(x)
data.test <- cbind(x$data, Observed=pred1.test$Observed)
woe.test <- create_infotables(data=data.test, y= "Observed", bins=6)



levels.list <- levels.ls(x$ind_var, data = x$data,level.names.list = names.list,  variable_names = variable_names)



factor_variable_names <- levels.list[["factor_variable_names"]]
numeric_variable_names <- levels.list[["numeric_variable_names"]]

WOE_all <- NULL
for(i in names(woe.test$Tables)){
  WOE_all <- c(WOE_all, woe.test$Tables[[i]]$WOE)
}

ylim.max <- max(WOE_all)
ylim.min <- min(WOE_all)

##
ylim <- c( min(woe.test$Summary$IV)-2, max(woe.test$Summary$IV)+0.5)



ind_var <- woe.test$Summary$Variable    # extract independent variables(ordered by information value) from "create_infotables" object
ind_var <- ind_var[ind_var %in% x$ind_var] #  remove variables other than selected independent variables
message(ind_var)
titles <-  paste(titles, 1:length( ind_var ), sep = ".")

l <- list()

for(i in ind_var){
  message(names(levels.list[[i]])) 
  
  data.woe <- woe.test$Tables[[i]]
  
  
  orderlist <- data.woe[[i]]
  
  order.org <- order(orderlist)
  
  
  data.woe$xvar <- data.woe[[i]]
  
  
  chosen_var <-  woe.test$Summary%>%filter(Variable== i)
  chosen_var_IV <- chosen_var[,2]
  
  data.woe <- transform(data.woe, xvar = factor(xvar, levels = orderlist))
  
  data.woe$xvar<- data.woe$xvar[order.org]
  data.woe <- transform(data.woe, xvar = factor(xvar, levels = orderlist[order.org]))
   

  if(is.factor(data.test[[i]]
              ) 
    ){
    
   
      # 處理遺漏或沒抓到的variable names    
      if(is.na(factor_variable_names[i])){
        message(factor_variable_names[i])
        prompt_i <- paste("no macthed factor name was found for",i, "please key in factor names:", sep=" ")
        
        cat_name <- readline(prompt = prompt_i)
        
        factor_variable_names[i] <- cat_name
        }
      
     #message(names(levels.list))
      # 處理遺漏或沒抓到的level names
      
      if( is.null(names(levels.list[[i]])
                  )
          ){
        levels_name <- NULL
        
        for(k in levels(data.woe$xvar)){
          level_name <- NULL
          prompt_levels <- paste("no mathced level name was found", 
                                 "\n pls key in the names of levels of", 
                                 print(k) ,
                                 "for factor",i,"?", sep = " ")
          level_name <- readline(prompt = prompt_levels)
          levels_name <- c(levels_name, ifelse(level_name=="",k,level_name) )
           
        }
        message(levels_name)
        names(levels.list[[i]]) <-  levels_name
          
      }
      
    
    
   woeplot <- ggplot(data=data.woe)+geom_col(aes(x= xvar, y=WOE))+
      ylim(ylim[1], ylim[2]+1.5)+  xlab(factor_variable_names[i])+
     scale_x_discrete( labels= names(levels.list[[i]])
                       )+
     ggtitle(titles[match(i, ind_var)])+
      geom_text(x=1.1, y= ylim[2]-0.3 , label=c(paste0( "IV=",round( chosen_var_IV, digits = 2)  )  ), size=size/3 )+ 
      theme_pubr(base_size = size)+
     theme(#text = element_text(size=size),
           axis.text.x = element_text(size = size/(1.4*k))
     )
     # check the level names of categorical factors. 
   
  }else{
    
    if(is.na(numeric_variable_names[i])){
      message(numeric_variable_names[i])
      prompt_i <- paste("no macthed name was found for the numeric variable",i, "please key in the name:", sep=" ")
      
      num_name <- readline(prompt = prompt_i)
      
      numeric_variable_names[i] <- num_name
    }
    
    woeplot <- ggplot(data=data.woe)+geom_col(aes(x= xvar, y=WOE))+
      ylim(ylim[1], ylim[2])+xlab(numeric_variable_names[i])+
 #     scale_x_discrete(size=size/3
#                         )+
      geom_text(x=1.4, y=ylim[2]-0.3 , label=c(paste0( "IV=",round( chosen_var_IV, digits = 2)  )  ), size=size/3 )+
      theme_pubr(base_size = size)+
      ggtitle(titles[match(i, ind_var)])+
      theme(#text = element_text(size=size)
        
        axis.text.x = element_text(size = size/(1.48*k))
      )
      
    
   } 

  name <- paste(i)
  l[[name]] <- woeplot

}
 
 return(l)
}



l <- plotwoe.mypaper(model1.woe,size = 9, k=0.9, titles = "1B")
l3 <- plotwoe.mypaper(model3.woe,size = 9, k=0.9, titles = "1B")







vars_vector_victim_act<-c("victim_3group_act","q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                      "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                      "q22_0_a", "firstrses","bis1_total")

vars_names <- c("Victimization", "Gender", "School PR", "Liviing with Parents", "Needy family", "Mother's employment","Father's employment",
            "Smoking habit","PHQ-9", "AUDIT-C", "MDSS", "RSES", "Impulsivity")

names(vars_names) <- vars_vector_victim_act

names(vars_vector_victim_act) <- vars_names
names.ls <-list( victim_3group=c("Never", "Once or twice", "Moderate"), 
                 q3_a=c( "Female","Male"), 
                 school_PR=c("PR70-89", "PR<70"),
                 q8_a= c("No","Yes"),
                 q7_a= c("No","Yes"),
                 q6_a= c("Yes","No"),
                 q5_a= c("Yes","No"),#"q5.a",
                 q68_a= c("Yes","No"),
                 PHQ9_1yr_new= c("0-5","6-10","11-14",">15"),
                 AUDIT_C_4_a= c("<4",">=4")) 




firstyrbully3_arg <-list(ind_var=vars_vector_victim_act, dep_var = "firstyr_bully2_act",  data=dataset,  # additional_var 加上其他變數
                          imputation.mis.data=T,
                          span=0.8, group=12,
                          validate.boot40= F, 
                          val.B=10, 
                          confusion.matrix= F,
                          cut.off=0.5,
                          p.threshhold= 0.05, 
                          correct= F)

model3.test <- do.call(mypaper, firstyrbully3_arg)


firstyrbully_arg <-list(ind_var=vars_vector_victim_act, dep_var = "firstyrbully2",  data=dataset,  # additional_var 加上其他變數
                         imputation.mis.data=T,
                         span=0.8, group=12,
                         validate.boot40= F, 
                         val.B=10, 
                         confusion.matrix= F,
                         cut.off=0.5,
                         p.threshhold= 0.05, 
                         correct= F)

model1.test <- do.call(mypaper, firstyrbully_arg)

recbully_arg <-list(ind_var=vars_vector_victim_act, dep_var = "rec_bully_firstyr",  data=dataset,  # additional_var 加上其他變數
                        imputation.mis.data=T,
                        span=0.8, group=12,
                        validate.boot40= F, 
                        val.B=10, 
                        confusion.matrix= F,
                        cut.off=0.5,
                        p.threshhold= 0.05, 
                        correct= F)

model2.test <- do.call(mypaper, recbully_arg)

firstyrbullyr_arg <-list(ind_var=vars_vector_victim_act, dep_var = "relationalbully",  data=dataset,  # additional_var 加上其他變數
                         imputation.mis.data=T,
                         span=0.8, group=12,
                         validate.boot40= F, 
                         val.B=10, 
                         confusion.matrix= F,
                         cut.off=0.5,
                         p.threshhold= 0.05, 
                         correct= F)

modelr.test <- do.call(mypaper, firstyrbullyr_arg)


firstyrbullyp_arg <-list(ind_var=vars_vector_victim_act, dep_var = "physicalbully",  data=dataset,  # additional_var 加上其他變數
                         imputation.mis.data=T,
                         span=0.8, group=12,
                         validate.boot40= F, 
                         val.B=10, 
                         confusion.matrix= F,
                         cut.off=0.5,
                         p.threshhold= 0.05, 
                         correct= F)

modelp.test <- do.call(mypaper, firstyrbullyp_arg)
l <- plotwoe.mypaper(modelp.test, titles = "1B")





fig_s3 <- ggarrange(
  l[[1]], l[[2]], l[[3]],l[[4]],l[[5]],l[[6]],l[[7]],l[[8]],l[[9]],l[[10]],l[[11]],l[[12]],
  labels = c(
    "A", "B", "C","D","E","F","G","H","I","J","K","L"
  ), legend = "right", common.legend = TRUE
)

fig_s3 <- ggarrange(
  l[[1]], l[[2]], l[[3]],l[[4]],l[[6]],l[[9]],
  labels = c(
    "A", "B", "C","D","E","F"
  ), legend = "right", common.legend = TRUE
)

fig_s3_revised <- annotate_figure(
  fig_s3, top = text_grob("                                           ", 
                         face = "plain", size = 12, hjust=0.45, vjust = 0.27
  ),
  fig.lab = "Figure 1: Weight of Analysis (WOE) and Information Value (IV) of Bullying Perpetration", fig.lab.face = "bold", 
  fig.lab.size = 13, fig.lab.pos= "top.left"
)


fig_1_revised <- fig_s3_revised


l <- plotwoe.mypaper(model1.test, titles = "A1")
F

fig_A1 <- ggarrange(
  l[[1]], l[[2]], l[[3]],l[[4]],l[[5]],l[[6]],l[[7]],l[[8]],l[[9]],l[[10]],l[[11]],l[[12]],l[[13]],l[[14]],l[[15]],
   legend = "right", common.legend = TRUE
)

l <- plotwoe.mypaper(model3.test, titles = "A2")
F
fig_A2 <- ggarrange(
  l[[1]], l[[2]], l[[3]],l[[4]],l[[5]],l[[6]],l[[7]],l[[8]],l[[9]],l[[10]],l[[11]],l[[12]],l[[13]],l[[14]],l[[15]],
  legend = "right", common.legend = TRUE
)



l<- plotwoe.mypaper(modelr.test, titles = "A3")
F
fig_A3 <- ggarrange(
  l[[1]], l[[2]], l[[3]],l[[4]],l[[5]],l[[6]],l[[7]],l[[8]],l[[9]],l[[10]],l[[11]],l[[12]],l[[13]],l[[14]],l[[15]],
  legend = "right", common.legend = TRUE
)

l<- plotwoe.mypaper(modelr.test, titles = "A4")

fig_A4 <- ggarrange(
  l[[1]], l[[2]], l[[3]],l[[4]],l[[5]],l[[6]],l[[7]],l[[8]],l[[9]],l[[10]],l[[11]],l[[12]],l[[13]],l[[14]],l[[15]],
  legend = "right", common.legend = TRUE
)
