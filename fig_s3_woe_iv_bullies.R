library(Information)

# weight of evidence analysis
selected_variables <- c("firstyrbully2", "victim_3group", 
                        "AUDIT_C_4_a", "q31_0_a", "q22_0_a", 
                        "firstrses", 
                        "bis1_total"#,"bis1_lack_of_selfcontrol", 
                        #"bis1_novelty_seeking", "bis1_inability_to_plan"
)


dataset_selected_num <- dataset[,c("q3_a","school_PR", "q5_a",
                                   "q6_a","q7_a","q8_a","q68_a","PHQ9_1yr_new",
                                   "AUDIT_C_4_a", selected_variables
                                   )]

k <- as.data.frame(c("q3_a","school_PR", "q5_a",
             "q6_a","q7_a","q8_a","q68_a","PHQ9_1yr_new",
             "AUDIT_C_4_a", selected_variables
))




dataset_selected_num<- mutate(dataset_selected_num, firstyrbully2_1= ifelse(firstyrbully2=="有", 1,0))

woe_bully<-create_infotables(data=dataset_selected_num, y="firstyrbully2_1", bins = 6)


woe_plot1<- plot_infotables(woe_bully, variables = woe_bully$Summary$Variable[1],same_scales = TRUE)+
  geom_text(x=1, y=1 , label=c(paste0( "IV=",round( woe_bully$Summary[,2][1], digits = 2)
  )
  )
  )+
  ggtitle("Frequency of Victimization")

woe_plot2<- plot_infotables(woe_bully, variables = woe_bully$Summary$Variable[2],same_scales = TRUE)+
  geom_text(x=2, y=0.6 , label=c(paste0( "IV=",round( woe_bully$Summary[,2][2], digits = 2))))+
  ggtitle("Impulsivity(BIS)")

woe_plot3<- plot_infotables(woe_bully, variables = woe_bully$Summary$Variable[3],same_scales = TRUE)+
  geom_text(x=2, y=0.25 , label=c(paste0( "IV=",round( woe_bully$Summary[,2][3], digits = 2) )  )  )+
  ggtitle("Depression(PHQ9)")
woe_plot4<- plot_infotables(woe_bully, variables = woe_bully$Summary$Variable[4],same_scales = TRUE)+
  geom_text(x=4, y=0.3 , label=c(paste0( "IV=",round( woe_bully$Summary[,2][4], digits = 2))  )  )+
  ggtitle("Social Support(MDSS)")
woe_plot5<- plot_infotables(woe_bully, variables = woe_bully$Summary$Variable[5],same_scales = TRUE)+
  geom_text(x=1, y=1 , label=c(paste0( "IV=",round( woe_bully$Summary[,2][5], digits = 2)  )  )  )+
  ggtitle("Alcohol Use(AUDIT-C)")

woe_plot6<- plot_infotables(woe_bully, variables = woe_bully$Summary$Variable[6],same_scales = TRUE)+
  geom_text(x=3.5, y=0.2 , label=c(paste0( "IV=",round( woe_bully$Summary[,2][6], digits = 2)  )  )  )+
  ggtitle("Self-esteem(RSES)")

fig_s3 <-  ggarrange(
  woe_plot1, woe_plot2, woe_plot3,woe_plot4,woe_plot5,woe_plot6,
  labels = c(
    "A", "B", "C","D","E","F"
  ), legend = "right", common.legend = TRUE
)


fig_s3_revised <- annotate_figure(
  fig_s3, 
  top = text_grob( "\n  The Weight of Evidence(WOE) and Information Value(IV) on Bullying Perpetration", 
                   face = "plain", size = 13, hjust=0.7067, vjust = 0.27),
  bottom = text_grob(paste("Other Variable's IV: Sex: 0.043,School PR: 0.072, Father's Unemployment: 0.058,", 
                           "Mother's Unemployment: 0.019, Needy Family: 0.001, Living with Parents: 0.014,",
                           "Smoking Status: 0.04",sep="\n"),just = "left"),
  
  fig.lab = "Figure S3", fig.lab.face = "bold", 
  fig.lab.size = 13, fig.lab.pos= "top.left"
)


## model 3 test, 
vars_vector_victim<-c("victim_3group","q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                      "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                      "q22_0_a", "firstrses","bis1_total")
vars_names <- c("Victimization", "Gender", "School PR", "Liviing with Parents", "Needy family", "Mother's employment","Father's employment",
                "Smoking habit","PHQ-9", "AUDIT-C", "MDSS", "RSES", "Impulsivity")
names(vars_vector_victim) <- vars_names
names(vars_names) <- vars_vector_victim
names.ls <-list( victim_3group_act=c("Never", "1 or 2 times", "Frequent"), 
                 q3_a=c( "Female","Male"), 
                 school_PR=c("PR70-89", "PR<70"),
                 q8_a= c("No","Yes"),
                 q7_a= c("No","Yes"),
                 q6_a= c("Yes","No"),
                 q5_a= c("Yes","No"),#"q5.a",
                 q68_a= c("Yes","No"),
                 PHQ9_1yr_new= c("0-5","6-10","11-14",">15"),
                 AUDIT_C_4_a= c("<4",">=4")) 


firstyrbully21_arg <-list(ind_var=vars_vector_victim, dep_var = "firstyr_bully2",  data=dataset,  # additional_var 加上其他變數
                          imputation.mis.data=F,
                          span=0.8, group=12,
                          validate.boot40= F, 
                          val.B=10, 
                          confusion.matrix= F,
                          cut.off=0.5,
                          p.threshhold= 0.05, 
                          correct= F)

model1.test <- do.call(mypaper, firstyrbully21_arg)


## 畫WOE plot


plotwoe.mypaper <- function(x, names.list= names.ls, variable_names=vars_names, ...){  


pred1.test <- predicted.mypaper(x)
data.test <- cbind(x$data, Observed=pred1.test$Observed)
woe.test <- create_infotables(data=data.test, y= "Observed", bins=6)



levels.list <- levels.ls(x$ind_var, data = x$data,level.names.list = names.list,  variable_names = variable_names)



factor_variable_names <<- levels.list[["factor_variable_names"]]
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
        names(levels.list[[i]]) <- levels_name
      }
      
    
    
   woeplot <- ggplot(data=data.woe)+geom_col(aes(x= xvar, y=WOE))+
      ylim(ylim[1], ylim[2])+  
      scale_x_discrete("", labels= names(levels.list[[i]])
                       )+
      ggtitle(factor_variable_names[i])+
      geom_text(x=1, y= ylim[2]-0.3 , label=c(paste0( "IV=",round( chosen_var_IV, digits = 2)  )  )  )+
      theme_pubr(base_size = 10)
     # check the level names of categorical factors. 
   
  }else{
    
    if(is.na(numeric_variable_names[i])){
      message(numeric_variable_names[i])
      prompt_i <- paste("no macthed name was found for the numeric variable",i, "please key in the name:", sep=" ")
      
      num_name <- readline(prompt = prompt_i)
      
      numeric_variable_names[i] <- num_name
    }
    
    woeplot <- ggplot(data=data.woe)+geom_col(aes(x= xvar, y=WOE))+
      ylim(ylim[1], ylim[2])+xlab(NULL)+
      geom_text(x=1.4, y=ylim[2]-0.3 , label=c(paste0( "IV=",round( chosen_var_IV, digits = 2)  )  )  )+
      ggtitle(numeric_variable_names[i])+
      theme_pubr(base_size = 10)
    
   } 

  name <- paste(i)
  l[[name]] <- woeplot

}
 
 return(l)
}



l <- plotwoe.mypaper(model3.imp)

ggarrange_arg <- list(l)





vars_vector_victim_act<-c("victim_3group_act","q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                      "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                      "q22_0_a", "firstrses_standardized","bis1_standardized")
vars_names <- c("Victimization", "Gender", "School PR", "Liviing with Parents", "Needy family", "Mother's employment","Father's employment",
            "Smoking habit","PHQ-9", "AUDIT-C", "MDSS", "RSES", "Impulsivity")
names(vars_names) <- vars_vector_victim_act
names(vars_vector_victim) <- vars_names
names.ls <-list( victim_3group_act=c("Never", "1 or 2 times", "Frequent"), 
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
                          imputation.mis.data=F,
                          span=0.8, group=12,
                          validate.boot40= F, 
                          val.B=10, 
                          confusion.matrix= F,
                          cut.off=0.5,
                          p.threshhold= 0.05, 
                          correct= F)

model3.test <- do.call(mypaper, firstyrbully3_arg)


plotwoe1 <- plotwoe.mypaper(model1)
plotwoe2 <- plotwoe.mypaper(model2)
plotwoe3 <- plotwoe.mypaper(model3.test)





fig_s3 <- ggarrange(
  l[[1]], l[[2]], l[[3]],l[[4]],l[[5]],l[[6]],l[[7]],l[[8]],l[[9]],l[[10]],l[[11]],l[[12]],l[[13]],
  labels = c(
    "A", "B", "C","D","E","F","G","H","I","J","K","L","M"
  ), legend = "right", common.legend = TRUE
)
