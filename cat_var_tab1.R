
##cat_var_tab 個別類別變數的levels, OR.CI與p value

cat_var_tab <- function( x,y="firstyrbully2", data_frame #= dataset_table2
                         ) 
{
  
  
  
  ind_var <- data_frame[[x]]
  table2_q3_a <-table(ind_var, data_frame[[y]])
  cat_var_df<- data.frame(class_var=NULL, Var=NULL, level=NULL,OR.CI=NULL, p_value=NULL,star=NULL,dep_var=NULL)
  # 處理>2 levels的問題
  for (i in 1:(nlevels(ind_var)-1))  {
    
    
    
    chisq_q3_a <-   chisq.test(table2_q3_a[c(1, i+1),],correct=TRUE)
    esc_q3_a <-  esc_chisq(chisq = chisq_q3_a$statistic, 
                           totaln = sum(table2_q3_a[c(1, i+1),]), 
                           es.type = "or")
    q3_a <- paste(round( esc_q3_a$es,digits = 2), "(",round( esc_q3_a$ci.lo,digits = 2),"-",round( esc_q3_a$ci.hi, digits = 2),")",sep = "")
    level_df<-  data.frame( class_var="categorical", Var=x,level= levels(ind_var)[[i+1]],
                              OR.CI= q3_a, 
                              p_value=round(chisq_q3_a$p.value, digits = 6), 
                              star=ifelse(chisq_q3_a$p.value<0.001,"***",
                                          ifelse(chisq_q3_a$p.value<0.01,"**",
                                                 ifelse(chisq_q3_a$p.value<0.05,"*","ns"
                                                 )
                                          )
                              ),
                              dep_var=y
    )
    cat_var_df =rbind(cat_var_df,level_df)                  
    
  } 
  cat_var_df
}



levels.ls <-  function(ind_var, #dep_var, 
                       data, keyin=T, level.names.list= names.ls,variable_names=vars_names ){
  
  
  sorted <- sort_var(ind_var,data = data)
  cat_var <- sorted[[1]]
  num_var <- sorted[[2]]
  levels.list <- NULL
  cat_names <- NULL
  num_names <- NULL
  
  keyin <- readline("Key in or not? T or F (using name lists)")
  
  # categorical variables 
   for(i in cat_var){

    levels_name <- NULL
    levels_cat_i <- levels(data[[i]])
    
   
    
    if(keyin){ 
          prompt_i <- paste("what is the formal name of the categorical variable", i,"?", sep = " ")
   
          cat_name <- readline(prompt = prompt_i)
    
          cat_names <- c(cat_names, ifelse(cat_name=="", i,cat_name) )
    
    
      for(k in levels_cat_i){
        prompt_levels <- paste("what are the names of levels of", print(k) ,"for factor",i,"?", sep = " ")
        level_name <- readline(prompt = prompt_levels)
        levels_name <- c(levels_name, ifelse(level_name=="",k,level_name) )  
        }
      
        
          
     
    }else{
      
      levels_name <- level.names.list[[i]]
      cat_name <- variable_names[i]
      cat_names <- c(cat_names, cat_name)
    }
   
   
    names(levels_cat_i) <- levels_name
   
    
    levels.list[[i]] <- levels_cat_i 
   
   }
   names(cat_names) <- cat_var  
   names(levels.list) <- cat_var
  
#numeric variables
  
 
    for(i in num_var){
    
      if(keyin){
      
        prompt_i <- paste("what is the formal name of the numeric variable", i,"?", sep = " ")
      
        num_name <- readline(prompt = prompt_i)
      
        num_names <- c(num_names, ifelse(num_name =="", i, num_name) )
                      
      }else{
      
      num_name<- variable_names[i]
      num_names <- c(num_names, num_name)
      
      }
      
    }
  
  names(num_names) <- num_var
 
  
  
  levels.list[["factor_variable_names"]] <- cat_names
  levels.list[["numeric_variable_names"]] <- num_names
  return(levels.list)
  
}






vars_vector_victim<-c("victim_3group_act","q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",#"q5.a",
                      "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                      "q22_0_a", "firstrses_standardized","bis1_standardized")
vars_names <- c("Victimization", "Gender", "School PR", "Liviing with Parents", "Needy family", "Mother's unemployment","Father's unemployment",
                "Smoking habit","PHQ-9", "AUDIT-C", "MDSS", "RSES", "Impulsivity")
names(vars_names) <- vars_vector_victim
names.ls <-list( victim_3group_act=c("Never", "1 or 2 times", "Frequent"), 
                 q3_a=c( "Female","Male"), 
                 school_PR=c("PR70-89", "PR<70"),
                 q8_a= c("Yes","No"),
                 q7_a= c("No","Yes"),
                 q6_a= c("Yes","No"),
                 q5_a= c("Yes","No"),#"q5.a",
                 q68_a= c("No","Yes"),
                 PHQ9_1yr_new= c("0-5","6-10","11-14",">15"),
                 AUDIT_C_4_a= c("<4",">=4")) 

levels.list <- levels.ls(vars_vector_victim, data = dataset)



