
##cat_var_tab 個別類別變數的levels, OR.CI與p value

cat_var_tab <- function( x,y="firstyrbully2", data_frame #= dataset_table2
                         ) 
{
  
  
  
  ind_var <- data_frame[[x]]
  table2_q3_a <-table(ind_var, data_frame[[y]])
  cat_var_list<- data.frame(class_var=NULL, Var=NULL, level=NULL,OR.CI=NULL, p_value=NULL,star=NULL,dep_var=NULL)
  # 處理>2 levels的問題
  for (i in 1:(nlevels(ind_var)-1))  {
    
    
    
    chisq_q3_a <-   chisq.test(table2_q3_a[c(1, i+1),],correct=TRUE)
    esc_q3_a <-  esc_chisq(chisq = chisq_q3_a$statistic, 
                           totaln = sum(table2_q3_a[c(1, i+1),]), 
                           es.type = "or")
    q3_a <- paste(round( esc_q3_a$es,digits = 2), "(",round( esc_q3_a$ci.lo,digits = 2),"-",round( esc_q3_a$ci.hi, digits = 2),")",sep = "")
    level_list<-  data.frame( class_var="categorical", Var=x,level= levels(ind_var)[[i+1]],
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
    cat_var_list =rbind(cat_var_list,level_list)                  
    
  } 
  cat_var_list
}