##num_var_tab 個別數值變數的OR.CI與 p value

num_var_tab <- function(x="q22_0_a", y="firstyrbully2", data_frame=dataset_table2){
  dep_var_tab <- table(data_frame[[y]])
  t.test_xy <-t.test(data_frame[[x]]~ data_frame[[y]], var.equal=TRUE)
  esc_t_xy <- esc_t(t=(t.test_xy$statistic)*(-1),grp1n = dep_var_tab[[2]], grp2n = dep_var_tab[[1]], es.type = "or") 
  or.ci.xy <-  paste(round( esc_t_xy$es,digits = 2), "(",round( esc_t_xy$ci.lo,digits = 2),"-",round( esc_t_xy$ci.hi, digits = 2),")",sep = "")
  num_var_list<-  data.frame( class_var="numeric", Var=x,level= "",
                              OR.CI= or.ci.xy, 
                              p_value=round(t.test_xy$p.value, digits = 6), 
                              star=ifelse(t.test_xy$p.value<0.001,"***",
                                          ifelse(t.test_xy$p.value<0.01,"**",
                                                 ifelse(t.test_xy$p.value<0.05,"*","ns"
                                                 )
                                          )
                              ),
                              dep_var=y
  )
  return(num_var_list)
  
}

num_var_tab_t <- function(x="q22_0_a", y="firstyrbully2", data_frame=dataset_table2){
  dep_var_tab <- table(data_frame[[y]])
  t.test_xy <-t.test(data_frame[[x]]~ data_frame[[y]], var.equal=TRUE)
 t_xy <- t.test_xy$statistic*(-1) 
  #or.ci.xy <-  paste(round( esc_t_xy$es,digits = 2), "(",round( esc_t_xy$ci.lo,digits = 2),"-",round( esc_t_xy$ci.hi, digits = 2),")",sep = "")
  num_var_list<-  data.frame( class_var="numeric", Var=x,level= "",
                              t= t_xy, 
                              p_value=round(t.test_xy$p.value, digits = 6), 
                              star=ifelse(t.test_xy$p.value<0.001,"***",
                                          ifelse(t.test_xy$p.value<0.01,"**",
                                                 ifelse(t.test_xy$p.value<0.05,"*","ns"
                                                 )
                                          )
                              ),
                              dep_var=y
  )
  return(num_var_list)
  
}

num_var_tab_t()
num_var_tab_t(x="firstrses")
num_var_tab_t(x="bis1_standardized")

num_var_tab_t(y="firstyrvic2",data_frame = dataset_table3)
num_var_tab_t(x="firstrses",y="firstyrvic2",data_frame = dataset_table3)
num_var_tab_t(x="bis1_standardized",y="firstyrvic2",data_frame = dataset_table3)
