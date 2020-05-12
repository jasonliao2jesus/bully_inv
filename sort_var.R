a<-c("q3_a","q5_a","school_PR","q6_a","q7_a","q8_a","q10_a","q22_a",
     "q68_a","AUDIT_C_4_a","PHQ9_1yr_new",
     "q22_0_a", "firstrses","bis1_total","bis1_standardized","bis1_total")


# sort_var 分類函數 分出numeric /categorical variables

sort_var<- function(a=a, data_frame=dataset_table2){
  k<-NULL
  m<-NULL
  for (i in a) {
    if(is.factor(data_frame[[i]])) {
      
      k<-c(k, i)}else{m<-c(m,i)}
    
  }
  a<-list(k,m)
  
}