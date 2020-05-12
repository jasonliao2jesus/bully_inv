source("creat_table2.R")


#setting independent variables
vars_vector_victim<-c("q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",
               "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
               "q22_0_a", "firstrses_standardized","bis1_standardized","victim_3group")
vars_vector<-c("q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",
                      "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                      "q22_0_a", "firstrses_standardized","bis1_standardized")
# setting working data frames : 
#  goal: comparison group= non-involved group 



dataset_table2<-dataset%>%filter(firstyrpurev=="킠")
dataset_table3<-dataset%>%filter(firstyr_pureb=="no")
dataset_bv_vs_noninvolved<-dataset%>%filter(firstyrpureb=="킠" & firstyrpurev=="킠")
dataset_pureb<-dataset%>%filter(firstyrvic2=="킠")
dataset_purev<-dataset%>%filter(firstyr_bully2=="no") 
dataset_table2_rec<-dataset%>%filter(firstyrpurev=="킠")
dataset_table3_rec<-dataset%>%filter(firstyrpureb=="킠")






#create arguments for all comparison(bullies vs. non-involved; victims vs non-involved;... etc.  )


firstyrbully2_arg <-list(ind_var=vars_vector, dep_var = "firstyrbully2", dataset_table2)
firstyrvic2_arg <- list(ind_var=vars_vector, dep_var = "firstyrvic2", dataset_table3)
bv_arg <- list(ind_var=vars_vector, dep_var = "firstyrbv", dataset_bv_vs_noninvolved)
pureb_arg <- list(ind_var=vars_vector, dep_var = "firstyrbully2", dataset_pureb)
purev_arg <- list(ind_var=vars_vector, dep_var = "firstyrvic2", dataset_purev)
rec_b_arg <- list(ind_var=vars_vector, dep_var = "rec_bully_firstyr", dataset_table2_rec)
rec_v_arg <- list(ind_var=vars_vector, dep_var = "rec_victim_firstyr", dataset_table3_rec)






## create data

OR2_firstyrbully <- do.call(creat_table2, firstyrbully2_arg)
OR2_firstyrvic  <- do.call(creat_table2, firstyrvic2_arg)
OR2_bv <- do.call(creat_table2, bv_arg)
OR2_pureb <- do.call(creat_table2, pureb_arg)
OR2_purev <- do.call(creat_table2,purev_arg)
OR2_rec_b <- do.call(creat_table2, rec_b_arg)
OR2_rec_v <- do.call(creat_table2,rec_v_arg)



table2 <-  list(bullies= OR2_firstyrbully,
                victims= OR2_firstyrvic[,c(2,3)],
                bully_victims= OR2_bv[,c(2,3)],
                pure_bullies= OR2_pureb[,c(2,3)],
                pure_victims= OR2_purev[,c(2,3)],
                repeated_bullies= OR2_rec_b[,c(2,3)],
                repeated_victims= OR2_rec_v[,c(2,3)])




write.table(table2, "table2.csv",sep = ",")


## predicting models: model and model 2. 


firstyrbully21_arg <-list(ind_var=vars_vector_victim, dep_var = "firstyrbully2", dataset)
rec_b1_arg <- list(ind_var=vars_vector_victim, dep_var = "rec_bully_firstyr", dataset)


OR21_firstyrbully <- do.call(creat_table2, firstyrbully21_arg)
OR21_rec_b <- do.call(creat_table2, rec_b1_arg)


table3 <-  list(bullies= OR21_firstyrbully,
                repeated_bullies= OR21_rec_b[,c(2,3)])

write.table(table3, "table3.csv",sep = ",")


##  missing data filled with multiple imputation by chained equations. 
firstyrbully21_arg <-list(ind_var=vars_vector_victim, dep_var = "firstyrbully2", data= dataset_selected)
rec_b1_arg <- list(ind_var=vars_vector_victim, dep_var = "rec_bully_firstyr", data=dataset_selected)


OR21_firstyrbully <- do.call(creat_table2, firstyrbully21_arg)
OR21_rec_b <- do.call(creat_table2, rec_b1_arg)


table3 <-  list(bullies= OR21_firstyrbully,
                repeated_bullies= OR21_rec_b[,c(2,3)])

write.table(table3, "table3.csv",sep = ",")

