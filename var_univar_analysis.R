a<-c("q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",
     "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
     "q22_0_a", "firstrses","bis1_total")



# sort_var 分類函數 分出numeric /categorical variables
source("sort_var.R")


##cat_var_tab 個別類別變數的levels, OR.CI與p value

source("cat_var_tab1.R")

##num_var_tab 個別數值變數的OR.CI與 p value

source("num_var_tab.R")

## result of univariate analysis of categorical+numeric variables vs firstyrbully2
var_univar_analyses <- function(ind_var, y="firstyrbully2", data_frame =dataset_table2
                                )
  {

 #   keyin=readline("type the dependent variable(specify or choose enter:firstyrbully2 2:rec_bully_firstyr 3:firstyrvic2 4: rec_victim_firstyr)")
    
    #specify y   可以選 也可以自行輸入
  #  if(keyin=="2"){
   #         y="rec_bully_firstyr"}
  #  else if(keyin=="3"){
   #         y="firstyrvic2"}
  #  else if(keyin=="4"){
  #         y="rec_victim_firstyr"}
   # else if(keyin=="")
    #{ 
#      warning("the default dependent var is firstyrbully2")
 #   }
  #  else{
   #   y=keyin
  #  }

sorted <- sort_var(ind_var,data_frame = data_frame)

var_list<- data.frame(class_var=NULL, Var=NULL, level=NULL,OR.CI=NULL, p_value=NULL,star=NULL,dep_var=NULL)

for(i in sorted[[1]]){
  
var_arg <- list(i, y, data_frame)
  var_list <- rbind(  var_list,do.call(cat_var_tab, var_arg))
}

for(i in sorted[[2]]){
  
  var_arg <- list(i, y, data_frame)
  var_list <- rbind(  var_list,do.call(num_var_tab, var_arg))
}


#cat_var_list <-   map2(sorted[[1]],y,cat_var_tab)
#cat_var_df <- do.call(rbind, cat_var_list)


 return(var_list)
}








##

dataset_table2<-dataset%>%filter(firstyrpurev=="無")
dataset_table3<-dataset%>%filter(firstyr_pureb=="no")
dataset_bv_vs_noninvolved<-dataset%>%filter(firstyr_pureb=="no" & firstyrpurev=="無")
dataset_pureb<-dataset%>%filter(firstyrvic2=="無")
dataset_purev<-dataset%>%filter(firstyr_bully2=="no") 
dataset_table2_rec<-dataset%>%filter(firstyrpurev=="無")
dataset_table3_rec<-dataset%>%filter(firstyrpureb=="無")
dataset_bv_vs_noninvolved<-dataset%>%filter(firstyrpureb=="無" & firstyrpurev=="無")

tab2 <-  list(ind_var = a, data_frame = dataset_table2) 
tab3 <- list(ind_var=a, y ="firstyrvic2", data_frame = dataset_table3)
tabbv <- list(ind_var = a, y= "firstyr_bv", data_frame = dataset_bv_vs_noninvolved)
tabpureb <- list(ind_var = a, data_frame = dataset_pureb)
tabpurev <- list(ind_var= a, y ="firstyrvic2", data_frame=dataset_purev)
tabrecb <- list(ind_var = a,y = "rec_bully_firstyr", data_frame= dataset_table2_rec)
tabrecv <- list(ind_var = a, y = "rec_victim_firstyr", data_frame = dataset_table3_rec)

list_vars <- list(tab2, tab3, tabbv, tabpureb,tabpurev, tabrecb,tabrecv)

var_df<-data.frame(class_var=NULL, Var=NULL, level=NULL,OR.CI=NULL, p_value=NULL,star=NULL,dep_var=NULL)
v_df <- data.frame(class_var=NULL, Var=NULL, level=NULL,OR.CI=NULL, p_value=NULL,star=NULL,dep_var=NULL)
for(i in list_vars)
{ 
  
  v_df <- do.call(var_univar_analyses, i)
  v_df <- mutate(v_df, or.ci.star= ifelse(star == "ns", paste0(OR.CI), paste0(OR.CI,star)))
  v_df <- v_df[,c(-1, -4,-5,-6)]
  
  var_df <- rbind(var_df, v_df) 
                
}





var_df_firsryrbully <- var_univar_analyses(ind_var = a, data_frame = dataset_table2) 
var_df_firstyrvic <- var_univar_analyses(ind_var=a, y ="firstyrvic2", data_frame = dataset_table3)
var_df_pureb <- var_univar_analyses(ind_var = a, data_frame = dataset_pureb)
    var_df_pureb <-mutate( var_df_pureb,dep_var="pureb") 
var_df_purev <- var_univar_analyses(ind_var= a, y ="firstyrvic2", data_frame=dataset_purev)
    var_df_purev <-mutate( var_df_purev,dep_var="purev")
var_df_rec_b <- var_univar_analyses(ind_var = a,y = "rec_bully_firstyr", data_frame= dataset_table2_rec)
var_df_rec_v <- var_univar_analyses(ind_var = a, y = "rec_victim_firstyr", data_frame = dataset_table3_rec)
var_df_bv <- var_univar_analyses(ind_var = a, y= "firstyr_bv", data_frame = dataset_bv_vs_noninvolved)


var_df <- rbind(var_df_firsryrbully, 
      var_df_firstyrvic, 
      var_df_bv,
      var_df_pureb, 
      var_df_purev,
  
      var_df_rec_b,
      var_df_rec_v)

var_df <- mutate(var_df, or.ci.star= ifelse(star == "ns", paste0(OR.CI), paste0(OR.CI,star)))

k <- var_df[,c(-1,-4,-5,-6)]

k<-unite(k,"Var_level",c("Var","level"))
k <- spread(k,"dep_var", "or.ci.star")
write.table(var_df, "var_df.csv",sep = ",")




