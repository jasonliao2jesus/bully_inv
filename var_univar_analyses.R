


# sort_var 分類函數 分出numeric /categorical variables
source("sort_var.R")


##cat_var_tab 個別類別變數的levels, OR.CI與p value

source("cat_var_tab1.R")

##num_var_tab 個別數值變數的OR.CI與 p value

source("num_var_tab.R")

## result of univariate analysis of categorical+numeric variables vs firstyrbully2
var_univar_analyses <- function(ind_var, y="firstyrbully2", data_frame #=dataset_table2
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
cat_var <- sorted[[1]]
num_var <- sorted[[2]]


var_list<- data.frame(class_var=NULL, Var=NULL, level=NULL,OR.CI=NULL, p_value=NULL,star=NULL,dep_var=NULL)


# Chi square test for categorical variables
for(i in sorted[[1]]){
  
var_arg <- list(i, y, data_frame)
  var_list <- rbind(  var_list,do.call(cat_var_tab, var_arg))
}

# t.test for numeric variables

for(i in sorted[[2]]){
  
  var_arg <- list(i, y, data_frame)
  var_list <- rbind(  var_list,do.call(num_var_tab, var_arg))
}




 return(var_list)
}












