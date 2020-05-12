sourced_files <- c("creat_adj_OR_df.R","creat_unadj_OR_df.R")


for(i in sourced_files){
do.call(source,list(i))
}

creat_table2 <- function(ind_var, dep_var="firstyrbully2", data=dataset)
{
  dep_var_arg <- list(ind_var, dep_var ,data)
  adj_OR_dep_var <- do.call(creat_adj_OR_df, dep_var_arg)
  unadj_OR_dep_var<- do.call(creat_unadj_OR_df,dep_var_arg)
  OR_dep_var_arg <- list(unadj_OR_dep_var,adj_OR_dep_var,by="Var_level")
  OR2_dep_var <-do.call(dplyr::full_join, OR_dep_var_arg)
  return(OR2_dep_var)
}