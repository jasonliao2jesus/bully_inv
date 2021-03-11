
summary.mypaper <- function(x, ... )
{ 
  
  
  
  ##
  
  dep_var_arg_adj <- list(ind_var=x$ind_var, dep_var=x$dep_var ,data=x$data)
  dep_var_arg_unadj <- list(ind_var=x$ind_var, dep_var=x$dep_var ,data=x$data)
  
  adj_OR_dep_var <- do.call(creat_adj_OR.mypaper, list(x=x))
  unadj_OR_dep_var<- do.call(creat_unadj_OR_df,dep_var_arg_unadj)
  OR_dep_var_arg <- list(unadj_OR_dep_var,adj_OR_dep_var,by="Var_level")
  
  results <-do.call(dplyr::full_join, OR_dep_var_arg)
  
  class(results) <- c("mypaper", "data.frame")
  results
}

