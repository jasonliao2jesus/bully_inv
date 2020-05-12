source("var_univar_analyses.R")

creat_unadj_OR_df <- function(ind_var=vars_vector, dep_var="firstyrbully2", data=dataset_table2){
  
  var_df <- var_univar_analyses(ind_var , dep_var, data_frame = data
                                )
  
  var_df <- mutate(var_df, or.ci.star= ifelse(star == "ns", paste0(OR.CI), paste0(OR.CI,star)))
  var_df<-unite(var_df,"Var_level",c("Var","level"), sep = "")
  var_df_selected <-var_df[,c("Var_level","or.ci.star")
                           ] 
  colnames(var_df_selected) <- c("Var_level","unadjusted OR(CI)")
  return(var_df_selected)
}