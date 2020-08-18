

sourced_files <- c("creat_adj_OR_df.R","creat_unadj_OR_df.R", "mypaper_df.R")


for(i in sourced_files){
  do.call(source,list(i))
}


set.seed(125)
span=0.8
group= 20
validate.boot40= T
val.B= 40
confusion.matrix= T
p.threshhold= 0.05
imputation=F
cut.off=0.5


firstyrbully21_arg <-list(ind_var=vars_vector_victim, dep_var = "firstyrbully2", data=dataset, 
                          imputation.mis.data=imputation,
                          span=span, group=group,
                          validate.boot40= validate.boot40, 
                          val.B=val.B, 
                          confusion.matrix= confusion.matrix,
                          cut.off=cut.off,
                          p.threshhold= p.threshhold)

mypaper <- function(...) UseMethod("mypaper" )



mypaper.cal.auc <- function(x, ...){}

mypaper.conf.matrix <- function(x, ...){}


OR_firstyrbully <- do.call(mypaper.default, firstyrbully21_arg)


summary.mypaper <- function(x, ... )
  { 
  
    
    
    ##
    
    
    dep_var_arg_unadj <- list(ind_var=x$ind_var, dep_var=x$dep_var ,data=x$data)
    dep_var_arg_adj <- list(ind_var=x$ind_var, dep_var=x$dep_var ,data=x$data, 
                            span=x$span, group=x$group,  
                            confusion.matrix=x$confusion.matrix, cut.off = x$cut.off,
                            p.threshhold=x$p.threshhold, 
                            validate.boot40 = F, val.B= val.B
                            #boot.cal.plot= F
    )
    adj_OR_dep_var <- do.call(creat_adj_OR_df, dep_var_arg_adj)
    unadj_OR_dep_var<- do.call(creat_unadj_OR_df,dep_var_arg_unadj)
    OR_dep_var_arg <- list(unadj_OR_dep_var,adj_OR_dep_var,by="Var_level")
    results <-do.call(dplyr::full_join, OR_dep_var_arg)
    
    class(results) <- c("mypaper", "data.frame")
    results
}






