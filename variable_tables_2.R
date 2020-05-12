


variable.tables <- function(  independent_var,data_frame= "dataset_selected", dependent_var="firstyrbully2") 
{ 
  k= data_frame[independent_var]%>%sapply(nlevels)  # k is a vector of levels of independent variables
  
  all_factor_var=NULL
  all_levels_var= NULL#sum(k)-length(independent_var[k!=0]) # sum of the numbers of levels of all factors(categorial variables).
  all_num_var= NULL #length(independent_var[k==0]) # sum of the numbers of all numeric variables.
 
  not_factor <- NULL   # rep(NA, length(independent_var))    # create non_factor to store TRUE/FALSE vector(the variable is a factor or not)
  result_factor_var <-NULL      # create result_factor_var to store the result of OR/confint/power of all factors(categorical variables)
  result_num_var <-NULL            # create result_num_var to store the result of OR/confint/power of all numeric variables 
     
    result_levels_var_selected <-NULL # create result_levels_var_selected to store the result of TRUE/FALSE (if factors vs dependent_var has p<0.05)
    result_num_var_selected <-NULL
    result_factor_p_value_list <- NULL
    result_num_p_value_list <- NULL
    result_factors_var_selected<-NULL 
  for (i in  seq_along(independent_var))      #seq_along(independent_var):  build a 1:length(independent_Var) sequence vector 
   {  
      table_variable <- table(data_frame[[independent_var[i]]],data_frame[[dependent_var]])
      #not_factor[i]<- !is.factor(data_frame[[independent_var[i]]])
      
      
      if (is.factor(data_frame[[independent_var[i]]]))
        { #  p_value <- NULL  #rep(NA, length(nlevels(independent_var[i])-1))
          
        all_factor_var <- c(all_factor_var, independent_var[i])
        
        
          for (x in 1:(nlevels(data_frame[[independent_var[i]]])-1))
             {
        all_levels_var <- c(all_levels_var, paste0(c(independent_var[i], levels(data_frame[[independent_var[i]]])[x+1]), collapse = ""))
                p_value <- NULL  
         p_value <-chisq.test(table_variable[c(1, x+1),],correct=TRUE )$p.value
         p_selected <-  !(p_value > 0.05|is.na(p_value))

         result_levels_var_selected<- c(result_levels_var_selected,p_selected)

              if(x==1){
                  result_factors_var_selected<-  c( result_factors_var_selected, p_selected)  
              }else{                          
                  result_factors_var_selected[length(result_factors_var_selected)] <- result_factors_var_selected[length(result_factors_var_selected)] | p_selected #, result_factors_var_selected[length(result_factors_var_selected)]|p_selected)
              }                                                                          
         or_variable <- fisher.test(table_variable[c(1, x+1 ),])
         result_factor_var<- c(result_factor_var, paste0(                     #只有此行才被輸出
                                                          c( "Odds ratio ( confident interval) of ", independent_var[i], levels(data_frame[[independent_var[i]]])[x+1],
                                                             " for ",dependent_var," in ",deparse(substitute(data_frame)) ," is ",round(or_variable$estimate,digit=2),
                                                            "(",round(or_variable$conf.int,digit=2),")", " ( p= ", round(p_value, digit=10), " )"
                                                            ),collapse=" "
                                                         )
                               )
        fac <- list(result_factor_var, result_levels_var_selected, all_levels_var, result_factors_var_selected)
        
             }
        
         fac<-c(fac,list(all_factor_var))    
        
           
         
         } 
          else{         
           
                
    t_independent_var <- t.test(data_frame[[independent_var[i]]]~ data_frame[,dependent_var], var.equal=FALSE)
    p_value <- t_independent_var$p.value
    p_selected<- !(p_value > 0.05|is.na(p_value))
    result_num_var_selected<- c(result_num_var_selected,p_selected)
    all_num_var <- c(all_num_var, independent_var[i])
    d_var <- cohen.d(data_frame[[independent_var[i]]]~ data_frame[[dependent_var]])
    result_num_var<- c(result_num_var, paste0(                                              #只有此行才被輸出 
      c("Odds ratio ( confident interval) of ", independent_var[i]," for ",dependent_var," in ",
        deparse(substitute(data_frame)) ," is ", round(exp(-d_var$estimate*pi/sqrt(3)),digits = 2),"(", round(exp(-d_var$conf.int*pi/sqrt(3)),digits = 2),")", 
        " ( p= ", round(p_value, digit=10), " )"
      ),collapse=" "
    ))
    num <-  list(result_num_var,result_num_var_selected, all_num_var) 
    
  } 
   # fac <- fac
    
        #print(fac,num)
    #list_fac_num <- list(fac, num)
  }
    #result_factor_var <- result_factor_var[!is.na(result_factor_var)]
    #result_levels_var_selected <- result_levels_var_selected[!is.na(result_levels_var_selected)]
    #result_num_var <- result_num_var[!is.na(result_num_var)]
    #result_num_var_selected <- result_num_var_selected[!is.na(result_num_var_selected)]
 #fac<- list_fac_num[[1]]
 #num <-  list_fac_num[[2]]
     
    return( c( 
                c( fac[[1]],#[!is.na(result_factor_var)],  
                   paste0(c("p< 0.05的類別變數factors與層級levels 如下:" , fac[[3]][fac[[2]]]), collapse=", "),
                   paste0(c("p< 0.05的類別變數為如下:" , fac[[5]][fac[[4]]]), collapse="+ ")
                  )
               , 
               c(num[[1]], #[!is.na(result_num_var)],  
                  paste0(c("p< 0.05的數值變數為以下:" , num[[3]][num[[2]]]), collapse="+ "
                        )
                )
               )
    
      )
}    
    





