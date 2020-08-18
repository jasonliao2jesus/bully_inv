
# Make a list of variable names and their level names, 
# with an index of assigned factor/level names 


levels.ls <-  function(ind_var, #dep_var, 
                       data, keyin=T, level.names.list= names.ls,variable_names=vars_names ){
  
  
  sorted <- sort_var(ind_var,data = data)
  cat_var <- sorted[[1]]
  num_var <- sorted[[2]]
  levels.list <- NULL
  cat_names <- NULL
  num_names <- NULL
  
  keyin <- readline("Key in or not? T or F (using name lists)")
  
  # categorical variables 
  for(i in cat_var){
    
    levels_name <- NULL
    levels_cat_i <- levels(data[[i]])
    
    
    
    if(keyin){ 
      prompt_i <- paste("what is the formal name of the categorical variable", i,"?", sep = " ")
      
      cat_name <- readline(prompt = prompt_i)
      
      cat_names <- c(cat_names, ifelse(cat_name=="", i,cat_name) )
      
      
      for(k in levels_cat_i){
        prompt_levels <- paste("what are the names of levels of", print(k) ,"for factor",i,"?", sep = " ")
        level_name <- readline(prompt = prompt_levels)
        levels_name <- c(levels_name, ifelse(level_name=="",k,level_name) )  
      }
      
      
      
      
    }else{
      
      levels_name <- level.names.list[[i]]
      cat_name <- variable_names[i]
      cat_names <- c(cat_names, cat_name)
    }
    
    
    names(levels_cat_i) <- levels_name
    
    
    levels.list[[i]] <- levels_cat_i 
    
  }
  names(cat_names) <- cat_var  
  names(levels.list) <- cat_var
  
  #numeric variables
  
  
  for(i in num_var){
    
    if(keyin){
      
      prompt_i <- paste("what is the formal name of the numeric variable", i,"?", sep = " ")
      
      num_name <- readline(prompt = prompt_i)
      
      num_names <- c(num_names, ifelse(num_name =="", i, num_name) )
      
    }else{
      
      num_name<- variable_names[i]
      num_names <- c(num_names, num_name)
      
    }
    
  }
  
  names(num_names) <- num_var
  
  
  
  levels.list[["factor_variable_names"]] <- cat_names
  levels.list[["numeric_variable_names"]] <- num_names
  return(levels.list)
  
}
