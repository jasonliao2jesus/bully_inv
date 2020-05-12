
source("var_univar_analyses.R")


# Creat formula
creat_adj_OR_df <- function(ind_var=vars_vector, 
                            dep_var="firstyrbully2", 
                            data=dataset_table2,
                            p.threshhold=0.05,
                            must.have.var=c("firstrses_standardized"))
  {
  
  var_df_firsryrbully <- var_univar_analyses(ind_var, dep_var, data_frame = data)
  
  sig_df <-  var_df_firsryrbully%>%filter(p_value<p.threshhold)
  sig_var <- sig_df$Var
  sig_var_str <- paste(c("q3_a",as.character(sig_var),must.have.var #"firstrses_standardized"
                         ), collapse = "+ ")
  sig_fmla <- as.formula( paste(dep_var,"~",sig_var_str))
  
  glm_model <-  glm(sig_fmla, family = binomial(), method =  "brglmFit", data = data)
  model_summary <-  summary(glm_model)
  
  a <- exp(coef(glm_model))
  b <- exp(confint(glm_model))
  
  a <- cbind(round(a,digits = 2),round(b, digits = 2))    # 自動整理好格式以免累死
  
  a <- cbind(a, paste0(a[,1], "(", a[,2], "-", a[,3],")", 
                       ifelse(model_summary$coefficients[,4]<0.001,"***",
                              ifelse(model_summary$coefficients[,4]<0.01, "**",  
                                     ifelse(model_summary$coefficients[,4]<0.05,
                                            "*", ifelse(model_summary$coefficients[,4]<0.1,"+","")
                                                       
                                            )
                                    )
                              )
                      )
              )
  a <- cbind(rownames(a),as.data.frame(a) #,c("firstyrbully2")
            )
  a <- a[c(-1),c(-2,-3,-4)]
  a <- data.frame(a,row.names = NULL)
  colnames(a) <- c("Var_level","adjusted OR(CI)"
                   #,"dep_var"
   )
  
  return(a)
  
  
}

creat_adj_model <- function(ind_var=vars_vector, 
                            dep_var="firstyrbully2", 
                            data=dataset_table2,
                            p.threshhold=0.05,
                            must.have.var=c("q3_a", "firstrses_standardized"))
{
  
  var_df_firsryrbully <- var_univar_analyses(ind_var, dep_var, data_frame = data)
  
  sig_df <-  var_df_firsryrbully%>%filter(p_value<p.threshhold)
  sig_var <- sig_df$Var
  sig_var_str <- paste(c(as.character(sig_var),must.have.var #"firstrses_standardized"
  ), collapse = "+ ")
  sig_fmla <- as.formula( paste(dep_var,"~",sig_var_str))
  
  glm_model <-  glm(sig_fmla, family = binomial(), method =  "brglmFit", data = data)
 # model_summary <-  summary(glm_model)




}