creat_adj_OR <- function(x){ UseMethod("creat_adj_OR")}

creat_adj_OR.mypaper <- function(x,...
)
{
  glm_model = x$model$GLM
  
  
  
  model_summary <-  summary(glm_model)
  
  vif_model <- rms::vif(glm_model)
   
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

  a <- cbind(a, VIF = round(vif_model, digits = 2
                            )
             )
  
  
  return(a)
  
  
}


