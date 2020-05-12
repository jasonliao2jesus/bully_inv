#

build_model <- function(data=dataset,
                        ind_var= c("victim_group", "q3_a","bis1_standardized"),
                        dep_var= "firstyrbully2",
                        penalized= T ,
                        machine= "glm" )
{
  fo_dep <- paste(dep_var,"~", collapse = "")
  fo_ind <-  paste(ind_var, collapse = "+")
  fo <- formula(paste(fo_dep, fo_ind))
  
 
if(machine=="glm")
  {  
    model_0<- glm(formula = fo, 
                  family = binomial(),method = ifelse(penalized, "brglmFit", "glm.fit"),  # method= "glm.fit"
                  data =  data)
  }  
  
   
if(machine=="lm")
{  
  model_0<- glm(formula = fo, 
                  # method= "glm.fit"
                data =  data)
}
  

if(machine=="lrm")
  model_0 <- lrm( fo, 
                          x=TRUE, y=TRUE, data =  data)
  model_0 
}



model_2_rec <- glm(formula = rec_bully_firstyr ~ victim_3group+q3_a +  AUDIT_C_4_a+ firstrses+
                     bis1_standardized, 
                   family = binomial(), data =  dataset)
model_1_rec <- glm(formula = rec_bully_firstyr ~ 1, 
                   family = binomial(), data =  dataset)



vif_model_2_rec <- vif(model_2_rec)


dom_model_2_rec <- dominanceanalysis::dominanceAnalysis(model_2_rec)
plot(dom_model_2_rec, fit.function="r2.m",  which.graph='conditional')


write.table(vif_model_2_rec, "vif_rec.csv",sep = ",")