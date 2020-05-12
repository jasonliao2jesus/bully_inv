
##
cat_var_barplot <- function(ind_var= q3_a, 
                            ind_var_name= "Gender",
                            ind_var_level= c("Female", "Male"),
                            dep_var= firstyrbully2, 
                            dep_var_name="Bullying Involvement", 
                            dep_var_level=c(  "Pure Bullies", "Bully-Victims","Pure Victims","Not Involved"),
                            
                            data=dataset)
{
  
  
a <- as.data.frame.table(table(data[,dep_var], data[,ind_var]))
a<-plyr::rename(a, replace= c("Var1"= dep_var, "Var2"= ind_var,"Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(data[,ind_var]))
b<-plyr::rename(b, replace= c("Var1"= ind_var,"Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n= ", counts))


car_var_barplot<- ggplot(data, aes_string(x= ind_var))+ 
  
  geom_bar(aes_string(fill= dep_var,x= ind_var), position="fill")+
  
  geom_text(a, aes_string(x= ind_var,label="n_count", y= "count" ),size=3,vjust = 1.2, position="fill")+
  geom_text(b, aes_string(x= ind_var,  label= paste(n_counts,                                   
                                             "(",round(100*counts/733, digit = 1),"%",")", sep = "" ),
                          y="counts"), 
  size=3,vjust=-0.4, position = "fill"
  )+ 
  scale_fill_discrete(name = dep_var_name, 
                      labels = dep_var_level_name)+
  scale_x_discrete(labels=ind_var_level)+
  xlab(ind_var_name)+scale_y_continuous("Relative Proportion")+
  expand_limits(y=1.05, by=0.25)

}



###



##
cat_var_barplot <- function(ind_var= q3_a, 
                            ind_var_name= "Gender",
                            ind_var_level= c("Female", "Male"),
                            dep_var= firstyrbully2, 
                            dep_var_name="Bullying Involvement", 
                            dep_var_level=c(  "Pure Bullies", "Bully-Victims","Pure Victims","Not Involved"),
                            
                            data=dataset)
{
  ind_varName <- substitute(ind_var)
  dep_varName <-  substitute(dep_var)
#  eval(substitute(aes(ind_var, dep_var), list(ind_var=ind_varName, dep_var=dep_varName)))

  a <- as.data.frame.table(table(data[,deparse(dep_varName)], data[,deparse(ind_varName)]))
 # a <- as.data.frame.table(table(dataset$bully_3group, dataset$q3_a))
  a<-plyr::rename(a, replace= c("Var1"= deparse(dep_varName), 
                                "Var2"= deparse(ind_varName),"Freq"="count"))
  
  a <- mutate(a, n_count= paste0("n = ", count))
  b<- as.data.frame.table(table(data[,deparse(ind_varName)]))
  b<-plyr::rename(b, replace= c("Var1"= deparse(ind_varName),"Freq"="counts"))
  b <- mutate(b, n_counts= paste0("total n= ", counts))
  
#  aes1 <- eval(substitute(aes(x), list(x=ind_varName) ))
  
  ind_var <-  enquo(ind_var) 
  dep_var <-  enquo(dep_var)
  
  car_var_barplot<- ggplot(data, aes(x= !!ind_var))+ 
    
 
     geom_bar(aes(x= !!ind_var, fill= fct_rev(!!dep_var)), position="fill")+
    scale_fill_discrete(name = dep_var_name, 
                        labels = dep_var_level)+
    scale_x_discrete(labels=ind_var_level)+
    xlab(ind_var_name)+scale_y_continuous("Relative Proportion")+
    expand_limits(y=1.05, by=0.25)
  
}

#comds=paste0("fill=", deparse(qoute(bully_2group)) )