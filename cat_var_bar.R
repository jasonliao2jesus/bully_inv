

cat_var_bar <- function(ind_var= "q3_a", 
                                  ind_var_name= NULL,
                                  ind_var_level= NULL,
                        dep_var= "bully_inv", 
                                  dep_var_name= "Bullying Involvement", 
                                  dep_var_level= c(  "Pure Bullies", "Bully-Victims","Pure Victims","Not Involved"),
                                  
                        data=dataset,
                                  position="fill", legend.present= TRUE, font.size=3)
{

a <- as.data.frame.table(table(data[,dep_var], data[,ind_var] ,exclude = NULL
                               )
                         )

a<-plyr::rename(a, replace= c("Var1"= dep_var, 
                              "Var2"= ind_var,"Freq"="count"))

a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(data[,ind_var], exclude = NULL))
b<-plyr::rename(b, replace= c("Var1"= ind_var,"Freq"="counts"))
b <- mutate(b, n_counts= paste0("t.n=", counts))
b<- mutate(b, 
           n_pro = paste(n_counts, 
                            "(",round(100*counts/733, digit = 1),"%",")", sep = "" 
                            )
          )

ind_varname <- as.name(ind_var)  # convert string to name
dep_varname <- as.name(dep_var) 

ind_var1 <-  enquo(ind_varname)   # convert name to quosure/formula to be used by ggplot.
dep_var1 <-  enquo(dep_varname)

cat_var <- ggplot(data)+  
  geom_bar(aes(x= !!ind_var1, fill= fct_rev(!!dep_var1)), position= position) #+scale_fill_discrete( as.name(paste("name =", dep_var_name)), as.name(paste("level =", dep_var_level)))




ifelse( is.null(dep_var_name)&is.null(dep_var_level),
        cat_var <- cat_var, 
        ifelse(is.null(dep_var_name)& !is.null(dep_var_level), 
               cat_var <- cat_var+ scale_fill_discrete( labels = dep_var_level),
               ifelse(!is.null(dep_var_name)& is.null(dep_var_level), 
                      cat_var <- cat_var+ scale_fill_discrete( name = dep_var_name), 
                      cat_var <- cat_var+ scale_fill_discrete( name = dep_var_name, labels= dep_var_level)
               )
               
        )
        
)

if( !is.null(ind_var_level) ){
  cat_var <- cat_var+ scale_x_discrete(labels=ind_var_level)
  
}
  


 if(!is.null(ind_var_name)){
    cat_var <- cat_var+ xlab(  ind_var_name)  
 }

cat_var <- cat_var+
  geom_text(data=a, aes( x= !!ind_var1 , 
                         y= count,label=ifelse(count==0, "",n_count) ),
            size=font.size,vjust = 1.2, 
            position=position)+
  geom_text(data=b, aes( x=!!ind_var1 ,y= counts,   label=n_pro
  ),   
  size=font.size,vjust=-0.4, position = position
  )+ 
  
  scale_y_continuous(ifelse(position=="fill", "Relative Proportion",
                            ifelse(position=="stack", "Count",
                                   "Noted: position='fill' or 'stack' only")))+
  expand_limits(y=1.05, by=0.25)+
  theme_pubr()+
  theme(axis.title.y = element_text(size=10), axis.text.x = element_text(size=9))

if(!legend.present){
  cat_var <- cat_var+ theme(legend.position= "none")
 }

  
 
  
   cat_var
}
