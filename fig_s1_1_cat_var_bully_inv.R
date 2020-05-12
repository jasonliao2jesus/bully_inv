


# independent variable: bully-perpetrator; dependent variables: victims.

#a <- as.data.frame.table(table(dataset$bully_3group, dataset$victim_3group))
#a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "victim_3group","Freq"="count"))
#a <- mutate(a, n_count= paste0("n = ", count))
#b<- as.data.frame.table(table(dataset$bully_3group))
#b<-plyr::rename(b, replace= c("Var1"= "bully_3group","Freq"="counts"))
#b <- mutate(b, n_counts= paste0("total n= ", counts))
#bullies<- ggplot(data=dataset, aes(x=bully_3group))+ 
  
# geom_bar(aes(fill=fct_rev(victim_3group), x=bully_3group), position="fill")+
# geom_text(data=a, aes(x=bully_3group,label=n_count, y=count ),vjust = 1.0, position="fill")+
# geom_text(data=b, aes(x=bully_3group, label=n_counts, y=counts), vjust=-0.1, position="fill")

#

#ggplot(data=dataset, aes(fill=victim_group, x=bully_group))+ geom_bar(position="stack")

#ggplot(data=dataset, aes(fill=victim_3group, x=bully_3group))+ geom_bar(position="dodge")



#  bullying involvement by categorial variables
#a <- as.data.frame.table(table(dataset$bully_3group, dataset$q3_a, dataset$school_PR, dataset$q8_a, dataset$q7_a, dataset$q5_a, dataset$q6_a, dataset$victim_3group ))
#a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "q3_a", "Var3"= "school_PR", "Var4"="q8_a", "Var5"="q7_a", "Var6"="q5_a", "Var7"="q6_a","Var8"= "victim_3group", "Freq"="count"))

#ggplot(as.data.frame(a),
#      aes(y = count, axis1= bully_3group, axis5=fct_rev( q5_a), axis4= q6_a, axis6= q7_a, axis3= school_PR, axis7= q3_a, axis8= bully_3group, axis2= victim_3group)) +
# geom_alluvium(aes(fill = bully_3group), width = 1/12) +
# geom_stratum( width = 1/6, fill = "black", color = "grey", alpha=0.6) +
# 
# geom_label(stat = "stratum", infer.label = TRUE, size=3, color="white", fill="black", alpha=0.1, label.size = 0 ) +
# scale_x_continuous(breaks = 1:8, labels = c("Bullies", "Victims",  "School_PR" ,"Mother's","Father's job","Needy","Gender", "Bullies")) +
# coord_flip() +
# scale_fill_brewer(type = "qual", palette = "Set1") +
  
# ggtitle("bullying involvement")


#  ggtitle("levels perpetration by levels of victimization")

 
  #隔年對照
#a <- as.data.frame.table(table(dataset_sec_yr_testing$bully_3group, dataset_sec_yr_testing$victim_3group))
#a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "victim_3group","Freq"="count"))
#a <- mutate(a, n_count= paste0("n = ", count))
#b<- as.data.frame.table(table(dataset$victim_3group))
#b<-plyr::rename(b, replace= c("Var1"= "victim_3group","Freq"="counts"))
#b <- mutate(b, n_counts= paste0("total n= ", counts))
#ggplot(data=dataset_sec_yr_testing,aes( x=victim_3group))+ 
# geom_bar(aes(fill= fct_rev(bully_3group)
#             , x=victim_3group),position="fill")+
# geom_text(data=a, aes(x=victim_3group,label=n_count, y=count ),vjust = 1.2, position="fill")+
#geom_text(data=b, aes(x=victim_3group, label=n_counts, y=counts), vjust=-0.1, position="fill")



#ggplot(data=dataset_sec_yr_testing%>%filter(otherbully=="無"), aes(fill=victim_3group, x=bully_3group))+ geom_bar(position="fill")

 
 # age
#ggplot(data=dataset, aes(fill=bully_inv, x=yr))+ geom_bar(position="stack")+
#geom_vline(aes(xintercept=mean(yr)),  
#          color="red", linetype="dashed", size=1)+
# annotate("text", x = mean(dataset$yr), y = 70, label = paste0("mean age = ", mean(dataset$yr)), hjust=-0.1, vjust=-0.8
# )
  # age: density 似乎有差別
           #ggplot(data=dataset, aes(fill=fct_rev(bully_inv), x=yr))+ geom_density(alpha= .5)+scale_fill_manual(values=c("ghostwhite", "cadetblue1", "limegreen" , "red1"))+
           #   theme_bw()+geom_vline(aes(xintercept=mean(yr)),  
#           color="red", linetype="dashed", size=1)

arg_3 <- list(ind_var = "q3_a", dep_var= "bully_inv", ind_var_name = "Gender", ind_var_level = c("Male","Female"), legend.present = T, position = "fill")
arg_5 <- list(ind_var = "q5_a", dep_var= "bully_inv", ind_var_name = "Father's employment", ind_var_level = c("Yes", "No", "Unknown"), legend.present = T, position = "fill", font.size=2.5)
arg_6 <- list(ind_var = "q6_a", dep_var= "bully_inv", ind_var_name = "Mother's employment", ind_var_level = c("Yes", "No", "Unknown"), legend.present = T, position = "fill", font.size=2.5)
arg_7 <- list(ind_var = "q7_a", dep_var= "bully_inv", ind_var_name = "Needly family", ind_var_level = c("Yes", "No"), legend.present = T, position = "fill")
arg_8 <- list(ind_var = "q8_a", dep_var= "bully_inv", ind_var_name = "Living with parents", ind_var_level = c("Yes", "No"), legend.present = T, position = "fill")
arg_school <- list(ind_var = "school_PR", dep_var= "bully_inv",ind_var_name = "School PR", ind_var_level = NULL, legend.present = T, position = "fill")
arg_68 <- list(ind_var = "q68_a",dep_var= "bully_inv", ind_var_name = "Smoking status", ind_var_level = c("Yes", "No"), legend.present = T, position = "fill")


plot3 <- do.call(cat_var_bar, arg_3)
plot5 <-  do.call(cat_var_bar, arg_5)
plot6 <- do.call(cat_var_bar, arg_6)
plot7 <- do.call(cat_var_bar, arg_7)
plot8 <- do.call(cat_var_bar, arg_8)
plotschool <- do.call(cat_var_bar, arg_school)
plot68 <- do.call(cat_var_bar, arg_68)

# Figure S1
figure_s1 <-  ggarrange(
  plot3, plot5, plot6, plot7, plot8, plotschool, plot68,#bully_inv_more,
  labels = c(
    "A", "B", "C", "D", "E", "F","G"
  ), 
  legend = "right", 
  common.legend = TRUE
)




annotate_figure(
  figure_s1, top = text_grob("\n Sample Characteristics: Demographic/Categorical Variables by Bullying Involvement", 
                             face = "plain", size = 13, hjust=0.7067, vjust = 0.27
  ),
  fig.lab = "Figure S1.1", fig.lab.face = "bold", 
  fig.lab.size = 13, fig.lab.pos= "top.left"
)



# bully_involvement: gender
#ggplot(data=dataset, aes(fill=bully_inv, x=q3_a))+ geom_bar(position="dodge")

a <- as.data.frame.table(table(dataset$bully_inv, dataset$q3_a))
a<-plyr::rename(a, replace= c("Var1"= "bully_inv", "Var2"= "q3_a","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$q3_a))
b<-plyr::rename(b, replace= c("Var1"= "q3_a","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n= ", counts))
q3_a_bully_inv<- ggplot(data=dataset, aes(x=q3_a))+ 
  
  geom_bar(aes(fill=fct_rev(bully_inv),x=q3_a), position="fill", )+
  
  geom_text(data=a, aes(x=q3_a,label=n_count, y=count ),size=3,vjust = 1.2, position="fill")+
  geom_text(data=b, aes(x=q3_a,  label=paste(n_counts,                                   
                                             "(",round(100*counts/733, digit = 1),"%",")", sep = ""
                                             ), y=counts), 
            size=3,vjust=-0.4, position = "fill"
            )+ 
  scale_fill_discrete(name = "Bullying Involvement", 
                      labels = c(  "Pure Bullies", "Bully-Victims","Pure Victims","Not Involved"))+
  scale_x_discrete(labels=c("Female", "Male"))+
  xlab("Gender")+scale_y_continuous("Relative Proportion")+
  expand_limits(y=1.05, by=0.25)


# bullying involvement: school_PR
a <- as.data.frame.table(table(dataset$bully_inv, dataset$school_PR))
a<-plyr::rename(a, replace= c("Var1"= "bully_inv", "Var2"= "school_PR","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$school_PR))
b<-plyr::rename(b, replace= c("Var1"= "school_PR","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n= ", counts))
school_PR <- ggplot(data=dataset, aes(x=school_PR))+ 
  
  geom_bar(aes(fill=fct_rev(bully_inv),x=school_PR), position="fill")+
  geom_text(data=a, aes(x=school_PR,label=n_count, y=count ),size=3,vjust = 1.2, position="fill")+
  geom_text(data=b, 
             aes(x=school_PR,  label=paste(n_counts,                                    
                                           "(",round(100*counts/733, digit = 1),"%",")", sep = ""
                                           ), y=counts),
             size=3, vjust=-0.3, position="fill")+
  scale_fill_discrete(name = "Bullying Involvement", 
                      labels = c(  "Pure Bullies", "Bully-Victims","Pure Victims","Not Involved"))+

  xlab("School PR")+ylab("")+
  expand_limits(y=1.05, by=0.25)+
  theme(legend.position= "none")

# q5_a
a <- as.data.frame.table(table(dataset$bully_inv, dataset$q5_a, exclude = "我不知道"))
a<-plyr::rename(a, replace= c("Var1"= "bully_inv", "Var2"= "q5_a","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$q5_a,exclude = "我不知道"))
b<-plyr::rename(b, replace= c("Var1"= "q5_a","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n=", counts))
q5_a <- ggplot(data=dataset, aes(x=q5_a))+ 
  
  geom_bar(aes(fill=fct_rev(bully_inv),x=q5_a), position="fill")+
  geom_text(data=a, aes(x=q5_a,label=n_count, y=count ),size=3,vjust = 1.2, position="fill")+
  geom_text(data=b, aes(x=q5_a,  label=paste(n_counts,                                    
                                             "(",round(100*counts/733, digit = 1),"%",")", sep = ""
                                             ), y=counts),size=3, vjust=-0.3, position="fill"
                  )+
  scale_fill_discrete(name = "Bullying Involvement", 
                      labels = c(  "Pure Bullies", "Bully-Victims","Pure Victims","Not Involved"))+
  scale_x_discrete(labels=c("Yes", "No", "Unknown"))+
  xlab("Father's Employment")+ylab("")+
  expand_limits(y=1.05, by=0.25)+
  theme(legend.position= "none")

#q6_a
a <- as.data.frame.table(table(dataset$bully_inv, dataset$q6_a, exclude = "我不知道"))
a<-plyr::rename(a, replace= c("Var1"= "bully_inv", "Var2"= "q6_a","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$q6_a, exclude = "我不知道"))
b<-plyr::rename(b, replace= c("Var1"= "q6_a","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n=", counts))
q6_a <- ggplot(data=dataset, aes(x=q6_a))+ 
  
  geom_bar(aes(fill=fct_rev(bully_inv),x=q6_a), position="fill")+
  geom_text(data=a, aes(x=q6_a,label=ifelse(count==0, "",n_count), y=count ),size=3,vjust = 1.2, position="fill")+
  geom_text(data=b, aes(x=q6_a,  label=paste(n_counts,                                    
                                             "(",round(100*counts/733, digit = 1),"%",")", sep = ""
                                             ), y=counts),size=3, vjust=-0.3, position="fill")+
  scale_fill_discrete(name = "Bullying Involvement", 
                      labels = c(  "Pure Bullies", "Bully-Victims","Pure Victims","Not Involved"))+
  scale_x_discrete(labels=c("Yes", "No", "Unknown"))+
  xlab("Mother's Employment")+ylab("")+
  expand_limits(y=1.05, by=0.25)+
  theme(legend.position= "none")

# q7_a
a <- as.data.frame.table(table(dataset$bully_inv, dataset$q7_a))
a<-plyr::rename(a, replace= c("Var1"= "bully_inv", "Var2"= "q7_a","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$q7_a))
b<-plyr::rename(b, replace= c("Var1"= "q7_a","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n= ", counts))
q7_a <- ggplot(data=dataset, aes(x=q7_a))+ 
  
  geom_bar(aes(fill=fct_rev(bully_inv),x=q7_a), position="fill")+
  geom_text(data=a, aes(x=q7_a,label=n_count, y=count ),size=3,vjust = 1.2, position="fill")+
  geom_text(data=b, aes(x=q7_a,  label=paste(n_counts,                                     
                                             "(",round(100*counts/733, digit = 1),"%",")", sep = ""
                                             ), y=counts),size=3, vjust=-0.3, position="fill")+
  scale_fill_discrete(name = "Bullying Involvement", 
                      labels = c(  "Pure Bullies", "Bully-Victims","Pure Victims","Not Involved"))+
  scale_x_discrete(labels=c("Yes", "No"))+
  xlab("Needy Family")+ylab("Relative Proportion")+
  expand_limits(y=1.05, by=0.25)+
  theme(legend.position= "none")
 

# q8_a
a <- as.data.frame.table(table(dataset$bully_inv, dataset$q8_a))
a<-plyr::rename(a, replace= c("Var1"= "bully_inv", "Var2"= "q8_a","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$q8_a))
b<-plyr::rename(b, replace= c("Var1"= "q8_a","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n= ", counts))
q8_a <- ggplot(data=dataset, aes(x=q8_a))+ 
  
  geom_bar(aes(fill=fct_rev(bully_inv),x=q8_a), position="fill")+
  geom_text(data=a, aes(x=q8_a,label=n_count, y=count ),size=3,vjust = 1.2, position="fill")+
  geom_text(data=b, aes(x=q8_a,  label=paste(n_counts,                                     
                                             "(",round(100*counts/733, digit = 1),"%",")", 
                                             sep = ""
                                             ), y=counts),size=3, vjust=-0.3, position="fill")+
  scale_fill_discrete(name = "Bullying Involvement", 
                      labels = c(  "Pure Bullies", "Bully-Victims","Pure Victims","Not Involved"))+
  scale_x_discrete(labels=c("Yes", "No"))+
  xlab("Living with Parents")+ylab("")+
  expand_limits(y=1.05, by=0.25)+
  theme(legend.position= "none")

# q68_a
a <- as.data.frame.table(table(dataset$bully_inv, dataset$q68_a))
a<-plyr::rename(a, replace= c("Var1"= "bully_inv", "Var2"= "q68_a","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$q68_a))
b<-plyr::rename(b, replace= c("Var1"= "q68_a","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n= ", counts))
q68_a <- ggplot(data=dataset, aes(x=q68_a))+ 
  
  geom_bar(aes(fill=fct_rev(bully_inv),x=q68_a), position="fill")+
  geom_text(data=a, aes(x=q68_a,label=n_count, y=count ),size=3, vjust = 1.2, position="fill")+
  geom_text(data=b, aes(x=q68_a,  label=paste(n_counts,                                     
                                              "(",round(100*counts/733, digit = 1),
                                              "%",")", sep = ""                                     
                                              ), 
                        y=counts), size=3, vjust=-0.3, position="fill")+
  scale_fill_discrete(name = "Bullying Involvement", 
                      labels = c(  "Pure Bullies", "Bully-Victims","Pure Victims","Not Involved"))+
  scale_x_discrete(labels=c("Yes", "No"))+
  expand_limits(y=1.05, by=0.25)+
  xlab("Smoking Status")+ylab("Relative Proportion")+
  theme(legend.position= "none")



# Figure S1
figure_s1 <-  ggarrange(
  q3_a_bully_inv, q5_a, q6_a, q7_a, q8_a, school_PR, q68_a,#bully_inv_more,
  labels = c(
    "A", "B", "C", "D", "E", "F","G"
  ), 
  legend = "right", 
  common.legend = TRUE
)




annotate_figure(
  figure_s1, top = text_grob("\n Sample Characteristics: Demographic/Categorical Variables by Bullying Involvement", 
                             face = "plain", size = 13, hjust=0.7067, vjust = 0.27
  ),
  fig.lab = "Figure S1.1", fig.lab.face = "bold", 
  fig.lab.size = 13, fig.lab.pos= "top.left"
)







