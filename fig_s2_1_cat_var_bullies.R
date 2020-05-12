
source("sort_var.R")

#setting independent variables
vars_vector_victim<-c("q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",
                      "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
                      "q22_0_a", "firstrses_standardized","bis1_standardized","victim_3group")
vars_vector<-c("q3_a","school_PR","q8_a","q7_a","q6_a","q5_a",
               "q68_a","PHQ9_1yr_new","AUDIT_C_4_a",
               "q22_0_a", "firstrses_standardized","bis1_standardized")

a <-  sort_var(vars_vector, data_frame = dataset)


# setting arguments for cat_var_bar()
arg_3 <- list(ind_var = "q3_a", dep_var= "bully_3group", ind_var_name = "Gender", ind_var_level = c("Male","Female"), dep_var_name= "Bullying Perpetration", dep_var_level= c( "Frequent", "One or Two Times","Never"), legend.present = T, position = "fill")

arg_5 <- list(ind_var = "q5_a", dep_var= "bully_3group", ind_var_name = "Father's employment", ind_var_level = c("Yes", "No", "Unknown"),  dep_var_name= "Bullying Perpetration", dep_var_level= c( "Frequent", "One or Two Times","Never"),legend.present = T, position = "fill", font.size=2.5)
arg_6 <- list(ind_var = "q6_a", dep_var= "bully_3group", ind_var_name = "Mother's employment", ind_var_level = c("Yes", "No", "Unknown"),  dep_var_name= "Bullying Perpetration", dep_var_level= c( "Frequent", "One or Two Times","Never"),legend.present = T, position = "fill", font.size=2.5)
arg_7 <- list(ind_var = "q7_a", dep_var= "bully_3group", ind_var_name = "Needly family", ind_var_level = c("Yes", "No"),  dep_var_name= "Bullying Perpetration", dep_var_level= c( "Frequent", "One or Two Times","Never"),legend.present = T, position = "fill")
arg_8 <- list(ind_var = "q8_a", dep_var= "bully_3group", ind_var_name = "Living with parents", ind_var_level = c("Yes", "No"), dep_var_name= "Bullying Perpetration", dep_var_level= c( "Frequent", "One or Two Times","Never"), legend.present = T, position = "fill")
arg_school <- list(ind_var = "school_PR", dep_var= "bully_3group",ind_var_name = "School PR", ind_var_level = NULL, dep_var_name= "Bullying Perpetration", dep_var_level= c( "Frequent", "One or Two Times","Never"), legend.present = T, position = "fill")
arg_68 <- list(ind_var = "q68_a",dep_var= "bully_3group", ind_var_name = "Smoking status", ind_var_level = c("Yes", "No"), dep_var_name= "Bullying Perpetration", dep_var_level= c( "Frequent", "One or Two Times","Never"), legend.present = T, position = "fill")
arg_audit <- list(ind_var = "AUDIT_C_4_a",dep_var= "bully_3group", ind_var_name = "AUDIT-C",ind_var_level=NULL  , dep_var_name= "Bullying Perpetration", dep_var_level= c( "Frequent", "One or Two Times","Never"), legend.present = T, position = "fill")

plot3 <- do.call(cat_var_bar, arg_3)
plot5 <-  do.call(cat_var_bar, arg_5)
plot6 <- do.call(cat_var_bar, arg_6)
plot7 <- do.call(cat_var_bar, arg_7)
plot8 <- do.call(cat_var_bar, arg_8)
plotschool <- do.call(cat_var_bar, arg_school)
plot68 <- do.call(cat_var_bar, arg_68)

# Figure S1
figure_s2 <-  ggarrange(
  plot3, plot5, plot6, plot7, plot8, plotschool, plot68,#bully_inv_more,
  labels = c(
    "A", "B", "C", "D", "E", "F","G"
  ), 
  legend = "right", 
  common.legend = TRUE
)




annotate_figure(
  figure_s2, top = text_grob("\n Sample Characteristics: Demographic/Categorical Variables by Frequency of Bullying Perpetration", 
                             face = "plain", size = 13, hjust=0.7067, vjust = 0.27
  ),
  fig.lab = "Figure S2.1", fig.lab.face = "bold", 
  fig.lab.size = 13, fig.lab.pos= "top.left"
)


# bullying perpetration: independent variable: victims; dependent variables: bully-perpetrator

a <- as.data.frame.table(table(dataset$bully_3group, dataset$victim_3group))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "victim_3group","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$victim_3group))
b<-plyr::rename(b, replace= c("Var1"= "victim_3group","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n = ", counts))
victim_3group <-  ggplot(data=dataset, aes(x=victim_3group))+ 
  
  geom_bar(aes(fill=fct_rev(bully_3group), x=victim_3group), position="fill")+
  geom_text_repel(data=a, aes(x=victim_3group,label=n_count, y=count ),size=3,vjust = 1.1, position="fill",
                  direction = "x", force=0.01)+
geom_text(data=b, aes(x=victim_3group, label=n_counts, y=counts),size=3, vjust=-0.3, position="fill",  label.padding = unit(0.1, "lines"))+
  scale_x_discrete(labels = c("v0_not_being_bullied" = "Never","v1_1or2_times" = "1-2 Times",
                                     "v2_repeated_being_bullied" = "Frequent"))+
  scale_fill_discrete(name = "Bullying Perpetration", 
                      labels = c( "Frequent", "One or Two Times","Never"))+
  
  xlab("Bullying Victimization") +expand_limits(y=1.05, by=0.25)+ylab("Relative Proportion")



# bullying perpetRelative Proportionn: gender
#ggplot(data=dataset, aes(fill=bully_3group, x=q3_a))+ geom_bar(position="dodge")

a <- as.data.frame.table(table(dataset$bully_3group, dataset$q3_a))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "q3_a","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$q3_a))
b<-plyr::rename(b, replace= c("Var1"= "q3_a","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n = ", counts))
q3_a_bully<- ggplot(data=dataset, aes(x=q3_a))+ 
  
  geom_bar(aes(fill=fct_rev(bully_3group),x=q3_a), position="fill")+
  geom_text_repel(data=a, aes(x=q3_a,label=n_count, y=count ),direction = "x",force = 0.02, size=3,vjust = 1.2, position="fill")+
geom_text(data=b, aes(x=q3_a, label=n_counts, y=counts), size=3,vjust=-0.3, position = "fill",  label.padding = unit(0.1, "lines"))+
  scale_fill_discrete(name = "Bullying Perpetration", 
                      labels = c( "Frequent", "One or Two Times","Never"))+
  scale_x_discrete(labels=c("Female", "Male"))+
  xlab("Gender") +expand_limits(y=1.05, by=0.25)+ylab("Relative Proportion")

# bullying PerpetRelative Proportionn: school_PR
a <- as.data.frame.table(table(dataset$bully_3group, dataset$school_PR))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "school_PR","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$school_PR))
b<-plyr::rename(b, replace= c("Var1"= "school_PR","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n = ", counts))
school_PR <- ggplot(data=dataset, aes(x=school_PR))+ 
  
  geom_bar(aes(fill=fct_rev(bully_3group),x=school_PR), position="fill")+
  geom_text_repel(data=a, aes(x=school_PR,label=n_count, y=count*(1.01)  ),direction = "x",force = 0.02,size=3,vjust = 1.2, position="fill")+
geom_text(data=b, 
             aes(x=school_PR, label=n_counts, y=counts),
             size=3, vjust=-0.3, position="fill")+
  scale_fill_discrete(name = "Bullying Perpetration", 
                      labels = c(  "Frequent", "One or Two Times","Never"))+
  xlab("School PR") +expand_limits(y=1.05, by=0.25)+ylab("")+
  theme(legend.position= "none")

# q5_a
a <- as.data.frame.table(table(dataset$bully_3group, dataset$q5_a, exclude = NULL))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "q5_a","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$q5_a,exclude = NULL))
b<-plyr::rename(b, replace= c("Var1"= "q5_a","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n = ", counts))
q5_a <- ggplot(data=dataset, aes(x=q5_a))+ 
  
  geom_bar(aes(fill=fct_rev(bully_3group),x=q5_a), position="fill")+
  geom_text(data=a, aes(x=q5_a,label= ifelse(count==0, "",n_count), y=count*(1.04)  ),size=3,vjust = 1.2, position="fill")+
geom_text(data=b, aes(x=q5_a, label=n_counts, y=counts),size=3, vjust=-0.3, position="fill")+
  scale_fill_discrete(name = "Bullying Perpetration", 
                      labels = c(  "Frequent", "One or Two Times","Never"))+
  scale_x_discrete(labels=c("Yes", "No", "Unknown"))+
  xlab("Father's Employment") +expand_limits(y=1.05, by=0.25)+ylab("")+
  theme(legend.position= "none")

#q6_a
a <- as.data.frame.table(table(dataset$bully_3group, dataset$q6_a, exclude = NULL))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "q6_a","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$q6_a))
b<-plyr::rename(b, replace= c("Var1"= "q6_a","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n = ", counts))
q6_a <- ggplot(data=dataset, aes(x=q6_a))+ 
  
  geom_bar(aes(fill=fct_rev(bully_3group),x=q6_a), position="fill")+
  geom_text(data=a, aes(x=q6_a,label=ifelse(count==0, "",n_count), y=count ),size=3,vjust = 1.2, position="fill")+
geom_text(data=b, aes(x=q6_a, label=n_counts, y=counts), 
          size=3,vjust=-0.3, position="fill"
          )+
  scale_fill_discrete(name = "Bullying Perpetration", 
                      labels = c(  "Frequent", "One or Two Times","Never"))+
  scale_x_discrete(labels=c("Yes", "No", "Unknown"))+
  xlab("Mother's Employment") +expand_limits(y=1.05, by=0.25)+ylab("")+ 
  theme(legend.position= "none")

# q7_a
a <- as.data.frame.table(table(dataset$bully_3group, dataset$q7_a))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "q7_a","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$q7_a))
b<-plyr::rename(b, replace= c("Var1"= "q7_a","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n = ", counts))
q7_a <- ggplot(data=dataset, aes(x=q7_a))+ 
  
  geom_bar(aes(fill=fct_rev(bully_3group),x=q7_a), position="fill")+
  geom_text_repel(data=a, aes(x=q7_a,label=n_count, y=count ),
                  direction = "x",force = 0.02, size=3,vjust = 1.2, position="fill")+
geom_text(data=b, aes(x=q7_a, label=n_counts, y=counts), size=3,vjust=-0.3, position="fill")+
  scale_fill_discrete(name = "Bullying Perpetration", 
                      labels = c(  "Frequent", "One or Two Times","Never"))+
  scale_x_discrete(labels=c("Yes", "No"))+
  xlab("Needy Family") +expand_limits(y=1.05, by=0.25)+ylab("Relative Proportion")+
  theme(legend.position= "none")


# q8_a
a <- as.data.frame.table(table(dataset$bully_3group, dataset$q8_a))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "q8_a","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$q8_a))
b<-plyr::rename(b, replace= c("Var1"= "q8_a","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n = ", counts))
q8_a <- ggplot(data=dataset, aes(x=q8_a))+ 
  
  geom_bar(aes(fill=fct_rev(bully_3group),x=q8_a), position="fill")+
  geom_text_repel(data=a, aes(x=q8_a,label=n_count, y=count ),direction = "x",force = 0.02,size=3,vjust = 1.2, position="fill")+
geom_text(data=b, aes(x=q8_a, label=n_counts, y=counts), size=3,vjust=-0.3, position="fill",  label.padding = unit(0.1, "lines"))+
  scale_fill_discrete(name = "Bullying Perpetration", 
                      labels = c(  "Frequent", "One or Two Times","Never"))+
  scale_x_discrete(labels=c("Yes", "No"))+
  xlab("Living with Parents") +expand_limits(y=1.05, by=0.25)+ylab("")+
  theme(legend.position= "none")

# q68_a
a <- as.data.frame.table(table(dataset$bully_3group, dataset$q68_a))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "q68_a","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$q68_a))
b<-plyr::rename(b, replace= c("Var1"= "q68_a","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n = ", counts))
q68_a <- ggplot(data=dataset, aes(x=q68_a))+ 
  
  geom_bar(aes(fill=fct_rev(bully_3group),x=q68_a), position="fill")+
  geom_text_repel(data=a, aes(x=q68_a,label=n_count, y=count  ),direction = "x",force = 0.02,size=3,vjust = 1.1, position="fill")+
  geom_text(data=b, aes(x=q68_a, label=n_counts, y=counts), size=3, vjust=-0.3, position="fill")+
  scale_fill_discrete(name = "Bullying Perpetration", 
                      labels = c(  "Frequent", "One or Two Times","Never"))+
  scale_x_discrete(labels=c("Yes", "No"))+
  xlab("Smoking Status") +expand_limits(y=1.05, by=0.25)+ylab("")+
  theme(legend.position= "none")

#
figure_s2_1 <-  ggarrange(
  q3_a_bully, q5_a, q6_a, q7_a, q8_a, school_PR, q68_a,
  labels = c(
            "A", "B", "C", "D", "E", "F","G"
            ), 
  legend = "right", 
  common.legend = TRUE
)

annotate_figure(
  figure_s2_1, top = text_grob("\n Sample Characteristics: Demographic/Categorical Variables by Frequency of Bullying Perpetration", 
                               face = "plain", size = 13, hjust=0.7067, vjust = 0.27
                              ),
  fig.lab = "Figure S2.1", fig.lab.face = "bold", 
  fig.lab.size = 13, fig.lab.pos= "top.left"
              )  

