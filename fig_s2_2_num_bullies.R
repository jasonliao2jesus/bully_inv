#q3_a_bully下方會用到
#ggplot(data=dataset, aes(fill=bully_3group, x=q3_a))+ geom_bar(position="dodge")

a <- as.data.frame.table(table(dataset$bully_3group, dataset$q3_a))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "q3_a","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$q3_a))
b<-plyr::rename(b, replace= c("Var1"= "q3_a","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n = ", counts))
q3_a_bully<- ggplot(data=dataset, aes(x=q3_a))+ 
  
  geom_bar(aes(fill=fct_rev(bully_3group),x=q3_a), position="fill")+
  geom_text_repel(data=a, aes(x=q3_a,label=n_count, y=count ),size=3,vjust = 1.2, position="fill")+
  geom_text(data=b, aes(x=q3_a, label=n_counts, y=counts), size=3,vjust=-0.3, position = "fill",  label.padding = unit(0.1, "lines"))+
  scale_fill_discrete(name = "Bullying Perpetration", 
                      labels = c( "Frequent", "One or Two Times","Never"))+
  scale_x_discrete(labels=c("Female", "Male"))+
  xlab("Sex")+ylab("Ratio")


# numeric variabls vs bullying perpetration
selected_variables <- c("bully_3group",
                        "q68_a",
                        "AUDIT_C_a", "q31_0_a","q22_0_a", 
                        "firstrses",
                        "bis1_total"
)
dataset_selected <- dataset[,selected_variables]

a<- ggpairs(dataset_selected, ggplot2::aes(colour=fct_rev( bully_3group)),
            columns = c(  "AUDIT_C_a", "q31_0_a", "q22_0_a", "firstrses", 
                          "bis1_total","bully_3group"),
            columnLabels = c( "AUDIT-C", "PHQ-9", "MDSS", "RSES", "BIS", 
                              "Bullies"),lower =list(continuous=wrap( "smooth_loess", size=0.6, alpha=0.5), 
                                                     combo=wrap("dot_no_facet", size=0.6, alpha=0.5)),
            diag = list(continuous = wrap("densityDiag",alpha = 0.5), discrete="barDiag"), 
            upper =list( combo="box"), axisLabels = "show",
            legend = grab_legend(q3_a_bully), showStrips = FALSE,
)   #q3_a_bully是從類別變數圖表bullies by categorical variables.R借來的

b1 <- a[6,6]
b1 <- b1+ scale_x_discrete(labels=c("No bullying", "One or Two Times","Frequent" )) +
     theme(axis.text.x = element_text(
                                       angle = 90 ,hjust = 1, vjust=0.4
                                       )
           )

a[6,6] <- b1

b2 <- a[6,1]
b2 <- b2+ 
  scale_x_discrete(limits = rev(levels(dataset$bully_3group)),
                   labels=c( "Frequent", "One or Two Times","No bullying"))+  
  # scale_x_discrete(limits = rev(levels(dataset$bully_inv)),
  scale_fill_discrete(name = "Frequency of Bullying Perpetration", 
                      labels = c(  "Frequent", "One or Two Times","No bullying"))
a[6,1] <- b2



a_bis <-  ggplot(data=dataset, aes(x= bully_3group, y=bis1_total, fill= fct_rev( bully_3group)))+
  geom_boxplot()
a[5,6]<- a_bis #+
#stat_compare_means( #comparisons = my_comparison,
# ref.group = "no_bullying",
#                     symnum.args =list(cutpoints =
#                                        c(0, 0.0001, 0.001, 0.01, 0.05, 0.10, 1), 
#                                     symbols = c("****","***", "**", "*", "???", "ns")
#                  ), 
#                label.y = c(83.6),
#               method="anova",
#               size=2.3
#)


fig_s2_2 <- a +labs(  tag = "Figure S2.2", 
                         title= "The Correlation Matrix of Bully perpetration and the Numeric Variables",  
                         fill="Frequency of Bully Perpetration")


ggsave(plot = figure_s2_2, filename ="figure_s2_2.png", 
       #width = 15, height = 15, units = "cm",
       device = "png",dpi = 320)



