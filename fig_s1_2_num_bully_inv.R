#q3_a_bully_inv會用到
#ggplot(data=dataset, aes(fill=bully_inv, x=q3_a))+ geom_bar(position="dodge")

a <- as.data.frame.table(table(dataset$bully_inv, dataset$q3_a))
a<-plyr::rename(a, replace= c("Var1"= "bully_inv", "Var2"= "q3_a","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$q3_a))
b<-plyr::rename(b, replace= c("Var1"= "q3_a","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n = ", counts))
q3_a_bully_inv<- ggplot(data=dataset, aes(x=q3_a))+ 
  
  geom_bar(aes(fill=fct_rev(bully_inv),x=q3_a), position="fill")+
  geom_text(data=a, aes(x=q3_a,label=n_count, y=count ),size=3,vjust = 1.2, position="fill")+
  geom_text(data=b, aes(x=q3_a, label=n_counts, y=counts), size=3,vjust=-0.3, position = "fill")+
  scale_fill_discrete(name = "Bullying Involvement", 
                      labels = c(  "Pure Bullies", "Bully-Victims","Pure Victims","Not Involved"))+
  scale_x_discrete(labels=c("Female", "Male"))+
  xlab("Sex")+ylab("Ratio")
# numeric variables vs bullying involvement
selected_variables <- c("bully_inv",
                        "q68_a",
                        "AUDIT_C_a", "q31_0_a","q22_0_a", 
                        "firstrses", 
                        "bis1_total"
)
dataset_selected <- dataset[,selected_variables]

a<- ggpairs(dataset_selected, ggplot2::aes(colour=fct_rev(bully_inv)), title = "G.",
            columns = c(  "AUDIT_C_a", "q31_0_a", "q22_0_a", "firstrses", 
                          "bis1_total","bully_inv"),
            columnLabels = c( "AUDIT-C", "PHQ-9", "MDSS", "RSES", "BIS", 
                             "Bullying Involvement"),lower =list(continuous=wrap("smooth_loess", size=0.6, alpha=0.5), 
                                                                 combo=wrap("dot_no_facet", size=0.6, alpha=0.5)),
            diag = list(continuous = wrap("densityDiag",alpha = 0.6), discrete="barDiag"), 
            upper =list( combo="box"), axisLabels = "show",

                        legend = grab_legend(q3_a_bully_inv), showStrips = FALSE) #q3_a_bully_inv是從類別變數圖表Figure.R借來的


b1 <- a[6,6]
b1 <- b1+ scale_x_discrete(labels=c("Not Involved" ,"Pure Victims","Bully-Victims","Bullies"  ))+
     theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=0.9))
a[6,6] <- b1
b2 <- a[6,1]
b2 <- b2+ scale_x_discrete(limits = rev(levels(dataset$bully_inv)),
                           labels=c( "Bullies" ,"Bully-Victims","Pure Victims","Not Involved" )  )
a[6,1] <- b2


#a[5,6] <- bis by bully_inv;

#my_comparison <- list(#c("4_pureb", "1_not_inv"),
                      #c("3_bv", "1_not_inv"), 
 #                     c("2_purev", "1_not_inv"),  
  #                    c("2_purev", "3_bv"),c("3_bv", "4_pureb")
   #                  )
a_bis <-  ggplot(data=dataset, aes(x= bully_inv, y=bis1_total, fill=fct_rev(bully_inv)))+
  geom_boxplot()
a[5,6]<- a_bis #+
         #stat_compare_means(comparisons = my_comparison,
          #                 
           #                symnum.args =list(cutpoints =
            #                                    c(0, 0.001, 0.01, 0.05, 0.10, 1), 
             #                                 symbols = c("****", "***", "**", "???", "ns")
              #                                ), label.y = c(80.6, 83.1, 85.6),
              #              size=2.3
               #             )


fig_s1_2<- a +labs( tag = "Figure S2.1", 
        title= "The Correlation Matrix of Bullying Involvement, Smoking and the Numeric Variables", 
        fill="Types of Bullying Involvement")

 


 
 
 

