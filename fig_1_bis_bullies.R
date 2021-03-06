
library(ggrepel)
#source("fig1_bully_inv.R")  # ��A

# 
#bully_inv_ <- ggplot(data=dataset, aes(x=bully_inv))+geom_bar()+ geom_text(aes(x=bully_inv, y=count(bully_inv)))

a <- as.data.frame.table(table(dataset$bully_3group, dataset$bully_inv))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "bully_inv","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$bully_inv))
b<-plyr::rename(b, replace= c("Var1"= "bully_inv","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n= ", counts))
bully_inv_more <- ggplot(data=dataset, aes(x=bully_inv))+ 
  
  geom_bar(aes(fill=fct_rev(bully_3group),x=bully_inv), position="fill")+
  geom_text(data=a, aes(x=bully_inv,label=ifelse(count==0, "",n_count), y=count ),
            size=3, vjust = 1.2, position="fill")+
  geom_text(data=b, aes(x=bully_inv, 
                        label=paste(n_counts,
                                    "( ",round(100*counts/733, digit = 1),"%",")",sep = ""
                        ), 
                        y=counts), 
            size=3, 
            vjust=-0.3, 
            position="fill")+
scale_fill_discrete( guide=FALSE)+#name = "Bullying Perpetration", 
  #                    labels = c( "Frequent","1-2 times",  "Never"))+
  scale_x_discrete( labels=c(  "Not Involved", "Pure Victims","Bully-Victims","Pure Bullies"))+
  expand_limits(y=1.05, by=0.25)+
  #theme(legend.position = c(0.95, ))
  xlab("Bullying Involvement")+ylab("Relative Proportion")+
  labs(title = "A.")+
  theme(text=element_text(size=8),axis.title.y = element_text(size=7), axis.text.x = element_text(size=7))+
  theme_pubr(base_size = 8)


#
#figure 1

selected_variables <- c("bully_inv", "bully_3group","victim_3group","firstyrvic2","rec_victim_firstyr",
                        "q68_a", "q3_a",
                        "AUDIT_C_a", "q31_0_a","q22_0_a", 
                        "firstrses", 
                        "bis1_total"
)
dataset_selected <- dataset[,selected_variables]

dataset_selected <- mutate(dataset_selected, bully_3group_rev= fct_rev(bully_3group))
dataset_selected <- mutate(dataset_selected,bully_inv_rev= fct_rev(bully_inv))



my_comparison <- list( # c("4_pureb", "1_not_inv"),
  c("2_purev", "1_not_inv"),
  c("2_purev", "3_bv"),
  c("3_bv", "4_pureb"),
  c("3_bv", "1_not_inv") ,c("4_pureb", "1_not_inv")
) 

# a_bis_legend <-  ggplot(data=dataset, aes(x= bully_inv, y=bis1_total, fill=fct_rev(bully_3group)))+
#                 geom_boxplot() +
# scale_fill_discrete(name = " Frequency of\n Bullying Perpetration", 

#                    labels = c(  "Frequent", "One or Two Times","Never"),
# )



a_bis <-  ggboxplot(data=dataset_selected, 
                    "bully_inv", "bis1_total", #fill = "bully_inv_rev",# facet.by = "q3_a", #fill="bully_3group_rev",
                    xlab = "Bullying Involvement", ylab = "Barret Impulsiveness Scale   ",
                    outlier.size=0.8, title = "1C.")




fig_k <-  a_bis +
  stat_compare_means(comparisons = my_comparison,
                     tip.length = 0.015, 
                     symnum.args =list(cutpoints =
                                         c(0, 0.001, 0.01, 0.05, 0.10, 1), 
                                       symbols = c("p <0.001", "p<0.01", "p<0.05", "p<0.1", "ns")
                                       ), 
                     label.y = c(81, 82.5, 84,91),
                     size=3
                     )+
  expand_limits(y=c(10,100))+
 # stat_compare_means(method= "anova",label.y = 93, size=3.5,
  #                   symnum.args =list(cutpoints =
   #                                      c(0, 0.001, 0.01, 0.05, 0.10, 1), 
    #                                   symbols = c( "p<0.001", "p<0.01", "p<0.05", "p<0.1", "ns")
     #                                  ) 
      #               )+
  scale_x_discrete( name= "Type of Bullying Involvement",
                    limits = levels(dataset$bully_inv),
                    labels=c( 
                      "Not Involved", "Pure Victims" ,"Bully-Victims","Pure Bullies" )
  )+
  theme(text=element_text(size=8),axis.text.x  = element_text(size=7),axis.title.y = element_text(size=7))+
  
  scale_y_continuous(name = "Impulsivity")







  

# bullying perpetration: independent variable: victims; dependent variables: bully-perpetrator

a <- as.data.frame.table(table(dataset$bully_3group, dataset$victim_3group))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "victim_3group","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
b<- as.data.frame.table(table(dataset$victim_3group))
b<-plyr::rename(b, replace= c("Var1"= "victim_3group","Freq"="counts"))
b <- mutate(b, n_counts= paste0("total n= ", counts))
victim_3group <-  ggplot(data=dataset, aes(x=victim_3group))+ 
  
  geom_bar(aes(fill=fct_rev(bully_3group), x=victim_3group), position="fill")+
  geom_text_repel(data=a, aes(x=victim_3group,
                              label=n_count, 
                              y=count ),
                  size=3,vjust = 1.2, position="fill",
                  direction = "x", force=0.01, segment.alpha = 0)+
  geom_text(data=b, aes(x=victim_3group, label=paste(n_counts,
                                                     "( ",round(100*counts/733, digit = 1),"%",")",sep = ""
                                                     ),
                        y=counts),
            size=3, 
            vjust=-0.3, 
            position="fill")+
  labs(title = "1A.")+
  scale_x_discrete( labels = c("v0_not_being_bullied" = "Never being victimized","v1_1or2_times" = "Once or twice",
                              "v2_repeated_being_bullied" = "Moderate"))+
  scale_fill_discrete(name = "Bullying Perpetration", 
                      labels = c( "Moderate", "Once or twice","Never"))+
  expand_limits(y=1.05, by=0.25)+
  xlab("Bullying Victimization")+ylab("Relative Proportion")+
  theme_pubr(base_size = 8)+
  theme(text = element_text(size=8), axis.title.y = element_text(size=7), axis.text.x = element_text(size=7), 
        legend.position = "right")


  

##
a_bis <-  ggboxplot(data=dataset_selected, 
                    "bully_3group", "bis1_total", fill="bully_3group_rev", facet.by = c("victim_3group"), #fill="bully_3group_rev",
                    xlab = "Bullying Perpetration", ylab = "Barret Impulsiveness Scale",
                    panel.labs = list(victim_3group=c("Victimization: Never","Victimization: Once or twice", "Victimization: Moderate")),
                    outlier.size=0.8, title = "1D."
                    
                    
)



fig_1_b <-  a_bis +
  stat_compare_means(comparisons = my_comparison,
                     tip.length = 0.015,aes(label = paste0(..method..,"p =", ..p.format..)),
                     method = "t.test", 
                     #symnum.args =list(cutpoints =
                     #                   c(0, 0.001, 0.01, 0.05, 0.10, 1), 
                     #                symbols = c("p <0.001", "p<0.01", "p<0.05", "p<0.1", "ns")
                     # ), 
                     label.y = c(86.5, 83.5, 81),
                     size=3
  )+
  stat_compare_means(method= "anova",label.y = 91, size=3.5
                     
  )+
  scale_x_discrete( name= "Bullying Perpetration",
                    limits = levels(dataset$bully_3group),
                    labels=c( 
                      "Never", "Once or twice" ,"Moderate" 
                    ) )+
  theme(text = element_text(size=8),axis.text.x  = element_text(size=7),axis.title.y = element_text(size=7))+
  
  scale_y_continuous(name = " Impulsivity")+
  theme_pubr(base_size = 8)

##

fig_1_b <- fig_1_b+rremove("legend")


victim_3group_revised <- victim_3group+rremove("legend")
lgd <-  as_ggplot( get_legend(victim_3group))
plotlist <- list(#bully_inv_more, 
                 victim_3group_revised, 
                 l[[1]],l[[2]], l[[3]], 
                 l[[5]], l[[7]],l[[4]],
                 fig_k, fig_1_b, lgd)
layout.m <- rbind(
  c(1,1,1,10),
  c(1,1,1,NA),
  c(2,2,3,3),
  c(4,4,5,5),
  c(6,6,7,7),
  c(8,8,8,8),
  c(8,8,8,8),
  c(9,9,9,9),
  c(9,9,9,9)
)

fig_1 <- gridExtra::grid.arrange(grobs=plotlist, layout_matrix= layout.m)

##########################

fig_2<-  ggarrange(
  #bully_inv_more, 
  victim_3group,fig_k, fig_1_b,
  labels = c(
    "A", "B","C","D"
  ),label.y = 1.05, legend = "right", common.legend = T, nrow = 4
)

fig_2_revised <- annotate_figure(
  fig_1, top = text_grob("                                           ", 
                         face = "plain", size = 12, hjust=0.45, vjust = 0.27
  ),
  fig.lab = "Figure 2", fig.lab.face = "bold", 
  fig.lab.size = 13, fig.lab.pos= "top.left"
)
 
