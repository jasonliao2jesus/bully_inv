#figure 1


selected_variables <- c("bully_inv", "bully_3group","victim_3group","firstyrvic2","rec_victim_firstyr",
                        "bully_3group_act", "victim_3group_act",
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
                    outlier.size=0.8)




fig_k <-  a_bis +
  stat_compare_means(comparisons = my_comparison,
                     tip.length = 0.015, 
                     symnum.args =list(cutpoints =
                                         c(0, 0.001, 0.01, 0.05, 0.10, 1), 
                                       symbols = c("p <0.001", "p<0.01", "p<0.05", "p<0.1", "ns")
                     ), 
                     label.y = c(81, 82.5, 84,87),
                      size=3
  )+
  stat_compare_means(method= "anova",label.y = 93, size=3.5
                     )+
  scale_x_discrete( name= "Type of Bullying Involvement",
                    limits = levels(dataset$bully_inv),
                    labels=c( 
                      "Not Involved", "Pure Victims" ,"Bully-Victims","Pure Bullies" 
                    )
  )+
  theme(axis.text.x  = element_text(size=9),axis.title.y = element_text(size=10))+
  
  scale_y_continuous(name = "Barratt Impulsiveness Scale   ")






