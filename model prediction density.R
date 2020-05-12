
source("cat_var_bar.R")

k <- cat_var_bar(dep_var = "bully_3group", dep_var_name = "Bullying Perpetration", 
            dep_var_level = c("Never", "1 or 2 times", "Frequent"), ind_var = "victim_3group")
legend_bully_grp <-get_palette(k) 

#Fig S4
  model_density <-  ggdensity(data1,
            x="predicted_pro_2", y= "..count..", #add = "median", 
            rug = TRUE,alpha=0.3, col="bully_3group",
            facet.by = c("q3_a"),panel.labs =list( q3_a= c("Female", "Male")),
            fill="bully_3group",xlab = "Predicted Probability of Bullying Perpetration",
            ylab = "Relative Proportion",
            
            #palette = legend_bully_grp, 
            position="fill")
  
  
  k="bully_3group"
  facet_var="physicalbully" 
  ggdensity(data1,
            x="predicted_pro_2", #add = "median", 
            rug = TRUE,alpha=0.3, col= k,
            facet.by = c(facet_var),panel.labs =list( physicalbully= c("physical bullying-", "physical bullying+")),
            fill=k,xlab = "Predicted Probability of Bullying Perpetration",
            ylab = "Relative Proportion",
            #palette = grab_legend(q3_a_bully), 
            position="fill")
###############################################################  
  
  k="bully_3group"
  facet_var="relationalbully" 
  ggdensity(data1,
            x="predicted_pro_2", #add = "median", 
            rug = TRUE,alpha=0.3, col= k,
            facet.by = c(facet_var),
            fill=k,xlab = "Predicted Probability of Bullying Perpetration",
            ylab = "Relative Proportion",
            #palette = grab_legend(q3_a_bully), 
            position="fill")   
  
  k="bully_3group"
  facet_var="verbalbully" 
  ggdensity(data1,
            x="predicted_pro_2", #add = "median", 
            rug = TRUE,alpha=0.3, col= k,
            facet.by = c(facet_var),
            fill=k,xlab = "Predicted Probability of Bullying Perpetration",
            ylab = "Relative Proportion",
            #palette = grab_legend(q3_a_bully), 
            position="fill")  
  
  k="bully_3group"
  facet_var="verbalbully" 
  ggdensity(data1,
            x="predicted_pro_2", #add = "median", 
            rug = TRUE,alpha=0.3, col= k,
            facet.by = c(facet_var, "relationalbully"),
            fill=k,xlab = "Predicted Probability of Bullying Perpetration",
            ylab = "Relative Proportion",
            #palette = grab_legend(q3_a_bully), 
            position="fill")  
  
  ##
  
  
  
  k="relationalbully" 
  facet_var="q3_a"
  plot1 <-  ggdensity(data1,
            x="predicted_pro_2",, y="..count..", #add = "median", 
            rug = TRUE,alpha=0.3, col= k,
            facet.by = c(#facet_var,
              "bully_3group"),
            fill=k,xlab = "Predicted Probability of Bullying Perpetration",
            ylab = "Relative Proportion",
            #palette = grab_legend(q3_a_bully), 
            position="fill") 
  
  
  k="verbalbully" 
  facet_var="q3_a"
  plot2 <- ggdensity(data1,
            x="predicted_pro_2",, y="..count..", #add = "median", 
            rug = TRUE,alpha=0.3, col= k,
            facet.by = c(#facet_var,
              "bully_3group"),
            fill=k,xlab = "Predicted Probability of Bullying Perpetration",
            ylab = "Relative Proportion",
            #palette = grab_legend(q3_a_bully), 
            position="fill") 
  
  
  
  
  k= "physicalbully"
 
  plot3 <- ggdensity(data1,
            x="predicted_pro_2",, y="..count..", #add = "median", 
            rug = TRUE,alpha=0.3, col= k,
            facet.by = c(#facet_var,
                         "bully_3group"),
            fill=k,xlab = "Predicted Probability of Bullying Perpetration",
            ylab = "Relative Proportion",
            #palette = grab_legend(q3_a_bully), 
            position="fill") 
  
  
  fig_1 <-  ggarrange(
    plot1,plot2, plot3,
    labels = c(
      "A", "B","C"
    ),label.y = 1.05, legend = "right", common.legend = T, nrow = 3
  )
  
  
  k= "physicalbully"
  
  ggdensity(data1,
            x="predicted_pro_2",, y="..count..", #add = "median", 
            rug = TRUE,alpha=0.3, col= k,
            facet.by = c("bully_3group",
              "victim_3group"),
            fill=k,xlab = "Predicted Probability of Bullying Perpetration",
            ylab = "Relative Proportion",
            #palette = grab_legend(q3_a_bully), 
            position="fill") 
  
  
  # by different form of bullying perpetration
  model_density <-  ggdensity(data1,
                              x="predicted_pro_2", #add = "median", 
                              rug = TRUE,alpha=0.3, col="physicalbully",
                              facet.by = "bully_3group",
                              fill="physicalbully",xlab = "Predicted Probability of Bullying Perpetration",
                              ylab = "Relative Proportion",
                              #palette = grab_legend(q3_a_bully), 
                              position="fill")
  
  model_density <-  ggdensity(data1,
                              x="predicted_pro_2", #add = "median", 
                              rug = TRUE,alpha=0.3, col="verbalbully",
                              facet.by = "bully_3group",
                              fill="verbalbully",xlab = "Predicted Probability of Bullying Perpetration",
                              ylab = "Relative Proportion",
                              #palette = grab_legend(q3_a_bully), 
                              position="fill")
  
  model_density <-  ggdensity(data1,
                              x="predicted_pro_2", #add = "median", 
                              rug = TRUE,alpha=0.3, col="relationalbully",
                              facet.by = "bully_3group",
                              fill="relationalbully",xlab = "Predicted Probability of Bullying Perpetration",
                              ylab = "Relative Proportion",
                              #palette = grab_legend(q3_a_bully), 
                              position="fill")
  
k="physicalbully" 
facet_var="bully_3group"
  ggdensity(data1,
            x="predicted_pro_2", #add = "median", 
            rug = TRUE,alpha=0.3, col= k,
            facet.by = facet_var,
            fill=k,xlab = "Predicted Probability of Bullying Perpetration",
            ylab = "Relative Proportion",
            #palette = grab_legend(q3_a_bully), 
            position="fill") 

  

  
  
  
density_plot <- model_density+rremove("legend")

fig_1 <-  ggarrange(
  density_plot, ,
  labels = c(
    "A", "B","C"
  ), legend = "right", common.legend = TRUE, nrow = 3
)


   
#model 3  
ggplot()+
  geom_density(data=data1%>%filter(bully_3group=="1or2_times") ,
               aes(x=predicted_pro_3), alpha=0.1, fill="green3")+
  geom_density(data=data1%>%filter(bully_3group=="no_bullying") ,
               aes(x=predicted_pro_3), alpha=0.1, fill="blue")+
  geom_density(data=data1%>%filter(bully_3group=="repeated_bullying") ,
               aes(x=predicted_pro_3), alpha=0.1,fill="red")
#model 4
ggplot()+
  geom_density(data=data1%>%filter(bully_3group=="1or2_times") ,
               aes(x=predicted_pro_4), alpha=0.1, fill="green3")+
  geom_density(data=data1%>%filter(bully_3group=="no_bullying") ,
               aes(x=predicted_pro_4), alpha=0.1, fill="blue")+
  geom_density(data=data1%>%filter(bully_3group=="repeated_bullying") ,
               aes(x=predicted_pro_4), alpha=0.1,fill="red")  

#model 5
ggplot()+
  geom_density(data=data1%>%filter(bully_3group=="1or2_times") ,
               aes(x=predicted_pro_5), alpha=0.1, fill="green3")+
  geom_density(data=data1%>%filter(bully_3group=="no_bullying") ,
               aes(x=predicted_pro_5), alpha=0.1, fill="blue")+
  geom_density(data=data1%>%filter(bully_3group=="repeated_bullying") ,
               aes(x=predicted_pro_5), alpha=0.1,fill="red") 


ggplot()+
  geom_density(data=data1%>%filter(bully_inv=="3_bv") ,
               aes(x=predicted_pro_5), alpha=0.1, fill="green3")+
  geom_density(data=data1%>%filter(bully_inv=="1_not_inv") ,
               aes(x=predicted_pro_5), alpha=0.1, fill="purple")+
  geom_density(data=data1%>%filter(bully_inv=="2_purev") ,
               aes(x=predicted_pro_5), alpha=0.1,fill="blue") +
  geom_density(data=data1%>%filter(bully_inv=="4_pureb") ,
               aes(x=predicted_pro_5), alpha=0.1,fill="red") 

