
source("cat_var_bar.R")

cat_var_bar()
#
model0 <- model1.imp
pred1 <- predicted.mypaper(model1.s)
pred2 <- predicted.mypaper(model2.s)
pred3 <- predicted.mypaper(model3.s)

#


data0 <- cbind(dataset_selected,  predicted_pro_2 = pred1$Predicted, predicted_pro_21=pred3$Predicted, predicted_pro_rec= pred2$Predicted)

k <- cat_var_bar(dep_var = "bully_3group", dep_var_name = "Bullying Perpetration", 
            dep_var_level = c("Never", "1 or 2 times", "Frequent"), ind_var = "victim_3group")
legend_bully_grp <-get_legend(k) 

#
plot.mod2 <- ggplot() + geom_boxplot(data=data0, aes(x= predicted_pro_2, y= bully_3group), alpha=0.1)

plot.mod21 <- ggplot() + geom_boxplot(data=data0, aes(x= predicted_pro_21, y= bully_3group_act),alpha=0.1)

plot.modrec <- ggplot() + geom_boxplot(data=data0, aes(x= predicted_pro_rec, y= bully_3group), alpha=0.1)

#Fig S4
#data0= data1 
facet_var="q3_a" 
predicted= "predicted_pro_21"
bully= "bully_3group_act"

 model_density <-  ggdensity(data0, #%>%filter(firstyrbully2=="有"),
            x=predicted, y= "..count..", #add = "median", 
            rug = TRUE,alpha=0.3, col=bully,
           # facet.by = facet_var,
            panel.labs =list( q3_a= c("Female", "Male")),
            fill=bully,xlab = "Predicted probability of any active bullying",
            ylab = "Relative Proportion",
            
            palette = grab_legend(q3_a_bully), 
            position="fill")

  
  #data0= data1%>%filter(firstyrbully2=="有")
  
  k="relationalb_3group" 

  plot1 <-  ggdensity(data0%>%filter(firstyrbully2=="有"),
                      x=predicted, y="..count..", #add = "median", 
                      rug = TRUE,alpha=0.3, col= k,
                    #  facet.by = facet_var,
                      panel.labs =list( q3_a= c("Female", "Male")),
                      fill=k,xlab = "Predicted Probability of Bullying Perpetration",
                      ylab = "Relative Proportion",
                      #palette = grab_legend(q3_a_bully), 
                      position="fill") 
  
  
  k="verbalb_3group" 
  facet_var="q3_a"
  plot2 <- ggdensity(data0%>%filter(firstyrbully2=="有"),
                     x=predicted, y="..count..", #add = "median", 
                     rug = TRUE,alpha=0.3, col= k,
                  #  facet.by = c(facet_var),
                    panel.labs =list( q3_a= c("Female", "Male")),
                     fill=k,xlab = "Predicted Probability of Bullying Perpetration",
                     ylab = "Relative Proportion",
                     #palette = grab_legend(q3_a_bully), 
                     position="fill") 
  
  
  
  
  k= "physicalb_3group"

  plot3 <- ggdensity(data0%>%filter(firstyrbully2=="有"),
                     x=predicted, y="..count..", #add = "median", 
                     rug = TRUE,alpha=0.3, col= k,
                   # facet.by = c(facet_var),
                    panel.labs =list( q3_a= c("Female", "Male")),
                     fill=k,xlab = "Predicted Probability of Bullying Perpetration",
                     ylab = "Relative Proportion",
                     #palette = grab_legend(q3_a_bully), 
                     position="fill" ) 
  
  k= "otherb_3group"
  facet_var= "q3_a"
  plot4 <- ggdensity(data0%>%filter(firstyrbully2=="有"),
                     x=predicted, y="..count..", #add = "median", 
                     rug = TRUE,alpha=0.3, col= k,
                    # facet.by = facet_var,
                     panel.labs =list( q3_a= c("Female", "Male")),
                     fill=k,xlab = "Predicted Probability of Bullying Perpetration",
                     ylab = "Relative Proportion",
                     #palette = grab_legend(q3_a_bully), 
                     position="fill") 

  
  fig_0 <-  ggarrange(
    model_density,plot3,plot2, plot1,
    labels = c(
      "A", "B","C","D"
    ),label.y = 1.05, legend = "right", common.legend = F, nrow = 4
  )
  

#############################  
  #Fig S4
  data0= data1 
  
  model_density1 <-  ggdensity(data0, #%>%filter(firstyrbully2=="有"),
                              x="predicted_pro_21", y= "..count..", #add = "median", 
                              rug = TRUE,alpha=0.3, col="bully_3group_act",
                              facet.by = c("q3_a"),
                              panel.labs =list( q3_a= c("Female", "Male")),
                              fill="bully_3group_act",xlab = "Predicted Probability of Bullying Perpetration",
                              ylab = "Relative Proportion",
                              
                              #palette = legend_bully_grp, 
                              position="fill")
  
  
 # data0= data1%>%filter(firstyr_bully2_act=="yes")
  
  k="relationalb_3group" 
  facet_var="q3_a" 
  plot1 <-  ggdensity(data0,
                      x="predicted_pro_21", y="..count..", #add = "median", 
                      rug = TRUE,alpha=0.3, col= k,
                       facet.by = facet_var,
                      #   "bully_3group"),
                      fill=k,xlab = "Predicted Probability of Bullying Perpetration",
                      ylab = "Relative Proportion",
                      #palette = grab_legend(q3_a_bully), 
                      position="fill") 
  
  
  k="verbalb_3group" 
  plot2 <- ggdensity(data0,
                     x="predicted_pro_21", y="..count..", #add = "median", 
                     rug = TRUE,alpha=0.3, col= k,
                      facet.by = facet_var,
                     #   "bully_3group"),
                     fill=k,xlab = "Predicted Probability of Bullying Perpetration",
                     ylab = "Relative Proportion",
                     #palette = grab_legend(q3_a_bully), 
                     position="fill") 
  
  
  
  
  k= "physicalb_3group"
  plot3 <- ggdensity(data0,
                     x="predicted_pro_21", y="..count..", #add = "median", 
                     rug = TRUE,alpha=0.3, col= k,
                     facet.by = facet_var,
                    
                     fill=k,xlab = "Predicted Probability of Bullying Perpetration",
                     ylab = "Relative Proportion",
                     #palette = grab_legend(q3_a_bully), 
                     position="fill" ) 
  
  k= "otherb_3group"
  facet_var= "bully_3group"
  plot4 <- ggdensity(data0,
                     x="predicted_pro_21", y="..count..", #add = "median", 
                     rug = TRUE,alpha=0.3, col= k,
                     #facet.by = c(facet_var
                     #),
                     fill=k,xlab = "Predicted Probability of Bullying Perpetration",
                     ylab = "Relative Proportion",
                     #palette = grab_legend(q3_a_bully), 
                     position="fill" ) 
  
  
  fig_2 <-  ggarrange(
    model_density1,plot3,plot2, plot1,
    labels = c(
      "A", "B","C","D"
    ),label.y = 1.05, legend = "right", common.legend = F, nrow = 4
  )      
  
##
  data0= data1 
  
  model_density_rec <-  gghistogram(data0, #%>%filter(firstyrbully2=="有"),
                               x="predicted_pro_rec", y= "..count..", #add = "median", 
                               rug = TRUE,alpha=0.3, col="bully_3group",
                               facet.by = c("q3_a"),
                               panel.labs =list( q3_a= c("Female", "Male")),
                               fill="bully_3group",xlab = "Predicted Probability of Bullying Perpetration",
                               ylab = "Relative Proportion",
                               
                               #palette = legend_bully_grp, 
                               position="fill")
  
  
  fig_models <- ggarrange(
    model_density,model_density1,model_density_rec,
    labels = c(
      "A", "B","C"
    ),label.y = 1.05, legend = "right", common.legend = F, nrow = 3
  ) 
  
  
#####  
  k="physicalb_3group" 
  facet_var= "bully_3group"
  ggdensity(data1%>%filter(firstyrbully2=="有"),
            x="predicted_pro_2",y= "..density..", #add = "median", 
            rug = TRUE,alpha=0.3, col= k,
            facet.by = c("victim_3group"), #facet_var),#panel.labs =list( physicalbully= c("physical bullying-", "physical bullying+")),
            fill=k,xlab = "Predicted Probability of Bullying Perpetration",
            ylab = "Relative Proportion",
            #palette = grab_legend(q3_a_bully), 
            position="fill"
            )
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
  density_plot, 
  labels = c(
    "A", "B","C"
  ), legend = "right", common.legend = TRUE, nrow = 3
)


ggplot(data=dataset)+ geom_bar(aes(x=victim_3group,y=..count.., fill=physicalbully), position="fill")+ facet_wrap(~bully_3group)


   
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

