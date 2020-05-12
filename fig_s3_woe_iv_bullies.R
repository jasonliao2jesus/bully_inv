# weight of evidence analysis
selected_variables <- c("firstyrbully2", "victim_3group", 
                        "AUDIT_C_4_a", "q31_0_a", "q22_0_a", 
                        "firstrses", 
                        "bis1_total"#,"bis1_lack_of_selfcontrol", 
                        #"bis1_novelty_seeking", "bis1_inability_to_plan"
)


dataset_selected_num <- dataset[,c("q3_a","school_PR", "q5_a",
                                   "q6_a","q7_a","q8_a","q68_a","PHQ9_1yr_new",
                                   "AUDIT_C_4_a", selected_variables
                                   )]

k <- as.data.frame(c("q3_a","school_PR", "q5_a",
             "q6_a","q7_a","q8_a","q68_a","PHQ9_1yr_new",
             "AUDIT_C_4_a", selected_variables
))

dataset_selected_num<- mutate(dataset_selected_num, firstyrbully2_1= ifelse(firstyrbully2=="¦³", 1,0))

woe_bully<-create_infotables(data=dataset_selected_num, y="firstyrbully2_1", bins = 6)
woe_plot1<- plot_infotables(woe_bully, variables = woe_bully$Summary$Variable[1],same_scales = TRUE)+
  geom_text(x=1, y=1 , label=c(paste0( "IV=",round( woe_bully$Summary[,2][1], digits = 2)
  )
  )
  )+
  ggtitle("Frequency of Victimization")

woe_plot2<- plot_infotables(woe_bully, variables = woe_bully$Summary$Variable[2],same_scales = TRUE)+
  geom_text(x=2, y=0.6 , label=c(paste0( "IV=",round( woe_bully$Summary[,2][2], digits = 2))))+
  ggtitle("Impulsivity(BIS)")

woe_plot3<- plot_infotables(woe_bully, variables = woe_bully$Summary$Variable[3],same_scales = TRUE)+
  geom_text(x=2, y=0.25 , label=c(paste0( "IV=",round( woe_bully$Summary[,2][3], digits = 2) )  )  )+
  ggtitle("Depression(PHQ9)")
woe_plot4<- plot_infotables(woe_bully, variables = woe_bully$Summary$Variable[4],same_scales = TRUE)+
  geom_text(x=4, y=0.3 , label=c(paste0( "IV=",round( woe_bully$Summary[,2][4], digits = 2))  )  )+
  ggtitle("Social Support(MDSS)")
woe_plot5<- plot_infotables(woe_bully, variables = woe_bully$Summary$Variable[5],same_scales = TRUE)+
  geom_text(x=1, y=1 , label=c(paste0( "IV=",round( woe_bully$Summary[,2][5], digits = 2)  )  )  )+
  ggtitle("Alcohol Use(AUDIT-C)")

woe_plot6<- plot_infotables(woe_bully, variables = woe_bully$Summary$Variable[6],same_scales = TRUE)+
  geom_text(x=3.5, y=0.2 , label=c(paste0( "IV=",round( woe_bully$Summary[,2][6], digits = 2)  )  )  )+
  ggtitle("Self-esteem(RSES)")

figure_s3 <-  ggarrange(
  woe_plot1, woe_plot2, woe_plot3,woe_plot4,woe_plot5,woe_plot6,
  labels = c(
    "A", "B", "C","D","E","F"
  ), legend = "right", common.legend = TRUE
)
figure_s3

annotate_figure(
  figure_s3, 
  top = text_grob( "\n  The Weight of Evidence(WOE) and Information Value(IV) on Bullying Perpetration", 
                   face = "plain", size = 13, hjust=0.7067, vjust = 0.27),
  bottom = text_grob(paste("Other Variable's IV: Sex: 0.043,School PR: 0.072, Father's Unemployment: 0.058,", 
                           "Mother's Unemployment: 0.019, Needy Family: 0.001, Living with Parents: 0.014,",
                           "Smoking Status: 0.04",sep="\n"),just = "left"),
  
  fig.lab = "Figure S3", fig.lab.face = "bold", 
  fig.lab.size = 13, fig.lab.pos= "top.left"
)