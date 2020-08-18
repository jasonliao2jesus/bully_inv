model_2 <- lrm( firstyrbully2 ~ victim_3group+q3_a + school_PR+q5_a+
                   PHQ9_1yr_new+ q68_a+AUDIT_C_4_a+ #firstrses_standardized+
                   bis1_standardized, 
                 x=TRUE, y=TRUE, data =  dataset_selected)


  
boot.cal.plot <- function(model, B=40,... )  {

set.seed(1)
cal <- calibrate(model, #predy = c(seq(0, 1, by = 0.01)), 
                 method="boot", B=B) # usually B=200 or 300

cal.attr <- attributes(cal)
cal.attr.df <- data.frame(predy=cal.attr$predicted)


cal.df <- as.data.frame( unclass(cal))
cal.df$ideal <-cal.df$predy 

cal.df.gather <- gather(data=cal.df, key="Type", value="Predicted_Probability",  calibrated.orig, 
                        calibrated.corrected)
boot.cal.df <<- list(cal.df.gather, cal.attr.df)
plot.cal.df <- ggplot(data=cal.df.gather)+ 
  geom_line(aes(x=predy, y=Predicted_Probability, col = Type, linetype=Type), size=1.1)+
  # ggMarginal(, type = "density")
  scale_x_continuous(name="Predicted Probability", limits=c(0, 1), breaks = c(seq(0, 1, by = 0.1)))+
  scale_y_continuous(name="Actual Probability", limits=c(0, 1), breaks = c(seq(0, 1, by = 0.1)))+
  annotate("segment", x = 0, xend = 1, y =0, yend = 1, colour = "grey")+
  geom_rug(data=cal.attr.df, mapping = aes(x= predy), stat = "identity",
           position = "identity",alpha=0.2, sides = "t") +
  scale_colour_manual(values=c("red", "green", "grey"))+
  theme_pubr()

}

#
boot.cal <- function(model, B=40,... )  {
  
  set.seed(1)
  cal <- calibrate(model, #predy = c(seq(0, 1, by = 0.01)), 
                   method="boot", B=B) # usually B=200 or 300
  
  cal.attr <- attributes(cal)
  cal.attr.df <- data.frame(predy=cal.attr$predicted)
  
  
  cal.df <- as.data.frame( unclass(cal))
  cal.df$ideal <-cal.df$predy 
  
  cal.df.gather <- gather(data=cal.df, key="Type", value="Predicted_Probability",  calibrated.orig, 
                          calibrated.corrected)
  boot.cal.df <- list(cal.df.gather, cal.attr.df)


}

#ggMarginal(plot.cal.df,data = cal.attr.df, x = "predy",  type = "densigram", margins = "x")


# and try
model_2_rec <- glm(formula = rec_bully_firstyr~ victim_3group+
                     q3_a+ firstrses+ 
                     AUDIT_C_4_a+ bis1_standardized, 
                   family = binomial(), data =  dataset_selected)

plot_model_2_rec <- plot_calibration(data1 = dataset_selected, model = model_2_rec)

set.seed(2)
a_rec <- validate(model_2_rec, B=2000)  
cal_rec <- calibrate(model_2_rec, method="boot", B=2000) # usually B=200 or 300
plot(cal_rec)
