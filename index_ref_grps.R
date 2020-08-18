

choose_grp <- rep(0, nlevels(dataset$bully_3group) * nlevels(dataset$victim_3group))


choose_grp <- list(ref= c(  
  1, 0, 0, 
  0, 0, 0,
  0, 0, 0
),
ind = c(
  0, 0, 0, 
  1, 1, 1,
  1 ,1, 1
)
)
###




#bullies
choose_grp <- list(grp= c(  
            1, 2, 2, 
            0, 2, 2,
            0, 2, 2
            )
     )



choose_grps <- ifelse(choose_grp$grp==1, "r", ifelse(choose_grp$grp==2,"i",NA))





a <- as.data.frame.table(table(dataset$bully_3group, dataset$victim_3group))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "victim_3group","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
a <- cbind(a, choose_grps)

b <-  ggplot(data=a)+ 
  
  geom_tile(aes(x=bully_3group, y=victim_3group, fill=choose_grps), col="white")+
  geom_text(aes(x=bully_3group,
                label=n_count, 
                y= victim_3group ),
            size=3.5,vjust = 1.2)+
  scale_fill_discrete(name="Groups", labels=c("Response Group","Reference Group","Not Selected"))+
  scale_y_discrete(name = "Victimization",labels = c("v0_not_being_bullied" = "Never","v1_1or2_times" = "1-2 Times",
                              "v2_repeated_being_bullied" = "Frequent"))+
  scale_x_discrete(name = "Perpetration", 
                   labels = c( "Frequent", "One or Two Times","Never"))+theme_pubr()




# victim
choose_grp <- list(grp= c(  
  1,0, 0, 
  2, 2, 2,
  2, 2, 2
)
)



choose_grps <- ifelse(choose_grp$grp==1, "r", ifelse(choose_grp$grp==2,"i",NA))





a <- as.data.frame.table(table(dataset$bully_3group, dataset$victim_3group))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "victim_3group","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
a <- cbind(a, choose_grps)

v <- ggplot(data=a)+ 
  
  geom_tile(aes(x=bully_3group, y=victim_3group, fill=choose_grps), col="white")+
  geom_text(aes(x=bully_3group,
                label=n_count, 
                y= victim_3group ),
            size=3.5,vjust = 1.2)+
  scale_y_discrete(name = "Victimization",labels = c("v0_not_being_bullied" = "Never","v1_1or2_times" = "1-2 Times",
                              "v2_repeated_being_bullied" = "Frequent"))+
  scale_x_discrete(name = "Perpetration", 
                   labels = c( "Frequent", "One or Two Times","Never"))+theme_pubr()




# bully-victim
choose_grp <- list(grp= c(  
  1,0, 0, 
  0, 2, 2,
  0, 2, 2
)
)



choose_grps <- ifelse(choose_grp$grp==1, "r", ifelse(choose_grp$grp==2,"i",NA))





a <- as.data.frame.table(table(dataset$bully_3group, dataset$victim_3group))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "victim_3group","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
a <- cbind(a, choose_grps)

bv <- ggplot(data=a)+ 
  
  geom_tile(aes(x=bully_3group, y=victim_3group, fill=choose_grps), col="white")+
  geom_text(aes(x=bully_3group,
                label=n_count, 
                y= victim_3group ),
            size=3.5,vjust = 1.2)+
  scale_y_discrete(name = "Victimization",labels = c("v0_not_being_bullied" = "Never","v1_1or2_times" = "1-2 Times",
                              "v2_repeated_being_bullied" = "Frequent"))+
  scale_x_discrete(name = "Perpetration", 
                   labels = c( "Frequent", "One or Two Times","Never"))+theme_pubr()




# pure-bully
choose_grp <- list(grp= c(  
  1,2, 2, 
  0, 0, 0,
  0, 0, 0
)
)



choose_grps <- ifelse(choose_grp$grp==1, "r", ifelse(choose_grp$grp==2,"i",NA))





a <- as.data.frame.table(table(dataset$bully_3group, dataset$victim_3group))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "victim_3group","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
a <- cbind(a, choose_grps)

pb <- ggplot(data=a)+ 
  
  geom_tile(aes(x=bully_3group, y=victim_3group, fill=choose_grps), col="white")+
  geom_text(aes(x=bully_3group,
                label=n_count, 
                y= victim_3group ),
            size=3.5,vjust = 1.2)+
  scale_y_discrete(name = "Victimization",labels = c("v0_not_being_bullied" = "Never","v1_1or2_times" = "1-2 Times",
                              "v2_repeated_being_bullied" = "Frequent"))+
  scale_x_discrete(name = "Perpetration", 
                   labels = c( "Frequent", "One or Two Times","Never"))+theme_pubr()




# pure-vic
choose_grp <- list(grp= c(  
  1,0, 0, 
  2, 0, 0,
  2, 0, 0
)
)



choose_grps <- ifelse(choose_grp$grp==1, "r", ifelse(choose_grp$grp==2,"i",NA))





 a <- as.data.frame.table(table(dataset$bully_3group, dataset$victim_3group))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "victim_3group","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
a <- cbind(a, choose_grps)

pv <- ggplot(data=a)+ 
  
  geom_tile(aes(x=bully_3group, y=victim_3group, fill=choose_grps), col="white")+
  geom_text(aes(x=bully_3group,
                label=n_count, 
                y= victim_3group ),
            size=3.5,vjust = 1.2)+
  scale_y_discrete(name = "Victimization",labels = c("v0_not_being_bullied" = "Never","v1_1or2_times" = "1-2 Times",
                              "v2_repeated_being_bullied" = "Frequent"))+
  scale_x_discrete(name = "Perpetration", 
                   labels = c( "Frequent", "One or Two Times","Never"))+theme_pubr()




# freq bullies1
choose_grp <- list(grp= c(  
  1,0, 2, 
  0, 0, 2,
  0, 0, 2
)
)



choose_grps <- ifelse(choose_grp$grp==1, "r", ifelse(choose_grp$grp==2,"i",NA))





a <- as.data.frame.table(table(dataset$bully_3group, dataset$victim_3group))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "victim_3group","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
a <- cbind(a, choose_grps)

fb <- ggplot(data=a)+ 
  
  geom_tile(aes(x=bully_3group, y=victim_3group, fill=choose_grps), col="white")+
  geom_text(aes(x=bully_3group,
                label=n_count, 
                y= victim_3group ),
            size=3.5,vjust = 1.2)+
  scale_y_discrete(name = "Victimization",labels = c("v0_not_being_bullied" = "Never","v1_1or2_times" = "1-2 Times",
                              "v2_repeated_being_bullied" = "Frequent"))+
  scale_x_discrete(name = "Perpetration", 
                      labels = c( "Frequent", "One or Two Times","Never"))+theme_pubr()




# freq victim1
choose_grp <- list(grp= c(  
  1,0, 0, 
  0, 0, 0,
  2, 2, 2
)
)



choose_grps <- ifelse(choose_grp$grp==1, "r", ifelse(choose_grp$grp==2,"i",NA))





a <- as.data.frame.table(table(dataset$bully_3group, dataset$victim_3group))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "victim_3group","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
a <- cbind(a, choose_grps)

fv <- ggplot(data=a)+ 
  
  geom_tile(aes(x=bully_3group, y=victim_3group, fill=choose_grps), col="white")+
  geom_text(aes(x=bully_3group,
                label=n_count, 
                y= victim_3group ),
            size=3.5,vjust = 1.2)+
  scale_y_discrete(name = "Victimization", labels = c("v0_not_being_bullied" = "Never","v1_1or2_times" = "1-2 Times",
                              "v2_repeated_being_bullied" = "Frequent"))+
  scale_x_discrete(name = "Perpetration", 
                   labels = c( "Frequent", "One or Two Times","Never"))+theme_pubr()










# freq bullies2
choose_grp <- list(grp= c(  
  1, 1, 2, 
  1, 1, 2,
  1, 1, 2
)
)



choose_grps <- ifelse(choose_grp$grp==1, "r", ifelse(choose_grp$grp==2,"i",NA))





a <- as.data.frame.table(table(dataset$bully_3group, dataset$victim_3group))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "victim_3group","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
a <- cbind(a, choose_grps)

b2 <- ggplot(data=a)+ 
  
  geom_tile(aes(x=bully_3group, y=victim_3group, fill=choose_grps), col="white")+
  geom_text(aes(x=bully_3group,
                label=n_count, 
                y= victim_3group ),
            size=3.5,vjust = 1.2)+
  scale_y_discrete(name = "Victimization", labels = c("v0_not_being_bullied" = "Never","v1_1or2_times" = "1-2 Times",
                                                      "v2_repeated_being_bullied" = "Frequent"))+
  scale_x_discrete(name = "Perpetration", 
                   labels = c( "Frequent", "One or Two Times","Never"))+theme_pubr()




#billies 2

choose_grp <- list(grp= c(  
  1, 2, 2, 
  1, 2, 2,
  1,2, 2
)
)




choose_grps <- ifelse(choose_grp$grp==1, "r", ifelse(choose_grp$grp==2,"i",NA))





a <- as.data.frame.table(table(dataset$bully_3group, dataset$victim_3group))
a<-plyr::rename(a, replace= c("Var1"= "bully_3group", "Var2"= "victim_3group","Freq"="count"))
a <- mutate(a, n_count= paste0("n = ", count))
a <- cbind(a, choose_grps)

b1 <- ggplot(data=a)+ 
  
  geom_tile(aes(x=bully_3group, y=victim_3group, fill=choose_grps), col="white")+
  geom_text(aes(x=bully_3group,
                label=n_count, 
                y= victim_3group ),
            size=3.5,vjust = 1.2)+
  scale_fill_discrete(name="Groups", labels=c("Response Group","Reference Group","Not Selected"))+
  scale_y_discrete(name = "Victimization", labels = c("v0_not_being_bullied" = "Never","v1_1or2_times" = "1-2 Times",
                                                      "v2_repeated_being_bullied" = "Frequent"))+
  scale_x_discrete(name = "Perpetration", 
                   labels = c( "Frequent", "One or Two Times","Never"))+theme_pubr()




fig_s0<-  ggarrange(
  b,v, bv,pb,pv,fb, fv,b1,b2,
  labels = c(
    "e1. Bullies vs. Not-Involved", "e2. Victims vs. Not-Involved", "e3. Bully-Victims vs. Not-Involved",
    "e4. Pure Bullies vs. Not-Involved","e5. Pure Victims vs. Not-Involved","e6. Frequent Bullies vs. Not Involved",
    "e7. Frequent Victims vs. Not-Involved", "e8. Bullis vs. not Bullies", "e9. Frequent Bullies vs. not Frequent Bullies"
  ),label.y =1.04,label.x =-0.1, legend = "top", common.legend = T, nrow = 3, ncol = 3, font.label = list(size=10))







####

var1_levels <- levels(dataset$bully_3group)
var2_levels <- levels(dataset$victim_3group)

var1_times <- rep(var1_levels,nlevels(dataset$bully_3group))
var2_each <- rep(var2_levels, each=nlevels(dataset$victim_3group))

var1_times <- factor(var1_times, levels =levels(dataset$bully_3group))
var2_each <- factor(var2_each, levels=levels(dataset$victim_3group))

#z <- function(ref_grp_var1=c(1,0,0), ref_grp_var2=c(1,0,0),risk_grp=())

data_test <-data.frame(bully =var1_each, victim =var2_times, z= c("r",NA,NA,"c","c","c","c","c","c"))




ggplot(data=data_test)+ geom_tile(aes(x=bully, y=victim, fill=as.factor(z)), alpha= 0.5)



+ scale_fill_discrete()

  scale_x_discrete(breaks=c("a","b","c"))+ scale_y_discrete(breaks=c("a","b","c"))


data_test <- data.frame(x=levels(dataset$bully_3group), y=levels(dataset$victim_3group))


y <- matrix(c(1,0,0,0,0,0,0,0,0), nrow=3, ncol = 3)