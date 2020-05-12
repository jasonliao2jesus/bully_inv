

PPV_PNV <- function(sens=0.4241, spec=0.8936, pre=0.2698,n=733){
#            diseased           not diseased
# positive   a=sens*pre*n         b= (1-spec)*(1-pre)*n
#  
# negative   c=(1-sens)*pre*n     d= spec*(1-pre)*n
  
# total    a+c= pre*n           b+d= (1-pre)*n
  
   b= (1-spec)*( 1-pre)   
  a= sens*pre      
  d= spec*( 1-pre)
  c= (1-sens)*pre
  PPV=a/(a+b)
  NPV=d/(c+d)
  PPV_PNV <- list(PPV=PPV, NPV=NPV,true_pos_cases=a*n, false_pos_cases=b*n, false_neg_cases=c*n)
}

k <- PPV_PNV()


seq_sen <-  seq(0, 1, length.out = 11)

prevalence <- seq(0.1,0.99,length.out = 1000)
#do.call(PPV,list(sens = seq_sen, spec = 0.9, pre=0.27))


key_in <- function(x){
  keyin=readline("1. PPV, 2.NPV, 3. true positive cases, 4. false positive cases, 5. false negative cases")
  P <- ifelse(keyin==1,"PPV",ifelse(keyin==2,"NPV",
                                    ifelse(keyin==3, "true_pos_cases",
                                           ifelse(keyin==4, "false_pos_cases",
                                                  ifelse(keyin==5, "false_neg_cases",keyin
                                                  )
                                           )
                                    )
  )
  )
    return(P)
  }
P <- key_in()



#p=eval(parse(P))

y1=PPV_PNV(sens=0.1, spec=0.8936, pre=prevalence)
y1=y1[[P]]

y2=PPV_PNV(sens=0.2, spec=0.8936, pre=prevalence)
y2=y2[[P]]

y3=PPV_PNV(sens=0.4241, spec=0.8936, pre=prevalence)
y3=y3[[P]]

y4=PPV_PNV(sens=0.6, spec=0.8936, pre=prevalence)
y4=y4[[P]]

y5=PPV_PNV(sens=0.8, spec=0.8936, pre=prevalence)
y5=y5[[P]]

y6=PPV_PNV(sens=0.9, spec=0.8936, pre=prevalence)
y6=y6[[P]]

y7=PPV_PNV(sens=1, spec=0.8936, pre=prevalence)
y7=y7[[P]]

df1 <- data.frame(pre, y1, y2,y3,y4,y5,y6,y7)
df2 <- gather(df1, key="sensi", value=P,2:8)
ggplot(data=df2)+ geom_point(aes(x=pre, y=P, col=sensi))
