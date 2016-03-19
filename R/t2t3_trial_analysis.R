
setwd("~/Dropbox/Egernia striolata social learning/")
library(MCMCglmm)

t2_probcormod.1<-readRDS("output/t2_probcormod.1")

plot(t2_probcormod.1)
summary(t2_probcormod.1)
posterior.mode(t2_probcormod.1$Sol)
HPDinterval(t2_probcormod.1$Sol)

autocorr.diag(t2_probcormod.1$Sol)
heidel.diag(t2_probcormod.1$Sol)  
geweke.diag(t2_probcormod.1$Sol)

autocorr.diag(t2_probcormod.1$VCV)
heidel.diag(t2_probcormod.1$VCV)  
geweke.diag(t2_probcormod.1$VCV)

#Making predictions with t2_probcormod.1

  Social = 1
  Control = 0
  Trial = c(rep(1:30))

#Probability of making correct choice for social group

#    posterior.mode(t2_probcormod.1$Sol)
#(Intercept)       Treatment1            Trial Treatment1:Trial 
#0.89692572       1.93204399       0.05661718      -0.13391014 

t2_log.odds.pc.social <- 0.89692572  + 1.93204399*Social + 0.05661718*Trial - 0.13391014*Social*Trial

t2_odds_social <- exp(t2_log.odds.pc.social)

t2_prob_social <- t2_odds_social/(1+t2_odds_social)

#Probability of making correct choice for control group

t2_log.odds.pc.con <- 0.89692572 +  1.93204399*Control +  0.05661718*Trial - 0.13391014*Control*Trial

t2_odds_con <- exp(t2_log.odds.pc.con)

t2_prob_con <- t2_odds_con/(1+t2_odds_con)

#Plotting prob correction choice for social group
par(mfrow=c(1,2), mar= c(5, 5, 4, 2) + 0.1)

plot(t2_prob_social~Trial, ylim=c(0,1),pch=19, bg= "Black", cex=1.5, ylab="Probability of making a correct choice", cex.lab = 1.2, cex.axis=1.2)
lines(spline(t2_prob_social~Trial), lwd=2)

#Plotting prob correction choice for control group

points(t2_prob_con~Trial, ylim=c(0,1), cex=1.5)
lines(spline(t2_prob_con~Trial), lty=2, lwd=2)

legend(x=2, y=0.2, legend=c("Social", "Control"), pch=c(19, 1), col= c("black"), cex= 1.2, text.font=1)        


############################

t2_correctonly.1<-readRDS("output/t2_correctonly.1")

plot(t2_correctonly.1)
summary(t2_correctonly.1)
posterior.mode(t2_correctonly.1$Sol)
HPDinterval(t2_correctonly.1$Sol)

autocorr.diag(t2_correctonly.1$Sol)
heidel.diag(t2_correctonly.1$Sol)  
geweke.diag(t2_correctonly.1$Sol)

autocorr.diag(t2_correctonly.1$VCV)
heidel.diag(t2_correctonly.1$VCV)  
geweke.diag(t2_correctonly.1$VCV)


#Iterations = 15001:1999001
#Thinning interval  = 1000
#Sample size  = 1985 

#DIC: 475.6108 

#G-structure:  ~us(1 + Trial):LizardID

#post.mean   l-95% CI u-95% CI eff.samp
#(Intercept):(Intercept).LizardID  1.973715  0.0007740  5.92110   1757.0
#Trial:(Intercept).LizardID        0.027528 -0.1159105  0.15955   1457.7
#(Intercept):Trial.LizardID        0.027528 -0.1159105  0.15955   1457.7
#Trial:Trial.LizardID              0.009407  0.0002704  0.02572    923.9

#R-structure:  ~units

#         post.mean l-95% CI u-95% CI eff.samp
#units         1        1        1        0

#Location effects: Choose.only.correct.dish ~ Treatment * Trial 

#               post.mean l-95% CI u-95% CI eff.samp  pMCMC    
#(Intercept)       -3.08296 -4.52605 -1.66422     1833 <5e-04 ***
#  Treatment1         1.56723 -0.05831  3.16621     1985 0.0423 *  
#  Trial             -0.06125 -0.17442  0.04702     1119 0.2418    
#Treatment1:Trial  -0.02095 -0.14645  0.09163     1777 0.6972    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#posterior.mode(t2_correctonly.1$Sol)
#(Intercept)       Treatment1            Trial Treatment1:Trial 
#-3.14999361       1.12071828      -0.02790077      -0.02594583 


#Task 2 Probability making correct choice only for social lizards

t2_log.odds.coronly.social <- -3.14999361 + 1.12071828*Social - 0.02790077*Trial - 0.02594583*Social*Trial

t2_odds_cor_social <- exp(t2_log.odds.coronly.social)

t2_prob_cor_social <- t2_odds_cor_social/(1+t2_odds_cor_social)

#Task 2 Probability making correct choice only for control group

t2_log.odds.coronly.con <- -3.14999361 + 1.12071828*Control - 0.02790077*Trial - 0.02594583*Control*Trial

t2_odds_cor_con <- exp(t2_log.odds.coronly.con)

t2_prob_cor_con <- t2_odds_cor_con/(1+t2_odds_cor_con)

#Plotting prob making correct choice ONLY for social group

plot(t2_prob_cor_social~Trial, ylim=c(0,1),pch=19, bg= "Black", cex=1.5, ylab="Probability of flipping correct lid only", cex.lab = 1.2, cex.axis=1.2)
lines(spline(t2_prob_cor_social~Trial), lwd=2)

#Plottingprob making correct choice ONLY for control group

points(t2_prob_cor_con~Trial, ylim=c(0,1), cex=1.5)
lines(spline(t2_prob_cor_con~Trial), lty=2, lwd=2)

legend(x=15, y=0.99, legend=c("Social", "Control"), pch=c(19, 1), col= c("black"), cex= 1.2, text.font=1)   

#Interaction removed for Task 2: probability of making a correct choice ONLY

t2_correctonly.2<-readRDS("output/t2_correctonly.2")

plot(t2_correctonly.2)
summary(t2_correctonly.2)
posterior.mode(t2_correctonly.2$Sol)
HPDinterval(t2_correctonly.2$Sol)

autocorr.diag(t2_correctonly.2$Sol)
heidel.diag(t2_correctonly.2$Sol)  
geweke.diag(t2_correctonly.2$Sol)

autocorr.diag(t2_correctonly.2$VCV)
heidel.diag(t2_correctonly.2$VCV)  
geweke.diag(t2_correctonly.2$VCV)

# posterior.mode(t2_correctonly.2$Sol)
#(Intercept)  Treatment1       Trial 
#-3.06653751  1.26417421 -0.05846962 

#Task 2 Probability making correct choice only for social lizards INTERACTION REMOVED

t2_log.odds.coronly.social.inrm <- -3.06653751 + 1.26417421*Social - 0.05846962*Trial 

t2_odds_cor_social_inrm <- exp(t2_log.odds.coronly.social.inrm)

t2_cor_social_inrm <- t2_odds_cor_social_inrm/(1+t2_odds_cor_social_inrm)

#Task 2 Probability making correct choice only for control group INTERACTION REMOVED

t2_log.odds.coronly.con.inrm <- -3.06653751 + 1.26417421*Control - 0.05846962*Trial 

t2_odds_cor_con_inrm <- exp(t2_log.odds.coronly.con.inrm)

t2_cor_con_inrm <- t2_odds_cor_con_inrm/(1+t2_odds_cor_con_inrm)

#Plotting prob correct choice ONLY for social group INTERACTION REMOVED

plot(t2_cor_social_inrm~Trial, ylim=c(0,1),pch=19, bg= "Black", cex=1.5, ylab="Probability of flipping correct lid only", cex.lab = 1.2, cex.axis=1.2)
lines(spline(t2_cor_social_inrm~Trial), lwd=2)

#Plotting prob correct choice ONLY for control group INTERACTION REMOVED

points(t2_cor_con_inrm~Trial, ylim=c(0,1), cex=1.5)
lines(spline(t2_cor_con_inrm~Trial), lty=2, lwd=2)

legend(x=15, y=0.99, legend=c("Social", "Control"), pch=c(19, 1), col= c("black"), cex= 1.2, text.font=1)   

######Reversal Task

t3_probcormod.1<-readRDS("output/t3_probcormod.1")

plot(t3_probcormod.1)
summary(t3_probcormod.1)
posterior.mode(t3_probcormod.1$Sol)
HPDinterval(t3_probcormod.1$Sol)

autocorr.diag(t3_probcormod.1$Sol)
heidel.diag(t3_probcormod.1$Sol)  
geweke.diag(t3_probcormod.1$Sol)

autocorr.diag(t3_probcormod.1$VCV)
heidel.diag(t3_probcormod.1$VCV)  
geweke.diag(t3_probcormod.1$VCV)

#Plotting probability of making a correct choice in reversal task
#posterior.mode(t3_probcormod.1$Sol)
#(Intercept)       Treatment1            Trial Treatment1:Trial 
#0.099206999     -0.034557945      0.007794019      0.056655420 

Social = 1
Control = 0
Trial2 = c(rep(1:20))

#Task 3: Probability of making a correct choice for social lizards

t3_log.odds.pc.social <- 0.099206999 - 0.034557945*Social + 0.007794019 *Trial2 + 0.056655420*Social*Trial2

t3_odds_social <- exp(t3_log.odds.pc.social)

t3_prob_social <- t3_odds_social/(1+t3_odds_social)

#Task 3 Probability of making correct choice for control group

t3_log.odds.pc.con <- 0.099206999 - 0.034557945*Control +  0.007794019*Trial2 + 0.056655420*Control*Trial2

t3_odds_con <- exp(t3_log.odds.pc.con)

t3_prob_con <- t3_odds_con/(1+t3_odds_con)

#Plotting prob correction choice for social group
par(mfrow=c(1,2), mar= c(5, 5, 4, 2) + 0.1)

plot(t3_prob_social~Trial2, ylim=c(0,1),pch=19, bg= "Black", cex=1.5, ylab="Probability of making a correct choice", cex.lab = 1.2, cex.axis=1.2)
lines(spline(t3_prob_social~Trial2), lwd=2)

#Plotting prob correction choice for control group

points(t3_prob_con~Trial2, ylim=c(0,1), cex=1.5)
lines(spline(t3_prob_con~Trial2), lty=2, lwd=2)

legend(x=1.5, y=0.2, legend=c("Social", "Control"), pch=c(19, 1), col= c("black"), cex= 1.2, text.font=1)  


#Interaction removed for Task 3: probability of making a correct choice

t3_probcormod.2<-readRDS("output/t3_probcormod.2")

plot(t3_probcormod.2)
summary(t3_probcormod.2)
posterior.mode(t3_probcormod.2$Sol)
HPDinterval(t3_probcormod.2$Sol)

autocorr.diag(t3_probcormod.2$Sol)
heidel.diag(t3_probcormod.2$Sol)  
geweke.diag(t3_probcormod.2$Sol)

autocorr.diag(t3_probcormod.2$VCV)
heidel.diag(t3_probcormod.2$VCV)  
geweke.diag(t3_probcormod.2$VCV)

#Task 3: Probability of making a correct choice for social lizards INTERACTION REMOVED
#posterior.mode(t3_probcormod.2$Sol)
#(Intercept)  Treatment1       Trial 
#-0.09952972  0.60276129  0.03019814 

t3_log.odds.pc.social_inrm <- -0.09952972 + 0.60276129*Social + 0.03019814 *Trial2 

t3_odds_social_inrm <- exp(t3_log.odds.pc.social_inrm)

t3_prob_social_inrm <- t3_odds_social_inrm/(1+t3_odds_social_inrm)

#Task 3 Probability of making correct choice for control group INTERACTION REMOVED

t3_log.odds.pc.con_inrm <- -0.09952972 + 0.60276129*Control + 0.03019814*Trial2 

t3_odds_con_inrm <- exp(t3_log.odds.pc.con_inrm)

t3_prob_con_inrm <- t3_odds_con_inrm/(1+t3_odds_con_inrm)

#Plotting prob correction choice for social group INTERACTION REMOVED
par(mfrow=c(1,2), mar= c(5, 5, 4, 2) + 0.1)

plot(t3_prob_social_inrm~Trial2, ylim=c(0,1),pch=19, bg= "Black", cex=1.5, ylab="Probability of making a correct choice", cex.lab = 1.2, cex.axis=1.2)
lines(spline(t3_prob_social_inrm~Trial2), lwd=2)

#Plotting prob correction choice for control group INTERACTION REMOVED

points(t3_prob_con_inrm~Trial2, ylim=c(0,1), cex=1.5)
lines(spline(t3_prob_con_inrm~Trial2), lty=2, lwd=2)

legend(x=1.5, y=0.2, legend=c("Social", "Control"), pch=c(19, 1), col= c("black"), cex= 1.2, text.font=1)  


#Task 3:Probability of flipping correct dish ONLY 

t3_correctonly.1<-readRDS("output/t3_correctonly.1")

plot(t3_correctonly.1)
summary(t3_correctonly.1)
posterior.mode(t3_correctonly.1$Sol)
HPDinterval(t3_correctonly.1$Sol)

autocorr.diag(t3_correctonly.1$Sol)
heidel.diag(t3_correctonly.1$Sol)  
geweke.diag(t3_correctonly.1$Sol)

autocorr.diag(t3_correctonly.1$VCV)
heidel.diag(t3_correctonly.1$VCV)  
geweke.diag(t3_correctonly.1$VCV)

#posterior.mode(t3_correctonly.1$Sol)
#(Intercept)       Treatment1            Trial Treatment1:Trial 
#-3.76747098       1.44407412      -0.05871379       0.01798566 

#Task 3: Probability of flipping the correct dish only for SOCIAL lizards

t3_log.odds.pc_cor.social <- -3.76747098  + 1.44407412*Social - 0.05871379*Trial2 + 0.01798566 *Social*Trial2

t3_odds_social_pc_cor <- exp(t3_log.odds.pc_cor.social)

t3_prob_cor_only_social <- t3_odds_social_pc_cor /(1+t3_odds_social_pc_cor)

#Probability of flipping the correct dish only for CONTROL lizards

t3_log.odds.pc_cor.con <- -3.76747098 + 1.44407412*Control - 0.05871379*Trial2 + 0.01798566*Control*Trial2

t3_odds_con_pc_cor <- exp(t3_log.odds.pc_cor.con)

t3_prob_con_pc_cor <- t3_odds_con_pc_cor/(1+t3_odds_con_pc_cor)

#Plotting prob flipping the correct dish only for social group

plot(t3_prob_cor_only_social~Trial2, ylim=c(0,1),pch=19, bg= "Black", cex=1.5, ylab="Probability of flipping correct lid only", cex.lab = 1.2, cex.axis=1.2)
lines(spline(t3_prob_cor_only_social~Trial2), lwd=2)

#Plotting prob flipping the correct dish only  for control group

points(t3_prob_con_pc_cor~Trial2, ylim=c(0,1), cex=1.5)
lines(spline(t3_prob_con_pc_cor~Trial2), lty=2, lwd=2)

legend(x=13, y=0.99, legend=c("Social", "Control"), pch=c(19, 1), col= c("black"), cex= 1.2, text.font=1)

#INTERACTION REMOVED for Task 3: probability of making a correct choice ONLY

t3_correctonly.2<-readRDS("output/t3_correctonly.2")

plot(t3_correctonly.2)
summary(t3_correctonly.2)
posterior.mode(t3_correctonly.2$Sol)
HPDinterval(t3_correctonly.2$Sol)

autocorr.diag(t3_correctonly.2$Sol)
heidel.diag(t3_correctonly.2$Sol)  
geweke.diag(t3_correctonly.2$Sol)

autocorr.diag(t3_correctonly.2$VCV)
heidel.diag(t3_correctonly.2$VCV)  
geweke.diag(t3_correctonly.2$VCV)

#Task 3: Probability of flipping the correct dish only for SOCIAL lizards INTERACTION REMOVED

#posterior.mode(t3_correctonly.2$Sol)
#(Intercept)  Treatment1       Trial 
#-3.75744292  1.79269573 -0.04974663 

t3_log.odds.cor.social_inrm <- -3.75744292  + 1.79269573*Social - 0.04974663*Trial2 

t3_odds_social_cor_inrm <- exp(t3_log.odds.cor.social_inrm)

t3_prob_cor_only_social_inrm <- t3_odds_social_cor_inrm/(1+t3_odds_social_cor_inrm)

#Probability of flipping the correct dish only for CONTROL lizards INTERACTION REMOVED

t3_log.odds.cor.con_inrm <- -3.75744292  + 1.79269573*Control - 0.04974663*Trial2 

t3_odds_con_cor_inrm <- exp(t3_log.odds.cor.con_inrm)

t3_prob_cor_only_con_inrm <- t3_odds_con_cor_inrm/(1+t3_odds_con_cor_inrm)

#Plotting prob flipping the correct dish only for social group INTERACTION REMOVED

plot(t3_prob_cor_only_social_inrm~Trial2, ylim=c(0,1),pch=19, bg= "Black", cex=1.5, ylab="Probability of flipping correct lid only", cex.lab = 1.2, cex.axis=1.2)
lines(spline(t3_prob_cor_only_social_inrm~Trial2), lwd=2)

#Plotting prob flipping the correct dish only  for control group INTERACTION REMOVED

points(t3_prob_cor_only_con_inrm~Trial2, ylim=c(0,1), cex=1.5)
lines(spline(t3_prob_cor_only_con_inrm~Trial2), lty=2, lwd=2)

legend(x=10, y=0.99, legend=c("Social", "Control"), pch=c(19, 1), col= c("black"), cex= 1.2, text.font=1)

