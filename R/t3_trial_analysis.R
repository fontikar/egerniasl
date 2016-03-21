#Reversal Task

setwd("~/Dropbox/Egernia striolata social learning/")
library(MCMCglmm)

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

