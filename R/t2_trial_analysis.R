
setwd("~/Dropbox/Egernia striolata social learning/")

assocdat <- read.csv("output/data/task2_final.csv", stringsAsFactors = FALSE)
head(assocdat)
str(assocdat)
library(MCMCglmm)

t2_probcormod.1<-readRDS("output/t2_probcormod.1")

plot(t2_probcormod.1)

summary(t2_probcormod.1$Sol)

####Table 3 for ms####

Table3 <- data.frame(matrix(nrow = 5, ncol = 6))
colnames(Table3)[1:3] <- "Probcor"
colnames(Table3)[4:6] <- "Cor only"

rownames(Table3) <- c("Intercept", "Treatment (SOC)", "Trial", "Batch (2)", "Treatment:Trial")

posterior.mode(t2_probcormod.1$Sol)
probcor_lower_HPD <- HPDinterval(t2_probcormod.1$Sol)[,1]
probcor_upper_HPD <- HPDinterval(t2_probcormod.1$Sol)[,2]

#Est

Table3[1,1] <- round(posterior.mode(t2_probcormod.1$Sol)[1],2)
Table3[2,1] <- round(posterior.mode(t2_probcormod.1$Sol)[2],2)
Table3[3,1] <- round(posterior.mode(t2_probcormod.1$Sol)[3],2)
Table3[4,1] <- round(posterior.mode(t2_probcormod.1$Sol)[4],2)
Table3[5,1] <- round(posterior.mode(t2_probcormod.1$Sol)[5],2)

#L

Table3[1,2] <- round(probcor_lower_HPD[1],2)
Table3[2,2] <- round(probcor_lower_HPD[2],2)
Table3[3,2] <- round(probcor_lower_HPD[3],2)
Table3[4,2] <- round(probcor_lower_HPD[4],2)
Table3[5,2] <- round(probcor_lower_HPD[5],2)

#U

Table3[1,3] <- round(probcor_upper_HPD[1],2)
Table3[2,3] <- round(probcor_upper_HPD[2],2)
Table3[3,3] <- round(probcor_upper_HPD[3],2)
Table3[4,3] <- round(probcor_upper_HPD[4],2)
Table3[5,3] <- round(probcor_upper_HPD[5],2)

#################################

posterior.mode(t2_probcormod.1$Sol)
sqrt(diag(var(t2_probcormod.1$Sol)))
posterior.mode(t2_probcormod.1$Sol)
HPDinterval(t2_probcormod.1$Sol)

autocorr.diag(t2_probcormod.1$Sol)
heidel.diag(t2_probcormod.1$Sol)  
geweke.diag(t2_probcormod.1$Sol)

autocorr.diag(t2_probcormod.1$VCV)
heidel.diag(t2_probcormod.1$VCV)  
geweke.diag(t2_probcormod.1$VCV)

#Making predictions with t2_probcormod.1 for social lizards 

#Using predict.MCMCglmm


newdata1=data.frame(Treatment = rep((c(1,0)), each=30),
                    Trial = c(1:30),
                    Batch = factor(c(1)))

Correct<-0

test1<-predict.MCMCglmm(t2_probcormod.1, newdata=newdata1, type="response", interval="confidence", verbose = TRUE)

predict.MCMCglmm(t2_probcormod.1, marginal = t2_probcormod.1$Random$formula, type="response", interval="confidence", verbose = TRUE)

str(t2_probcormod.1)
t2_probcormod.1$Random

summary(t2_probcormod.1)


#By hand

newdata1=data.frame(Treatment = 1,
                   Trial = c(rep(1:30)),
                   Batch = 1)



X <-model.matrix(~Treatment+Trial+Batch+Treatment*Trial, data=newdata1)

V <-rowSums(t2_probcormod.1$VCV)

beta <-t2_probcormod.1$Sol

c2 <- (16 * sqrt(3)/(15 * pi))^2

pred1<-t(plogis(t(beta%*%t(X)/sqrt(1+c2*V))))

socialprob<-colMeans(pred1)

#Predict for control lizards

newdata2=data.frame(Treatment = 0,
                   Trial = c(rep(1:30)),
                   Batch = 1)


X <-model.matrix(~Treatment+Trial+Batch+Treatment*Trial, data=newdata2)

V <-rowSums(t2_probcormod.1$VCV)

beta <-t2_probcormod.1$Sol

c2 <- (16 * sqrt(3)/(15 * pi))^2

pred2<-t(plogis(t(beta%*%t(X)/sqrt(1+c2*V))))

controlprob <-colMeans(pred2)

 #Hand calculations

  Social = 1
  Control = 0
  Trial = c(rep(1:30))
  Batch = 1

#Probability of making correct choice for social group

#    posterior.mode(t2_probcormod.1$Sol)
#(Intercept)       Treatment1            Trial           Batch2 
#1.3353846        1.7700864        0.0563850       -0.6348790 
#Treatment1:Trial 
#-0.1278705

t2_log.odds.pc.social <- 1.3353846  + 1.7700864*Social + 0.0563850*Trial - 0.6348790*Batch - 0.1278705*Social*Trial

t2_odds_social <- exp(t2_log.odds.pc.social)

t2_prob_social <- t2_odds_social/(1+t2_odds_social)

#Probability of making correct choice for control group

t2_log.odds.pc.con <- 0.89692572 +  1.93204399*Control +  0.05661718*Trial - 0.13391014*Control*Trial

t2_odds_con <- exp(t2_log.odds.pc.con)

t2_prob_con <- t2_odds_con/(1+t2_odds_con)

#Plotting prob correction choice for social group
par(mfrow=c(1,2), mar= c(5, 5, 4, 2))

plot(t2_prob_social~Trial, ylim=c(0,1),pch=19, bg= "Black", cex=1.5, ylab="Probability of making a correct choice", cex.lab = 1.2, cex.axis=1.2)
lines(spline(t2_prob_social~Trial), lwd=2)

#Plotting prob correction choice for control group

points(t2_prob_con~Trial, ylim=c(0,1), cex=1.5)
lines(spline(t2_prob_con~Trial), lty=2, lwd=2)

legend(x=2, y=0.2, legend=c("Social", "Control"), pch=c(19, 1), col= c("black"), cex= 1.2, text.font=1)        

#Predicting from model

#Predict for Treatment
t2_probcor_dat <- predict(t2_probcormod.1, interval="confidence")
pred_assocdat <- cbind(assocdat[,1:12], t2_probcor_dat)
assocdat_probcor_pred <- ddply(pred_assocdat, .(Treatment, Trial), summarise, meanFit = mean(fit), LWR = (mean(lwr)), UPR=(mean(upr)))
assocdat_probcor_pred

#Graph for Treatment 
#polygon time
x1<-seq(1:30)
x2<-order(-x1)
x3<-c(x1,x2)

assocpred_probcor_dec<-assocdat_probcor_pred[with(assocdat_probcor_pred, order(Treatment, -Trial)),]

y1.probcor.c<-assocdat_probcor_pred[assocdat_probcor_pred$Treatment == "0",4] #LWR control
y2.probcor.c<-assocpred_probcor_dec[assocpred_probcor_dec$Treatment == "0",5] #UPR control
y3.probcor.c <-c(y1.probcor.c,y2.probcor.c)

y1.probcor.s<-assocdat_probcor_pred[assocdat_probcor_pred$Treatment == "1",4] #LWR social
y2.probcor.s<-assocpred_probcor_dec[assocpred_probcor_dec$Treatment == "1",5] #UPR social
y3.probcor.s <-c(y1.probcor.s,y2.probcor.s)

pdf("output/fig/t2_probcor.pdf") 
par(mar=c(5, 5, 4, 2) + 0.1)
plot(Correct~Trial, assocdat, ylim=c(0,1), xlim=c(1,25), ann = FALSE, type='n', cex.axis=1.5)
mtext("Trial", side = 1, line = 3, cex=1.5)
mtext("Predicted probability of correct choice", side = 2, line = 3, cex=1.5)

polygon(x3, y3.probcor.c, col=rgb(0,0,0,0.4), border=NA) #control
polygon(x3, y3.probcor.s, col=rgb(190,190,190,120, max=255), border=NA) #social

lines(meanFit~Trial, data = assocdat_probcor_pred[assocdat_probcor_pred$Treatment=="0",], col="gray27", lwd=3) #control
lines(meanFit~Trial, data = assocdat_probcor_pred[assocdat_probcor_pred$Treatment=="1",], col="gray47", lwd=3, lty=2)   

legend(x=16, y=0.25, legend=c("Social", "Control"),pch=c(15,15), col= c(rgb(0,0,0,0.4), rgb(190,190,190,120, max=255)), pt.cex=3, bty="n", y.intersp=2, x.intersp=2, cex=1.5)   


############################

t2_correctonly.1<-readRDS("output/t2_correctonly.1")

plot(t2_correctonly.1)
summary(t2_correctonly.1)
posterior.mode(t2_correctonly.1$Sol)
HPDinterval(t2_correctonly.1$Sol)

####Table 3 continues - just need interaction for this model

posterior.mode(t2_correctonly.1$Sol)
coronly_1_lower <- HPDinterval(t2_correctonly.1$Sol)[,1]
coronly_1_upper <- HPDinterval(t2_correctonly.1$Sol)[,2]

#Est
Table3[5,4] <- round(posterior.mode(t2_correctonly.1$Sol)[5],2)

#L
Table3[5,5] <- round(coronly_1_lower[5],2)

#U
Table3[5,6] <- round(coronly_1_upper[5],2)

#####

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

####Table 3 continues - just need main effects for this model

posterior.mode(t2_correctonly.2$Sol)
coronly_2_lower <- HPDinterval(t2_correctonly.2$Sol)[,1]
coronly_2_upper <- HPDinterval(t2_correctonly.2$Sol)[,2]

#Est
Table3[1,4] <- round(posterior.mode(t2_correctonly.2$Sol)[1],2)
Table3[2,4] <- round(posterior.mode(t2_correctonly.2$Sol)[2],2)
Table3[3,4] <- round(posterior.mode(t2_correctonly.2$Sol)[3],2)
Table3[4,4] <- round(posterior.mode(t2_correctonly.2$Sol)[4],2)

#L
Table3[1,5] <- round(coronly_2_lower[1],2)
Table3[2,5] <- round(coronly_2_lower[2],2)
Table3[3,5] <- round(coronly_2_lower[3],2)
Table3[4,5] <- round(coronly_2_lower[4],2)

#U
Table3[1,6] <- round(coronly_2_upper[1],2)
Table3[2,6] <- round(coronly_2_upper[2],2)
Table3[3,6] <- round(coronly_2_upper[3],2)
Table3[4,6] <- round(coronly_2_upper[4],2)

write.csv(Table3, file="Table3a.csv")

###########################
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

#Predicting from model without interaction

#Predict for Treatment
t2_coronly.2_dat <- predict(t2_correctonly.2, interval="confidence")
assocdat <- cbind(assocdat, t2_coronly.2_dat)
assocdat_pred_coronly <- ddply(assocdat, .(Treatment, Trial), summarise, meanFit = mean(fit), LWR = (mean(lwr)), UPR=(mean(upr)))
assocdat_pred_coronly

#Graph for Treatment 
#polygon time
x1<-seq(1:30)
x2<-order(-x1)
t2.x3<-c(x1,x2)

assocpred_dec_coronly<-assocdat_pred_coronly[with(assocdat_pred_coronly, order(Treatment, -Trial)),]

y1.coronly.c<-assocdat_pred_coronly[assocdat_pred_coronly$Treatment == "0",4] #LWR control
y2.coronly.c<-assocpred_dec_coronly[assocpred_dec_coronly$Treatment == "0",5] #UPR control
y3.coronly.c <-c(y1.coronly.c,y2.coronly.c)

y1.coronly.s<-assocdat_pred_coronly[assocdat_pred_coronly$Treatment == "1",4] #LWR social 
y2.coronly.s<-assocpred_dec_coronly[assocpred_dec_coronly$Treatment == "1",5] #LWR social
y3.coronly.s <-c(y1.coronly.s,y2.coronly.s)

pdf("output/fig/t2_probcor.pdf") 
par(mar=c(5,5,4,2))
plot(Choose.only.correct.dish~Trial, assocdat, ylim=c(0,0.5), xlim=c(1,30), ann = FALSE, type='n')
mtext("Trial", side = 1, line = 3, cex=1.2)
mtext("Predicted probability of making correct choice only", side = 2, line = 3, cex=1.2)

polygon(t2.x3, y3.coronly.c, col=rgb(0,0,0,0.4), border=NA) #control

polygon(t2.x3, y3.coronly.s, col=rgb(190,190,190,120, max=255), border=NA) #social

lines(meanFit~Trial, data = assocdat_pred_coronly[assocdat_pred_coronly$Treatment=="0",], col="gray27", lwd=3) #control
lines(meanFit~Trial, data = assocdat_pred_coronly[assocdat_pred_coronly$Treatment=="1",], col="gray47", lwd=3, lty=2) 

legend(x=22, y=0.50, legend=c("Social", "Control"),pch=c(15,15), col= c(rgb(0,0,0,0.4), rgb(190,190,190,120, max=255)), pt.cex=3, bty="n", y.intersp=2, x.intersp=2, cex=1.2)   

dev.off()



