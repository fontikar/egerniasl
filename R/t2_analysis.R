#Setting working directory
getwd()
#setwd("C:/Users/xufeng/Dropbox/social learning/")
setwd("~/Dropbox/Egernia striolata social learning/")

# install.packages("MCMCglmm")

#load library you need
library(plyr)
library(lme4)
library(MASS)
library(MCMCglmm)
library(survival)

#Read data
assocdat <- read.csv("output/data/task2_final.csv", stringsAsFactors = FALSE)
head(assocdat)
str(assocdat)

#Changing variable types
assocdat$LizardID <- as.factor(assocdat$LizardID)
assocdat$Batch <- as.factor(assocdat$Batch)
assocdat$Treatment <- as.factor(assocdat$Treatment)
assocdat$Date <-as.Date(assocdat$Date, format="%m/%d/%Y")
assocdat$Time <- as.factor(assocdat$Time)

str(assocdat)

# Exploration plots
hist(assocdat$Latency)

assocdat$log.latency<-log(assocdat$Latency)

hist(assocdat$log.latency)

#Descriptive statistics

length((unique(assocdat[assocdat$Treatment == "1",1]))) #n = 15 for social learning treatment lizards
length((unique(assocdat[assocdat$Treatment == "0",1]))) # n = 13 for control lizards


              #Proportion of lizards that learnt per trial
          
pdf("Task2_Proplearnt.pdf", 13, 7)

par(mfrow=c(1,2), mar = c(4, 5, 1.5, 1.5), cex.axis=1.5, mai=c(1,1,0.6,0.2))

              assoc_proplearndat <- ddply(.data=assocdat, .(Trial, Treatment), summarise, learnt = sum(lt==0),sample_size = length(Correct), proportion = round(learnt/sample_size,2))
              assoc_proplearndat <-assoc_proplearndat[with(assoc_proplearndat, order(Treatment)), ]
              
              condata <-assocdat[assocdat$Treatment == "0",]  
              length(unique(condata$LizardID )) #13 lizards in control
              
              con_vec<-as.vector(table(condata$lt, condata$Trial)[1,])
              
              socdata <-assocdat[assocdat$Treatment == "1",]  
              length(unique(socdata$LizardID )) #15 lizards in social
              
              soc_vec <-as.vector(table(socdata$lt, socdata$Trial)[1,])
              
              assoc_proplearndat$numlearnt <- append(con_vec, soc_vec)
              
              assoc_proplearndat$proportion <- assoc_proplearndat$numlearnt/assoc_proplearndat$sample_size
            
              SLprop <-assoc_proplearndat[assoc_proplearndat$Treatment == "1",]
              Cprop <-assoc_proplearndat[assoc_proplearndat$Treatment == "0",]
              
              #Plotting figure proportion learnt over trials
              plot(proportion~Trial, data=assoc_proplearndat, pch=c(1,19), col=("black"), cex=1.5, ylim = c(0,1), xlim=c(1,30), ann=F, cex.axis=1.5, type= "n", xaxt = "n")
              
              axis(1, at=c(1:30))
              
              title(ylab = list("Proportion of sample that learnt", cex=1.5),line=3)
              title(xlab = list("Trial number", cex=1.5),line=3)
              
              points(proportion~Trial, data=SLprop, cex=1.5, col="black", pch=19)
              points(proportion~Trial, data=Cprop, cex=1.5, pch=1)
              
              lines(proportion~Trial, data=SLprop, lwd=2)
              lines(proportion~Trial, data=Cprop, lwd=2, lty=5)
              
              legend(22, 0.10, c("Control", "Social"), lty= c(1, 5), pch=c(1,19), cex=1.2, bty='n')

              mtext("a)", adj = -0.15, padj = -0.2, cex=1.4)

              #dev.off()
              
              
              ####Cox Proportional Hazard analysis
            
              assoc_surv_dat <- ddply(.data=assocdat, .(LizardID, Treatment, Batch), summarise, Time = sum(lt), Event = unique(learnt))
              
              assoc_surv_dat
              
              egsurv.fit1 <- coxph(Surv(Time, Event)~strata(Treatment)*Batch, data=assoc_surv_dat)
              summary(egsurv.fit1)
              
              egsurv.fit2 <- coxph(Surv(Time, Event)~strata(Treatment)+Batch, data=assoc_surv_dat)
              summary(egsurv.fit2)
              summary(survfit(egsurv.fit2))
              
              egsurv.fit2a <- coxph(Surv(Time, Event)~Treatment+Batch, data=assoc_surv_dat)
              summary(egsurv.fit2a)
              
              #Plotting these curves
              pdf("Fig2A-D.pdf", 14, 14)
              
              par(mfrow=c(2,2), mar = c(4, 5, 1.5, 1.5), cex.axis=1.5, mai=c(1,1,0.6,0.2), cex.lab=1.5, las=1)
              
              plot(survfit(egsurv.fit2), lty=c(2,1), lwd=2)
              
              #Control
              con_x_trial <-  c(8, 9, 9, 10, 10, 12, 12, 13, 13, 14, 14, 15, 15, 20, 20, 21, 21, 26, 26)
              #summary(survfit(egsurv.fit2))$time[1:10]
              
              con_y_up <- round(rep(summary(survfit(egsurv.fit2))$upper[1:10], each = 2),2)[1:19]
              
              con_y_low <- round(rep(summary(survfit(egsurv.fit2))$lower[1:10], each = 2),2)[1:19]
              
              lines(con_x_trial, con_y_low)
              lines(con_x_trial[1:18], con_y_up[1:18])
              
              #Social
              soc_x_trial <- c(8,8,10,10,15,15)
              summary(survfit(egsurv.fit2))$time[11:13]
              soc_y_up <- c(1, round(rep(summary(survfit(egsurv.fit2))$upper[11:13], each =2)[1:5],2))
              soc_y_low <- c(1,round(rep(summary(survfit(egsurv.fit2))$lower[11:13], each =2)[1:5],2))
              
              lines(soc_x_trial, soc_y_low, lty= 3)
              lines(soc_x_trial, soc_y_up, lty= 3)
              
             # legend(0 ,0.15,  c("Control", "Social"), lty= c(2, 1), bty='n', cex=1.2, lwd = 2)
              title(ylab = list("Proportion of lizards that have not learnt", cex=1.5), line =3)
              title(xlab = list("Trial Number", cex=1.5))
              mtext("a)", adj = -0.15, padj = -0.2, cex=1.4)
      

 # Mean number of trials taken to learn
              
                assoclearntdat<-assocdat[assocdat$learnt ==1,]
                length(unique(assoclearntdat$LizardID)) #All lizards learnt

              sumdat2 <-ddply(.data=assocdat, .(LizardID, Treatment, Batch), summarise, trials_to_learn=sum((lt)), total_trials=length(lt))
              sumdat2
              
              str(sumdat2)
              sumdat2$Batch <- as.factor(sumdat2$Batch)
              sumdat2$Treatment <- as.factor(sumdat2$Treatment)
              
              #Fit a generalised linear model. Nb = Negative Binomial
              
              fit_1<-glm.nb(trials_to_learn~Treatment+Batch, data=sumdat2)
              summary(fit_1) 
              
              Table1.B <- data.frame(matrix(ncol=2, nrow=3))
              rownames(Table1.B) <- c("Intercept", "Treatment (SOC)", "Batch (2)")
              colnames(Table1.B) <- c("Est", "SE")
              
              summary(fit_1)$coefficients
              
              #Est
              
              Table1.B[1,1] <- round(summary(fit_1)$coefficients[1,1],2)
              Table1.B[2,1] <- round(summary(fit_1)$coefficients[2,1],2)
              Table1.B[3,1] <- round(summary(fit_1)$coefficients[3,1],2)
              #SE
              
              Table1.B[1,2] <- round(summary(fit_1)$coefficients[1,2],2)
              Table1.B[2,2] <- round(summary(fit_1)$coefficients[2,2],2)
              Table1.B[3,2] <- round(summary(fit_1)$coefficients[3,2],2)
              
              write.csv(Table1.B, file="Table1.B.csv")
              
              ##############################
              
              assoc_newdat <-data.frame(Treatment = c(0, 1),
                                  Batch = rep(1,2))
              
              assoc_newdat$Treatment <- as.factor(assoc_newdat$Treatment)
              assoc_newdat$Batch <- as.factor(assoc_newdat$Batch)
              
              assoc_trials_pred <-predict.glm(fit_1, type= "response", se.fit = T, newdata=assoc_newdat)
              
              assoc_newdat$trials_pred <- assoc_trials_pred$fit
              assoc_newdat$trials_pred_SE <- assoc_trials_pred$se.fit
              assoc_newdat$trials_pred_U <-assoc_trials_pred$fit + assoc_trials_pred$se.fit
              assoc_newdat$trials_pred_L <- assoc_trials_pred$fit - assoc_trials_pred$se.fit
              
              #Plotting figure Mean number of trials to learn
              
             # par(mfrow=c(1,2), mar = c(4, 5, 1.5, 1.5), cex.axis=1.5, mai=c(1,1,0.6,0.2))
            
              barplot(assoc_newdat$trials_pred, ylim = c(0,15), xlim =c(0,3.5), space=0.5, col=c("white", "grey"), cex.axis = 1.5) 
              box()
              
              title(ylab = list("Mean number of trials taken to learn", cex=1.5), line = 3)
              mtext("Control", at= 1, side = 1, line = 1.2 ,cex = 1.5)
              mtext("Social", at=2.5, side =1,line =1.2, cex= 1.5)
              
              up.x<-c(1,2.5)
              low.x<-up.x
              
              arrows(x0 = up.x, y0 = assoc_newdat$trials_pred, x1 = up.x, y1 = assoc_newdat$trials_pred_U, length = 0.2, angle = 90, lwd=2)
              arrows(x0 = low.x, y0 = assoc_newdat$trials_pred, x1 =low.x, y1 = assoc_newdat$trials_pred_L, length = 0.2, angle = 90, lwd=2)
              
              segments(x0 =1, y0=13.5, x1 = 2.6, y1 =13.5, lwd= 2)
              text(x=1.8, y=14, labels="P < 0.01", cex=1.5, font=1)
              
              mtext("b)", adj = -0.15, padj = -0.2, cex=1.4)
              
              dev.off()
              
              #Mean latency [This is not included in final analysis because it was not scored for all cases]
              
              stderror<-function(x){
                sd(x)/(sqrt(length(x)))
              }  
              
              latdat <-ddply(.data = assocdat, .(LizardID, Treatment), summarise, mean_latency = mean(Latency, na.rm=T))
              
              
              latdat2 <-ddply(.data = latdat, .(Treatment), summarise, mean_latency2 = mean(mean_latency), SE = stderror(mean_latency), upper = mean_latency2+SE, lower = mean_latency2-SE  )
              
              latdat2$value <- c(1.5,3.5)
              latdat2
            
              
              #Model testing difference in latency between treatment
              
              latfit<-glm(mean_latency~Treatment, data=latdat)
              summary(latfit)
              
              par(xaxt="n", mar=c(3,6,3,3))
              barplot(latdat2$mean_latency2, ylim = c(0, 800), xlim =c(0,3.5), space=0.5, col=c("white", "grey"), cex.axis=1.5) 
              box()
              
              title(ylab = list("Latency to flip lid (s)", cex=1.5), line=3)
              mtext("Control", at= 1, side = 1, line = 1 ,cex = 1.5)
              mtext("Social", at=2.5, side =1,line =1, cex= 1.5)
              
              up.value<-c(1, 2.5)
              down.value<-up.value
              
              arrows(x0 = up.value, y0 = latdat2$mean_latency2, x1 = up.value, y1 = latdat2$upper, length = 0.2, angle = 90, lwd=2)
              arrows(x0 = down.value, y0 = latdat2$mean_latency2,, x1 = down.value, y1 = latdat2$lower, length = 0.2, angle = 90, lwd=2)
              
              segments(x0 =1, y0=650, x1 = 2.6, y1 =650, lwd= 2)
              text(x=1.8, y=700, labels="0.08", cex=1.5, font=1)    

####Number of errors

learningphase<-assocdat[assocdat$lt == 1, ]
learningphase$LizardID<-factor(learningphase$LizardID)
str(learningphase)


incordat<-ddply(.data=learningphase, .(LizardID,Treatment, Batch), summarise, correct_choice=sum((Correct==1)), total=length(Correct), incorrect_choice=total-correct_choice)

incordat$Batch <- as.factor(incordat$Batch)
str(incordat)

incorfit<-glm.nb(incorrect_choice~Treatment+Batch, data=incordat)
summary(incorfit)

Table2 <- data.frame(matrix(ncol=2, nrow=3))
rownames(Table2) <- c("Intercept", "Treatment (SOC)", "Batch (2)")
colnames(Table2) <- c("Est", "SE")

summary(incorfit)$coefficients

#Est

Table2[1,1] <- round(summary(incorfit)$coefficients[1,1],2)
Table2[2,1] <- round(summary(incorfit)$coefficients[2,1],2)
Table2[3,1] <- round(summary(incorfit)$coefficients[3,1],2)

#SE

Table2[1,2] <- round(summary(incorfit)$coefficients[1,2],2)
Table2[2,2] <- round(summary(incorfit)$coefficients[2,2],2)
Table2[3,2] <- round(summary(incorfit)$coefficients[3,2],2)

write.csv(Table2, file="/Users/fontikar/Dropbox/egernia striolata social learning/output/tables/Table2.csv")

###############################
incor_newdat <- data.frame(Treatment = c("0", "1"),
                     Batch = c("1", "1"))

str(incor_newdat)

incordpred<-predict.glm(incorfit, newdata=incor_newdat, se.fit = TRUE, type = "response")

incor_newdat$se_incorr<- incordpred$se.fit
incor_newdat$pred<- incordpred$fit

incor_newdat$upper <- incordpred$fit + incordpred$se.fit
incor_newdat$lower <- incordpred$fit - incordpred$se.fit

#Plotting mean incorrect choices

par(mfrow=c(1,2), mar = c(4, 5, 1.5, 1.5), cex.axis=1.5, mai=c(1,1,0.6,0.2))

pdf("Task2_predictedMeanErrors.pdf", 7, 7)

barplot(incor_newdat$pred, ylim = c(0, 4), xlim =c(0,3.5), space=0.5, col=c("white", "grey"), cex.axis=1.5) 
box()

title(ylab = list("Mean number of errors", cex=1.5), line =3)
mtext("Control", at= 1, side = 1, line = 1 ,cex = 1.5)
mtext("Social", at=2.5, side =1,line =1, cex= 1.5)

up.value<-c(1, 2.5)
down.value<-up.value

arrows(x0 = up.value, y0 = incor_newdat$pred, x1 = up.value, y1 = incor_newdat$upper, length = 0.2, angle = 90, lwd=2)
arrows(x0 = down.value, y0 = incor_newdat$pred,, x1 = down.value, y1 = incor_newdat$lower, length = 0.2, angle = 90, lwd=2)

segments(x0 =1, y0=3.5, x1 = 2.6, y1 =3.5, lwd= 2)
text(x=1.8, y=3.65, labels="P < 0.001", cex=1.5, font=1)
mtext("c)", adj = -0.15, padj = -0.2, cex=1.4)

dev.off()

####Number of trials for each individual 

trialdat<-ddply(.data=assocdat, .(LizardID, Treatment), summarise, Number_of_Trials=max(Trial))

write.csv(trialdat, "Association_Table.csv")
getwd()


####MCMCglmm probability of making a correct choice
assocdat$Correct <- as.factor(assocdat$Correct)
str(assocdat)
names(assocdat)

prior.test<- list(R = list(V =1,fix=1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002)))


probcor.1<-MCMCglmm(Correct ~ Treatment*Trial, random = ~us(1+Trial):LizardID, family = "categorical", nitt = 2000000, thin = 5000, prior=prior.test, burnin = 15000, data=assocdat, verbose= T)

summary(probcor.1)

plot(probcor.1)
autocorr.diag(probcor.1$VCV)
heidel.diag(probcor.1$VCV)  
geweke.diag(probcor.1$VCV)

# Robustness of learning criteria
split2<-split(assocdat, assocdat$LizardID)


Robust<-function(x){
{
  start<-sum(x$lt == 1)+1
  test_1<-x[start:nrow(x),"Correct"]
  aftertrials<-length(test_1) 
  correct_after<-sum(test_1 == 1)
  propcorrect_after<-round((sum(test_1 == 1))/(length(test_1)),2) 
}
vec<-data.frame(start,aftertrials,correct_after,propcorrect_after)
}

Criteria_robust<-lapply(split2, function(x) Robust(x))

split2


#Motivation:
unprocessed2 <-read.csv("data/Task2.csv")

unprocessed2$TubID <- as.factor(unprocessed2$TubID)
unprocessed2$LizardID <- as.factor(unprocessed2$LizardID)
str(unprocessed2)

rawdat2 <- split(unprocessed2, unprocessed2$LizardID)


NaTest <-function(x){
  {
    {
      {
        {
          NoAttempt<-sum(is.na(x$Correct))
        }
        TrialsGiven<-max(x$Trial)
      }
      PropAttempt<-round(((TrialsGiven-NoAttempt)/TrialsGiven),2)
    }
    ReachCriteria<-PropAttempt>=0.85
  }
  checking<-data.frame(NoAttempt,TrialsGiven,PropAttempt,ReachCriteria)
}

motivdat2 <- as.vector(lapply(rawdat2, function(x) NaTest(x)))


###################
#Survival analysis#
###################

assoc_learndat <- ddply(.data=assocdat, .(LizardID, Trial, Treatment), summarise, lt = lt, did.it.learn = learnt)
assoc_learndat

assoc_surv_dat <-  ddply(.data=assoc_learndat, .(LizardID, Treatment), summarise, Time = sum(lt), Event = unique(did.it.learn))

#Data to plot control

fit_assoc_con<-survfit(Surv(Time[Treatment == "0"],Event[Treatment == "0"])~1,data=assoc_surv_dat)

#Plotting learning curve for CONTROL
pdf("Task2_survanalysis.pdf", 7, 7)

par(mfrow=c(1,1), mar = c(4, 5, 1.5, 1.5), cex.axis=1.5, mai=c(1,1,0.6,0.2), cex.lab=1.5)

plot(fit_assoc_con, xlab="Trial Number")
title(ylab = list("Probability of NOT learning", cex=1.5), line =3.5)

#plotting lines for social learning group
fit_assoc_sock<-survfit(Surv(Time[Treatment == "1"],Event[Treatment == "1"])~1,data=assoc_surv_dat)

summary(fit_assoc_sock)

x<-c( seq(1:8), seq(8,10), seq(10,15), seq(15,25)) 
length(x)
y<-c(1,1,1,1,1,1,1,1,0.2,0.2,0.2,0.133,0.133,0.133,0.133,0.133,0.133,0,0,0,0,0,0,0,0,0,0,0) 
length(y)

lines(x,y,col="orange2",lwd=2)

x2<-c( seq(1:8), seq(8,10), 10)
length(x2)
up<-c(1,1,1,1,1,1,1,1,0.550,0.550,0.550,0.484)
length(up)

lines(x2,up,col='orange2',lty=3,lwd=2)

lo<-c(1,1,1,1,1,1,1,1, 0.0727, 0.0727, 0.0727 ,0.0367)
length(lo)

lines(x2,lo,col='orange2',lty=3,lwd=2)

dev.off()

#getting a p-value to see if the learning curve are different: NO
con <- summary(fit_assoc)$surv[1:10]
sock <- summary(fit_assoc)$surv[11:13]

wilcox.test(con,sock)
# W=24.5, p=-.12777

#######################################################

newsurvdat <- data.frame(Treatment = rep(c(1,0), each = 26),
                         Batch = rep(c(2,2), each = 26), 
                         Trial = seq(1,26)) 

newsurvdat.batch1 <- data.frame(Treatment = rep(c(1,0), each = 26),
                         Batch = rep(c(1,1), each = 26), 
                         Trial = seq(1,26)) 

newsurvdat.batch1$Treatment <- as.factor(newsurvdat.batch1$Treatment)
newsurvdat.batch1$Batch <- as.factor(newsurvdat.batch1$Batch)

newsurvdat$Treatment <- as.factor(newsurvdat$Treatment)
newsurvdat$Batch <- as.factor(newsurvdat$Batch)

str(summary(survfit(egsurv.fit2, newdata = newsurvdat)))
plot(survfit(egsurv.fit2, newdata = newsurvdat))

surv.ob <- survfit(egsurv.fit2)
surv.ob$surv
summary(survfit(egsurv.fit2, newdata = newsurvdat.batch1))


##### Checking assumptions of the coxph model

cox.zph(egsurv.fit2)
cox.zph(egsurv.fit2a)
cox.zph(egsurv.fit1)


