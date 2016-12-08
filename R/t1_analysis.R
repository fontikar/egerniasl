#Setting working directory
getwd()
#setwd("C:/Users/xufeng/Dropbox/social learning/output/data/")
setwd("~/Dropbox/Egernia striolata social learning/")

#load library you need
library(plyr)
library(MASS)
library(lme4)
library(plyr)
library(survival)

#Read data
instrumdat <- read.csv("output/data/task1_finaldat.csv", stringsAsFactors = FALSE)
head(instrumdat) #261 obs
str(instrumdat)

#Changing variable types
instrumdat$LizardID <- as.factor(instrumdat$LizardID)
instrumdat$Batch <- as.factor(instrumdat$Batch)
instrumdat$Treatment <- as.factor(instrumdat$Treatment)
instrumdatlt <- as.factor(instrumdat$lt)
instrumdat$Date <-as.Date(instrumdat$Date, format="%m/%d/%Y")
instrumdat$Time <- as.factor(instrumdat$Time)

#Getting Batch variable

#batchinfo <- assocdat[,1:2]
#str(batchinfo)
#str(instrumdat)

#batchinfo<-unique(batchinfo)

#length(unique(batchinfo$LizardID))  == length(unique(instrumdat$LizardID))

#sort(unique(batchinfo$LizardID)) == sort(unique(instrumdat$LizardID)) 

#instrumdat <- merge(instrumdat, batchinfo, by = "LizardID")

#str(instrumdat)

#write.csv(instrumdat, file ="task1_finaldat.csv")

# Exploration plots
hist(instrumdat$Latency)

instrumdat$log.latency<-log(instrumdat$Latency)

hist(instrumdat$log.latency)

#Descriptive statistics

length((unique(instrumdat[instrumdat$Treatment == "SL",2]))) #n = 15 for social learning treatment lizards
length((unique(instrumdat[instrumdat$Treatment == "C",2]))) # n = 13 for control lizards


#Proportion of lizards that learnt per trial

instrumdat[instrumdat$learnt == "0",]
table(instrumdat$Trial)

library(plyr)

instrum_proplearndat<- ddply(.data=instrumdat, .(Trial, Treatment), summarise, learnt = sum((lt==0)), sample_size = length(LizardID), proportion = round(learnt/sample_size, 2))


instrum_proplearndat$Treatment <- as.character(instrum_proplearndat$Treatment)
instrum_proplearndat$Treatment[instrum_proplearndat$Treatment == "SL"] <- "1"
instrum_proplearndat$Treatment[instrum_proplearndat$Treatment == "C"] <- "0"
instrum_proplearndat$Treatment <- as.factor(instrum_proplearndat$Treatment)

instrum_proplearndat <-instrum_proplearndat[with(instrum_proplearndat, order(Treatment)), ]

in_condata <-instrumdat[instrumdat$Treatment == "C",] 
length(unique(in_condata$LizardID))

in_con_vec <-as.vector(table(in_condata$lt, in_condata$Trial)[1,])

in_socdata <-instrumdat[instrumdat$Treatment == "SL",]
length(unique(in_socdata$LizardID))

in_soc_vec <-as.vector(table(in_socdata$lt, in_socdata$Trial)[1,])

instrum_proplearndat$numlearnt <- append(in_con_vec, in_soc_vec)

instrum_proplearndat$proportion <- instrum_proplearndat$numlearnt/instrum_proplearndat$sample_size

SLprop <-instrum_proplearndat[instrum_proplearndat$Treatment == "1",]
Cprop <-instrum_proplearndat[instrum_proplearndat$Treatment == "0",]

#Plotting figure propportion learnt over trials
pdf("Figure1A-B.pdf", 13,7)

par(mfrow=c(1,2), mar = c(4, 5, 1.5, 1.5), cex.axis=1.5, mai=c(1,1,0.6,0.2), las=1)

plot(proportion~Trial, data=instrum_proplearndat, pch=c(1,19), col=("black"), cex=1.5, ylim = c(0,1), ann=F, xaxt = "n", type="n")

axis(1, at=c(1:10))

title(ylab = list("Proportion of sample that learnt", cex=1.5),line=3)
title(xlab = list("Trial", cex=1.5),line=2.5)

points(proportion~Trial, data=SLprop, cex=1.5, col="black", pch=19)
points(proportion~Trial, data=Cprop, cex=1.5, pch=1)

lines(proportion~Trial, data=SLprop, lwd=2)
lines(proportion~Trial, data=Cprop, lwd=2, lty=5)

legend(7.5, 0.1, c("Control", "Social"), lty= c(1, 5), pch=c(1,19), cex=1.2, bty='n')

mtext("a)", adj = -0.15, padj = -0.20, cex=1.4)

####Cox Proportional Hazard analysis

instrum_surv_dat<- ddply(.data=instrumdat, .(LizardID, Treatment, Batch), summarise, Time = sum(lt), Event = unique(learnt))

instrum_surv_dat$Treatment <- as.character(instrum_surv_dat$Treatment)
instrum_surv_dat[instrum_surv_dat$Treatment == "SL", 2] <- "1"
instrum_surv_dat[instrum_surv_dat$Treatment == "C", 2] <- "0"
instrum_surv_dat$Treatment <- as.factor(instrum_surv_dat$Treatment)

insurv.fit1 <- coxph(Surv(Time, Event)~strata(Treatment)*Batch, data=instrum_surv_dat)
summary(insurv.fit1)

insurv.fit2 <- coxph(Surv(Time, Event)~strata(Treatment)+Batch, data=instrum_surv_dat)
summary(insurv.fit2)
summary(survfit(insurv.fit2))

insurv.fit2a <- coxph(Surv(Time, Event)~Treatment+Batch, data=instrum_surv_dat)
summary(insurv.fit2a)

cox.zph(insurv.fit2)
cox.zph(insurv.fit2a)
cox.zph(insurv.fit1)

#Plotting these curves
pdf("Figure1A-B.pdf", 13,7)

par(mfrow=c(1,2), mar = c(4, 5, 1.5, 1.5), cex.axis=1.5, mai=c(1,1,0.6,0.2), las=1)

plot(survfit(insurv.fit2), lty=c(2,1), lwd=2)

#Control
con_x_trial <-  c(6, 6, 8)
summary(survfit(insurv.fit2))

con_y_up <- c(1,0.615, 0.615)
con_y_low <- c(1, 0.066, 0.066)

#lines(con_x_trial, con_y_low)
#lines(con_x_trial, con_y_up)

#Social
soc_x_trial <- c(8,8,10,10,15,15)
summary(survfit(insurv.fit2))$time[11:13]
soc_y_up <- c(1, round(rep(summary(survfit(insurv.fit2))$upper[11:13], each =2)[1:5],2))
soc_y_low <- c(1,round(rep(summary(survfit(insurv.fit2))$lower[11:13], each =2)[1:5],2))

lines(soc_x_trial, soc_y_low, lty= 3)
lines(soc_x_trial, soc_y_up, lty= 3)

#legend(0.3,0.1,  c("Control", "Social"), lty= c(2, 1), bty='n', cex=1.2, lwd = 2, x.intersp = 1.5)
title(ylab = list("Proportion of lizards that have not learnt", cex=1.5), line =3)
title(xlab = list("Trial Number", cex=1.5))

mtext("a)", adj = -0.15, padj = -0.20, cex=1.4)

#dev.off()


# Mean number of trials taken to learn

sumdat2 <-ddply(.data=instrumdat, .(LizardID, Treatment, Batch), summarise, trials_to_learn=sum((lt)), total_trials=length(lt))
sumdat2

sumdat2$Treatment <-as.character(sumdat2$Treatment)
sumdat2$Treatment[sumdat2$Treatment == "SL"] <- "1"
sumdat2$Treatment[sumdat2$Treatment == "C"] <- "0"
sumdat2$Treatment <-as.factor(sumdat2$Treatment)

str(sumdat2)

task1mod.1<-glm.nb(trials_to_learn~Treatment+Batch, data=sumdat2)

Table.1A <- data.frame(matrix(nrow = 3, ncol=2))
rownames(Table.1A) <- c("Intercept", "Treatment (SOC)", "Batch (2)")
colnames(Table.1A) <- c("Est", "SE")

summary(task1mod.1)$coefficients

#Est

Table.1A[1,1] <- round(summary(task1mod.1)$coefficients[1,1],2)
Table.1A[2,1] <- round(summary(task1mod.1)$coefficients[2,1],2)
Table.1A[3,1] <- round(summary(task1mod.1)$coefficients[3,1],2)

#SE

Table.1A[1,2] <- round(summary(task1mod.1)$coefficients[1,2],2)
Table.1A[2,2] <- round(summary(task1mod.1)$coefficients[2,2],2)
Table.1A[3,2] <- round(summary(task1mod.1)$coefficients[3,2],2)


write.csv(Table.1A, file = "Table.1A.csv")

##################

newdat <-data.frame(Treatment = c(0, 1), Batch = c(1,1))
newdat$Treatment <- as.factor(newdat$Treatment)
newdat$Batch <- as.factor(newdat$Batch)

trials_pred <-predict.glm(task1mod.1, type= "response", se.fit = T,newdata=newdat)

newdat$trials_pred <- trials_pred$fit
newdat$trials_pred_SE <- trials_pred$se.fit
newdat$trials_pred_U <- trials_pred$fit + trials_pred$se.fit
newdat$trials_pred_L <- trials_pred$fit - trials_pred$se.fit

#Plotting figure Mean number of trials to learn
#setwd("~/Dropbox/Egernia striolata social learning/output/fig/")

#pdf("Task1_Meantrials.pdf", 7.66, 7.55)

#par(xaxt="n", mar = c(4, 4.5, 1.5, 2), cex.axis=1.5, mai=c(0.5,0.7,0.5,0.5))

barplot(newdat$trials_pred, ylim = c(0, 10), xlim =c(0,3.5), space=0.5, col=c("white", "grey")) 
box()

title(ylab = list("Mean number of trials taken to learn", cex=1.5))
mtext("Control", at= 1, side = 1, line = 1.2 ,cex = 1.5)
mtext("Social", at=2.5, side =1,line = 1.2, cex= 1.5)

up.x<-c(1,2.5)
low.x<-up.x

arrows(x0 = up.x, y0 = newdat$trials_pred, x1 = up.x, y1 = newdat$trials_pred_U, length = 0.2, angle = 90, lwd=2)
arrows(x0 = low.x, y0 = newdat$trials_pred, x1 = low.x, y1 = newdat$trials_pred_L, length = 0.2, angle = 90, lwd=2)

segments(x0 =1, y0=8, x1 = 2.6, y1 =8, lwd= 2)
text(x=1.8, y=8.5, labels="n.s.", cex=1.5, font=1)

mtext("b)", adj = -0.15, padj = -0.2, cex=1.4)

dev.off()

#Plotting figure Mean number of trials to learn

par(xaxt="n", mar=c(3,6,3,3))

pdf("Task1_Meantrials.pdf", 7.66, 7.55)

barplot(sumdat3$mean_number_trials, ylim = c(0, 10), xlim =c(0,3.5), space=0.5, col=c("white", "grey"), cex.axis = 1.5) 
box()

title(ylab = list("Mean number of trials taken to learn", cex=1.5))
mtext("Control", at= 1, side = 1, line = 1 ,cex = 1.5)
mtext("Social", at=2.5, side =1,line =1, cex= 1.5)

up.x<-c(1,2.5)
low.x<-up.x

arrows(x0 = up.x, y0 = sumdat3$mean_number_trials, x1 = up.x, y1 = sumdat3$upper, length = 0.2, angle = 90, lwd=2)
arrows(x0 = low.x, y0 = sumdat3$mean_number_trials, x1 = low.x, y1 = sumdat3$lower, length = 0.2, angle = 90, lwd=2)

segments(x0 =1, y0=7, x1 = 2.6, y1 =7, lwd= 2)
text(x=1.8, y=7.5, labels="n.s.", cex=1.5, font=1)

dev.off()


#Mean latency

stderror<-function(x){
  sd(x)/(sqrt(length(x)))
}  

latdat <-ddply(.data = instrumdat, .(LizardID, Treatment), summarise, mean_latency = mean(Latency, na.rm=T))
               
               
latdat2 <-ddply(.data = latdat, .(Treatment), summarise, mean_latency2 = mean(mean_latency), SE = stderror(mean_latency), upper = mean_latency2+SE, lower = mean_latency2-SE  )

latdat2$value <- c(1.5,3.5)
latdat2

#Model testing difference in latency between treatment

latfit<-glm(mean_latency~Treatment, data=latdat)
summary(latfit)

par(xaxt="n", mar=c(3,6,3,3))
barplot(latdat2$mean_latency2, ylim = c(0, 1000), xlim =c(0,3.5), space=0.5, col=c("white", "grey"), cex.axis =1.5) 
box()

title(ylab = list("Mean latency to flip lid (s)", cex=1.5), line = 3)
mtext("Control", at= 1, side = 1, line = 1 ,cex = 1.5)
mtext("Social", at=2.5, side =1,line =1, cex= 1.5)

up.value<-c(1, 2.5)
down.value<-up.value

arrows(x0 = up.value, y0 = latdat2$mean_latency2, x1 = up.value, y1 = latdat2$upper, length = 0.2, angle = 90, lwd=2)
arrows(x0 = down.value, y0 = latdat2$mean_latency2,, x1 = down.value, y1 = latdat2$lower, length = 0.2, angle = 90, lwd=2)

segments(x0 =1, y0=850, x1 = 2.6, y1 =850, lwd= 2)
text(x=1.8, y=900, labels="P < 0.001", cex=1.5, font=1)

##############

#Number of successful attempts


learningphase<-instrumdat[instrumdat$lt == 1, ]
learningphase$LizardID<-factor(learningphase$LizardID)
str(learningphase)

incordat<-ddply(.data=learningphase, .(LizardID,Treatment), summarise, correct_choice=sum((Correct==1)), total=length(Correct), incorrect_choice=total-correct_choice)
incordat

stderror<-function(x){
  sd(x)/(sqrt(length(x)))
}  

meandat<-ddply(.data=incordat, .(Treatment), summarise, mean_corr=mean(correct_choice), se_corr=stderror(correct_choice))
meandat$value <- c(1.5, 3.5)

newdat <- data.frame(Treatment = c("C", "SL"))

incorfit<-glm.nb(correct_choice~Treatment, data=incordat)
summary(incorfit)

incordpred<-predict.glm(incorfit, newdata=newdat, se.fit = TRUE, type = "response")

meandat$se_corr<- incordpred$se.fit
meandat$upper <- meandat$mean_corr+meandat$se_corr
meandat$lower <- meandat$mean_corr-meandat$se_corr

par(xaxt="n", mar=c(3,6,3,3))

barplot(meandat$mean_corr, ylim = c(0, 10), xlim =c(0,3.5), space=0.5, col=c("white", "grey"), cex.axis=1.5) 
box()

title(ylab = list("Mean number of successful attempts", cex=1.5))
mtext("Control", at= 1, side = 1, line = 1 ,cex = 1.5)
mtext("Social", at=2.5, side =1,line =1, cex= 1.5)

up.value<-c(1, 2.5)
down.value<-up.value

arrows(x0 = up.value, y0 = meandat$mean_corr, x1 = up.value, y1 = meandat$upper, length = 0.2, angle = 90, lwd=2)
arrows(x0 = down.value, y0 = meandat$mean_corr,, x1 = down.value, y1 = meandat$lower, length = 0.2, angle = 90, lwd=2)

segments(x0 =1, y0=8, x1 = 2.6, y1 =8, lwd= 2)
text(x=1.8, y=8.5, labels="n.s.", cex=1.5, font=1)

#######

trialdat<-ddply(.data=instrumdat, .(LizardID, Treatment), summarise, Number_of_Trials=max(Trial))

write.csv(trialdat, "Instrumental_Table.csv")

#####


# Robustness of learning criteria
split1<-split(instrumdat, instrumdat$LizardID)


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

Criteria_robust<-lapply(split1, function(x) Robust(x))# 3 lizards below 80% mark

split1

#Motivation
unprocessed1 <-read.csv("data/Task1.csv")

unprocessed1$TubID <- as.factor(unprocessed1$TubID)
unprocessed1$LizardID <- as.factor(unprocessed1$LizardID)
str(unprocessed1)

rawdat1 <- split(unprocessed1, unprocessed1$LizardID)


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

motivdat1 <- as.vector(lapply(rawdat1, function(x) NaTest(x)))

###################
#Survival analysis#
###################

instrum_learndat <- ddply(.data=instrumdat, .(LizardID, Trial, Treatment), summarise, lt = lt, did.it.learn = learnt)
instrum_learndat

instrum_surv_dat <-  ddply(.data=instrum_learndat, .(LizardID, Treatment), summarise, Time = sum(lt), Event = unique(did.it.learn))

str(instrum_surv_dat)

instrum_surv_dat$Treatment <- as.character(instrum_surv_dat$Treatment)
instrum_surv_dat[instrum_surv_dat$Treatment == "SL", 2] <- "1"
instrum_surv_dat[instrum_surv_dat$Treatment == "C", 2] <- "0"
instrum_surv_dat$Treatment <- as.factor(instrum_surv_dat$Treatment)

#Data for control group 

fit_instru_con<-survfit(Surv(Time[Treatment == "0"],Event[Treatment == "0"])~1,data=instrum_surv_dat)

par(mfrow=c(1,1), mar = c(4, 5, 1.5, 1.5), cex.axis=1.5, mai=c(1,1,0.6,0.2), cex.lab=1.5)

plot(fit_instru_con, xlab="Trial Number")
title(ylab = list("Probability of NOT learning", cex=1.5), line =3.5)

#plotting lines for social learning group
fit_instru_sock<-survfit(Surv(Time[Treatment == "1"],Event[Treatment == "1"])~1,data=instrum_surv_dat)

summary(fit_instru_sock)

plot(fit_instru_sock, xlab="Trial Number")

x<-c( seq(0,6), 6, seq(6,7)) 
length(x)
y<-c(1,1,1,1,1,1,1,0.0667,0.0667, 0.0667)
length(y)

lines(x,y,col="orange2",lwd=2)

up<-c(1,1,1,1,1,1,1,0.443,0.443, 0.443)
length(up)

lines(x,up,col="orange2",lwd=2, lty= 3)

lo<-c(1,1,1,1,1,1,1,0.01 ,0.01, 0.01 )
length(lo)

lines(x,lo,col="orange2",lwd=2, lty= 3)

###################

wilcox.test(0.15384615,0.06666667)
# W=1, p=1


