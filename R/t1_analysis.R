#Setting working directory
getwd()
setwd("C:/Users/xufeng/Dropbox/social learning/output/data/")
setwd("~/Dropbox/Egernia striolata social learning//output/data/")

#load library you need
library(plyr)
library(MASS)
library(lme4)
library(plyr)

#Read data
instrumdat <- read.csv("task1_final.csv", stringsAsFactors = FALSE)
head(instrumdat) #261 obs
str(instrumdat)

#Changing variable types
instrumdat$LizardID <- as.factor(instrumdat$LizardID)
instrumdat$Treatment <- as.factor(instrumdat$Treatment)
instrumdat$Date <-as.Date(instrumdat$Date, format="%m/%d/%Y")
instrumdat$Time <- as.factor(instrumdat$Time)

str(instrumdat)

# Exploration plots
hist(instrumdat$Latency)

instrumdat$log.latency<-log(instrumdat$Latency)

hist(instrumdat$log.latency)

#Descriptive statistics

length((unique(instrumdat[instrumdat$Treatment == "SL",2]))) #n = 15 for social learning treatment lizards
length((unique(instrumdat[instrumdat$Treatment == "C",2]))) # n = 13 for control lizards


#Proportion of lizards that learnt per trial
table(instrumdat$Trial)

library(plyr)

instrum_proplearndat<- ddply(.data=instrumdat, .(Trial, Treatment), summarise, sample_size = length(Correct))

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

par(mfrow=c(1,2), mar = c(4, 5, 1.5, 1.5), cex.axis=1.5, mai=c(1,1,0.6,0.2))

plot(proportion~Trial, data=instrum_proplearndat, pch=c(1,19), col=("black"), cex=1.5, ylim = c(0,1), ann=F, xaxt = "n", type="n")

axis(1, at=c(1:10))

title(ylab = list("Proportion of sample that learn", cex=1.5),line=3)
title(xlab = list("Trial", cex=1.5),line=2.5)

points(proportion~Trial, data=SLprop, cex=1.5, col="black", pch=19)
points(proportion~Trial, data=Cprop, cex=1.5, pch=1)

lines(proportion~Trial, data=SLprop, lwd=2)
lines(proportion~Trial, data=Cprop, lwd=2, lty=5)

legend(7.5, 0.1, c("Control", "Social"), lty= c(1, 5), pch=c(1,19), cex=1.2, bty='n')

mtext("a)", adj = -0.15, padj = -0.20, cex=1.4)


# Mean number of trials taken to learn

sumdat2 <-ddply(.data=instrumdat, .(LizardID, Treatment), summarise, trials_to_learn=sum((lt)), total_trials=length(lt))
sumdat2

sumdat2$Treatment <-as.character(sumdat2$Treatment)
sumdat2$Treatment[sumdat2$Treatment == "SL"] <- "1"
sumdat2$Treatment[sumdat2$Treatment == "C"] <- "0"
sumdat2$Treatment <-as.factor(sumdat2$Treatment)

str(sumdat2)

task1mod.1<-glm.nb(trials_to_learn~Treatment, data=sumdat2)
summary(task1mod.1)

newdat <-data.frame(Treatment = c(0, 1))
newdat$Treatment <- as.factor(newdat$Treatment)

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

segments(x0 =1, y0=7.5, x1 = 2.6, y1 =7.5, lwd= 2)
text(x=1.8, y=8, labels="n.s.", cex=1.5, font=1)

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

