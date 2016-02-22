#Setting working directory
getwd()
setwd("C:/Users/xufeng/Dropbox/social learning/output/data/")
#setwd("~/Dropbox/social learning/output/data/")

#load library you need
library(plyr)
library(MASS)
library(lme4)
library(plyr)

#Read data
instrumdat <- read.csv("task1_final.csv", stringsAsFactors = FALSE)
head(instrumdat)
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

proplearndat<- ddply(.data=instrumdat, .(Trial, Treatment), summarise, Number_of_liz_perform_correct=sum(Correct), sample_size = length(Correct), proportion = round((Number_of_liz_perform_correct/sample_size), digits=2))
proplearndat

SLprop <-proplearndat[proplearndat$Treatment == "SL",]
Cprop <-proplearndat[proplearndat$Treatment == "C",]

#Plotting figure propportion learnt over trials

par(mar=c(5, 5, 4, 2) + 0.1)
plot(proportion~Trial, data=proplearndat, pch=c(1,19), col=("black"), cex=1.5, cex.axis=1.5, ylim = c(0,1), ann=F)
title(ylab = list("Proportion of sample that learn", cex=1.5),line=3)
title(xlab = list("Trial", cex=1.5),line=3.2)


lines(proportion~Trial, data=SLprop, lwd=2)
lines(proportion~Trial, data=Cprop, lwd=2, lty=5)

legend(7, 0.2, c("Social", "Control"), lwd = 3, lty= c(1, 5))

# Mean number of trials taken to learn

sumdat2 <-ddply(.data=instrumdat, .(LizardID, Treatment), summarise, trials_to_learn=sum((lt)), total_trials=length(lt))
sumdat2

stderror<-function(x){
  sd(x)/(sqrt(length(x)))
}  

sumdat3 <-ddply(.data = sumdat2, .(Treatment), summarise, mean_number_trials = round(mean(trials_to_learn),digits=3), SE=round(stderror(trials_to_learn),digits=3), upper = mean_number_trials+SE, lower = mean_number_trials-SE)

sumdat3$value <- c(1.5,3.5)
sumdat3

#Plotting figure Mean number of trials to learn

par(xaxt="n", mar=c(3,6,3,3))
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


#Mean latency

stderror<-function(x){
  sd(x)/(sqrt(length(x)))
}  

latdat <-ddply(.data = instrumdat, .(LizardID, Treatment), summarise, mean_latency = mean(Latency, na.rm=T))
               
               
latdat2 <-ddply(.data = latdat, .(Treatment), summarise, mean_latency2 = mean(mean_latency), SE = stderror(mean_latency), upper = mean_latency2+SE, lower = mean_latency2-SE  )

latdat2$value <- c(1.5,3.5)
latdat2

#Plotting figure Mean number of trials to learn

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

