#Setting working directory
getwd()
setwd("C:/Users/xufeng/Dropbox/social learning/output/data")
#setwd("~/Dropbox/social learning/output/data/")

#load library you need
library(plyr)
library(MASS)
library(lme4)

#Read data
revdat <- read.csv("task3_final.csv", stringsAsFactors = FALSE)
head(revdat)
str(revdat)

#Changing variable types
revdat$LizardID <- as.factor(revdat$LizardID)
revdat$Treatment <- as.factor(revdat$Treatment)
revdat$Date <-as.Date(revdat$Date, format="%m/%d/%Y")
revdat$Time <- as.factor(revdat$Time)

str(revdat)

# Exploration plots
hist(revdat$Latency)

revdat$log.latency<-log(revdat$Latency)

hist(revdat$log.latency)

#Descriptive statistics

length((unique(revdat[revdat$Treatment == "SL",2]))) #n = 15 for social learning treatment lizards
length((unique(revdat[revdat$Treatment == "C",2]))) # n = 13 for control lizards


              #Proportion of lizards that learnt per trial
              table(revdat$Trial)
          
              proplearndat<- ddply(.data=revdat, .(Trial, Treatment), summarise, Number_of_liz_perform_correct=sum(Correct), sample_size = length(Correct), proportion = round((Number_of_liz_perform_correct/sample_size), digits=2))
              proplearndat
              
              SLprop <-proplearndat[proplearndat$Treatment == "SL",]
              Cprop <-proplearndat[proplearndat$Treatment == "C",]
              
              #Plotting figure propportion learnt over trials
              
              par(mar=c(5, 5, 4, 2) + 0.1)
              plot(proportion~Trial, data=proplearndat, pch=c(1,19), col=("black"), cex=1.5, ylim = c(0,1), xlim=c(1,15), ann=F, cex.axis = 1.5)
              title(ylab = list("Proportion of sample that learn", cex=1.5),line=3)
              title(xlab = list("Trial number", cex=1.5),line=3)
              
              lines(proportion~Trial, data=SLprop, lwd=2)
              lines(proportion~Trial, data=Cprop, lwd=2, lty=5)

legend(11, 0.2, c("Social", "Control"), lwd = 3, lty= c(1, 5))

              
              # Mean number of trials taken to learn

  revlearnt<-revdat[revdat$learnt == "1",]
  str(revlearnt)
  revlearnt$LizardID <-factor(revlearnt$LizardID)

revnolearnt<-revdat[revdat$learnt == "0",]
str(revnolearnt)
revnolearnt$LizardID <-factor(revnolearnt$LizardID)
length(unique(revnolearnt[revnolearnt$Treatment == "SL",2]))
length(unique(revnolearnt[revnolearnt$Treatment == "C",2]))

              
              sumdat2 <-ddply(.data=revlearnt, .(LizardID, Treatment), summarise, trials_to_learn=sum((lt)), total_trials=length(lt))
              sumdat2
              
              stderror<-function(x){
                sd(x)/(sqrt(length(x)))
              }  
              
              sumdat3 <-ddply(.data = sumdat2, .(Treatment), summarise, mean_number_trials = round(mean(trials_to_learn),digits=3), SE=round(stderror(trials_to_learn),digits=3), upper = mean_number_trials+SE, lower = mean_number_trials-SE)
              
              sumdat3$value <- c(1.5,3.5)
              sumdat3
              
              
              #Fit a generalised linear model. Nb = Negative Binomial
              
              fit_1<-glm.nb(trials_to_learn~Treatment, data=sumdat2)
              summary(fit_1) 
            
              
              #Plotting figure Mean number of trials to learn
              
              par(xaxt="n", mar=c(3,6,3,3))
              barplot(sumdat3$mean_number_trials, ylim = c(0,17), xlim =c(0,3.5), space=0.5, col=c("white", "grey"), cex.axis = 1.5) 
              box()
              
              title(ylab = list("Mean number of trials taken to learn", cex=1.5), line =3)
              mtext("Control", at= 1, side = 1, line = 1,cex = 1.5)
              mtext("Social", at=2.5, side =1,line =1, cex= 1.5)
              
              up.x<-c(1,2.5)
              low.x<-up.x
              
              arrows(x0 = up.x, y0 = sumdat3$mean_number_trials, x1 = up.x, y1 = sumdat3$upper, length = 0.2, angle = 90, lwd=2)
              arrows(x0 = low.x, y0 = sumdat3$mean_number_trials, x1 =low.x, y1 = sumdat3$lower, length = 0.2, angle = 90, lwd=2)
              
              segments(x0 =1, y0=14, x1 = 2.6, y1 =14, lwd= 2)
              text(x=1.8, y=15, labels="n.s.", cex=1.5, font=1)
              
              
              
              
              #Mean latency
              
              stderror<-function(x){
                sd(x)/(sqrt(length(x)))
              }  
              
              latdat <-ddply(.data = revdat, .(LizardID, Treatment), summarise, mean_latency = mean(Latency, na.rm=T))
              
              
              latdat2 <-ddply(.data = latdat, .(Treatment), summarise, mean_latency2 = mean(mean_latency), SE = stderror(mean_latency), upper = mean_latency2+SE, lower = mean_latency2-SE  )
              
              latdat2$value <- c(1.5,3.5)
              latdat2
            
              
              #Model testing difference in latency between treatment
              
              latfit<-glm(mean_latency~Treatment, data=latdat)
              summary(latfit)
              
              par(xaxt="n", mar=c(3,6,3,3))
              barplot(latdat2$mean_latency2, ylim = c(0, 1150), xlim =c(0,3.5), space=0.5, col=c("white", "grey"), cex.axis=1.5) 
              box()
              
              title(ylab = list("Mean latency to flip lid (s)", cex=1.5), line=3)
              mtext("Control", at= 1, side = 1, line = 1 ,cex = 1.5)
              mtext("Social", at=2.5, side =1,line =1, cex= 1.5)
              
              up.value<-c(1, 2.5)
              down.value<-up.value
              
              arrows(x0 = up.value, y0 = latdat2$mean_latency2, x1 = up.value, y1 = latdat2$upper, length = 0.2, angle = 90, lwd=2)
              arrows(x0 = down.value, y0 = latdat2$mean_latency2,, x1 = down.value, y1 = latdat2$lower, length = 0.2, angle = 90, lwd=2)
              
              segments(x0 =1, y0=950, x1 = 2.6, y1 =950, lwd= 2)
              text(x=1.8, y=1000, labels="n.s.", cex=1.5, font=1)   
#####
learningphase<-revdat[revdat$lt == 1, ]
learningphase$LizardID<-factor(learningphase$LizardID)
str(learningphase)

incordat<-ddply(.data=learningphase, .(LizardID,Treatment), summarise, correct_choice=sum((Correct==1)), total=length(Correct), incorrect_choice=total-correct_choice)
incordat

stderror<-function(x){
  sd(x)/(sqrt(length(x)))
}  

meandat<-ddply(.data=incordat, .(Treatment), summarise, mean_incorr=mean(incorrect_choice), se_incorr=stderror(incorrect_choice))
meandat$value <- c(1.5, 3.5)
incorfit<-glm.nb(incorrect_choice~Treatment, data=incordat)
summary(incorfit)

newdat <- data.frame(Treatment = c("C", "SL"))
incordpred<-predict.glm(incorfit, newdata=newdat, se.fit = TRUE, type = "response")
meandat$se_incorr<- incordpred$se.fit

meandat$upper <- meandat$mean_incorr+meandat$se_incorr
meandat$lower <- meandat$mean_incorr-meandat$se_incorr

par(xaxt="n", mar=c(3,6,3,3))

barplot(meandat$mean_incorr, ylim = c(0, 13), xlim =c(0,3.5), space=0.5, col=c("white", "grey"), cex.axis=1.5) 
box()

title(ylab = list("Mean number of errors", cex=1.5))
mtext("Control", at= 1, side = 1, line = 1 ,cex = 1.5)
mtext("Social", at=2.5, side =1,line =1, cex= 1.5)

up.value<-c(1, 2.5)
down.value<-up.value

arrows(x0 = up.value, y0 = meandat$mean_incorr, x1 = up.value, y1 = meandat$upper, length = 0.2, angle = 90, lwd=2)
arrows(x0 = down.value, y0 = meandat$mean_incorr,, x1 = down.value, y1 = meandat$lower, length = 0.2, angle = 90, lwd=2)

segments(x0 =1, y0=11, x1 = 2.6, y1 =11, lwd= 2)
text(x=1.8, y=11.5, labels="0.054", cex=1.5, font=1)

#Total trials attempted

trialdat<-ddply(.data=revdat, .(LizardID, Treatment), summarise, Number_of_Trials=max(Trial))

write.csv(trialdat, "Reversal_Table.csv")
getwd()
              
# Robustness of learning criteria
split1<-split(revdat, revdat$LizardID)


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

t3_Criteria_robust<-lapply(split1, function(x) Robust(x))# 3 lizards below 80% mark

split1              
              