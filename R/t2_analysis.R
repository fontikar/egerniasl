#Setting working directory
getwd()
setwd("C:/Users/xufeng/Dropbox/social learning/")
setwd("~/Dropbox/Egernia striolata social learning/output/data/")

# install.packages("MCMCglmm")

#load library you need
library(plyr)
library(lme4)
library(MASS)
library(MCMCglmm)

#Read data
assocdat <- read.csv("task2_final.csv", stringsAsFactors = FALSE)
head(assocdat)
str(assocdat)

#Changing variable types
assocdat$LizardID <- as.factor(assocdat$LizardID)
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
              table(assocdat$Trial)
          
              assoc_proplearndat <- ddply(.data=assocdat, .(Trial, Treatment), summarise, sample_size = length(Correct))
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
              
              pdf("Task2_Proplearnt.pdf", 13, 7)
              
              par(mfrow=c(1,2), mar = c(4, 5, 1.5, 1.5), cex.axis=1.5, mai=c(1,1,0.6,0.2))
              
              plot(proportion~Trial, data=assoc_proplearndat, pch=c(1,19), col=("black"), cex=1.5, ylim = c(0,1), xlim=c(1,30), ann=F, cex.axis=1.5, type= "n", xaxt = "n")
              
              axis(1, at=c(1:30))
              
              title(ylab = list("Proportion of sample that learn", cex=1.5),line=3)
              title(xlab = list("Trial number", cex=1.5),line=3)
              
              points(proportion~Trial, data=SLprop, cex=1.5, col="black", pch=19)
              points(proportion~Trial, data=Cprop, cex=1.5, pch=1)
              
              lines(proportion~Trial, data=SLprop, lwd=2)
              lines(proportion~Trial, data=Cprop, lwd=2, lty=5)
              
              legend(22, 0.10, c("Control", "Social"), lty= c(1, 5), pch=c(1,19), cex=1.2, bty='n')

              mtext("a)", adj = -0.15, padj = -0.2, cex=1.4)

              #dev.off()

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
              
              Table1.B <- data.frame(matrix(ncol=2, nrow=2))
              rownames(Table1.B) <- c("Intercept", "Treatment (SOC)")
              colnames(Table1.B) <- c("Est", "SE")
              
              summary(fit_1)$coefficients
              
              #Est
              
              Table1.B[1,1] <- round(summary(fit_1)$coefficients[1,1],2)
              Table1.B[2,1] <- round(summary(fit_1)$coefficients[2,1],2)
              
              #SE
              
              Table1.B[1,2] <- round(summary(fit_1)$coefficients[1,2],2)
              Table1.B[2,2] <- round(summary(fit_1)$coefficients[2,2],2)
              
              write.csv(Table1.B, file="Table1.B.csv")
              
              ##############################
              
              assoc_newdat <-data.frame(Treatment = c(0, 1),
                                  Batch = rep(1,2))
              
              assoc_newdat$Treatment <- as.factor(newdat$Treatment)
              assoc_newdat$Batch <- as.factor(newdat$Batch)
              
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

Table2 <- data.frame(matrix(ncol=2, nrow=2))
rownames(Table2) <- c("Intercept", "Treatment (SOC)")
colnames(Table2) <- c("Est", "SE")

summary(incorfit)$coefficients

#Est

Table2[1,1] <- round(summary(incorfit)$coefficients[1,1],2)
Table2[2,1] <- round(summary(incorfit)$coefficients[2,1],2)

#SE

Table2[1,2] <- round(summary(incorfit)$coefficients[1,2],2)
Table2[2,2] <- round(summary(incorfit)$coefficients[2,2],2)

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

barplot(incor_newdat$pred, ylim = c(0, 5), xlim =c(0,3.5), space=0.5, col=c("white", "grey"), cex.axis=1.5) 
box()

title(ylab = list("Mean number of errors", cex=1.5), line =3)
mtext("Control", at= 1, side = 1, line = 1 ,cex = 1.5)
mtext("Social", at=2.5, side =1,line =1, cex= 1.5)

up.value<-c(1, 2.5)
down.value<-up.value

arrows(x0 = up.value, y0 = incor_newdat$pred, x1 = up.value, y1 = incor_newdat$upper, length = 0.2, angle = 90, lwd=2)
arrows(x0 = down.value, y0 = incor_newdat$pred,, x1 = down.value, y1 = incor_newdat$lower, length = 0.2, angle = 90, lwd=2)

segments(x0 =1, y0=3.5, x1 = 2.6, y1 =3.5, lwd= 2)
text(x=1.8, y=4, labels="P < 0.001", cex=1.5, font=1)

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
split1<-split(assocdat, assocdat$LizardID)


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

              
              