setwd("~/Dropbox/Egernia striolata social learning/output/")

# install.packages("MCMCglmm")

#load library you need
library(plyr)
library(lme4)
library(MASS)
library(MCMCglmm)

#Read data
assocdat <- read.csv("data/task2_final.csv", stringsAsFactors = FALSE)
head(assocdat)
str(assocdat)

#Changing variable types
assocdat$LizardID <- as.factor(assocdat$LizardID)
assocdat$Batch <- as.factor(assocdat$Batch)
assocdat$Treatment <- as.factor(assocdat$Treatment)
assocdat$Date <-as.Date(assocdat$Date, format="%m/%d/%Y")
assocdat$Time <- as.factor(assocdat$Time)

#Proportion of lizards that learnt per trial

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

pdf("Fig2A-D.pdf", 13, 13)

par(mfrow=c(2,2), mar = c(4, 5, 1.5, 1.5), cex.axis=1.5, mai=c(1,1,0.6,0.2))

plot(proportion~Trial, data=assoc_proplearndat, pch=c(1,19), col=("black"), cex=1.5, ylim = c(0,1), xlim=c(1,30), ann=F, type= "n", xaxt = "n")

axis(1, at=c(1:30))

title(ylab = list("Proportion of sample that learnt", cex=1.5),line=3)
title(xlab = list("Trial number", cex=1.5),line=3)

points(proportion~Trial, data=SLprop, cex=1.5, col="black", pch=19)
points(proportion~Trial, data=Cprop, cex=1.5, pch=1)

lines(proportion~Trial, data=SLprop, lwd=2)
lines(proportion~Trial, data=Cprop, lwd=2, lty=5)

legend(23, 0.15, c("Control", "Social"), lty= c(1, 5), pch=c(1,19), cex=1.2, bty='n')

mtext("a)", adj = -0.15, padj = -0.2, cex=1.4)

#Mean trials taken to learn

sumdat2 <-ddply(.data=assocdat, .(LizardID, Treatment, Batch), summarise, trials_to_learn=sum((lt)), total_trials=length(lt))
sumdat2

str(sumdat2)
sumdat2$Batch <- as.factor(sumdat2$Batch)
sumdat2$Treatment <- as.factor(sumdat2$Treatment)

#Fit a generalised linear model. Nb = Negative Binomial

fit_1<-glm.nb(trials_to_learn~Treatment+Batch, data=sumdat2)
summary(fit_1) 

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

####Number of errors

learningphase<-assocdat[assocdat$lt == 1, ]
learningphase$LizardID<-factor(learningphase$LizardID)
str(learningphase)


incordat<-ddply(.data=learningphase, .(LizardID,Treatment, Batch), summarise, correct_choice=sum((Correct==1)), total=length(Correct), incorrect_choice=total-correct_choice)

incordat$Batch <- as.factor(incordat$Batch)
incordat$Treatment <- as.factor(incordat$Treatment)
str(incordat)

incorfit<-glm.nb(incorrect_choice~Treatment+Batch, data=incordat)
summary(incorfit)

incor_newdat <- data.frame(Treatment = c("0", "1"),
                           Batch = c("1", "1"))

str(incor_newdat)

incordpred<-predict.glm(incorfit, newdata=incor_newdat, se.fit = TRUE, type = "response")

incor_newdat$se_incorr<- incordpred$se.fit
incor_newdat$pred<- incordpred$fit

incor_newdat$upper <- incordpred$fit + incordpred$se.fit
incor_newdat$lower <- incordpred$fit - incordpred$se.fit

#Plotting mean incorrect choices

#par(mfrow=c(1,2), mar = c(4, 5, 1.5, 1.5), cex.axis=1.5, mai=c(1,1,0.6,0.2))

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

mtext("c)", adj = -0.15, padj = -0.2, cex=1.4)

#Plotting predictions for probability

t2_probcormod.1<-readRDS("output/t2_probcormod.1")

newdata=data.frame(Treatment = rep((c(1,0)), each=30),
                   Trial = c(1:30),
                   Batch = c(1))

newdata$Treatment <- as.factor(newdata$Treatment)
newdata$Batch <- as.factor(newdata$Batch)
str(newdata)


X <-model.matrix(~Treatment*Trial+Batch, data=newdata)

V <-rowSums(t2_probcormod.1$VCV)

beta <-colMeans(t2_probcormod.1$Sol)

newdata$pred_latent <- X %*% beta
newdata$pred_orig  <- exp(pred_latent) / (1 + exp(pred_latent))
round(pred_orig,2) == round(plogis(pred_latent),2)

covbeta <- var(t2_probcormod.1$Sol) 

newdata$se    <- sqrt(diag(X %*% covbeta %*% t(X)))
newdata$up  <- exp(newdata$pred_latent + 1.96 *newdata$se) / (1 + exp(newdata$pred_latent + 1.96 *newdata$se))
newdata$low  <- exp(newdata$pred_latent - 1.96 *newdata$se) / (1 + exp(newdata$pred_latent  - 1.96 *newdata$se))

newdata

####Plotting this

par(mfrow=c(1,1))

socialdata<-newdata[newdata$Treatment == "1",]
controldata<-newdata[newdata$Treatment == "0",]

#polygon time
x1<-seq(1:30)
x2<-order(-x1)
x3<-c(x1,x2)

newdata_dec <- newdata[with(newdata, order(Treatment, -Trial)),]

y1.probcor.c <-newdata[newdata$Treatment == "0",8] #LWR control
y2.probcor.c <-newdata_dec[newdata_dec$Treatment == "0",7] #UPR control
y3.probcor.c <-c(y1.probcor.c,y2.probcor.c)

y1.probcor.s <-newdata[newdata$Treatment == "1",8] #LWR social
y2.probcor.s<-newdata_dec[newdata_dec$Treatment == "1",7] #UPR social
y3.probcor.s <-c(y1.probcor.s,y2.probcor.s)

#Plotting the polygons

plot(socialdata$pred_orig~socialdata$Trial, ylim=c(0,1),pch=19, bg= "Black", cex=1.5, ylab="Probability of making a correct choice", cex.lab = 1.5, xlab= "Trial number", type="n")

polygon(x3, y3.probcor.s, col=rgb(190,190,190,150, max=255), border=NA) #social
points(socialdata$pred_orig~socialdata$Trial, ylim=c(0,1),pch=19, bg= "Black", cex=1.5)
lines(spline(socialdata$pred_orig~socialdata$Trial), lwd=2)

#CIs
#lines(spline(socialdata$up~socialdata$Trial), lwd=2)
#lines(spline(socialdata$low~socialdata$Trial), lwd=2)

#Control
polygon(x3, y3.probcor.c, col=rgb(0,0,0,0), border=T) #control
points(controldata$pred_orig~controldata$Trial, ylim=c(0,1),pch=1, cex=1.5)
lines(spline(controldata$pred_orig~controldata$Trial), lwd=2, lty=2)

#CIs
#lines(spline(controldata$up~controldata$Trial),lwd=2, lty=2)
#lines(spline(controldata$low~controldata$Trial),lwd=2, lty=2)


legend(23, 0.15, c("Control", "Social"), lty= c(1, 5), pch=c(1,19), cex=1.2, bty='n')

mtext("d)", adj = -0.15, padj = -0.2, cex=1.4)

dev.off()
