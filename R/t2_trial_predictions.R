
setwd("~/Dropbox/Egernia striolata social learning/")

assocdat <- read.csv("output/data/task2_final.csv", stringsAsFactors = FALSE)
head(assocdat)
str(assocdat)
library(MCMCglmm)

t2_probcormod.1<-readRDS("output/t2_probcormod.1")

plot(t2_probcormod.1)

summary(t2_probcormod.1$Sol)

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


#################

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

plot(socialdata$pred_orig~socialdata$Trial, ylim=c(0,1),pch=19, bg= "Black", cex=1.5, ylab="Probability of making a correct choice", cex.lab = 1.2, cex.axis=1.2, xlab= "Trial", type="n")

#polygon(x3, y3.probcor.s, col=rgb(190,190,190,120, max=255), border=) #social
polygon(x3, y3.probcor.s, col=rgb(190,190,190,120, max=255), border=NA) #social
polygon(x3, y3.probcor.c, col=rgb(0,0,0,0), border=T) #control

points(socialdata$pred_orig~socialdata$Trial, ylim=c(0,1),pch=19, bg= "Black", cex=1.5)
lines(spline(socialdata$pred_orig~socialdata$Trial), lwd=2)

#CIs
#lines(spline(socialdata$up~socialdata$Trial), lwd=2)
#lines(spline(socialdata$low~socialdata$Trial), lwd=2)

#Control

points(controldata$pred_orig~controldata$Trial, ylim=c(0,1),pch=1, cex=1.5)
lines(spline(controldata$pred_orig~controldata$Trial), lwd=2, lty=2)

#CIs
lines(spline(controldata$up~controldata$Trial),lwd=2, lty=2)
lines(spline(controldata$low~controldata$Trial),lwd=2, lty=2)


legend(23.5, 0.10, c("Control", "Social"), lty= c(1, 5), pch=c(1,19), cex=1.5, bty='n')



##### Jarrod Hadfields, correcting for variance because it is a logistc model

c2 <- (16 * sqrt(3)/(15 * pi))^2

pred <- t(plogis(t(beta %*% t(X) /sqrt(1+c2*V))))




################
a<-(seq(1:30))
b<-(seq(1:30))
trial.num<-c(a,b)
treatment<-c("P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","P","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S","S")
length(treatment)
sex<-c("M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F")
length(sex)
newdataP<- data.frame(treatment,trial.num,sex) 
X <-model.matrix(~treatment*trial.num+sex, data=newdataP)
betas <-colMeans(F_M3_p$Sol)
##Calculate the fitted values in the predictor scale
newdataP$eta <- X %*% betas
newdataP$pred  <- exp(newdataP$eta) / (1 + exp(newdataP$eta))
#check:
check<-F_predictions$pred[newdata1$treatment == "P"]-newdataP$pred #should be CLOSE to 0
check
#pred from F_Predictions should be similar to PAIREDprob
covbetas<-var(F_M3_p$Sol) 
#Calculate the SEs on the scale of the predictor function


