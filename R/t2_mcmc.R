setwd("~/Dropbox/social learning/output/")

#load library you need
library(plyr)
library(lme4)
library(MASS)
library(MCMCglmm)

assocdat <- read.csv("data/task2_final.csv", stringsAsFactors = FALSE)

assocdat$LizardID <- as.factor(assocdat$LizardID)
assocdat$Treatment <- as.factor(assocdat$Treatment)
assocdat$Date <-as.Date(assocdat$Date, format="%m/%d/%Y")
assocdat$Time <- as.factor(assocdat$Time)
assocdat$Correct <- as.factor(assocdat$Correct)

prior.test<- list(R = list(V =1,fix=1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002)))


probcor.1<-MCMCglmm(Correct ~ Treatment*Trial, random = ~us(1+Trial):LizardID, family = "categorical", nitt = 2000000, thin = 5000, prior=prior.test, burnin = 15000, data=assocdat, verbose= T)

summary(probcor.1)

saveRDS(probcor.1, file="output/t2_probcormod.1")