setwd("/home/ubuntu/gitrepo/egerniasl/")

#load library you need

library(MCMCglmm)

assocdat <- read.csv("output/data/task2_final.csv", stringsAsFactors = FALSE)

assocdat$LizardID <- as.factor(assocdat$LizardID)
assocdat$Treatment <- as.factor(assocdat$Treatment)
assocdat$Date <-as.Date(assocdat$Date, format="%m/%d/%Y")
assocdat$Time <- as.factor(assocdat$Time)
assocdat$Correct <- as.factor(assocdat$Correct)

prior.test<- list(R = list(V =1,fix=1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002)))


probcor.1<-MCMCglmm(Correct ~ Treatment*Trial, random = ~us(1+Trial):LizardID, family = "categorical", nitt = 2000000, thin = 5000, prior=prior.test, burnin = 15000, data=assocdat, verbose= T)

saveRDS(probcor.1, file="output/t2_probcormod.1")


correctonly.1<-MCMCglmm(Choose.only.correct.dish ~ Treatment*Trial, random = ~us(1+Trial):LizardID, family = "categorical", nitt = 2000000, thin = 5000, prior=prior.test, burnin = 15000, data=assocdat, verbose= T)


saveRDS(correctonly.1, file="output/t2_correctonly.1")