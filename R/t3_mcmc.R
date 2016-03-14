setwd("/home/ubuntu/gitrepo/egerniasl")

#load library you need

library(MCMCglmm)

revdat <- read.csv("output/data/task3_final.csv", stringsAsFactors = FALSE)

revdat$LizardID <- as.factor(revdat$LizardID)
revdat$Treatment <- as.factor(revdat$Treatment)
revdat$Date <-as.Date(revdat$Date, format="%m/%d/%Y")
revdat$Time <- as.factor(revdat$Time)
revdat$Correct <- as.factor(revdat$Correct)


prior.test<- list(R = list(V =1,fix=1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002)))


probcor.1<-MCMCglmm(Correct ~ Treatment*Trial, random = ~us(1+Trial):LizardID, family = "categorical", nitt = 2000000, thin = 1000, prior=prior.test, burnin = 15000, data=revdat, verbose= T)

summary(probcor.1)

saveRDS(probcor.1, file="output/t3_probcormod.1")

correctonly.2<-MCMCglmm(Choose.only.correct.dish ~ Treatment*Trial, random = ~us(1+Trial):LizardID, family = "categorical", nitt = 2000000, thin = 1000, prior=prior.test, burnin = 15000, data=revdat, verbose= T)

summary(probcor.1)

saveRDS(correctonly.2, file="output/t3_correctonly.1")