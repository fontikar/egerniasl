setwd("/home/ubuntu/gitrepo/egerniasl/")

#load library you need

library(MCMCglmm)

assocdat <- read.csv("output/data/task2_final.csv", stringsAsFactors = FALSE)

assocdat$LizardID <- as.factor(assocdat$LizardID)
assocdat$Treatment <- as.factor(assocdat$Treatment)
assocdat$Batch <- as.factor(assocdat$Batch)
assocdat$Date <-as.Date(assocdat$Date, format="%m/%d/%Y")
assocdat$Time <- as.factor(assocdat$Time)
assocdat$Correct <- as.factor(assocdat$Correct)

#################################################################################################################################################

prior.test<- list(R = list(V =1,fix=1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002)))

chains <- c(runif(3, 0, 100))

probcor.1 <- list()
  for(i in 1:3){
    set.seed(chains[i])
    probcor.1[[i]] <- MCMCglmm(Correct ~ Treatment*Trial+Batch, random = ~us(1+Trial):LizardID, family = "categorical", nitt = 110000, thin = 100, prior=prior.test, burnin = 10000, data=assocdat, verbose= T)
  }

saveRDS(probcor.1, file="output/t2_probcormod.1")

#################################################################################################################################################

correctonly.1 <- list()
for(i in 1:3){
  set.seed(chains[i])
  correctonly.1[[i]] <- MCMCglmm(Choose.only.correct.dish ~ Treatment*Trial+Batch, random = ~us(1+Trial):LizardID, family = "categorical", nitt = 110000, thin = 100, prior=prior.test, burnin = 10000, data=assocdat, verbose= T)
}

saveRDS(correctonly.1, file="output/t2_correctonly.1")

#################################################################################################################################################

#Interaction Removed

correctonly.1.inrm <- list()
  for(i in 1:3){
    set.seed(chains[i])
    correctonly.1.inrm <- MCMCglmm(Choose.only.correct.dish ~ Treatment+Trial+Batch, random = ~us(1+Trial):LizardID, family = "categorical", nitt = 110000, thin = 100, prior=prior.test, burnin = 10000, data=assocdat, verbose= T)
}

saveRDS(correctonly.1.inrm, file="output/t2_correctonly.2")


