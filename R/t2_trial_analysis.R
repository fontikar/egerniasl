
setwd("~/Dropbox/Egernia striolata social learning/")

assocdat <- read.csv("output/data/task2_final.csv", stringsAsFactors = FALSE)
head(assocdat)
str(assocdat)
library(MCMCglmm)

##############################################################################################################################

MCMC.chains <- function(path){
  imp <- readRDS(path)
  #Import chains
  VCV.list <- mcmc.list(lapply(imp, function(x) x$VCV))
  Sol.list <- mcmc.list(lapply(imp, function(x) x$Sol))
  
  #Combine chains
  VCV.mode <- MCMCglmm::posterior.mode(as.mcmc(plyr::ldply(VCV.list)))
  VCV.HPD <- coda::HPDinterval(as.mcmc(plyr::ldply(VCV.list)))
  
  Sol.mode <- MCMCglmm::posterior.mode(as.mcmc(plyr::ldply(Sol.list)))
  Sol.HPD <- coda::HPDinterval(as.mcmc(plyr::ldply(Sol.list)))
  
  return(list(solVCVlist = list(VCV = VCV.list, Sol = Sol.list ), solVCVchain = list(VCV = cbind(VCV.mode, VCV.HPD), Sol = cbind( Sol.mode, Sol.HPD))))
}

################################################################################################################################

t2_probcormod.1<-MCMC.chains("output/t2_probcormod.1")

names(t2_probcormod.1)

plot(t2_probcormod.1$solVCVlist$Sol[[1]])
plot(t2_probcormod.1$solVCVlist$Sol[[2]])
plot(t2_probcormod.1$solVCVlist$Sol[[3]])

autocorr.diag(t2_probcormod.1$solVCVlist$Sol[[1]])
autocorr.diag(t2_probcormod.1$solVCVlist$Sol[[2]])
autocorr.diag(t2_probcormod.1$solVCVlist$Sol[[3]])

heidel.diag(t2_probcormod.1$solVCVlist$Sol[[1]])
heidel.diag(t2_probcormod.1$solVCVlist$Sol[[2]])
heidel.diag(t2_probcormod.1$solVCVlist$Sol[[3]])

geweke.diag(t2_probcormod.1$solVCVlist$Sol[[1]])
geweke.diag(t2_probcormod.1$solVCVlist$Sol[[2]])
geweke.diag(t2_probcormod.1$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(t2_probcormod.1$solVCVlist$Sol[[1]], t2_probcormod.1$solVCVlist$Sol[[2]], t2_probcormod.1$solVCVlist$Sol[[3]]))

t2_probcormod.1$solVCVchain$Sol

####Table 3 for ms####

Table3 <- data.frame(matrix(nrow = 5, ncol = 6))
colnames(Table3)[1:3] <- "Probcor"
colnames(Table3)[4:6] <- "Cor only"

rownames(Table3) <- c("Intercept", "Treatment (SOC)", "Trial", "Batch (2)", "Treatment:Trial")

t2_probcormod.1$solVCVchain$Sol[,1]
t2_probcormod.1$solVCVchain$Sol[,2]
t2_probcormod.1$solVCVchain$Sol[,3]

#Est

Table3[,1] <- round(t2_probcormod.1$solVCVchain$Sol[,1],2)

#L

Table3[,2] <- round(t2_probcormod.1$solVCVchain$Sol[,2],2)

#U

Table3[,3] <- round(t2_probcormod.1$solVCVchain$Sol[,3],2)


############################

t2_correctonly.1<-MCMC.chains("output/t2_correctonly.1")

names(t2_correctonly.1)

plot(t2_correctonly.1$solVCVlist$Sol[[1]])
plot(t2_correctonly.1$solVCVlist$Sol[[2]])
plot(t2_correctonly.1$solVCVlist$Sol[[3]])

autocorr.diag(t2_correctonly.1$solVCVlist$Sol[[1]])
autocorr.diag(t2_correctonly.1$solVCVlist$Sol[[2]])
autocorr.diag(t2_correctonly.1$solVCVlist$Sol[[3]])

heidel.diag(t2_correctonly.1$solVCVlist$Sol[[1]])
heidel.diag(t2_correctonly.1$solVCVlist$Sol[[2]])
heidel.diag(t2_correctonly.1$solVCVlist$Sol[[3]])

geweke.diag(t2_correctonly.1$solVCVlist$Sol[[1]])
geweke.diag(t2_correctonly.1$solVCVlist$Sol[[2]])
geweke.diag(t2_correctonly.1$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(t2_correctonly.1$solVCVlist$Sol[[1]], t2_correctonly.1$solVCVlist$Sol[[2]], t2_correctonly.1$solVCVlist$Sol[[3]]))

t2_correctonly.1$solVCVchain$Sol

############################

t2_batchcorrectonly.1<-MCMC.chains("output/t2_batchcorrectonly.1")

names(t2_batchcorrectonly.1)

plot(t2_batchcorrectonly.1$solVCVlist$Sol[[1]])
plot(t2_batchcorrectonly.1$solVCVlist$Sol[[2]])
plot(t2_batchcorrectonly.1$solVCVlist$Sol[[3]])

autocorr.diag(t2_batchcorrectonly.1$solVCVlist$Sol[[1]])
autocorr.diag(t2_batchcorrectonly.1$solVCVlist$Sol[[2]])
autocorr.diag(t2_batchcorrectonly.1$solVCVlist$Sol[[3]])

heidel.diag(t2_batchcorrectonly.1$solVCVlist$Sol[[1]])
heidel.diag(t2_batchcorrectonly.1$solVCVlist$Sol[[2]])
heidel.diag(t2_batchcorrectonly.1$solVCVlist$Sol[[3]])

geweke.diag(t2_batchcorrectonly.1$solVCVlist$Sol[[1]])
geweke.diag(t2_batchcorrectonly.1$solVCVlist$Sol[[2]])
geweke.diag(t2_batchcorrectonly.1$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(t2_batchcorrectonly.1$solVCVlist$Sol[[1]], t2_batchcorrectonly.1$solVCVlist$Sol[[2]], t2_batchcorrectonly.1$solVCVlist$Sol[[3]]))

t2_batchcorrectonly.1$solVCVchain$Sol

####Table 3 for ms####

t2_correctonly.1$solVCVchain$Sol[,1]
t2_correctonly.1$solVCVchain$Sol[,2]
t2_correctonly.1$solVCVchain$Sol[,3]

#Est - Need interaction only

Table3[5,4] <- round(t2_correctonly.1$solVCVchain$Sol[,1][5],2)

#L - Need interaction only

Table3[5,5] <- round(t2_correctonly.1$solVCVchain$Sol[,2][5],2)

#U - Need interaction only

Table3[5,6] <- round(t2_correctonly.1$solVCVchain$Sol[,3][5],2)

############################################

#Interaction removed for Task 2: probability of making a correct choice ONLY


t2_correctonly.2 <-MCMC.chains("output/t2_correctonly.2")

names(t2_correctonly.2)

plot(t2_correctonly.2$solVCVlist$Sol[[1]])
plot(t2_correctonly.2$solVCVlist$Sol[[2]])
plot(t2_correctonly.2$solVCVlist$Sol[[3]])

autocorr.diag(t2_correctonly.2$solVCVlist$Sol[[1]])
autocorr.diag(t2_correctonly.2$solVCVlist$Sol[[2]])
autocorr.diag(t2_correctonly.2$solVCVlist$Sol[[3]])

heidel.diag(t2_correctonly.2$solVCVlist$Sol[[1]])
heidel.diag(t2_correctonly.2$solVCVlist$Sol[[2]])
heidel.diag(t2_correctonly.2$solVCVlist$Sol[[3]])

geweke.diag(t2_correctonly.2$solVCVlist$Sol[[1]])
geweke.diag(t2_correctonly.2$solVCVlist$Sol[[2]])
geweke.diag(t2_correctonly.2$solVCVlist$Sol[[3]])

gelman.diag(mcmc.list(t2_correctonly.2$solVCVlist$Sol[[1]], t2_correctonly.2$solVCVlist$Sol[[2]], t2_correctonly.2$solVCVlist$Sol[[3]]))

t2_correctonly.2$solVCVchain$Sol

####Table 3 for ms####

t2_correctonly.2$solVCVchain$Sol[,1]
t2_correctonly.2$solVCVchain$Sol[,2]
t2_correctonly.2$solVCVchain$Sol[,3]

#Est - Need main effects 

Table3[1:4,4] <- round(t2_correctonly.2$solVCVchain$Sol[,1],2)

#L - Need main effects 

Table3[1:4,5] <- round(t2_correctonly.2$solVCVchain$Sol[,2],2)

#U - Need main effects 
Table3[1:4,6] <- round(t2_correctonly.2$solVCVchain$Sol[,3],2)


write.csv(Table3, file="output/tables/Table3_final.csv")

#####Trying lme4 for coronly batch and treatment interaction

library(lme4)

assocdat$LizardID <- as.factor(assocdat$LizardID)
assocdat$Treatment <- as.factor(assocdat$Treatment)
assocdat$Batch <- as.factor(assocdat$Batch)
assocdat$Date <-as.Date(assocdat$Date, format="%m/%d/%Y")
assocdat$Time <- as.factor(assocdat$Time)
assocdat$Correct <- as.factor(assocdat$Correct)

assocdat$newTrial <- scale(assocdat$Trial, center = T, scale = T)


t2.coronlybatch.mod<- glmer(Correct ~ Treatment*Trial+Treatment*Batch + (1+ Trial|LizardID), family = "binomial", data=assocdat)

summary(t2.coronlybatch.mod)

t2.coronlybatch.mod2<- glmer(Correct ~ Treatment*newTrial+Treatment*Batch + (1| LizardID), family = "binomial", data=assocdat)

summary(t2.coronlybatch.mod2)



