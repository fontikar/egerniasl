rm(list = ls())

#Setting working directory
getwd()
setwd("C:/Users/xufeng/Dropbox/social learning/")
setwd("/Users/fontikar/Dropbox/Egernia striolata social learning")

eg.t2.dat <- read.csv("data/Task2.csv", stringsAsFactors = FALSE)

head(eg.t2.dat)

str(eg.t2.dat)

eg.t2.dat$LizardID <- as.factor(eg.t2.dat$LizardID)

eg.t2.dat$Treatment <- as.character(eg.t2.dat$Treatment)
eg.t2.dat[eg.t2.dat$Treatment == "C", 3] <- 0 
eg.t2.dat[eg.t2.dat$Treatment == "SL", 3] <- 1
eg.t2.dat$Treatment<- as.factor(eg.t2.dat$Treatment)


eg.t2.dat$Date <-as.Date(eg.t2.dat$Date, format="%m/%d/%Y")
eg.t2.dat$Time <- as.factor(eg.t2.dat$Time)
head(eg.t2.dat)

str(eg.t2.dat)
noNAs <-complete.cases(eg.t2.dat[,7])
eg.t2.dat_2 <-eg.t2.dat[noNAs,]

splitdat<-split(eg.t2.dat_2, eg.t2.dat_2$LizardID)
head(splitdat)

str(eg.t2.dat_2)
eg.t2.dat_2$LizardID<-factor(eg.t2.dat_2$LizardID)
str(eg.t2.dat_2)
splitdat2<-split(eg.t2.dat_2, eg.t2.dat_2$LizardID)

trialNum <- function(x){
  {
    x$Trial<-seq(1:nrow(x))
  }
  x
}

splitdat2 <- as.vector(lapply(splitdat2, function(x) trialNum(x)))

Criteria<-function(x){
{
  lt <- c()
  for(i in 1:nrow(x)){ 
    if(sum(x$Correct[i:(i+7)]) >= 7 && length(lt) < 1){
      lt <- rep(1, i+8)
      lt[length(lt):nrow(x)] <- rep(0)
    } else { lt <- lt }
  }
  x$lt<-lt
}
x
}

splitdat3 <-lapply(splitdat2, function(x) Criteria(x))
eg.t2.dat_3 <-unsplit(splitdat3, eg.t2.dat_2$LizardID)
splitdat4<-split(eg.t2.dat_3, eg.t2.dat_3$LizardID)

Learnt<-function(x){  
  {
    if(0 %in% x$lt){
      x$learnt<-rep(1,length(nrow(x)))
    }else x$learnt<-rep(0,length(nrow(x)))
  }
  x
}

splitdat5<-lapply(splitdat4, function(x) Learnt(x))


eg.t2.dat_4<-unsplit(splitdat5, eg.t2.dat_3$LizardID)


#Merge Batch variables into this dataset

batchdat <- read.csv("data/batch_allocations.csv", stringsAsFactors = FALSE)

colnames(batchdat) <- c("Batch", "LizardID", "Treatment")
batchdat<-batchdat[,1:2]
batchdat$Batch <- as.factor(batchdat$Batch)
batchdat$LizardID <-as.factor(batchdat$LizardID)

str(batchdat)

levels(eg.t2.dat_4$LizardID) == levels(batchdat$LizardID)

eg.t2.dat_5<-merge(batchdat, eg.t2.dat_4, by = "LizardID")

str(eg.t2.dat_5)

unique(eg.t2.dat_5[eg.t2.dat_5$Batch == 1, c(1:4)])
unique(eg.t2.dat_5[eg.t2.dat_5$Batch == 2, c(1:4)])

#Writing the file

getwd()

setwd("C:/Users/xufeng/Dropbox/social learning")

write.csv(eg.t2.dat_5, "output/data/task2_final.csv")


#Checking tally after reaching learning criterion

splitdat6 <- split(eg.t2.dat_5, eg.t2.dat_5$LizardID)


Robust<-function(x){
  {
    start<-sum(x$lt == 1)+1
    test_1<-x[start:nrow(x),"Correct"]
    aftertrials<- length(test_1)
    correct_after<-sum(test_1)
    propcorrect_after<-round((correct_after)/((aftertrials)),2)
    output <- binom.test(x = correct_after, n = aftertrials, alternative = "greater")
  }
  vec<-data.frame(start, aftertrials,correct_after,propcorrect_after, output$p.value, output$p.value < 0.05 )
}

Criteria_robust <-lapply(splitdat6, function(x) Robust(x))

unique(eg.t2.dat_5[,c(1,4)])
