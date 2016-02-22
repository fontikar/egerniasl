#Setting working directory
setwd("C:/Users/xufeng/Dropbox/social learning")

#Reading data
eg.t1.dat <- read.csv("data/Task1.csv", stringsAsFactors = FALSE)
head(eg.t1.dat)
str(eg.t1.dat)

#Changing variable types
eg.t1.dat$LizardID <- as.factor(eg.t1.dat$LizardID)
eg.t1.dat$Treatment <- as.factor(eg.t1.dat$Treatment)
eg.t1.dat$Date <-as.Date(eg.t1.dat$Date, format="%m/%d/%Y")
eg.t1.dat$Time <- as.factor(eg.t1.dat$Time)

#Excluding NA in correct
noNAs <-complete.cases(eg.t1.dat[,8])
eg.t1.dat_2 <- eg.t1.dat[noNAs,]  
  
#Split data 
splitdat<-split(eg.t1.dat_2, eg.t1.dat_2$LizardID)

 #Remove lizards that did not learn first 

eg.t1.dat_3<-eg.t1.dat_2[!eg.t1.dat_2$LizardID == "1468476",]

eg.t1.dat_3<-eg.t1.dat_3[!eg.t1.dat_3$LizardID == "1468532",]

eg.t1.dat_3<-eg.t1.dat_3[!eg.t1.dat_3$LizardID == "3374264",]


#Change levels of LizardID
str(eg.t1.dat_3)

eg.t1.dat_3$LizardID<-factor(eg.t1.dat_3$LizardID)

str(eg.t1.dat_3)

splitdat2<-split(eg.t1.dat_3, eg.t1.dat_3$LizardID)

#Update trial number because we excluded NA corrects

trialNum <- function(x){
{
  x$Trial<-seq(1:nrow(x))
}
x
}

  splitdat2 <- as.vector(lapply(splitdat2, function(x) trialNum(x)))

#"lt_1" variable, learning criteria

Criteria<-function(x){
{
  lt <- c()
  for(i in 1:nrow(x)){ 
    if(sum(x$Correct[i:(i+5)]) >= 5 && length(lt) < 1){
      lt <- rep(1, i+6)
      lt[length(lt):nrow(x)] <- rep(0)
    } else { lt <- lt }
  }
  x$lt<-lt
}
x
}

  splitdat3 <-lapply(splitdat2, function(x) Criteria(x))
  
  eg.t1.dat_4 <-unsplit(splitdat3, eg.t1.dat_3$LizardID)  

#Putting back excluded lizards that did not learn


liz76dat <-eg.t1.dat_2[eg.t1.dat_2$LizardID == "1468476",]
liz76dat <- trialNum(liz76dat) 
liz76dat$lt <- rep(1, nrow(liz76dat))


liz32dat<-eg.t1.dat_2[eg.t1.dat_2$LizardID == "1468532",]
liz32dat <-trialNum(liz32dat)
liz32dat$lt <- rep(1, nrow(liz32dat))

liz64dat <- eg.t1.dat_2[eg.t1.dat_2$LizardID == "3374264",]
liz64dat <-trialNum(liz64dat)
liz64dat$lt <- rep(1, nrow(liz64dat))

liz76.32dat<-merge(liz76dat, liz32dat, all=T)

nolearn_liz_merged <- merge(liz76.32dat, liz64dat, all = T)

#Put it all together, no learn lizards and lizards that learnt and then split by lizardid

nrow(eg.t1.dat_4)

eg.t1.dat_5 <- merge(eg.t1.dat_4, nolearn_liz_merged, all=T)
nrow(eg.t1.dat_5)

splitdat4<-split(eg.t1.dat_5, eg.t1.dat_5$LizardID)

#Learnt or not variable 

Learnt<-function(x){  
{
  if(0 %in% x$lt){
    x$learnt<-rep(1,length(nrow(x)))
  }else x$learnt<-rep(0,length(nrow(x)))
}
x
}

  splitdat5<-lapply(splitdat4, function(x) Learnt(x))

#Unsplit and save as new file

  eg.t1.dat_6<-unsplit(splitdat5, eg.t1.dat_5$LizardID)
  getwd()
  write.csv(eg.t1.dat_6, "output/data/task1_final.csv")
  





















