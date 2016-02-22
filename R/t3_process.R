#Setting working directory
setwd("C:/Users/xufeng/Dropbox/social learning")

#Reading data
eg.t3.dat <- read.csv("data/Task3.csv", stringsAsFactors = FALSE)
head(eg.t3.dat)
str(eg.t3.dat)

#Changing variable types
eg.t3.dat$LizardID <- as.factor(eg.t3.dat$LizardID)
eg.t3.dat$Treatment <- as.factor(eg.t3.dat$Treatment)
eg.t3.dat$Date <-as.Date(eg.t3.dat$Date, format="%m/%d/%Y")
eg.t3.dat$Time <- as.factor(eg.t3.dat$Time)

#Excluding NA in correct
noNAs <-complete.cases(eg.t3.dat[,8])
eg.t3.dat_2 <- eg.t3.dat[noNAs,] 

#Split data 
splitdat<-split(eg.t3.dat_2, eg.t3.dat_2$LizardID)
head(splitdat)

#Remove lizards that did not learn first 1
eg.t3.dat_3<-eg.t3.dat_2[!eg.t3.dat_2$LizardID == "1468506",]

eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "1469223",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "1468535",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "1468476",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "1468532",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "3368899",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "1468509",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "3372126",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "1469222",]

eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "3374160",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "1470760",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "1469178",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "1468504",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "3371239",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "1468529",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "3371916",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "1468538",]

eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "1468502",]
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "1468549",]


#Lizard that learnt
eg.t3.dat_3<-eg.t3.dat_3[!eg.t3.dat_3$LizardID == "1468460",]

str(eg.t3.dat_3)

eg.t3.dat_3$LizardID<-factor(eg.t3.dat_3$LizardID)

splitdat2<-split(eg.t3.dat_3, eg.t3.dat_3$LizardID)

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

  eg.t3.dat_4<-unsplit(splitdat3, eg.t3.dat_3$LizardID)
  str(eg.t3.dat_4)
  

#Putting back excluded lizards that did not learn


liz8476dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1468476",]
liz8476dat <- trialNum(liz8476dat) 
liz8476dat$lt <- rep(1, nrow(liz8476dat))
liz8476dat 

liz8502dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1468502",]
liz8502dat <- trialNum(liz8502dat) 
liz8502dat$lt <- rep(1, nrow(liz8502dat))
liz8502dat 

liz8504dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1468504",]
liz8504dat <- trialNum(liz8504dat) 
liz8504dat$lt <- rep(1, nrow(liz8504dat))
liz8504dat 

liz8506dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1468506",]
liz8506dat <- trialNum(liz8506dat) 
liz8506dat$lt <- rep(1, nrow(liz8506dat))
liz8506dat 

liz8509dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1468509",]
liz8509dat <- trialNum(liz8509dat) 
liz8509dat$lt <- rep(1, nrow(liz8509dat))
liz8509dat

liz8529dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1468529",]
liz8529dat <- trialNum(liz8529dat) 
liz8529dat$lt <- rep(1, nrow(liz8529dat))
liz8529dat

liz8532dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1468532",]
liz8532dat <- trialNum(liz8532dat) 
liz8532dat$lt <- rep(1, nrow(liz8532dat))
liz8532dat

liz8535dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1468535",]
liz8535dat <- trialNum(liz8535dat) 
liz8535dat$lt <- rep(1, nrow(liz8535dat))
liz8535dat

liz8538dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1468538",]
liz8538dat <- trialNum(liz8538dat) 
liz8538dat$lt <- rep(1, nrow(liz8538dat))
liz8538dat

liz8549dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1468549",]
liz8549dat <- trialNum(liz8549dat) 
liz8549dat$lt <- rep(1, nrow(liz8549dat))
liz8549dat

liz9178dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1469178",]
liz9178dat <- trialNum(liz9178dat) 
liz9178dat$lt <- rep(1, nrow(liz9178dat))
liz9178dat

liz9222dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1469222",]
liz9222dat <- trialNum(liz9222dat) 
liz9222dat$lt <- rep(1, nrow(liz9222dat))
liz9222dat

liz9223dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1469223",]
liz9223dat <- trialNum(liz9223dat) 
liz9223dat$lt <- rep(1, nrow(liz9223dat))
liz9223dat

liz0760dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1470760",]
liz0760dat <- trialNum(liz0760dat) 
liz0760dat$lt <- rep(1, nrow(liz0760dat))
liz0760dat

liz8899dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "3368899",]
liz8899dat <- trialNum(liz8899dat) 
liz8899dat$lt <- rep(1, nrow(liz8899dat))
liz8899dat

liz1239dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "3371239",]
liz1239dat <- trialNum(liz1239dat) 
liz1239dat$lt <- rep(1, nrow(liz1239dat))
liz1239dat

liz1916dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "3371916",]
liz1916dat <- trialNum(liz1916dat) 
liz1916dat$lt <- rep(1, nrow(liz1916dat))
liz1916dat

liz2126dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "3372126",]
liz2126dat <- trialNum(liz2126dat) 
liz2126dat$lt <- rep(1, nrow(liz2126dat))
liz2126dat

liz4160dat <-eg.t3.dat_2[eg.t3.dat_2$LizardID == "3374160",]
liz4160dat <- trialNum(liz4160dat) 
liz4160dat$lt <- rep(1, nrow(liz4160dat))
liz4160dat

#Putting lizard that did learn but on last trial
liz8460dat<-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1468460",]
liz8460dat <- trialNum(liz8460dat) 
liz8460dat$lt <- rep(1, nrow(liz8460dat))


#Merging


liz8476.8502dat<-merge(liz8476dat, liz8502dat, all=T)
liz8476.8502.8504dat<-merge(liz8476.8502dat, liz8504dat, all=T)
liz8476.8502.8504.8506dat<-merge(liz8476.8502.8504dat, liz8506dat, all=T)
liz8476.8502.8504.8506.8509dat<-merge(liz8476.8502.8504.8506dat, liz8509dat, all=T)
liz8476.8502.8504.8506.8509.8529dat<-merge(liz8476.8502.8504.8506.8509dat, liz8529dat, all=T)
liz8476.8502.8504.8506.8509.8529.8532dat<-merge(liz8476.8502.8504.8506.8509.8529dat, liz8532dat, all=T)
liz8476.8502.8504.8506.8509.8529.8532.8535dat<-merge(liz8476.8502.8504.8506.8509.8529.8532dat, liz8535dat, all=T)
liz8476.8502.8504.8506.8509.8529.8532.8535.8538dat<-merge(liz8476.8502.8504.8506.8509.8529.8532.8535dat, liz8538dat, all=T)
liz8476.8502.8504.8506.8509.8529.8532.8535.8538.8549dat<-merge(liz8476.8502.8504.8506.8509.8529.8532.8535.8538dat, liz8549dat, all=T)
liz11dat<-merge(liz8476.8502.8504.8506.8509.8529.8532.8535.8538.8549dat,liz9178dat, all=T)

liz11dat$LizardID<-factor(liz11dat$LizardID)
str(liz11dat) #211 obs 11 lizards

liz11.9222dat<-merge(liz11dat,liz9222dat, all=T)
liz11.9222.9223dat<-merge(liz11.9222dat,liz9223dat, all=T)
liz11.9222.9223.0760dat<-merge(liz11.9222.9223dat,liz0760dat, all=T)
liz11.9222.9223.0760.8899dat<-merge(liz11.9222.9223.0760dat,liz8899dat, all=T)
liz11.9222.9223.0760.8899.1239dat<-merge(liz11.9222.9223.0760.8899dat,liz1239dat, all=T)
liz11.9222.9223.0760.8899.1239.1916dat<-merge(liz11.9222.9223.0760.8899.1239dat,liz1916dat, all=T)
liz11.9222.9223.0760.8899.1239.1916.2126dat<-merge(liz11.9222.9223.0760.8899.1239.1916dat,liz2126dat, all=T)
nolearn_liz_mergeA<-merge(liz11.9222.9223.0760.8899.1239.1916.2126dat,liz4160dat, all=T)

nolearn_liz_mergeB<-merge(nolearn_liz_mergeA, liz8460dat, all=T)
nolearn_liz_mergeB$LizardID<-factor(nolearn_liz_mergeB$LizardID)
str(nolearn_liz_mergeB) #373obs 20 lizards

str(eg.t3.dat_4)

eg.t3.dat_5 <- merge(eg.t3.dat_4, nolearn_liz_mergeB, all=T)
str(eg.t3.dat_5)

splitdat4<-split(eg.t3.dat_5, eg.t3.dat_5$LizardID)

Learnt<-function(x){  
  {
    if(0 %in% x$lt){
      x$learnt<-rep(1,length(nrow(x)))
    }else x$learnt<-rep(0,length(nrow(x)))
  }
  x
}

splitdat5<-lapply(splitdat4, function(x) Learnt(x))

eg.t3.dat_6<-unsplit(splitdat5, eg.t3.dat_5$LizardID)
str(eg.t3.dat_6)


#Fix lizard that learnt
liz8460dat<-eg.t3.dat_2[eg.t3.dat_2$LizardID == "1468460",]
liz8460dat <- trialNum(liz8460dat) 
liz8460dat$lt <- rep(1, nrow(liz8460dat))

eg.t3.dat_6$learnt[89:103] <- rep(1, 15)

eg.t3.dat_6[eg.t3.dat_6$LizardID == "1468460", 12]

write.csv(eg.t3.dat_6, "~/Dropbox/social learning/output/data/task3_final.csv")

#gc() 
#rm(list = ls())

getwd()
