library(caret)
library(e1071)
library(car)
library(pROC)
library(dplyr)
library(Hmisc)
library(tidyr)
library(ggplot2)


dfSK<-read.csv("inq2019.csv",na.strings=c("NA"," "))


summary(dfSK)
str(dfSK)

drops<-c("ACADEMIC_INTEREST_1", "ACADEMIC_INTEREST_2","IRSCHOOL","CONTACT_CODE1","CONTACT_DATE","sex","ETHNICITY","LEVEL_YEAR","satscore","telecq")
dfSK<-dfSK[,!(names(dfSK) %in% drops)]


#ACADEMIC_INTEREST_1, ACADEMIC_INTEREST_2, and IRSCHOOL
#CONTACT_CODE1 and CONTACT_DATE1
dfSK$Enroll<-factor(dfSK$Enroll)
dfSK$TERRITORY<-factor(dfSK$TERRITORY)
dfSK$CAMPUS_VISIT<-factor(dfSK$CAMPUS_VISIT)
dfSK$stucell<-factor(dfSK$stucell)
dfSK$premiere<-factor(dfSK$premiere)


vif(glm(formula=Enroll ~. , family = binomial(link='logit'),data = dfSK))

#drop rest of the variables that wont be used

drops1<-c("TOTAL_CONTACTS", "SELF_INIT_CNTCTS","TRAVEL_INIT_CNTCTS","SOLICITED_CNTCTS","REFERRAL_CNTCTS")
dfSK<-dfSK[,!(names(dfSK) %in% drops1)]


#impute missing values for avg_income and distance

dfSK$avg_income<-with(dfSK,impute(avg_income,mean))
dfSK$distance<-with(dfSK,impute(distance,mean))

# check histograms for all the coloumns for skewness

hist.data.frame(dfSK)

dfSK$distance<-log10(dfSK$distance+1)
pl <- ggplot(dfSK,aes(x=distance))+geom_histogram()


#pl2 <- ggplot(dfSK,aes(x=hscrat))+geom_histogram()
#dfSK$hscrat<-log10(dfSK$hscrat+1)

combine.hscrat <- function(x){
  if (is.na(x)){
    return(NA)
  }else if(x>0){
    return("High")
  }else{
    return("Low")
  }
}

dfSK$hscrat <- sapply(dfSK$hscrat,combine.hscrat)



trainIndex <- createDataPartition(dfSK$Enroll,p=0.7,list=FALSE,times=1)

#data partioning
dfSK.train<-dfSK[trainIndex,]
dfSK.valid<-dfSK[-trainIndex,]

baseline.model<-train(Enroll~.,
                      data=dfSK.train,
                      method='glm',
                      family='binomial',
                      na.action = na.pass)
summary(baseline.model)

prediction<-predict(baseline.model,newdata = dfSK.valid)

dfSK.valid.nomissing<-na.omit(dfSK.valid)

confusionMatrix(prediction,dfSK.valid.nomissing$Enroll)


#the ROC curve
pred.probabilities<-predict(baseline.model,newdata = dfSK.valid,type='prob')


regression.ROC <- roc(predictor=pred.probabilities$`1`,
                      response=dfSK.valid.nomissing$Enroll,
                      levels=levels(dfSK.valid.nomissing$Enroll))



plot(regression.ROC)
regression.ROC$auc

# User-defined functions to calculate cumulative lift & gains
lift <- function(depvar, predcol, groups=10) {
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  
  helper <- data.frame(cbind(depvar, predcol))
  helper <- helper[order(-helper$predcol),] 
  
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Apply the user-defined lift function 
dt=lift(dfSK.valid.nomissing$Enroll,pred.probabilities$`1`,groups=10)
print(dt)

# Plot the cumulative lift chart
plot(dt$bucket, dt$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")















