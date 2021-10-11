library(caret)
library(pROC)
library(rpart)
library(rpart.plot)


dfSK<-read.csv("inq2019.csv",na.strings=c("NA"," "))

str(dfSK)
drops<-c("ACADEMIC_INTEREST_1", "ACADEMIC_INTEREST_2","IRSCHOOL","CONTACT_CODE1","CONTACT_DATE","sex","ETHNICITY","LEVEL_YEAR")
dfSK<-dfSK[,!(names(dfSK) %in% drops)]

dfSK$Enroll<-factor(dfSK$Enroll)
dfSK$TERRITORY<-factor(dfSK$TERRITORY)
dfSK$CAMPUS_VISIT<-factor(dfSK$CAMPUS_VISIT)
dfSK$stucell<-factor(dfSK$stucell)
dfSK$premiere<-factor(dfSK$premiere)

trainIndex <- createDataPartition(dfSK$Enroll,p=0.7,list=FALSE,times=1)
dfSK.train<-dfSK[trainIndex,]
dfSK.valid<-dfSK[-trainIndex,]


# Build a decision tree model
tree.model <- train(Enroll~., 
                    data=dfSK.train,
                    method="rpart",
                    na.action=na.pass)
tree.model


# Display decision tree 
prp(tree.model$finalModel,type=2,extra=106)

prediction <- predict(tree.model,newdata=dfSK.valid,na.action = na.pass)
confusionMatrix(prediction,dfSK.valid$Enroll)


tree.probabilities <- predict(tree.model,newdata=dfSK.valid,type='prob',na.action=na.pass)
tree.ROC <- roc(predictor=tree.probabilities$`1`,
                response=dfSK.valid$Enroll,                      
                levels=levels(dfSK.valid$Enroll))

plot(tree.ROC)
tree.ROC$auc
