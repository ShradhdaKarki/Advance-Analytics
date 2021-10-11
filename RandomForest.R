
library(ISLR)
library(caret)
library(randomForest)

df_sk<-read.csv("BostonHousing.csv",na.strings=c("NA"," "))

summary(df_sk)
str(df_sk)



df_sk$CHAS<-factor(df_sk$CHAS)
df_sk$CAT..MEDV<-factor(df_sk$CAT..MEDV)


trainIndex<-createDataPartition(df_sk$CAT..MEDV,
                                 p=0.7,
                                 list=FALSE,
                                 times=1)
dfsk.train<-df_sk[trainIndex,]

dfsk.test<-df_sk[-trainIndex,]


rf_default <- train(CAT..MEDV~.,
                    data=dfsk.train,
                    method='rf',
                    metric='Accuracy',
                    ntree=100)
print(rf_default)


# More detailed model tuning to search the best mtry
tuneGrid <- expand.grid(.mtry=c(1:17))
rf_mtry <- train(CAT..MEDV~.,
                 data=dfsk.train,
                 method='rf',
                 metric='Accuracy',
                 tuneGrid=tuneGrid,
                 importance=TRUE,
                 ntree=100)
print(rf_mtry)


# Evaluate model performance
prediction <- predict(rf_mtry,dfsk.test)
confusionMatrix(prediction,dfsk.test$CAT..MEDV)

# Variable importance
varImp(rf_mtry)
