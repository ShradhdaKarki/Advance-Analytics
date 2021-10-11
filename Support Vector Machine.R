library(caret)
library(ggplot2)


df_sk<-read.csv("BostonHousing.csv",na.strings=c("NA"," "))


df_sk$CHAS<-factor(df_sk$CHAS)
df_sk$CAT..MEDV<-factor(df_sk$CAT..MEDV)



trainIndex<-createDataPartition(df_sk$CAT..MEDV,
                                p=0.7,
                                list=FALSE,
                                times=1)
dfsk.train<-df_sk[trainIndex,]

dfsk.test<-df_sk[-trainIndex,]


# Create 10-fold cross validataion with trainControl() function
trControl <- trainControl(method='cv',
                          number=10,
                          search='grid')


# SVM Model with the linear Kernel function
# Pre-processing data with centering and scaling
svm_linear <- train(CAT..MEDV~.,
                    data=dfsk.train,
                    method='svmLinear',
                    trControl=trControl,
                    preProcess=c('center','scale'))

print(svm_linear)

# Evaluate the linear SVM model performance
linear_pred <- predict(svm_linear,dfsk.test)
confusionMatrix(linear_pred,dfsk.test$CAT..MEDV)

# SVM Model with the Radial Kernel function
svm_radial <- train(CAT..MEDV~.,
                    data=dfsk.train,
                    method='svmRadial',
                    trControl=trControl,
                    preProcess=c('center','scale'))

print(svm_radial)

# Evaluate the radial SVM model performance
radial_pred <- predict(svm_radial,dfsk.test)
confusionMatrix(radial_pred,dfsk.test$CAT..MEDV)


# Additional model tuning for the radial SVM model
grid_radial <- expand.grid(sigma = c(0.0,0.5,0.75,1.0,1.3,1.5),
                           C = c(0,0.05, 0.25, 0.5, 0.75, 1))

svm_radial_tune <- train(CAT..MEDV~.,
                         data=dfsk.train,
                         method='svmRadial',
                         trControl=trControl,
                         preProcess=c('center','scale'),
                         tuneGrid=grid_radial)
print(svm_radial_tune)

# Evaluate the radial SVM model performance
radial_tune_pred <- predict(svm_radial_tune,dfsk.test)
confusionMatrix(radial_tune_pred,dfsk.test$CAT..MEDV)


