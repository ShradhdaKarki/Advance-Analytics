library(caret)
library(pROC)
library(corrplot)
library(factoextra)

df <- read.csv('Fundraising.csv')

df$Donor <- factor(df$Donor)
df$zipcode <- factor(df$zipcode)
df$homeowner.dummy <- factor(df$homeowner.dummy)
df$gender.dummy <- factor(df$gender.dummy)

trainIndex <- createDataPartition(df$Donor,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)

df.train <- df[trainIndex,]
df.valid <-df[-trainIndex,]

model1 <- train(Donor~.,
                data=df.train,
                method="glm",
                family="binomial")
summary(model1)

# Evaluate Model 1 performance
m1.prediction <- predict(model1,df.valid)
confusionMatrix(m1.prediction,df.valid$Donor)


# Model 2: PCA with all numeric variables, remove categorical variables 
df_num <- df[,c(3, 5:11)]

pca <- prcomp(df_num,center=TRUE,scale. = TRUE)

print(pca)
plot(pca,type='l')

summary(pca)  # standard deviation value is the eigenvalue


# Plot PCA

fviz_eig(pca)
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Predict PCA
pred_pca <- predict(pca, newdata=df_num)

# Create dataset for PC values and categorical variables
df2 <- cbind.data.frame(df[,c(1:2, 4, 12)],pred_pca[,c(1:3)])

df2.train <- df2[trainIndex,]
df2.valid <-df2[-trainIndex,]

# Model 2 with PCA-all numbers
model2 <- train(Donor~.,
                data=df2.train,
                method="glm",
                family="binomial")
summary(model2)
# Evaluate Model 2 performance
m2.prediction <- predict(model2,df2.valid)
confusionMatrix(m2.prediction,df2.valid$Donor)


# Run PCA with selective numerical variables that should be combined

M <- cor(df_num)
corrplot(M,type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#creating new pca just for those 3 variables
df_num2 <- df[,c(6:8)]
pca2 <- prcomp(df_num2,center=TRUE,scale. = TRUE)
print(pca2)
plot(pca2,type='l')
summary(pca2)

# Predict PCA-Selected columns
pred_pca2 <- predict(pca2, newdata=df_num2)

df3 <- cbind.data.frame(df[,c(1:5,9:12)],pred_pca2[,1])
names(df3)[10]='PC1'

df3.train <- df3[trainIndex,]
df3.valid <-df3[-trainIndex,]

# Model 3 with PCA-selected numbers
model3 <- train(Donor~.,
                data=df3.train,
                method="glm",
                family="binomial")
summary(model3)
# Evaluate Model 2 performance
m3.prediction <- predict(model3,df3.valid)
confusionMatrix(m3.prediction,df3.valid$Donor)







