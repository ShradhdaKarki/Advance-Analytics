library(quanteda)
library(stopwords)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(quanteda.textstats)
library(quanteda.textmodels)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(quanteda.textplots)

text_sk<-read.csv('gastext.csv',stringsAsFactors = F)

corpus_sk<-corpus(text_sk$Comment)
summary(corpus_sk)
mydfm_sk<-dfm(corpus_sk)
dim(mydfm_sk)
summary(mydfm_sk)

#tstat_freq <- textstat_frequency(myDfm_gram)
#head(tstat_freq, 20)
# Create an unigram
# myTokens <- tokens(corpus_sk)
# gram <- tokens_ngrams(myTokens,n=1)
# myDfm_gram <- dfm(gram)
# View(myDfm_gram)



#Preprocess, removing stopwords, puntuation and stemming 
myDfm <- dfm(corpus_sk,
             remove_punc = T,
             remove = c(stopwords("english")),
             stem = T)
dim(myDfm)

topfeatures(myDfm,40)

#remove user defined stop words
stopwords1<-c('use','get','can','alway','take')
myDfm <- dfm(myDfm,
             remove_punc = T,
             remove=c(stopwords('english'),stopwords1),
             stem = T) 

textplot_wordcloud(myDfm,max_words=200)


topfeatures(myDfm,40)



# Control sparse terms: to further remove some very infrequency words
myDfm<- dfm_trim(myDfm,min_termfreq=4, min_docfreq=2)
dim(myDfm)

text_sim1 <- textstat_simil(myDfm, 
                           selection="price",
                           margin="feature",
                           method="cosine")


as.list(text_sim1,n=5)

text_sim2 <- textstat_simil(myDfm, 
                           selection="shower",
                           margin="feature",
                           method="cosine")
as.list(text_sim2,n=5)


##Topic modeling


myDfm <- dfm_remove(myDfm, c('shower','point'))
myDfm <- as.matrix(myDfm)
myDfm <-myDfm[which(rowSums(myDfm)>0),]
myDfm <- as.dfm(myDfm)


myLda <- LDA(myDfm,k=4,control=list(seed=101))
myLda

myLda_td<-tidy(myLda)
myLda_td


#visualize most common terms in each topic

top_terms <- myLda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# View topic 8 terms in each topic
Lda_term<-as.matrix(terms(myLda,8))
View(Lda_term)


# Document-topic probabilities
ap_documents <- tidy(myLda, matrix = "gamma")
ap_documents

# View document-topic probabilities in a table
Lda_document<-as.data.frame(myLda@gamma)
View(Lda_document)


### Decision modeling
# Pre-process the training corpus
modelDfm <- dfm(corpus_sk,
                remove_punc = T,
                remove=c(stopwords('english'),stopwords1),
                stem = T) 

dim(modelDfm)


# Further remove very infrequent words 
modelDfm <- dfm_trim(modelDfm,min_termfreq=4, min_docfreq = 2)

dim(modelDfm)


# Weight the predictiv DFM by tf-idf
modelDfm_tfidf <- dfm_tfidf(modelDfm)
dim(modelDfm_tfidf)

# Perform SVD for dimension reduction
# Choose the number of reduced dimensions as 10
modelSvd <- textmodel_lsa(modelDfm_tfidf, nd=10)
head(modelSvd$docs)


#combined dataframe
modelData2 <-cbind(text_sk,as.data.frame(modelSvd$docs))
View(modelData2)

modelData2<-modelData2[-c(1,2)]


#changing the columns into factors
modelData2[,1:13]<-lapply(modelData2[,1:13],factor)



#datapartitioning for decision tree
trainIndex <- createDataPartition(modelData2$Target,p=0.7,list=FALSE,times=1)


modelData2.train<-modelData2[trainIndex,]
modelData2.valid<-modelData2[-trainIndex,]


# Build a decision tree model
tree.model <- train(Target~., 
                    data=modelData2.train,
                    method="rpart",
                    na.action=na.pass)
tree.model


# Display decision tree 
prp(tree.model$finalModel,type=2,extra=100)

prediction <- predict(tree.model,newdata=modelData2.valid,na.action = na.pass)
confusionMatrix(prediction,modelData2.valid$Target)












#Model 1
modelData1<-text_sk[-c(1,2)]
modelData1[,1:13]<-lapply(modelData1[,1:13],factor)

trainIndex <- createDataPartition(modelData1$Target,p=0.7,list=FALSE,times=1)


modelData1.train<-modelData1[trainIndex,]
modelData1.valid<-modelData1[-trainIndex,]


# Build a decision tree model
tree.model <- train(Target~., 
                    data=modelData1.train,
                    method="rpart",
                    na.action=na.pass)
tree.model


# Display decision tree 
prp(tree.model$finalModel,type=2,extra=100)

prediction <- predict(tree.model,newdata=modelData1.valid,na.action = na.pass)
confusionMatrix(prediction,modelData1.valid$Target)


















