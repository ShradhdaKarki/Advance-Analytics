library(dbplyr)
library(dplyr)
library(tidyverse)
library(magrittr)
library(sqldf)
library(tidytext)
library(wordcloud)
library(reshape2)
library(syuzhet)
library(textdata)
library(ggplot2)
library(gridExtra)


df<-read.csv('reviews.csv')


#df<-df%>%mutate(date=as.Date(date,format="%Y-%m-%d"))


#claimsData<-claimsData %>% mutate(IncidentDate=as.Date(IncidentDate, format="%m/%d/%Y"))%>%
summary(df)


#subset(df,format(as.Date(date),"%Y")==2020)

#filter.date(df$date,date.start="2020-01-01",date.end="2020-06-30")

new_df<-sqldf("select listing_id, id,date,reviewer_id,comments
              from df
              where date>='2020-01-01' AND date<='2020-06-30'
              ")
jan<-sqldf("select listing_id, id,date,reviewer_id,comments
              from new_df
              where date>='2020-01-01' AND date<='2020-01-31'
              ")
feb<-sqldf("select listing_id, id,date,reviewer_id,comments
              from new_df
              where date>='2020-02-01' AND date<='2020-02-28'
              ")
mar<-sqldf("select listing_id, id,date,reviewer_id,comments
              from new_df
              where date>='2020-03-01' AND date<='2020-03-31'
              ")

apr<-sqldf("select listing_id, id,date,reviewer_id,comments
              from new_df
              where date>='2020-04-01' AND date<='2020-04-30'
              ")

may<-sqldf("select listing_id, id,date,reviewer_id,comments
              from new_df
              where date>='2020-04-01' AND date<='2020-05-31'
              ")

june<-sqldf("select listing_id, id,date,reviewer_id,comments
              from new_df
              where date>='2020-06-01' AND date<='2020-06-30'
              ")


jan$month<-"Jan"
feb$month<-"Feb"
mar$month<-"Mar"
apr$month<-"Apr"
may$month<-"May"
june$month<-"June"

new_data<-rbind(jan,feb,mar,apr,may,june)
new_data$line <- 1:nrow(new_data) 
new_data$line<-as.character(new_data$line)

jan%>%unnest_tokens(word,comments)%>%
  anti_join(stop_words) %>% #remove stop words
  count(word, sort = T) %>% 
  mutate(word = str_extract(word, "[a-z]+")) %>% 
  na.omit() %>%
  #unnest_tokens(word,comments,token="ngrams",n=2)
  top_n(30)%>%
    #filter(n > 5) %>% #Extract words with frequencies > 20
  ggplot(., aes(reorder(word, n), n)) +
  geom_bar(stat = "identity",fill='light blue') +
  coord_flip() +
  ylab("Score") +
  xlab("Words") + ggtitle("Word Frequency") +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 13),
        axis.title.y = element_text(face = "bold", size = 13))

  
  
#Get the sentiments Variation
new_data %>% 
  unnest_tokens(word, comments) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(month, index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  ggplot(.,aes(index,sentiment, fill = month)) +
  geom_col(show.legend = FALSE, width = 3) +
  facet_wrap(~month, ncol = 18, scales = "free_x") +
  ggtitle("Sentiments Variation") + 
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 13),
        axis.title.y = element_text(face = "bold", size = 13))+theme_dark()







#plot a comparison of postive and negative words used by reviewers
new_data %>% 
  
  unnest_tokens(word, comments) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(sentiment,month) %>% 
  count(word) %>% 
  top_n(10) %>% 
  ggplot(., aes(reorder(word, n), n,fill=sentiment)) +
  geom_col(show.legend = T) +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free_y") +
  facet_wrap(~month, scales = "free_y")
  xlab("Words") +
  ylab("frequency") +
  ggtitle("Word Usage") +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 13),
        axis.title.y = element_text(face = "bold", size = 13))+theme_dark()
  
  
  new_data %>% 
    
    unnest_tokens(word, comments) %>% 
    mutate(word = gsub("problems", "problem", word)) %>% 
    inner_join(get_sentiments("bing")) %>% 
    count(word, sentiment) %>% 
    acast(word~sentiment, value.var = "n", fill = 0) %>% 
    comparison.cloud(color = c("red", "green"),
                     max.words = 100)+theme_dark()
  
  #%>%ggplot()+facet_wrap(~month, scales = "free_y")
  # Note the acast function is from the reshape2 package
  # Functions such as comparison.cloud() require you to turn the data frame into a matrix with reshape2's acast()

 
  #nrc sentiment
 h6<- june%>%
    #group_by(month='June')%>%
    unnest_tokens(word, comments) %>% right_join(get_sentiments("nrc")) %>%
    anti_join(stop_words) %>% 
    mutate(word = str_extract(word, "[a-z]+")) %>% 
    na.omit() %>%
    #group_by(month) %>% 
    count(sentiment, sort = TRUE)%>%
    ggplot(.,aes(n,sentiment )) +
    geom_col() +
    guides(fill=FALSE)+
    #facet_wrap(~month, ncol = 18, scales = "free_x") +
    ggtitle("June sentiment") + 
    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
          axis.title.x = element_text(face = "bold", size = 13),
          axis.title.y = element_text(face = "bold", size = 13),
          #panel.background = element_blank(fill='black'),
          legend.background = element_rect(fill = "black", color = NA))+theme_classic()
          
  
  
    h5<- may%>%
      #group_by(month='June')%>%
      unnest_tokens(word, comments) %>% right_join(get_sentiments("nrc")) %>%
      anti_join(stop_words) %>% 
      mutate(word = str_extract(word, "[a-z]+")) %>% 
      na.omit() %>%
      #group_by(month='June') %>% 
      count(sentiment, sort = TRUE)%>%
      ggplot(.,aes(n,sentiment )) +
      geom_col() +
      guides(fill=FALSE)+
    #facet_wrap(~month, ncol = 18, scales = "free_x") +
    ggtitle("May sentiment") + 
      theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
            axis.title.x = element_text(face = "bold", size = 13),
            axis.title.y = element_text(face = "bold", size = 13))
  
  
  h4<- apr%>%
    #group_by(month='June')%>%
    unnest_tokens(word, comments) %>% right_join(get_sentiments("nrc")) %>%
    anti_join(stop_words) %>% 
    mutate(word = str_extract(word, "[a-z]+")) %>% 
    na.omit() %>%
    #group_by(month='June') %>% 
    count(sentiment, sort = TRUE)%>%
    ggplot(.,aes(n,sentiment )) +
    geom_col() +
    guides(fill=FALSE)+
  #facet_wrap(~month, ncol = 18, scales = "free_x") +
  ggtitle("April sentiment") + 
    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
          axis.title.x = element_text(face = "bold", size = 13),
          axis.title.y = element_text(face = "bold", size = 13))
  
  
  
  h3<- mar%>%
    #group_by(month='June')%>%
    unnest_tokens(word, comments) %>% right_join(get_sentiments("nrc")) %>%
    anti_join(stop_words) %>% 
    mutate(word = str_extract(word, "[a-z]+")) %>% 
    na.omit() %>%
    #group_by(month='June') %>% 
    count(sentiment, sort = TRUE)%>%
    ggplot(.,aes(n,sentiment )) +
    geom_col() +
    guides(fill=FALSE)+
  #facet_wrap(~month, ncol = 18, scales = "free_x") +
  ggtitle("March sentiment") + 
    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
          axis.title.x = element_text(face = "bold", size = 13),
          axis.title.y = element_text(face = "bold", size = 13))
  
  
  
  h2<- feb%>%
    #group_by(month='June')%>%
    unnest_tokens(word, comments) %>% right_join(get_sentiments("nrc")) %>%
    anti_join(stop_words) %>% 
    mutate(word = str_extract(word, "[a-z]+")) %>% 
    na.omit() %>%
    #group_by(month='June') %>% 
    count(sentiment, sort = TRUE)%>%
    ggplot(.,aes(n,sentiment )) +
    geom_col() +
    guides(fill=FALSE)+
  #facet_wrap(~month, ncol = 18, scales = "free_x") +
  ggtitle("Feb sentiment") + 
    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
          axis.title.x = element_text(face = "bold", size = 13),
          axis.title.y = element_text(face = "bold", size = 13))
  
  
  
  
  
  
  
  h1<- jan%>%
    #group_by(month='June')%>%
    unnest_tokens(word, comments) %>% right_join(get_sentiments("nrc")) %>%
    anti_join(stop_words) %>% 
    mutate(word = str_extract(word, "[a-z]+")) %>% 
    na.omit() %>%
    #group_by(month='June') %>% 
    count(sentiment, sort = TRUE)%>%
    ggplot(.,aes(n,sentiment )) +
    geom_col() +
    guides(fill=FALSE)+
  #facet_wrap(~month, ncol = 18, scales = "free_x") +
  ggtitle("Jan sentiment") + 
    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
          axis.title.x = element_text(face = "bold", size = 13),
          axis.title.y = element_text(face = "bold", size = 13))
  
  
  
 # multiplot(h1, h2, h3, h4,h5,h6, cols=2)
  
  grid.arrange(h1, h2, h3, h4,h5,h6,ncol=2)
  
  
  
  
  
  
  
  
  
  
  
  