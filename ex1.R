train<-read.csv("train.csv")

td = tempfile()
dir.create(td)
results <- data.frame(queryId = numeric(), sim = numeric())

library(tm)

for (i in 1:1749)
{
  
  #preproccesing to query
  
  queries <- paste(train$query[i], collapse=" ")
  
  queries_source <- VectorSource(queries)
  
  corpus <- Corpus(queries_source)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  
  corpus <- tm_map(corpus, stripWhitespace)
  
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  
  dtm <- DocumentTermMatrix(corpus)
  
  query<-dtm$dimnames$Terms
  
  
  #preproccesing to title
  title<-paste(train$product_title[i], collapse=" ")
  
  queries_source <- VectorSource(title)
  
  corpus <- Corpus(queries_source)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  
  corpus <- tm_map(corpus, stripWhitespace)
  
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  
  dtm <- DocumentTermMatrix(corpus)
  title<-dtm$dimnames$Terms
  
  #cosSimilarity between title and query
  
  write( query, file=paste(td, "D1", sep="/"))
  write( title, file=paste(td, "D2", sep="/"))
  myMatrix = textmatrix(td, minWordLength=1)
  
  similaryTitleQuery <- lsa::cosine(myMatrix[,1], myMatrix[,2])
  
  
  #preproccesing to description
  desciption<-paste(train$product_description[i], collapse=" ")
  
  queries_source <- VectorSource(desciption)
  
  corpus <- Corpus(queries_source)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  
  corpus <- tm_map(corpus, stripWhitespace)
  
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  
  dtm <- DocumentTermMatrix(corpus)
  desciption<-dtm$dimnames$Terms
  
  write( desciption, file=paste(td, "D2", sep="/"))
  if()
  {
  myMatrix = textmatrix(td, minWordLength=1)
  
  similaryContentQuery <- lsa::cosine(myMatrix[,1], myMatrix[,2])
  
  totalSimilarity<-0.8*similaryTitleQuery[1,1]+0.2*similaryContentQuery[1,1]
  }
  
  else
  {
    totalSimilarity<-similaryTitleQuery[1,1]
  }
  results<-rbind(results, data.frame(queryId =train$id[i] , sim = totalSimilarity))
  
 

}



#preproccesing to query

queries <- paste(train$query[3], collapse=" ")

queries_source <- VectorSource(queries)

corpus <- Corpus(queries_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, removeWords, stopwords("english"))


dtm <- DocumentTermMatrix(corpus)

query<-dtm$dimnames$Terms


#preproccesing to title
title<-paste(train$product_title[3], collapse=" ")

queries_source <- VectorSource(title)

corpus <- Corpus(queries_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, removeWords, stopwords("english"))


dtm <- DocumentTermMatrix(corpus)
title<-dtm$dimnames$Terms

#cosSimilarity between title and query

write( query, file=paste(td, "D1", sep="/"))
write( title, file=paste(td, "D2", sep="/"))
myMatrix = textmatrix(td, minWordLength=1)

similaryTitleQuery <- lsa::cosine(myMatrix[,1], myMatrix[,2])


#preproccesing to description
desciption<-paste(train$product_description[3], collapse=" ")

queries_source <- VectorSource(desciption)

corpus <- Corpus(queries_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, removeWords, stopwords("english"))


dtm <- DocumentTermMatrix(corpus)
desciption<-dtm$dimnames$Terms

write( desciption, file=paste(td, "D2", sep="/"))
myMatrix = textmatrix(td, minWordLength=1)

similaryContentQuery <- lsa::cosine(myMatrix[,1], myMatrix[,2])

totalSimilarity<-0.8*similaryTitleQuery[1,1]+0.2*similaryContentQuery[1,1]


train$product_description[3]
