

library(readr)
train <- read_csv("train.csv")
td = tempfile()

dir.create(td)
train_After_Stemming <- data.frame(id = numeric(), query = character(),product_title = character(), product_description = character(), median_relevance = numeric(), relevance_variance = numeric())

library(tm)
library(lsa)


for (i in 1:10158)
{

  #preproccesing to query
  queries <- paste(train$query[i], collapse=" ")
  query=NULL
  if (length(queries)==1)
  {
    current_query=queries
  } else {
    queries_source <- VectorSource(queries)

    corpus <- Corpus(queries_source)

    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)

    corpus <- tm_map(corpus, stripWhitespace)

    corpus <- tm_map(corpus, removeWords, stopwords("english"))


    dtm <- DocumentTermMatrix(corpus)

    current_query<-dtm$dimnames$Terms
  }



  #preproccesing to title
  title<-paste(train$product_title[i], collapse=" ")

  queries_source <- VectorSource(title)

  corpus <- Corpus(queries_source)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)

  corpus <- tm_map(corpus, stripWhitespace)

  corpus <- tm_map(corpus, removeWords, stopwords("english"))


  dtm <- DocumentTermMatrix(corpus)
  current_title<-dtm$dimnames$Terms

  total_title=""
  for (word in current_title)
  {
    total_title<-paste(total_title,word)
  }


  #preproccesing to description
  desciption<-paste(train$product_description[i], collapse=" ")
  d=TRUE
  if(desciption!=""){
    d=FALSE
  }

  queries_source <- VectorSource(desciption)

  corpus <- Corpus(queries_source)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)

  corpus <- tm_map(corpus, stripWhitespace)

  corpus <- tm_map(corpus, removeWords, stopwords("english"))


  dtm <- DocumentTermMatrix(corpus)
  desciption<-dtm$dimnames$Terms
  desciption<-strsplit(gsub("[^[:alnum:] ]", "", desciption), " +")
  current_desc<-as.character(desciption)
  
  total_desc=""
  for (word in current_desc)
  {
    total_desc<-paste(total_desc,word)
  }
  
  
  median <- train$median_relevance[i]
  var <- train$relevance_variance[i]
  
  #train_After_Stemming<-rbind(train_After_Stemming, data.frame(id =train$id[i] , query = current_query,product_title = title, product_description = desciption, median_relevance = median, relevance_variance = var))
  train_After_Stemming<-rbind(train_After_Stemming, data.frame(id =train$id[i] , query = current_query,product_title = total_title, product_description = total_desc, median_relevance = median, relevance_variance = var))

}



write.csv(file="train_after_stemming.csv", x=train_After_Stemming)

















