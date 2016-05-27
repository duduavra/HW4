

library(readr)
train <- read_csv("train_after_stemming.csv")

td = tempfile()


dir.create(td)
results <- data.frame(queryId = numeric(), simTitle = numeric(),simDescription = numeric())

library(tm)
library(lsa)

for (i in 1:200)
{
  
  #preproccesing to query
  queries <- paste(train$query[i], collapse=" ")
  query=NULL
  if (length(queries)==1)
  {
    query=queries
  } else {
    queries_source <- VectorSource(queries)
    
    corpus <- Corpus(queries_source)

    dtm <- DocumentTermMatrix(corpus)
    
    query<-dtm$dimnames$Terms
  }
  
  
  
  #preproccesing to title
  title<-paste(train$product_title[i], collapse=" ")
  queries_source <- VectorSource(title)
  
  corpus <- Corpus(queries_source)
  dtm <- DocumentTermMatrix(corpus)
  title<-dtm$dimnames$Terms
  
 
  
  #cosSimilarity between title and query
  
  write( query, file=paste(td, "D1", sep="/"))
  write( title, file=paste(td, "D2", sep="/"))
  myMatrix = textmatrix(td, minWordLength=1)
  
  similaryTitleQuery <- lsa::cosine(myMatrix[,1], myMatrix[,2])
  
  
  #preproccesing to description
  desciption<-paste(train$product_description[i], collapse=" ")
  d=TRUE
  if(desciption!=""){
    d=FALSE
  }
  
  queries_source <- VectorSource(desciption)
  
  corpus <- Corpus(queries_source)
  
  
  
  dtm <- DocumentTermMatrix(corpus)
  desciption<-dtm$dimnames$Terms
  desciption<-strsplit(gsub("[^[:alnum:] ]", "", desciption), " +")
  desciption<-as.character(desciption)
  
  write( desciption, file=paste(td, "D2", sep="/"))
  
  if(d==FALSE)
  {
    myMatrix = textmatrix(td, minWordLength=1)
    
    similaryContentQuery <- lsa::cosine(myMatrix[,1], myMatrix[,2])
    
    
    
  }else {
    
    similaryContentQuery<-0
  }
  
  results<-rbind(results, data.frame(queryId =train$id[i] , simTitle = similaryTitleQuery,simDescription=similaryContentQuery))
  
  
  
}


results["Median_Rating"] <- train$median_relevance


write.csv(file="trainCosine.csv", x=results,  row.names = F)

train1 <- read_csv("trainCosine.csv")

train1$queryId=NULL

train1$Median_Rating <- as.factor(train1$Median_Rating)

library(RWeka)
library(partykit)

m1<-J48(Median_Rating~., data = train1)
summary(m1)








#load test set
test <- read_csv("test_after_stemming.csv")

test_featuers<-data.frame(simTitle = numeric(), simDescription = numeric())

for (i in 1:200)
{

  #preproccesing to query
  queries <- paste(test$query[i], collapse=" ")
  query=NULL
  if (length(queries)==1)
  {
    query=queries
  } else {
    queries_source <- VectorSource(queries)

    corpus <- Corpus(queries_source)

    dtm <- DocumentTermMatrix(corpus)

    query<-dtm$dimnames$Terms
  }




  #preproccesing to title
  title<-paste(test$product_title[i], collapse=" ")
  titlep=NULL

  if (length(queries)==1)
  {
    titlep=title
  } else {
    queries_source <- VectorSource(title)

    corpus <- Corpus(queries_source)

    dtm <- DocumentTermMatrix(corpus)
    titlep<-dtm$dimnames$Terms
  }


  #cosSimilarity between title and query

  write( query, file=paste(td, "D1", sep="/"))
  write( titlep, file=paste(td, "D2", sep="/"))
  myMatrix = textmatrix(td, minWordLength=1)

  similaryTitleQuery <- lsa::cosine(myMatrix[,1], myMatrix[,2])


  #preproccesing to description
  desciption<-paste(test$product_description[i], collapse=" ")
  d=TRUE
  if(desciption!=""){
    d=FALSE
  }

  queries_source <- VectorSource(desciption)

  corpus <- Corpus(queries_source)

  dtm <- DocumentTermMatrix(corpus)
  desciption<-dtm$dimnames$Terms

  write( desciption, file=paste(td, "D2", sep="/"))
  
  if(d==FALSE && length(desciption)!=0)
  {
    myMatrix = textmatrix(td, minWordLength=1)

    similaryContentQuery <- lsa::cosine(myMatrix[,1], myMatrix[,2])

  

  }else {
  
    similaryContentQuery<-0
  }

  test_featuers<-rbind(test_featuers, data.frame(simTitle = similaryTitleQuery,simDescription=similaryContentQuery))



}

write.csv(file="testCosine.csv", x=test_featuers,  row.names = F)


predictions <- predict(m1, test_featuers)
submit_data["id"] <- test$id
submit_data["prediction"] <- predictions
write.csv(submit_data, file = "Submission_2.csv")





submit_data <- select(test,id)


train1$queryId=NULLplot(m1)

m1

train_data_features$median_relevance<-as.factor(train_data_features$median_relevance)
fit <- J48(median_relevance~., data=train_data_features)
summary(fit)
predictions <- predict(fit, test_data_features)
submit_data <- read.csv("test.csv", header=TRUE)
submit_data <- select(submit_data,id)
submit_data["prediction"] <- predictions
write.csv(submit_data, file = "Submission_2.csv")












