

library(readr)
train <- read_csv("train.csv")
td = tempfile()

dir.create(td)
results <- data.frame(queryId = numeric(), simTitle = numeric(),simDescription = numeric())

library(tm)
library(lsa)

for (i in 1:10158)
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
    
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    
    corpus <- tm_map(corpus, stripWhitespace)
    
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    
    dtm <- DocumentTermMatrix(corpus)
    
    query<-dtm$dimnames$Terms
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
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)

  corpus <- tm_map(corpus, stripWhitespace)

  corpus <- tm_map(corpus, removeWords, stopwords("english"))


  dtm <- DocumentTermMatrix(corpus)
  desciption<-dtm$dimnames$Terms
  desciption<-strsplit(gsub("[^[:alnum:] ]", "", desciption), " +")
  desciption<-as.character(desciption)


  write( desciption, file=paste(td, "D2", sep="/"))
  #totalSimilarity<-0 #initialization
  if(d==FALSE)
  {
    myMatrix = textmatrix(td, minWordLength=1)

    similaryContentQuery <- lsa::cosine(myMatrix[,1], myMatrix[,2])

 #   totalSimilarity<-0.8*similaryTitleQuery[1,1]+0.2*similaryContentQuery[1,1]

  }else {
 #   totalSimilarity<-similaryTitleQuery[1,1]
    similaryContentQuery<-0
  }

  results<-rbind(results, data.frame(queryId =train$id[i] , simTitle = similaryTitleQuery,simDescription=similaryContentQuery))



}

#write.csv(file="train1.csv", x=results)

results["Median_Rating"] <- train$median_relevance
# library("rpart", lib.loc="C:/Program Files/R/R-3.2.4revised/library")
# tree <- rpart(results$Median_Rating ~ results$sim , method="class")
# plot(tree)
write.csv(file="train1.csv", x=results)

results$Median_Rating <- as.factor(results$Median_Rating)
library(RWeka)
m1 <- J48(Median_Rating ~simTitle,simDescription , data = results)
library(partykit)

plot(m1)

m1
# #preproccesing to title
# title<-paste(train$product_title[111], collapse=" ")
# 
# queries_source <- VectorSource(title)
# 
# corpus <- Corpus(queries_source)
# corpus <- tm_map(corpus, content_transformer(tolower))
# corpus <- tm_map(corpus, removePunctuation)
# 
# corpus <- tm_map(corpus, stripWhitespace)
# 
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
# 
# 
# dtm <- DocumentTermMatrix(corpus)
# title<-dtm$dimnames$Terms
# 
# #cosSimilarity between title and query
# 
# write( query, file=paste(td, "D1", sep="/"))
# write( title, file=paste(td, "D2", sep="/"))
# myMatrix = textmatrix(td, minWordLength=1)
# 
# similaryTitleQuery <- lsa::cosine(myMatrix[,1], myMatrix[,2])
# 
# 
# #preproccesing to description
# desciption<-paste(train$product_description[111], collapse=" ")
# 
# d=TRUE
# if(desciption!=""){
#   d=FALSE
# }
# 
# queries_source <- VectorSource(desciption)
# 
# corpus <- Corpus(queries_source)
# corpus <- tm_map(corpus, content_transformer(tolower))
# corpus <- tm_map(corpus, removePunctuation)
# 
# corpus <- tm_map(corpus, stripWhitespace)
# 
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
# 
# 
# dtm <- DocumentTermMatrix(corpus)
# desciption<-dtm$dimnames$Terms
# 
# 
# 
# 
#   desciption<-strsplit(gsub("[^[:alnum:] ]", "", desciption), " +")
# 
# 
# 
# 
# desciption<-as.character(desciption)
# write( desciption, file=paste(td, "D2", sep="/"))
# totalSimilarity<-0 #initialization
# #if(d==FALSE)
# #{
# myMatrix = textmatrix(td, minWordLength=1)
# 
# similaryContentQuery <- lsa::cosine(myMatrix[,1], myMatrix[,2])
# 
# totalSimilarity<-0.8*similaryTitleQuery[1,1]+0.2*similaryContentQuery[1,1]
# 
# #}else {
# totalSimilarity<-similaryTitleQuery[1,1]
# #}
# 
# totalSimilarity
# results<-rbind(results, data.frame(queryId =train$id[i] , sim = totalSimilarity))
# 
# #
# # #
# # #
# # # ######
# # #
# # # Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")
# # # install.packages(Needed, dependencies=TRUE)
# # #
# # # install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
# # # train<-read.csv("train.csv")
# # # cname <- file.path("C:\\Users\\Tomer\\Documents\\GitHub\\HW4\\", "train.csv")
# # # dir(cname) # Use this to check to see that your texts have loaded.
# # #
# # # for (query in train$query)
# # #
# # #
# # # library(tm)
# # # docs <- Corpus(DirSource(cname))
# # # summary(docs)
# # # inspect(docs[1]) # read the second document (for example)
# # # docs <- tm_map(docs, removePunctuation)
# # 
# # 

