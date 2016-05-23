setwd('C:\\Users\\Tomer\\Documents\\GitHub\\HW4\\train.csv') #switch working dir
install.packages('tm')
library(tm)
df <-data.frame(query=numeric())
train <- read.csv ("train.csv",stringsAsFactors=FALSE)
str(train)
queries <- paste(train$query, collapse=" ")
queries_source <- VectorSource(queries)
corpus <- Corpus(queries_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus.Content

stopwords("english")
queries_source[0].content

dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
head(frequency)
install.packages('wordcloud')
library(wordcloud)
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])
