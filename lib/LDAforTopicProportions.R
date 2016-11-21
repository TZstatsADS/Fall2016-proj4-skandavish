rm(list=ls())
cat('\014')


library(NLP)
library(tm)
library(lda)
library(LDAvis)
library(LDAvisData)

musicData <- read.csv('FeatureData.csv',header = T, stringsAsFactors = F)
wordData <- musicData[1997:ncol(musicData)]
featureData <- musicData[c(1997,1:1996)]

# pre-processing:
colnames(wordData)[2:ncol(wordData)] <- gsub("'", "", colnames(wordData)[2:ncol(wordData)])  # remove apostrophes
colnames(wordData)[2:ncol(wordData)] <- gsub("[[:punct:]]", " ", colnames(wordData)[2:ncol(wordData)])  # replace punctuation with space
colnames(wordData)[2:ncol(wordData)] <- gsub("[[:cntrl:]]", " ", colnames(wordData)[2:ncol(wordData)])  # replace control characters with space
colnames(wordData)[2:ncol(wordData)] <- gsub("^[[:space:]]+", "", colnames(wordData)[2:ncol(wordData)]) # remove whitespace at beginning of documents
colnames(wordData)[2:ncol(wordData)] <- gsub("[[:space:]]+$", "", colnames(wordData)[2:ncol(wordData)]) # remove whitespace at end of documents
colnames(wordData)[2:ncol(wordData)] <- tolower(colnames(wordData)[2:ncol(wordData)])  # force to lowercase

vocab <- colnames(wordData)[2:ncol(wordData)]

lyrics <- character()

#Creating 2000 documents
for(i in 1:nrow(wordData)){
  for(j in 2:ncol(wordData)){
    
    temp <- rep(colnames(wordData)[j],wordData[i,j])
    if(!sum(nchar(temp)) == 0){
      
      lyrics <- c(lyrics,temp)
    }
  }
  spaceBetween <- "???"
  lyrics <- c(lyrics,spaceBetween)
}

chk <- paste(lyrics,collapse = ' ')

# tokenize on space and output as a list:
doc.list <- strsplit(chk, "???",fixed = T)
doc.list <- doc.list[[1]]
length(doc.list)       # Length: number of documents

doc.list2 <- list()
for(i in 1:length(doc.list)){
  doc.list2[[i]] <- strsplit(doc.list[i]," ")[[1]]
}

length(doc.list2)
length(doc.list2[[1]])  # A splited string of words

# compute the table of terms:
term.table <- table(unlist(doc.list2))
term.table <- sort(term.table, decreasing = TRUE) 
head(term.table,n=100)      # Just to give an idea of how it looks like
class(term.table) #array

# remove terms that are stop words or occur fewer than 5 times:
stop_words <- stopwords("SMART")
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)           # Vocab: a vector of all words
head(term.table,n=100)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index-1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list2, get.terms)
documents[1]

# x <- doc.list[[1]]
# index <- match(x,vocab)
# 
# doc.list[[1]][1] #"films"
# index[1] #6
# vocab[6] #"films"
# 
# index <- index[!is.na(index)]
# 
# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 
N <- sum(doc.length)  # total number of tokens in the data 
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus 

# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop

theta <- as.matrix(t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x))))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

#rowSums(theta)
topicProp <- cbind(data.frame(theta),wordData$dat2.track_id)

write.csv(topicProp,'topicProp.csv',row.names = F)
write.csv(data.frame(phi),'wordTopicProp.csv',row.names = F)
#getwd()