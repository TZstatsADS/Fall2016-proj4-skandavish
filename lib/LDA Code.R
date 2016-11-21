rm(list=ls())
cat('\014')

#install.packages("LDAvis") 
#devtools::install_github("cpsievert/LDAvisData")

library(NLP)
library(tm)
library(lda)
library(LDAvis)
library(LDAvisData)

wordDataWithClusLab <- read.csv("C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/clusterData.csv", stringsAsFactors = F, header = T)
  
wordData <- wordDataWithClusLab[1:(ncol(wordDataWithClusLab)-1)]

# pre-processing:
colnames(wordData)[2:ncol(wordData)] <- gsub("'", "", colnames(wordData)[2:ncol(wordData)])  # remove apostrophes
colnames(wordData)[2:ncol(wordData)] <- gsub("[[:punct:]]", " ", colnames(wordData)[2:ncol(wordData)])  # replace punctuation with space
colnames(wordData)[2:ncol(wordData)] <- gsub("[[:cntrl:]]", " ", colnames(wordData)[2:ncol(wordData)])  # replace control characters with space
colnames(wordData)[2:ncol(wordData)] <- gsub("^[[:space:]]+", "", colnames(wordData)[2:ncol(wordData)]) # remove whitespace at beginning of documents
colnames(wordData)[2:ncol(wordData)] <- gsub("[[:space:]]+$", "", colnames(wordData)[2:ncol(wordData)]) # remove whitespace at end of documents
colnames(wordData)[1:ncol(wordData)] <- tolower(colnames(wordData)[1:ncol(wordData)])  # force to lowercase
  
lyrics <- character()

#Creating 3 documents
for(i in 1:nrow(wordData)){
  for(j in 1:ncol(wordData)){
      
    temp <- rep(colnames(wordData)[j],wordData[i,j])
    if(sum(nchar(temp)) != 0){
        
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
#stop_words <- stopwords("SMART")
#del <- names(term.table) %in% stop_words | term.table < 5
#term.table <- term.table[!del]
vocab <- names(term.table)           # Vocab: a vector of all words
#head(term.table,n=100)
length(vocab)
  
# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index-1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list2, get.terms)
#documents[1]
  
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
D <- length(documents)  # number of documents (3)
W <- length(vocab)  # number of terms in the vocab (4,772)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 216837 289202  46877
N <- sum(doc.length)  # total number of tokens in the data 552916
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
t2 - t1  # 
  
theta <- as.matrix(t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x))))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
dim(phi)
dim(theta)

OutPutMatrix <- theta %*% phi
OutPutMatrix1 <- as.data.frame(OutPutMatrix)

OPMatrix <- OutPutMatrix1
TranformedOPMatrix <- as.data.frame(t(OPMatrix))

r1 = as.data.frame(rank(TranformedOPMatrix$V1, ties.method = ("max")))
r2 = as.data.frame(rank(TranformedOPMatrix$V2, ties.method = ("max")))
r3 = as.data.frame(rank(TranformedOPMatrix$V3, ties.method = ("max")))

# RanksOrdering1 = transform(TranformedOPMatrix$V1, FirstClusterValues = ave(TranformedOPMatrix$V1, FUN = function(x) rank(-x, ties.method = "first")))
# RanksOrdering2 = transform(TranformedOPMatrix$V2, SecondClusterValues = ave(TranformedOPMatrix$V2, FUN = function(x) rank(-x, ties.method = "first")))
# RanksOrdering3 = transform(TranformedOPMatrix$V3, ThirdClusterValues = ave(TranformedOPMatrix$V3, FUN = function(x) rank(-x, ties.method = "first")))

FinalOPMatrix <- cbind(r1,r2,r3)
FinalOPMatrix <- as.data.frame(t(FinalOPMatrix))
rownames(FinalOPMatrix) <- c("Cluster1", "Cluster2", "Cluster3")
colnames(FinalOPMatrix) <- colnames(OPMatrix)
FinalOPMatrix <- FinalOPMatrix[,order(colnames(FinalOPMatrix))]
write.csv(FinalOPMatrix,'C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/OPMatrix.csv',row.names = F)
