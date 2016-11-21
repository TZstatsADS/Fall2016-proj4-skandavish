rm(list=ls())
cat('\014')
library(rhdf5)
H5close()
#install.packages(c("RTextTools","topicmodels"))
library(RTextTools)
#library(topicmodels)

######Reading essential files into R#####################################
#########################################################################

songId = as.data.frame(read.table("C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/common_id.txt"))
load("C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/lyr.Rdata")
WordData = lyr
WordData = WordData[,-c(2:30)]

######Extracting, pivoting and cleaning the features data################
#########################################################################
songId$Path <- 0
#i = 1
for(i in 1:length(songId[,1])){
  name <- songId[i,1]
  pehla <- substring(name,3,3)
  dusra <- substring(name,4,4)
  teesra <- substring(name,5,5)
  songId[i, 2] <- paste("C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/data/",pehla,"/",dusra,"/",teesra,"/",name,".h5",sep = "")
}

vectorFiles <- songId$Path
for(i in vectorFiles){
  fileName <- i
  chk <- h5ls(fileName)
  temp <- data.frame(chk)
  temp$V2 <- NA
  temp$V2[which(temp$dclass == "FLOAT" | temp$dclass == "INTEGERS")] = strtoi(temp$dim[which(temp$dclass == "FLOAT" | temp$dclass == "INTEGERS")])
  temp$V2[is.na(temp$V2)]=temp$dim[is.na(temp$V2)]
  tempTrans <- t(temp)
  
  if(!(exists('featureData'))){
    temp <- t(data.frame(tempTrans[2,]))  
    columnNames <- chk$name
    featureData <- data.frame(temp[-1,])
  }
  
  tempTrans <- data.frame(tempTrans)
  tempTrans <- tempTrans[6,]
    
  featureData <- rbind(featureData,tempTrans)  
}

colnames(featureData) <- columnNames
featureData <- cbind(featureData,songId)
featureData[,c("Path","analysis","musicbrainz", "songs", "metadata")] <- NULL


#######Creating numerical variables out of categorical variables##################
##################################################################################

k <- featureData
library(caret)
dmy <- dummyVars(" ~ segments_pitches+segments_timbre", data = k, fullRank=T)
DummyFeatures <- data.frame(predict(dmy, newdata = k))

#######Adding the newly generated numerical variables to our feature file##########
###################################################################################

featureData[,c("segments_pitches","segments_timbre", "V1","songs", "similar_artists","metadata")] <- NULL
featureData[,c("songs")]<- NULL
featureDataToNumerics <- as.data.frame(sapply(featureData, function(x) strtoi(x)))
featureData <- cbind(featureDataToNumerics,DummyFeatures)
featureData1 <- cbind(featureData,WordData)
write.csv(featureData,'C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/featureData.csv',row.names = F)
write.csv(featureData1,'C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/featureData1.csv',row.names = F)

#View(featureData)
WordData[,c("dat2$track_id")] <- NULL
TransposedWordData = as.data.frame(t(WordData))

###############################~FINAL FEATURE FILE = FEATUREDATA~############################
#############################################################################################
#############################################################################################

###Transfering variables to an n dimensional space and storing Weighted Average Calculations#
#############################################################################################

totalValuesInRows = rowSums(TransposedWordData)
#class(totalValuesInRows)
o  = as.matrix(TransposedWordData) %*% as.matrix(featureData)
weightedAvg <- cbind(data.frame(o),data.frame(totalValuesInRows))
weightedAvg[,c(1:1996)] <- weightedAvg[,c(1:1996)]/weightedAvg[,1997]
weightedAvg <- weightedAvg[,c(1:1996)]
write.csv(weightedAvg,'C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/weightedAvg.csv',row.names = F)

#########Testing the above algorithm and creating rank orders
rm('RanksOrdering')
l = c(4,7,110,103)
SongTest <- function(weightedAvg, l, featureData){
  for(i in 1:length(l)){
    Test <- featureData[l[i],]
    Test <- do.call(rbind, replicate(100, Test, simplify=FALSE)) # where m is your matrix
    Test <- do.call(rbind, replicate(50, Test, simplify=FALSE)) # where m is your matrix
    Test = Test[1:4971,]
    u = paste0("RankofSong", i)
    RowTotal = as.data.frame(sqrt(rowSums((weightedAvg - Test)*(weightedAvg - Test))))
    RanksOrdering = transform(RowTotal, u = ave(RowTotal, FUN = function(x) rank(-x, ties.method = "first")))
    RanksOrdering[,1] <- NULL
    names(RanksOrdering) <- c(paste0("RankofSong", i))
    if(i == 1){
      FinalFile = RanksOrdering
    }
    else{
      FinalFile = cbind(FinalFile,RanksOrdering)
    }
  }
  return(FinalFile)
}

y = SongTest(weightedAvg = weightedAvg, l= l, featureData = featureData)


#########################################################################
##################kmeans check for features and lyrics###################

#########################################################################
####################Kmeans for Topic Proportions#########################

# TopicProportionFile <- read.csv("C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/topicProp.csv")
# TopicProportionFile$wordData.dat2.track_id <- NULL
# TopicProportionFile <- scale(TopicProportionFile)
# km.out <- kmeans(TopicProportionFile,3,nstart=20)
# km.out1A <- kmeans(TopicProportionFile,centers = km.out$centers,nstart=20)

##############Kmeans for Features#######################################
#######################################################################

# featureDataToNumericsScaled <- as.data.frame(scale(featureDataToNumerics))
# featureDataToNumericsScaledWithDummyVariables <- cbind(featureDataToNumericsScaled,DummyFeatures)
# km.out2 <-kmeans(featureDataToNumericsScaledWithDummyVariables,3,nstart=20)
# km.out2A <- kmeans(featureDataToNumericsScaledWithDummyVariables,centers = km.out2$centers,nstart=20) 
# #km.out$tot.withinss
# #km.out2$tot.withinss


###############Merging feature cluster and topic clusters########################
#################################################################################

# FinalCluster <- as.data.frame(km.out1A$cluster)
# FinalCluster <- cbind(FinalCluster,as.data.frame(km.out2A$cluster))
# FinalCluster <- cbind(FinalCluster, songId)
# FinalCluster$Path <- NULL
# colnames(FinalCluster) <- c("WordCluster", "FeatureCluster", "Name")
# write.csv(FinalCluster, file = "C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/ClusterData.csv", row.names = F)

#################################################################################
######Similarity Matching
# rm('finalMatrix')
# rm('tempMatrix')
# i = 1
# j = 1
# finalMatrix1 <- matrix(nrow=1,ncol=3)
# tempMatrix <- matrix(nrow=1,ncol=3)
# for(i in 1:3){
#   temp = WordsClusters[WordsClusters$WordCluster == i,c("WordCluster", "Name")]
#   lengthOfDenominator = nrow(temp)
#   for(j in 1:3){
#     u = WordsClusters[WordsClusters$FeatureCluster == j,c("FeatureCluster", "Name")] 
#     temp1 = merge(u,temp)
#     lengthOfNumerator = nrow(temp1)
#     k = (lengthOfNumerator/lengthOfDenominator)*100
#     tempMatrix =  as.data.frame(cbind(tempMatrix,k))
#     }
#   if(i == 1)
#   {
#     finalMatrix1 = tempMatrix
#   }
#   else
#   {
#     finalMatrix1 = as.data.frame(rbind(finalMatrix1,tempMatrix))  
#   }
#   tempMatrix <- matrix(nrow=1,ncol=1)
# }

##########################Creating cluster level word count###################
##############################################################################

# tempData <- cbind(WordData,FinalCluster$FeatureCluster)
# colnames(tempData)[ncol(tempData)] <- 'clusterLabel'
# clusterData <- tempData[-c(1:nrow(tempData)),]
# 
# for(i in unique(tempData$clusterLabel)){
#   temp <- tempData[which(tempData$clusterLabel == i),]
#   temp <- temp[,-ncol(temp)]
#   temp <- data.frame(cbind(t(data.frame(colSums(temp))), clusterLabel = i))
#   clusterData <- rbind(clusterData,temp)
# }
# write.csv(clusterData,'C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/clusterData.csv',row.names = F)
# 
# clusterCenters <- data.frame(km.out2A$centers)
# clusterCentersOrd <- clusterCenters[c(3,1,2),]
# write.csv(clusterCentersOrd,'C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/clusterCentre.csv',row.names = F)



