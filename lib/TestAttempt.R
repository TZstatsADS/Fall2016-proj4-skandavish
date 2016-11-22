library(caret)


#OPMatrix <- read.csv("C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/OPMatrix.csv")
#weightedAvg <- read.csv('C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/weightedAvg.csv')
listfiles = list.files(path = "C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/TestSongFile100")
featureData = read.csv('C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/featureData.csv',stringsAsFactors = F)
#ClusterCentres = read.csv('C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/clusterCentre.csv',stringsAsFactors = F)
WeightedAverage = read.csv('C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/weightedAvg.csv',stringsAsFactors = F)
songId = as.data.frame(read.table("C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/common_id.txt"))


rm('featureDatak')
for(i in listfiles){
  fileName <- paste("C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/TestSongFile100/",i,sep = "")
  chk <- h5ls(fileName)
  temp <- data.frame(chk)
  temp$V2 <- NA
  temp$V2[which(temp$dclass == "FLOAT" | temp$dclass == "INTEGERS")] = strtoi(temp$dim[which(temp$dclass == "FLOAT" | temp$dclass == "INTEGERS")])
  temp$V2[is.na(temp$V2)]=temp$dim[is.na(temp$V2)]
  tempTrans <- t(temp)
  
  if(!(exists('featureDatak'))){
    temp <- t(data.frame(tempTrans[2,]))  
    columnNames <- chk$name
    featureDatak <- data.frame(temp[-1,])
    }

  tempTrans <- data.frame(tempTrans)
  tempTrans <- tempTrans[6,]
  featureDatak <- rbind(featureDatak,tempTrans)
  }

colnames(featureDatak) <- columnNames
#featureDatak <- cbind(featureDatak,songId)
featureDatak[,c("Path","analysis","musicbrainz", "songs", "metadata","V1","similar_artists")] <- NULL
featureDatak[,c("songs")]<- NULL


#CODE
featureDatak$segments_pitches <- as.character(featureDatak$segments_pitches)
featureDatak$segments_timbre <- as.character(featureDatak$segments_timbre)
#featureDatak$songs <- as.character(featureDatak$songs) 

testData <- featureData[-c(1:nrow(featureData)),]
for(i in 1:nrow(featureDatak)){
  tempData <- featureData[-c(2:nrow(featureData)),]
  tempData[1,] <- 0
  
  for(j in 1:ncol(featureDatak)){
    
    colName <- colnames(featureDatak)[j]
    colIndex <- match(colName,colnames(featureData))
    if(colName %in% c('segments_timbre','segments_pitches')){
      value <- gsub(' ','.',featureDatak[i,j])
      colIndex <- match(paste(colName,value,sep='.'),colnames(featureData))
      if(is.na(colIndex) == F){
        tempData[1,colIndex] <- 1
      }
    }
    else{
      if(is.na(colIndex) == F){
        tempData[1,colIndex] <- featureDatak[i,j]
      }
    }
  }
  testData <- rbind(testData,tempData)
}

#View(testData)

#listfilesDataFrame <- as.data.frame(listfiles)
# testData <- cbind(testData,listfilesDataFrame)

# FinalFinalOPMatrix <- OPMatrix[-(1:nrow(OPMatrix)),]
# dis <- rep(0,3)
# minValues <- integer()
# 
# for(i in 1:100) 
# {
#   for(j in 1:3) 
#   {
#     dis[j] = dist(rbind(ClusterCentres[j,],testData[i,]), method = "euclidean")
#   }
#   MinimumValue <- which.min(dis)
#   minValues <- c(minValues,MinimumValue)
#   FinalFinalOPMatrix = rbind(FinalFinalOPMatrix, OPMatrix[MinimumValue,])
# }
  
  

#########Testing the above algorithm and creating rank orders
# rm('RanksOrdering')
# 
# SongTest <- function(weightedAvg, testData){
#   for(i in 1:nrow(testData)){
#     Test <- testData[i,]
#     Test <- do.call(rbind, replicate(100, Test, simplify=FALSE)) # where m is your matrix
#     Test <- do.call(rbind, replicate(50, Test, simplify=FALSE)) # where m is your matrix
#     Test = Test[1:4971,]
#     u = paste0("RankofSong", i)
#     RowTotal = as.data.frame(sqrt(rowSums((weightedAvg - Test)*(weightedAvg - Test))))
#     RanksOrdering = transform(RowTotal, u = ave(RowTotal, FUN = function(x) rank(-x, ties.method = "first")))
#     RanksOrdering[,1] <- NULL
#     names(RanksOrdering) <- c(paste0("RankofSong", i))
#     if(i == 1){
#       FinalFile = RanksOrdering
#     }
#     else{
#       FinalFile = cbind(FinalFile,RanksOrdering)
#     }
#   }
#   return(FinalFile)
# }
# 
# y = SongTest(weightedAvg = weightedAvg, testData = testData)
# #y = cbind(y, )
# y = as.data.frame(t(y))
# colnames(y) = rownames(weightedAvg)
# testData <- cbind(y,listfilesDataFrame)
# row.names(y) <- y$listfiles
# write.csv(y,'C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/SpaceDataOP.csv',row.names = F)
# 
#

#Method 3 - Using proportions from LDA where 1 song = 1 document
phiAll <- read.csv('C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/phi1.csv',stringsAsFactors = F)
thetaAll <- read.csv('C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/theta1.csv', stringsAsFactors = F)

thetaAllMat <- as.matrix(thetaAll[1:(ncol(thetaAll)-1)])
phiAll <- as.matrix(phiAll)
thetaAllMat <- as.matrix(thetaAll)

OPnewAll <- thetaAllMat %*% phiAll

OPMatrixFull <- as.data.frame(t(OPnewAll))
colnames(OPMatrixFull) <- 1:ncol(OPMatrixFull)
Method3FinalData = OPMatrixFull[,-c(1:ncol(OPMatrixFull))]

for(i in 1:ncol(OPMatrixFull))
{
  r1 = as.data.frame(rank(OPMatrixFull[,i], ties.method = ("max")))  
  Method3FinalData = cbind(Method3FinalData,r1)  
}

Method3FinalData = as.data.frame(t(Method3FinalData))
Method3FinalData <- Method3FinalData[,order(colnames(Method3FinalData))]
listfilesDataFrame <- as.data.frame(listfiles)
row.names(Method3FinalData) <- thetaAll$wordData.dat2.track_id

write.csv(Method3FinalData,'C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 4/OPMatrixFULL.csv')

featurDataWithNames <- cbind(as.character(songId$V1),featureData)
colnames(featurDataWithNames)[1] <- 'songId'

OPmatrixFULLWithNames <- as.data.frame(Method3FinalData)
OPmatrixFULLWithNames$songId <- thetaAll$wordData.dat2.track_id
#OPmatrixFULLWithNames$songId <- as.character(row.names(Method3FinalData))
row.names(OPmatrixFULLWithNames) <- thetaAll$wordData.dat2.track_id
featurDataWithNames <- featurDataWithNames[order(featurDataWithNames$songId),]
#OPmatrixFULLWithNames <- OPmatrixFULLWithNames[order(OPmatrixFULLWithNames$songId),]
FinalOPMatrixFull <- OPmatrixFULLWithNames[-(1:nrow(OPmatrixFULLWithNames)),-ncol(OPmatrixFULLWithNames)]

dis <- rep(0,nrow(featurDataWithNames))
#minValues <- integer()
FinalFinalOPMatrix <- FinalOPMatrixFull[-(1:nrow(FinalOPMatrixFull)),]
featurDataWithNames$songId <- as.character(featurDataWithNames$songId)


for(i in 1:nrow(testData))
{
  dis <- rep(0,500)
  for(j in 500:999) 
    {
      dis[j-499] = dist(rbind(featurDataWithNames[j,2:ncol(featurDataWithNames)],testData[i,]), method = "euclidean")
    }
  MinimumValue <- which.min(dis)
  #closestSong <- featurDataWithNames$songId[MinimumValue]
  FinalFinalOPMatrix <- as.data.frame(rbind(FinalFinalOPMatrix, OPmatrixFULLWithNames[MinimumValue,]))
}

View(FinalFinalOPMatrix)
#nrow(featurDataWithNames)
#




