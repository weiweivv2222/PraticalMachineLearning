library(doParallel);
rCluster <- makePSOCKcluster(4);
registerDoParallel(rCluster);
trainRawData <- read.csv("pml-training.csv",na.strings=c("NA",""));

NAs <- apply(trainRawData,2,function(x) {sum(is.na(x))});
validData <- trainRawData[,which(NAs == 0)];
library(caret);
trainIndex <- createDataPartition(y = validData$classe, p=0.7,list=FALSE);
trainData <- validData[trainIndex,];
removeIndex <- grep("timestamp|X|user_name|new_window",names(trainData));
trainData <- trainData[,-removeIndex];
set.seed(1234);

trControl = trainControl(method = "cv", number = 4, allowParallel =TRUE);
modFitRF <- train(trainData$classe ~.,data = trainData,method="rf",trControl=trControl);
modFitRF;
testData <- validData[-trainIndex,];
removeIndex <- grep("timestamp|X|user_name|new_window",names(testData));
testData <- testData[,-removeIndex];

predictedValues<-predict(modFitRF,testData);
View(predictedValues);
testData$predictedValues<-predictedValues;
testData$Comparision<-testData$predictedValues==testData$classe;

length(testData$Comparision[testData$Comparision==FALSE]);
length(testData$Comparision[testData$Comparision==TRUE]);

