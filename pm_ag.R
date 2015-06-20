library(caret); library(kernlab);
# Assign the training set
train <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))
# Assign the testing set
test <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))
#test cases to submit: test$problem_id

#column percentage of NA
naPerc = colSums(is.na(train))/dim(train)[1]
#only use columns with <80% NA
train2=train[,naPerc<0.8]
#remove time stamp as category
train3=train2[,-5]
#remove mostly empty cols
emptyPerc=colSums(train3=='')/dim(train3)[1]
train4=train3[,emptyPerc<0.8]

#set seed to create my train and test set (60%/40%)
set.seed(12442)
inMyTrain <- createDataPartition(y=train3$classe, p=0.6,list=FALSE)
myTrain <- train4[inMyTrain,]
myTest <- train4[-inMyTrain,]


#get  PCAs
myTrainPCAs=myTrain[,-c(1:6,59)]
preProcMyTrainPCAs=preProcess(myTrainPCAs,method="pca")
myTrainPC=predict(preProcMyTrainPCAs,myTrainPCAs)

myTestPCAs=myTest[,-c(1:6,59)]
#preProcMyTestPCAs=preProcess(myTestPCAs,method="pca")
myTestPC=predict(preProcMyTrainPCAs,myTestPCAs)


?preProcess

library(ElemStatLearn);
library(rattle)
library(rpart)

#** GLM
#> modelFit=train(myTrain$classe~.,method="glm",data=trainPC)
#-> ONLY 2 levels can be used

#** RPart
modelFitRPart=train(myTrain$classe~.,method="rpart",data=myTrainPC)
fancyRpartPlot(modelFitRPart$finalModel)
ptrainRPart=predict(modelFitRPart,newdata=myTrainPC)
sum(ptrainRPart==myTrain$classe)/dim(myTrain)[1]
ptestRPart=predict(modelFitRPart,newdata=myTestPC)
sum(ptestRPart==myTest$classe)/dim(myTest)[1]
#TODO use correct PCAs not preprocess PCAs

#** RF
modelFitRF=train(myTrain$classe~.,method="rf",data=myTrainPC)
modelFitRF
#getTree(modelFit$finalModel,k=2)
ptrainRF=predict(modelFitRF,newdata=myTrainPC)
sum(ptrainRF==myTrain$classe)/dim(myTrain)[1]
ptestRF=predict(modelFit,newdata=myTrainPC)
sum(ptestRF==myTest$classe)/dim(myTest)[1]



