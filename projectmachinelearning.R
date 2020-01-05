library(caret)
library(corrplot)
library(rpart)
library(rpart.plot)
library(rattle)
data=read.csv("pml-training.csv")
datat=read.csv("pml-testing.csv")
str(data)

coltoremove=which(colSums(is.na(data)|data=="")>0.9*dim(data)[1])
traindata=data[,-coltoremove]
View(traindata)
traindata=traindata[,-c(1:7)]
View(traindata)

set.seed(10)
intrain=createDataPartition(traindata$classe,p=0.7,list = FALSE)
training=traindata[intrain,]
validation=traindata[-intrain,]
dim(training)
dim(validation)
cort=cor(training[,-53])
corrplot(cort,order = "FPC",method = "color",type = "upper",tl.cex = "1.0")
highlycorrelate=findCorrelation(cort, cutoff = 0.75)
names(training[highlycorrelate])
set.seed(10)
model1=rpart(classe~.,data = training,method="class")
fancyRpartPlot(model1)
predict1=predict(model1,validation,type = "class")
matrix1=confusionMatrix(predict1,validation$classe)

control=trainControl(method = "cv",number = 3,verboseIter = FALSE)
model2=train(classe~.,data = training,method="rf", trControl=control)
predict2=predict(model2,newdata=validation)
matrix2=confusionMatrix(predict2,validation$classe)
plot(model2)

control=trainControl(method = "repeatedcv",number = 3,repeats = 3)
model3=train(classe~.,data = training,method="gbm", trControl=control)
predict3=predict(model3,newdata=validation)
matrix3=confusionMatrix(predict3,validation$classe)
plot(model3)


result = predict(model2,newdata  =datat)
View(datat)