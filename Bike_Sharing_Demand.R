library(ggplot2)
library(caret)

setwd("C:/Joel_Work/Kaggle/2015_05_29_Bike_Sharing_Demand/01_Background_and_Data")

train_data = read.csv("train.csv",stringsAsFactors = FALSE)
train_data$date = as.Date(sapply(strsplit(train_data$datetime,' '),"[",1),"%m/%d/%Y")
train_data$hour = as.numeric(sapply(strsplit(sapply(strsplit(train_data$datetime,' '),"[",2),':'),"[",1))
train_data$datetime = NULL

test_data = read.csv("test.csv",stringsAsFactors = FALSE)
test_data$date = as.Date(sapply(strsplit(test_data$datetime,' '),"[",1),"%m/%d/%Y")
test_data$hour = as.numeric(sapply(strsplit(sapply(strsplit(test_data$datetime,' '),"[",2),':'),"[",1))
test_data$datetime = NULL

str(train_data)
summary(train_data)

qplot(train_data[,1], count, data = train_data, xlab=colnames(train_data[1]))
qplot(train_data[,2], count, data = train_data, xlab=colnames(train_data[2]))
qplot(train_data[,3], count, data = train_data, xlab=colnames(train_data[3]))
qplot(train_data[,4], count, data = train_data, xlab=colnames(train_data[4]))
qplot(train_data[,5], count, data = train_data, xlab=colnames(train_data[5]))
qplot(train_data[,6], count, data = train_data, xlab=colnames(train_data[6]))
qplot(train_data[,7], count, data = train_data, xlab=colnames(train_data[7]))
qplot(train_data[,8], count, data = train_data, xlab=colnames(train_data[8]))
qplot(train_data[,13], count, data = train_data, xlab=colnames(train_data[13]))




featurePlot(x=train_data[,c(1:8,13)],y = train_data$count, plot = "pairs")

#Remove columns for random forest

#First print off some plots...







# modFit_randomForest = randomForest(classe ~.,data = training)
# print(modFit_randomForest)
# plot(modFit_randomForest,log="y")
# randomForest_prediction = predict(modFit_randomForest, newdata = testing)
# randomForest_prediction
# #MDSplot(modFit_randomForest, training$classe)
# #summary(modFit_randomForest)
# require(tree)
# tree(formula = classe ~., data = training)
# 
# 
# train_predict_randomForest = predict(modFit_randomForest,training)
# cf_train_randomForest = confusionMatrix(train_predict_randomForest, training$classe)
# 
# validation_predict_randomForest = predict(modFit_randomForest,validation)
# cf_validation_randomForest = confusionMatrix(validation_predict_randomForest, validation$classe)
# 
# cf_train_randomForest
# cf_validation_randomForest