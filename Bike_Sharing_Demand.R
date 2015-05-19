library(ggplot2)
library(caret)
library(randomForest)

setwd("C:/Joel_Work/Kaggle/2015_05_29_Bike_Sharing_Demand/01_Background_and_Data")

set.seed(100)

train_data = read.csv("train.csv",stringsAsFactors = FALSE)
train_data$date = as.Date(sapply(strsplit(train_data$datetime,' '),"[",1),"%m/%d/%Y")
train_data$hour = as.numeric(sapply(strsplit(sapply(strsplit(train_data$datetime,' '),"[",2),':'),"[",1))
train_data$datetime = NULL

test_data = read.csv("test.csv",stringsAsFactors = FALSE)
test_data$date = as.Date(sapply(strsplit(test_data$datetime,' '),"[",1),"%m/%d/%Y")
test_data$hour = as.numeric(sapply(strsplit(sapply(strsplit(test_data$datetime,' '),"[",2),':'),"[",1))
test_data$datetime = NULL

# Change column number for anything from 1 to 8, or 13, to plot a variable.
# qplot(train_data[,1], count, data = train_data, xlab=colnames(train_data[1]))

# featurePlot(x=train_data[,c(1:8,13)],y = train_data$count, plot = "pairs")

# Create data frame to hold results:
#summary_statistics = data.frame(method = character(0),mse = numeric(0))
method_list = character(0)
mse_list = numeric(0)
rmsle_list = numeric(0)

# Create my own data partition:
validation_split = createDataPartition(train_data$count, p = .7)[[1]]
jal_train = train_data[validation_split,]
jal_test = train_data[-validation_split,]

# Remove columns for random forest

# METHOD 1: Use all columns on count field, random forest
count_all_rF = randomForest(count ~ season +
                               holiday +
                               workingday +
                               weather +
                               temp +
                               atemp +
                               humidity +
                               windspeed +
                               hour,data = jal_train)
# METHOD 1 jal_train
count_all_rF_in_stats = count_all_rF
cou_all_rF_in = predict(count_all_rF, newdata = jal_train)
jal_train = cbind(jal_train,cou_all_rF_in)
method = "count_all_rF_in"
mse = mean((jal_train$count - jal_train$cou_all_rF_in)^2)
rmsle = sqrt(mean((log(jal_train$cou_all_rF_in + 1) - log(jal_train$count + 1))^2))
method_list = c(method_list, method)
mse_list = c(mse_list, mse)
rmsle_list = c(rmsle_list, rmsle)

# METHOD 1 jal_test
cou_all_rF_out = predict(count_all_rF, newdata = jal_test)
jal_test = cbind(jal_test,cou_all_rF_out)
method = "count_all_rF_out"
mse = mean((jal_test$count - jal_test$cou_all_rF_out)^2)
rmsle = sqrt(mean((log(jal_test$cou_all_rF_out + 1) - log(jal_test$count + 1))^2))
method_list = c(method_list, method)
mse_list = c(mse_list, mse)
rmsle_list = c(rmsle_list, rmsle)

# END METHOD 1


# METHOD 2: Use all columns on casual and registered fields, then combine the results and compare to count, random forest
casual_all_rF = randomForest(casual ~ season +
                              holiday +
                              workingday +
                              weather +
                              temp +
                              atemp +
                              humidity +
                              windspeed +
                              hour,data = jal_train)
registered_all_rF = randomForest(registered ~ season +
                               holiday +
                               workingday +
                               weather +
                               temp +
                               atemp +
                               humidity +
                               windspeed +
                               hour,data = jal_train)
# METHOD 2 jal_test
casual_all_rF_in = predict(casual_all_rF, newdata = jal_train)
registered_all_rF_in = predict(registered_all_rF, newdata = jal_train)
jal_train = cbind(jal_train,casual_all_rF_in,registered_all_rF_in)
jal_train$cas_reg_all_rF_in = jal_train$casual_all_rF_in + jal_train$registered_all_rF_in
method = "cas_reg_all_rF_in"
mse = mean((jal_train$count - jal_train$cas_reg_all_rF_in)^2)
rmsle = sqrt(mean((log(jal_train$cas_reg_all_rF_in + 1) - log(jal_train$count + 1))^2))
method_list = c(method_list, method)
mse_list = c(mse_list, mse)
rmsle_list = c(rmsle_list, rmsle)

# METHOD 2 jal_train
casual_all_rF_out = predict(casual_all_rF, newdata = jal_test)
registered_all_rF_out = predict(registered_all_rF, newdata = jal_test)
jal_test = cbind(jal_test,casual_all_rF_out,registered_all_rF_out)
jal_test$cas_reg_all_rF_out = jal_test$casual_all_rF_out + jal_test$registered_all_rF_out
method = "cas_reg_all_rF_out"
mse = mean((jal_test$count - jal_test$cas_reg_all_rF_out)^2)
rmsle = sqrt(mean((log(jal_test$cas_reg_all_rF_out + 1) - log(jal_test$count + 1))^2))
method_list = c(method_list, method)
mse_list = c(mse_list, mse)
rmsle_list = c(rmsle_list, rmsle)

# END METHOD 2

str(jal_train)

# METHOD 3: Use all columns on count field, poisson regression
count_all_poi = glm(count ~ season +
                              holiday +
                              workingday +
                              weather +
                              temp +
                              atemp +
                              humidity +
                              windspeed +
                              hour,data = jal_train, family = poisson())
# METHOD 3 jal_train
count_all_poi_in_stats = count_all_poi
cou_all_poi_in = predict(count_all_poi, newdata = jal_train)
jal_train = cbind(jal_train,cou_all_poi_in)
method = "count_all_poi_in"
mse = mean((jal_train$count - jal_train$cou_all_poi_in)^2)
rmsle = sqrt(mean((log(jal_train$cou_all_poi_in + 1) - log(jal_train$count + 1))^2))
method_list = c(method_list, method)
mse_list = c(mse_list, mse)
rmsle_list = c(rmsle_list, rmsle)

# METHOD 3 jal_test
cou_all_poi_out = predict(count_all_poi, newdata = jal_test)
jal_test = cbind(jal_test,cou_all_poi_out)
method = "count_all_poi_out"
mse = mean((jal_test$count - jal_test$cou_all_poi_out)^2)
rmsle = sqrt(mean((log(jal_test$cou_all_poi_out + 1) - log(jal_test$count + 1))^2))
method_list = c(method_list, method)
mse_list = c(mse_list, mse)
rmsle_list = c(rmsle_list, rmsle)

# END METHOD 3


# METHOD 4: Use all columns on casual and registered fields, then combine the results and compare to count, poisson regression
casual_all_poi = glm(casual ~ season +
                      holiday +
                      workingday +
                      weather +
                      temp +
                      atemp +
                      humidity +
                      windspeed +
                      hour,data = jal_train, family = poisson())
registered_all_poi = glm(casual ~ season +
                       holiday +
                       workingday +
                       weather +
                       temp +
                       atemp +
                       humidity +
                       windspeed +
                       hour,data = jal_train, family = poisson())
# METHOD 4 jal_test
casual_all_poi_in = predict(casual_all_poi, newdata = jal_train)
registered_all_poi_in = predict(registered_all_poi, newdata = jal_train)
jal_train = cbind(jal_train,casual_all_poi_in,registered_all_poi_in)
jal_train$cas_reg_all_poi_in = jal_train$casual_all_poi_in + jal_train$registered_all_poi_in
method = "cas_reg_all_poi_in"
mse = mean((jal_train$count - jal_train$cas_reg_all_poi_in)^2)
rmsle = sqrt(mean((log(jal_train$cas_reg_all_poi_in + 1) - log(jal_train$count + 1))^2))
method_list = c(method_list, method)
mse_list = c(mse_list, mse)
rmsle_list = c(rmsle_list, rmsle)

# METHOD 4 jal_train
casual_all_poi_out = predict(casual_all_poi, newdata = jal_test)
registered_all_poi_out = predict(registered_all_poi, newdata = jal_test)
jal_test = cbind(jal_test,casual_all_poi_out,registered_all_poi_out)
jal_test$cas_reg_all_poi_out = jal_test$casual_all_poi_out + jal_test$registered_all_poi_out
method = "cas_reg_all_poi_out"
mse = mean((jal_test$count - jal_test$cas_reg_all_poi_out)^2)
rmsle = sqrt(mean((log(jal_test$cas_reg_all_poi_out + 1) - log(jal_test$count + 1))^2))
method_list = c(method_list, method)
mse_list = c(mse_list, mse)
rmsle_list = c(rmsle_list, rmsle)

# END METHOD 4

#Combine the vectors
summary_statistics = cbind(method_list,mse_list, rmsle_list)
summary_statistics

print(count_all_rF_in_stats)

setwd("C:/Joel_Work/Kaggle/2015_05_29_Bike_Sharing_Demand/02_Output_Files")
write.csv(jal_train,"jal_train.csv",row.names = FALSE)
write.csv(jal_test,"jal_test.csv",row.names = FALSE)
setwd("C:/Joel_Work/Kaggle/2015_05_29_Bike_Sharing_Demand/01_Background_and_Data")

#PREDICT ACTUAL TEST SET

# METHOD 1 test
cou_all_rF_test = predict(count_all_rF, newdata = test_data)
test_data = cbind(test_data,cou_all_rF_test)

# METHOD 2 test
casual_all_rF_test = predict(casual_all_rF, newdata = test_data)
registered_all_rF_test = predict(registered_all_rF, newdata = test_data)
test_data = cbind(test_data,casual_all_rF_test,registered_all_rF_test)
test_data$cas_reg_all_rF_test = test_data$casual_all_rF_test + test_data$registered_all_rF_test

setwd("C:/Joel_Work/Kaggle/2015_05_29_Bike_Sharing_Demand/02_Output_Files")
write.csv(test_data,"jal_submission_data.csv",row.names = FALSE)
setwd("C:/Joel_Work/Kaggle/2015_05_29_Bike_Sharing_Demand/01_Background_and_Data")


#create a specific submisstion to submit
submission_template = read.csv("sampleSubmission.csv",stringsAsFactors = FALSE)
submission_template$count = NULL
submission_template = cbind(submission_template,test_data$cas_reg_all_rF_test)
colnames(submission_template)[2] = "count"

setwd("C:/Joel_Work/Kaggle/2015_05_29_Bike_Sharing_Demand/02_Output_Files")
write.csv(submission_template,"2015_05_18_submission_cas_reg_all.csv",row.names = FALSE)
setwd("C:/Joel_Work/Kaggle/2015_05_29_Bike_Sharing_Demand/01_Background_and_Data")
