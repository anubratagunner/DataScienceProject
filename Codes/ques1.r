#Identification of the relation between trends/characteristics in continuous vital signs and complications

#Import complications
complicationPatients <- read.csv(file = "/Users/anubratabhowmick/Desktop/UTwente Masters/Courses/Quartile 2/Data Science/Project/MoViSign (Data Science course)/Data/Event_info.csv", header=TRUE)
complicationPatients
complicationData <- data.frame(
      patientID = complicationPatients$subjectID,
      complicationTime = complicationPatients$complication_timing,
      complicationName = complicationPatients$complication_type,
      complicationClass = complicationPatients$complication_class)

library(lubridate)
a <- as.POSIXct(complicationData$complicationTime, format = "%d-%m-%Y %H:%M")
b <- as.POSIXct(complicationData$complicationTime, format = "%m/%d/%Y %I:%M:%S %p")
b[is.na(b)] <- a[!is.na(a)]

complicationData$complicationTime <- b
complicationData$complicationTime <- format(complicationData$complicationTime, "%d-%b-%Y %H:%M:%S")
complicationData

#Import vitals file  
vitalsPatients <- read.csv(file = "/Users/anubratabhowmick/Desktop/UTwente Masters/Courses/Quartile 2/Data Science/Project/MoViSign (Data Science course)/Data/sensor_vitals.csv", header=TRUE)
vitalsPatients <- na.omit(vitalsPatients)
vitalsPatients

#Find the vitalsPatients with complications
commonID = intersect(complicationData$patientID, vitalsPatients$subjectID)

 for(i in 1:nrow(vitalsPatients)){
	if(vitalsPatients[i, "subjectID"] %in% commonID){
		vitalsPatients[i, "isComplicated"] <- "Y"
	}
	else{
		vitalsPatients[i, "isComplicated"] <- "N"
	}
}
write.csv(vitalsPatients, "/Users/anubratabhowmick/Desktop/UTwente Masters/Courses/Quartile 2/Data Science/Project/Complicated_Vitals_Info.csv", row.names = TRUE)

#Should I consider the processed or the non-processed rows for ML?
#Taking Processed as of now

#Decision Tree
library("rpart")
library("rpart.plot")
vitalsPatients$isComplicated <- as.factor(vitalsPatients$isComplicated)
decision_tree = rpart(isComplicated ~ HR + RR + TEMP + SPO2, data = vitalsPatients, method = "class")
rpart.plot(decision_tree) #Plot the decision tree you made


library(caret)
set.seed(100)
myDat <- vitalsPatients
trainRowNumbers <- createDataPartition(myDat$isComplicated, p=0.8, list=FALSE)
trainData <- myDat[trainRowNumbers, c(3, 4, 5, 6, 11)]
testData <- myDat[-trainRowNumbers, c(3, 4, 5, 6, 11)]

#Logistic Regression Model
fit_logistic_regression <- train(isComplicated ~ HR + RR + TEMP + SPO2, data = trainData, method="glm", family="binomial")
predicted_val <- predict(fit_logistic_regression, testData)
confusionMatrix(reference = testData$isComplicated, data = predicted_val, mode="everything")
varImp(fit_logistic_regression) #Find the most important features 


#Decision Tree Model
predicted_val_decision_tree <- predict(decision_tree, testData,type="class")
confusionMatrix(reference = testData$isComplicated, data = predicted_val_decision_tree, mode="everything", positive="Y")
varImp(decision_tree) #Find the most important features

#Random Forest Model
library(randomForest)
require(caTools)
rf <- randomForest(isComplicated ~ HR + RR + TEMP + SPO2, data = trainData)
predicted_val_rf <- predict(rf, testData)
confusionMatrix(reference = testData$isComplicated, data = predicted_val_rf, mode="everything", positive="Y")
varImp(rf) #Find the most important features
#Plot the Random Forest Classification 
library(pROC)
roc_curve_rf=roc(response=testData$isComplicated, predictor= factor(predicted_val_rf, ordered = TRUE), plot=TRUE)
plot(roc_curve_rf, col="red", lwd=3, main="ROC curve")
auc_rf<-auc(roc_curve_rf) #AUC = 0.7138


#SVM Model
library(e1071)
svm_classifier = svm(formula = isComplicated ~ HR + RR + TEMP +SPO2, data = trainData, type = 'C-classification', kernel = 'linear') 
predicted_val_svm <- predict(svm_classifier, testData)
confusionMatrix(reference = testData$isComplicated, data = predicted_val_svm)

#Neural Network Model
procValues <- preProcess(trainData, method = c("center", "scale"))
scaledTrainData <-  predict(procValues, trainData)
scaledTestData <-  predict(procValues, testData)
#Still not converging



