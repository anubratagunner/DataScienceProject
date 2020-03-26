library("dplyr")
library("rpart")
library("rpart.plot")

#Classification of person being at risk or non-risk based on mews
obsList <- read.csv(file = "/Users/anubratabhowmick/Desktop/UTwente Masters/Courses/Quartile 2/Data Science/Project/MoViSign (Data Science course)/Data/Observations.csv", header=TRUE)
obsList <- data.frame(subjectId = obsList$subjectID, time= obsList$T, HR=obsList$HR, RR=obsList$RR, TEMP=obsList$TEMP, SPO2=obsList$SPO2, hf_note=obsList$HF_note, mews=obsList$MEWS)
obsList <- na.omit(obsList)

obsList <- mutate(obsList, mews = ifelse(mews > 2, "risk", "not_at_risk"))
obsList <- na.omit(obsList)

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

commonID = intersect(complicationData$patientID, obsList$subjectId)
names(complicationData)[1] <- "subjectId"


#Break it down into risk and nonRisk set
riskSet <- data.frame()
nonRiskSet <- data.frame()
for(i in 1:nrow(obsList))
{
	if(obsList[i, "mews"] == "risk")
	{
    	riskSet <- rbind(riskSet, obsList[i, ])
	}
	else
	{
		nonRiskSet<- rbind(nonRiskSet, obsList[i, ])
	}
}



#finalObsList$complicationName <- as.character(finalObsList$complicationName) #Convert NA to string for replacement
#finalObsList$complicationName[is.na(finalObsList$complicationName)] <- "NC" #Replace with NC stating it's not complicated

for(i in 1:nrow(riskSet))
{
	if(riskSet[i, "subjectId"] %in% commonID)
	{
		riskSet[i, "isComplicated"] <- "Y"
	}
	else
	{
		riskSet[i, "isComplicated"] <- "N"
	}
}
riskSet


for(i in 1:nrow(nonRiskSet))
{
	if(nonRiskSet[i, "subjectId"] %in% commonID)
	{
		nonRiskSet[i, "isComplicated"] <- "Y"
	}
	else
	{
		nonRiskSet[i, "isComplicated"] <- "N"
	}
}
nonRiskSet



#Draw barplots for complication sets
compSet <- c(riskSet$isComplicated)
countsCompSet <- table(compSet)
barplot(countsCompSet, ylim=c(0,50), xlab = "Did complications occur?", ylab = "Number of patients", col = rainbow(2)) 


#Draw barplots fofr non-complication sets
nonCompSet <- c(nonRiskSet$isComplicated)
countsNonCompSet <- table(nonCompSet)
barplot(countsNonCompSet, ylim=c(0,50), xlab = "Did complications occur?", ylab = "Number of patients", col = rainbow(2))
#Almost 66% of people who were not at risk developed complications





#Decision tree considering risk and notRisk set
decisionTreeRiskSet = rpart(isComplicated ~ HR + RR + TEMP + SPO2, data = riskSet, method = "class") #Almost 93% is classified as yes, so no node leaf is needed
rpart.plot(decisionTreeRiskSet) #Plot the decision tree for Risk Set

decisionTreeNonRiskSet = rpart(isComplicated ~ HR + RR + TEMP + SPO2, data = nonRiskSet, method = "class")
rpart.plot(decisionTreeNonRiskSet) #Plot the decision tree for Risk Set
#Make the decision tree the new threshold and compare





# library(caret)
# set.seed(100)

# myDat <- obsList
# trainRowNumbers <- createDataPartition(obsList$mews, p=0.8, list=FALSE)

# trainData <- myDat[trainRowNumbers, c(3, 4, 5, 6, 8)]
# testData <- myDat[-trainRowNumbers, c(3, 4, 5, 6, 8)]


# #Logistic Regression Model
# fit_logistic_regression <- train(mews ~ HR + RR + TEMP + SPO2, data = trainData, method="glm", family="binomial")
# predicted_val <- predict(fit_logistic_regression, testData)
# confusionMatrix(reference = testData$mews, data = predicted_val, mode="everything") #88% accuracy
# varImp(fit_logistic_regression) #Find the most important features 

# #Random Forest Model
# library(randomForest)
# require(caTools)
# rf <- randomForest(mews ~ HR + RR + TEMP + SPO2, data = trainData)
# predicted_val_rf <- predict(rf, testData)
# confusionMatrix(reference = testData$mews, data = predicted_val_rf, mode="everything", positive="Y") #92.31% accuracy
# varImp(rf) #Find the most important features
# #Plot the Random Forest Classification 
# library(pROC)
# roc_curve_rf=roc(response=testData$mews, predictor= factor(predicted_val_rf, ordered = TRUE), plot=TRUE)
# plot(roc_curve_rf, col="red", lwd=3, main="ROC curve")
# auc_rf<-auc(roc_curve_rf) #AUC = 0.875

# #Lets train that frigging NEURAL NETWORK
# procValues <- preProcess(trainData, method = c("center", "scale"))
# scaledTrainData <-  predict(procValues, trainData)
# scaledTestData <-  predict(procValues, testData)
# scaledTestData$mews <- ifelse(scaledTestData$mews=="not_at_risk", 1, 0) #To convert categorical to numerical
# library(neuralnet)
# nn <- neuralnet(mews ~ HR + RR + TEMP + SPO2, data=scaledTrainData, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
# plot(nn)

#Rest will do later! :3
#nn$result.matrix
#nn.results <- compute(nn, scaledTestData)
#results <- data.frame(actual = scaledTestData$mews, prediction = nn.results$net.result)
#roundedResults<-sapply(results,round,digits=0)
#roundedResultsDF=data.frame(roundedResults)

