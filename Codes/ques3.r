#Identification of the relation between clinical measurements/subject characteristics and complications
library(dplyr)
subInfo <- read.csv(file = "/Users/anubratabhowmick/Desktop/UTwente Masters/Courses/Quartile 2/Data Science/Project/MoViSign (Data Science course)/Data/subject_info.csv", header=TRUE)
subInfo <- data.frame(subjectId = subInfo$subjectID, subGroup = subInfo$group, age = subInfo$age, gender = subInfo$gender, length = subInfo$length, weight = subInfo$weight, ASA = subInfo$ASA)
subInfo <- na.omit(subInfo)
subInfo
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

commonID = intersect(complicationData$patientID, subInfo$subjectId)
complicatedSubList <- subInfo[subInfo$subjectId %in% commonID, ] #Get the subject list which has complications
'%ni%' <- Negate('%in%')
notComplicatedSubList <- subInfo[subInfo$subjectId %ni% commonID, ] #Get the subject list which do not have complications
#From thhe not complicatedSubList we can inffer that subjects with ASA 2/3 mainly doesn't come across complications.

names(complicationData)[1] <- "subjectId" #Change the name of the column for merging

finalComplicationSet <- merge(complicatedSubList, complicationData, by="subjectId", all = T) #Merge the complication set and the set with the subject Info having complications
finalComplicationSet <- mutate(finalComplicationSet, gender = ifelse(gender == 1, "male", "female"))
finalComplicationSet <- na.omit(finalComplicationSet) #Remove any remaining NA

#Create a male sublist with the complications
# maleComplicatedSubList <- data.frame()
# for(i in 1:nrow(finalComplicationSet))
# {
# 	if(finalComplicationSet[i, "gender"]=="male")
# 	{
# 		maleComplicatedSubList <- rbind(maleComplicatedSubList, finalComplicationSet[i, ])
# 	}
# }
# maleComplicatedSubList

# #Create a female sublist with the complications
# femaleComplicatedSubList <- data.frame()
# for(i in 1:nrow(finalComplicationSet))
# {
# 	if(finalComplicationSet[i, "gender"]=="female")
# 	{
# 		femaleComplicatedSubList <- rbind(femaleComplicatedSubList, finalComplicationSet[i, ])
# 	}
# }
# femaleComplicatedSubList
#From the male and female classification, we can say that both male and female get atrial fibrillation and pneumonia complications most!

#Create a new barplot for age of men with complications
# for(i in 1:nrow(maleComplicatedSubList))
# {
# 	if(maleComplicatedSubList[i, "age"]>=60 && maleComplicatedSubList[i, "age"]<70)
# 	{
# 		maleComplicatedSubList[i, "age"] <- "60-70"
# 	} 
# 	else if(maleComplicatedSubList[i, "age"]>=50 && maleComplicatedSubList[i, "age"]<60)
# 	{
# 		maleComplicatedSubList[i, "age"] <- "50-60"
# 	} 
# 	else if(maleComplicatedSubList[i, "age"]>=70 && maleComplicatedSubList[i, "age"]<80) 
# 	{
# 		maleComplicatedSubList[i, "age"] <- "70-80"
# 	} 
# 	else if(maleComplicatedSubList[i, "age"]>=80 && maleComplicatedSubList[i, "age"]<90) 
# 	{
# 		maleComplicatedSubList[i, "age"] <- "80-90"
# 	}
# }
# HAge <- c(maleComplicatedSubList$age)
# countsAge <- table(HAge)
# barplot(countsAge, xlab = "Age Range", ylab = "Number of men", col = rainbow(4))

#Create a new barplot for height of men with complications
# for(i in 1:nrow(maleComplicatedSubList))
# {
# 	if(maleComplicatedSubList[i, "length"]>=170 && maleComplicatedSubList[i, "length"]<180)
# 	{
# 		maleComplicatedSubList[i, "length"] <- "170-180"
# 	} 
# 	else if(maleComplicatedSubList[i, "length"]>=180 && maleComplicatedSubList[i, "length"]<190)
# 	{
# 		maleComplicatedSubList[i, "length"] <- "180-190"
# 	} 
# 	else if(maleComplicatedSubList[i, "length"]>=190 && maleComplicatedSubList[i, "length"]<200) 
# 	{
# 		maleComplicatedSubList[i, "length"] <- "190-200"
# 	} 
# }
# HHeight <- c(maleComplicatedSubList$length)
# countsHeight <- table(HHeight)
# barplot(countsHeight, xlab = "Height Range", ylab = "Number of men", col = rainbow(3))

#Create a barplot for weight after fixing the data issue
# for(i in 1:nrow(maleComplicatedSubList))
# {
# 	if(maleComplicatedSubList[i, "weight"]>=60 && maleComplicatedSubList[i, "weight"]<70)
# 	{
# 		maleComplicatedSubList[i, "weight"] <- "60-70"
# 	} 
# 	else if(maleComplicatedSubList[i, "weight"]>=70 && maleComplicatedSubList[i, "weight"]<80)
# 	{
# 		maleComplicatedSubList[i, "weight"] <- "70-80"
# 	} 
# 	else if(maleComplicatedSubList[i, "weight"]>=80 && maleComplicatedSubList[i, "weight"]<90) 
# 	{
# 		maleComplicatedSubList[i, "weight"] <- "80-90"
# 	}
# 	else if((maleComplicatedSubList[i, "weight"]>=90) && maleComplicatedSubList[i, "weight"]<100) 
# 	{
# 		maleComplicatedSubList[i, "weight"] <- "90-100"
# 	}  
# 	else if(maleComplicatedSubList[i, "weight"]>=100 && maleComplicatedSubList[i, "weight"]<110) 
# 	{
# 		maleComplicatedSubList[i, "weight"] <- "100-110"
# 	} 
# 	else if(maleComplicatedSubList[i, "weight"]>=110 && maleComplicatedSubList[i, "weight"]<120) 
# 	{
# 		maleComplicatedSubList[i, "weight"] <- "110-120"
# 	} 
# }
# #What's the problem with the 90 range? FUCKER -_-
# HWeightM <- c(maleComplicatedSubList$weight)
# countsWeightM <- table(HWeightM)
# barplot(countsWeightM, las=3, cex.names = .5, xlab = "Weight Range", ylab = "Number of men", col = rainbow(8)) #Even there's a problem with the graph, need to check -_-

#Create a new barplot for age of women with complications
# for(i in 1:nrow(femaleComplicatedSubList))
# {
# 	if(femaleComplicatedSubList[i, "age"]>=60 && femaleComplicatedSubList[i, "age"]<70)
# 	{
# 		femaleComplicatedSubList[i, "age"] <- "60-70"
# 	} 
# 	else if(femaleComplicatedSubList[i, "age"]>=50 && femaleComplicatedSubList[i, "age"]<60)
# 	{
# 		femaleComplicatedSubList[i, "age"] <- "50-60"
# 	} 
# 	else if(femaleComplicatedSubList[i, "age"]>=70 && femaleComplicatedSubList[i, "age"]<80) 
# 	{
# 		femaleComplicatedSubList[i, "age"] <- "70-80"
# 	} 
# 	else if(femaleComplicatedSubList[i, "age"]>=80 && femaleComplicatedSubList[i, "age"]<90) 
# 	{
# 		femaleComplicatedSubList[i, "age"] <- "80-90"
# 	}
# 	else if(femaleComplicatedSubList[i, "age"]>=90 && femaleComplicatedSubList[i, "age"]<100) 
# 	{
# 		femaleComplicatedSubList[i, "age"] <- "90-100"
# 	}
# }
# HAgeF <- c(femaleComplicatedSubList$age)
# countsAgeF <- table(HAgeF)
# barplot(countsAgeF, xlab = "Age Range", ylab = "Number of women", col = rainbow(4))

# #Create a new barplot for height of women with complications
# for(i in 1:nrow(femaleComplicatedSubList))
# {
# 	if(femaleComplicatedSubList[i, "length"]>=170 && femaleComplicatedSubList[i, "length"]<180)
# 	{
# 		femaleComplicatedSubList[i, "length"] <- "170-180"
# 	} 
# 	else if(femaleComplicatedSubList[i, "length"]>=150 && femaleComplicatedSubList[i, "length"]<160)
# 	{
# 		femaleComplicatedSubList[i, "length"] <- "150-160"
# 	} 
# 	else if(femaleComplicatedSubList[i, "length"]>=160 && femaleComplicatedSubList[i, "length"]<170) 
# 	{
# 		femaleComplicatedSubList[i, "length"] <- "160-170"
# 	} 
# }
# HHeightF <- c(femaleComplicatedSubList$length)
# countsHeightF <- table(HHeightF)
# barplot(countsHeightF, xlab = "Height Range", ylab = "Number of women", col = rainbow(3))

# #Create a barplot for weight of women with complications
# for(i in 1:nrow(femaleComplicatedSubList))
# {
# 	if(femaleComplicatedSubList[i, "weight"]>=40 && femaleComplicatedSubList[i, "weight"]<50)
# 	{
# 		femaleComplicatedSubList[i, "weight"] <- "40-50"
# 	} 
# 	else if(femaleComplicatedSubList[i, "weight"]>=50 && femaleComplicatedSubList[i, "weight"]<60)
# 	{
# 		femaleComplicatedSubList[i, "weight"] <- "50-60"
# 	} 
# 	else if(femaleComplicatedSubList[i, "weight"]>=60 && femaleComplicatedSubList[i, "weight"]<70) 
# 	{
# 		femaleComplicatedSubList[i, "weight"] <- "60-70"
# 	}
# 	else if((femaleComplicatedSubList[i, "weight"]>=70) && femaleComplicatedSubList[i, "weight"]<81) 
# 	{
# 		femaleComplicatedSubList[i, "weight"] <- "70-80"
# 	} 
# }
# HWeightF <- c(femaleComplicatedSubList$weight)
# countsWeightF <- table(HWeightF)
# barplot(countsWeightF, xlab = "Weight Range", ylab = "Number of women", col = rainbow(4))

#Find the number of people suffering from a certain type of complication and draw a barchart on that
countsComplication <- table(finalComplicationSet$complicationName)
barplot(countsComplication, las=3, ylim=c(0,10), cex.names=.5, xlab = "Complications", ylab = "Number of subjects", col=rainbow(18)) #Draw a bar chart with the names rotated for viewing purpose



#Create a sublist based on subGroups
groupOneTwoComplicatedSubList <- data.frame()
for(i in 1:nrow(finalComplicationSet))
{
	if(finalComplicationSet[i, "subGroup"]==1 || finalComplicationSet[i, "subGroup"]==2)
	{
		groupOneTwoComplicatedSubList <- rbind(groupOneTwoComplicatedSubList, finalComplicationSet[i, ])
	}
}


# groupTwoComplicatedSubList <- data.frame()
# for(i in 1:nrow(finalComplicationSet))
# {
# 	if(finalComplicationSet[i, "subGroup"]==2)
# 	{
# 		groupTwoComplicatedSubList <- rbind(groupTwoComplicatedSubList, finalComplicationSet[i, ])
# 	}
# }

groupThreeComplicatedSubList <- data.frame()
for(i in 1:nrow(finalComplicationSet))
{
	if(finalComplicationSet[i, "subGroup"]==3)
	{
		groupThreeComplicatedSubList <- rbind(groupThreeComplicatedSubList, finalComplicationSet[i, ])
	}
}

#Create a new barplot for age of group1 with complications
for(i in 1:nrow(groupOneTwoComplicatedSubList))
{
	if(groupOneTwoComplicatedSubList[i, "age"]>=60 && groupOneTwoComplicatedSubList[i, "age"]<70)
	{
		groupOneTwoComplicatedSubList[i, "age"] <- "60-70"
	} 
	else if(groupOneTwoComplicatedSubList[i, "age"]>=50 && groupOneTwoComplicatedSubList[i, "age"]<60)
	{
		groupOneTwoComplicatedSubList[i, "age"] <- "50-60"
	} 
	else if(groupOneTwoComplicatedSubList[i, "age"]>=70 && groupOneTwoComplicatedSubList[i, "age"]<80) 
	{
		groupOneTwoComplicatedSubList[i, "age"] <- "70-80"
	} 
	else if(groupOneTwoComplicatedSubList[i, "age"]>=80 && groupOneTwoComplicatedSubList[i, "age"]<90) 
	{
		groupOneTwoComplicatedSubList[i, "age"] <- "80-90"
	}
}

HAge <- c(groupOneTwoComplicatedSubList$age)
countsAge <- table(HAge)
barplot(countsAge, ylim=c(0,10), xlab = "Age Range", ylab = "Number of patients", col = rainbow(4))


for(i in 1:nrow(groupOneTwoComplicatedSubList))
{
	if(groupOneTwoComplicatedSubList[i, "length"]>=150 && groupOneTwoComplicatedSubList[i, "length"]<160)
	{
		groupOneTwoComplicatedSubList[i, "length"] <- "150-160"
	} 
	else if(groupOneTwoComplicatedSubList[i, "length"]>=160 && groupOneTwoComplicatedSubList[i, "length"]<170)
	{
		groupOneTwoComplicatedSubList[i, "length"] <- "160-170"
	} 
	if(groupOneTwoComplicatedSubList[i, "length"]>=170 && groupOneTwoComplicatedSubList[i, "length"]<180)
	{
		groupOneTwoComplicatedSubList[i, "length"] <- "170-180"
	} 
	else if(groupOneTwoComplicatedSubList[i, "length"]>=180 && groupOneTwoComplicatedSubList[i, "length"]<190)
	{
		groupOneTwoComplicatedSubList[i, "length"] <- "180-190"
	} 
	else if(groupOneTwoComplicatedSubList[i, "length"]>=190 && groupOneTwoComplicatedSubList[i, "length"]<200) 
	{
		groupOneTwoComplicatedSubList[i, "length"] <- "190-200"
	} 
}
HHeight <- c(groupOneTwoComplicatedSubList$length)
countsHeight <- table(HHeight)
barplot(countsHeight, ylim=c(0,10), xlab = "Height Range", ylab = "Number of patients", col = rainbow(5))

for(i in 1:nrow(groupOneTwoComplicatedSubList))
{
	if(groupOneTwoComplicatedSubList[i, "weight"]>=60 && groupOneTwoComplicatedSubList[i, "weight"]<70)
	{
		groupOneTwoComplicatedSubList[i, "weight"] <- "60-70"
	} 
	else if(groupOneTwoComplicatedSubList[i, "weight"]>=70 && groupOneTwoComplicatedSubList[i, "weight"]<80)
	{
		groupOneTwoComplicatedSubList[i, "weight"] <- "70-80"
	} 
	else if(groupOneTwoComplicatedSubList[i, "weight"]>=80 && groupOneTwoComplicatedSubList[i, "weight"]<90) 
	{
		groupOneTwoComplicatedSubList[i, "weight"] <- "80-90"
	}
	else if((groupOneTwoComplicatedSubList[i, "weight"]>=90) && groupOneTwoComplicatedSubList[i, "weight"]<100) 
	{
		groupOneTwoComplicatedSubList[i, "weight"] <- "90-100"
	}  
	else if(groupOneTwoComplicatedSubList[i, "weight"]>=100 && groupOneTwoComplicatedSubList[i, "weight"]<110) 
	{
		groupOneTwoComplicatedSubList[i, "weight"] <- "100-110"
	} 
	else if(groupOneTwoComplicatedSubList[i, "weight"]>=110 && groupOneTwoComplicatedSubList[i, "weight"]<120) 
	{
		groupOneTwoComplicatedSubList[i, "weight"] <- "110-120"
	} 
	else
	{
		groupOneTwoComplicatedSubList[i, "weight"] <- "120+"
	}
}
HWeightM <- c(groupOneTwoComplicatedSubList$weight)
countsWeightM <- table(HWeightM)
barplot(countsWeightM, ylim=c(0,10), las=3, cex.names = .5, xlab = "Weight Range", ylab = "Number of patients", col = rainbow(7)) 

#Create a new barplot for age of group 3 with complications
for(i in 1:nrow(groupThreeComplicatedSubList))
{
	if(groupThreeComplicatedSubList[i, "age"]>=70 && groupThreeComplicatedSubList[i, "age"]<80) 
	{
		groupThreeComplicatedSubList[i, "age"] <- "70-80"
	} 
	else if(groupThreeComplicatedSubList[i, "age"]>=80 && groupThreeComplicatedSubList[i, "age"]<90) 
	{
		groupThreeComplicatedSubList[i, "age"] <- "80-90"
	}
	else if(groupThreeComplicatedSubList[i, "age"]>=90 && groupThreeComplicatedSubList[i, "age"]<100) 
	{
		groupThreeComplicatedSubList[i, "age"] <- "90-100"
	}
}

HAge <- c(groupThreeComplicatedSubList$age)
countsAge <- table(HAge)
barplot(countsAge, ylim=c(0,10), xlab = "Age Range", ylab = "Number of patients", col = rainbow(3))


for(i in 1:nrow(groupThreeComplicatedSubList))
{
	if(groupThreeComplicatedSubList[i, "length"]>=150 && groupThreeComplicatedSubList[i, "length"]<160)
	{
		groupThreeComplicatedSubList[i, "length"] <- "150-160"
	} 
	else if(groupThreeComplicatedSubList[i, "length"]>=160 && groupThreeComplicatedSubList[i, "length"]<170)
	{
		groupThreeComplicatedSubList[i, "length"] <- "160-170"
	} 
	if(groupThreeComplicatedSubList[i, "length"]>=170 && groupThreeComplicatedSubList[i, "length"]<180)
	{
		groupThreeComplicatedSubList[i, "length"] <- "170-180"
	} 
	else if(groupThreeComplicatedSubList[i, "length"]>=180 && groupThreeComplicatedSubList[i, "length"]<190)
	{
		groupThreeComplicatedSubList[i, "length"] <- "180-190"
	} 
	else if(groupThreeComplicatedSubList[i, "length"]>=190 && groupThreeComplicatedSubList[i, "length"]<200) 
	{
		groupThreeComplicatedSubList[i, "length"] <- "190-200"
	} 
}
HHeight <- c(groupThreeComplicatedSubList$length)
countsHeight <- table(HHeight)
barplot(countsHeight, ylim=c(0,10), xlab = "Height Range", ylab = "Number of patients", col = rainbow(5))


for(i in 1:nrow(groupThreeComplicatedSubList))
{

	if(groupThreeComplicatedSubList[i, "weight"]>=50 && groupThreeComplicatedSubList[i, "weight"]<60)
	{
		groupThreeComplicatedSubList[i, "weight"] <- "50-60"
	} 
	else if(groupThreeComplicatedSubList[i, "weight"]>=60 && groupThreeComplicatedSubList[i, "weight"]<70)
	{
		groupThreeComplicatedSubList[i, "weight"] <- "60-70"
	} 
	else if(groupThreeComplicatedSubList[i, "weight"]>=70 && groupThreeComplicatedSubList[i, "weight"]<80)
	{
		groupThreeComplicatedSubList[i, "weight"] <- "70-80"
	} 
	else if(groupThreeComplicatedSubList[i, "weight"]>=80 && groupThreeComplicatedSubList[i, "weight"]<90) 
	{
		groupThreeComplicatedSubList[i, "weight"] <- "80-90"
	}
	else if((groupThreeComplicatedSubList[i, "weight"]>=90) && groupThreeComplicatedSubList[i, "weight"]<100) 
	{
		groupThreeComplicatedSubList[i, "weight"] <- "90-100"
	}
}
HWeightM <- c(groupThreeComplicatedSubList$weight)
countsWeightM <- table(HWeightM)
barplot(countsWeightM, ylim=c(0,10), las=3, cex.names = .5, xlab = "Weight Range", ylab = "Number of patients", col = rainbow(5)) 


#Most common type of complication for patients of group 1, 2 and 3
countsComplicationGroupOneTwo <- table(groupOneTwoComplicatedSubList$complicationName)
barplot(countsComplicationGroupOneTwo, las=3, ylim=c(0,10), cex.names=.5, xlab = "Complications", ylab = "Number of patients", col=rainbow(20)) #Draw a bar chart with the names rotated for viewing purpose
#Atrial Fibrilation and pneuomonia

countsComplicationGroupThree <- table(groupThreeComplicatedSubList$complicationName)
barplot(countsComplicationGroupThree, las=3, ylim=c(0,10), cex.names=.5, xlab = "Complications", ylab = "Number of patients", col=rainbow(20)) #Draw a bar chart with the names rotated for viewing purpose
#Atrial Fibrilation and Urinary Tract Infection most common among group 3





#Is there a clear pattern based on these?
Yes, we found a clear pattern within the data and we can conclude based on point 3.
