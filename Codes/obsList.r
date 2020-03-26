obsList <- read.csv(file = "/Users/anubratabhowmick/Desktop/UTwente Masters/Courses/Quartile 2/Data Science/Project/MoViSign (Data Science course)/Data/Observations.csv", header=TRUE)

obsList <- data.frame(subjectId = obsList$subjectID, time= obsList$T, HR=obsList$HR, RR=obsList$RR, TEMP=obsList$TEMP, SPO2=obsList$SPO2, hf_note=obsList$HF_note, mews=obsList$MEWS)

obsList <- na.omit(obsList)

library("dplyr")

obsList <- mutate(obsList, mews = ifelse(mews > 2, "risk", "not_at_risk"))

complicationPatients <- read.csv(file = "/Users/anubratabhowmick/Desktop/UTwente Masters/Courses/Quartile 2/Data Science/Project/MoViSign (Data Science course)/Data/Event_info.csv", header=TRUE)

complicationData <- data.frame(patientID = complicationPatients$subjectID, complicationTime = complicationPatients$complication_timing, complicationName = complicationPatients$complication_type)
 
complicationData

commonID = intersect(complicationData$patientID, obsList$subjectId)

observations <- obsList[obsList$subjectId %in% commonID, ]

observations

#The first question states that we need to find trends in continuous vital signs and complications.
#Does my list fulfill that?

write.csv(observations, "/Users/anubratabhowmick/Desktop/UTwente Masters/Courses/Quartile 2/Data Science/Project/observListWithComplications.csv", row.names = TRUE)

a <- as.POSIXct(complicationData$complicationTime, format = "%d-%m-%Y %H:%M") # Produces NA when format is not "%m/%d/%Y"
b <- as.POSIXct(complicationData$complicationTime, format = "%m/%d/%Y %I:%M:%S %p") # Produces NA when format is not "%d.%m.%Y"
b[is.na(b)] <- a[!is.na(a)] # Combine both while keeping their ranks
complicationData$complicationTime <- b # Put it back in your dataframe
#complicationData$complicationTime

a <- as.POSIXct(observations$time, format = "%d-%b-%Y %H:%M")
observations$time <- a

 commonTiming = intersect(complicationData$complicationTime, observations$time)
 #If I try to find a common timing for the complications, I don't get anything. 
 #Can we say that the complications might not play a vital role here?
complicationSetMews <- list()
k <- 1
for(i in 1:nrow(observations))
{
	if(observations$mews=="risk")
	{
		complicationSetMews[[k]] <- observations[i, ]
		print(observations[i, ])
		k <- k+1
	}
}

library(data.table)

complicationSetMews <- rbindlist(complicationSetMews)
write.csv(complicationSetMews, "/Users/anubratabhowmick/Desktop/UTwente Masters/Courses/Quartile 2/Data Science/Project/riskSet.csv", row.names = TRUE)

notComplicationSetMews <- list()
k <- 1
for(i in 1:nrow(observations))
{
	if(observations$mews=="not_at_risk")
	{
		notComplicationSetMews[[k]] <- observations[i, ]
		print(observations[i, ])
		k <- k+1
	}
}

notComplicationSetMews <- rbindlist(notComplicationSetMews)
write.csv(notComplicationSetMews, "/Users/anubratabhowmick/Desktop/UTwente Masters/Courses/Quartile 2/Data Science/Project/nonRiskSet.csv", row.names = TRUE)
