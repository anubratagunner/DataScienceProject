library(dplyr)
library(data.table)
library(lubridate)

#import event file
complicationPatients <- read.csv(file = "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/MoViSign (Data Science course)/Data/Event_info.csv", header=TRUE)

#save important data for patients with complications 
complicationData <- data.frame(
  patientID = complicationPatients$subjectID,
  complicationTime = complicationPatients$complication_timing,
  complicationName = complicationPatients$complication_type)

#import vitals file  
vitalsPatients <- read.csv(file = "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/MoViSign (Data Science course)/Data/sensor_vitals.csv", header=TRUE)

#find common ids between complication and vitals sets
commonID = intersect(complicationData$patientID, vitalsPatients$subjectID)

#save in vitals only the data for complication patients
vitals <- vitalsPatients[vitalsPatients$subjectID %in% commonID, ]
write.csv(vitals, "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/vitals.csv", row.names = FALSE)

#convert date and time format from complication patients to "%d-%b-%Y %H:%M:%S"
a <- as.POSIXct(complicationData$complicationTime, format = "%d-%m-%Y %H:%M") # Produces NA when format is not "%m/%d/%Y"
b <- as.POSIXct(complicationData$complicationTime, format = "%m/%d/%Y %I:%M:%S %p") # Produces NA when format is not "%d.%m.%Y"
b[is.na(b)] <- a[!is.na(a)] # Combine both while keeping their ranks
complicationData$complicationTime <- b # Put it back in your dataframe
#complicationData$complicationTime
complicationData$complicationTime <- format(complicationData$complicationTime, "%d-%b-%Y %H:%M:%S")
#complicationData$complicationTime

#match common ids with times from event file
temp <- list()
j <- 1 #index used to add new elements to the list of lists
for (i in 1: nrow(complicationData))
  { #for each common id create a pair of (id, complicationTime)
    if (complicationData[i, "patientID"] %in% commonID) 
      {
      temp[[j]] <- list(complicationData[i, "patientID"], complicationData[i, "complicationTime"], complicationData[i, "complicationName"])
      j <- j + 1
      }
  }
#temp

#extract vitals based on id&time from complication patients
complicationSet <- list()
k <- 1 #index used to add new elements to the list of lists
for (z in 1:nrow(vitals))
  { 
    for(j in 1:length(temp))
    { 
      if ((vitals[z, "subjectID"] == temp[[j]][1]) & (vitals[z, "T"] == temp[[j]][2]))
        
      { 
        complicationSet[[k]] <- c(vitals[z, ], temp[[j]][3])
        #print(complicationSet[[k]])
        #print(typeof(complicationSet[[k]]))
        print(vitals[z, ])
        k <- k + 1
          
        }
    }
  }
#complicationSet

complicationSet <- rbindlist(complicationSet)
write.csv(complicationSet, "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/complicationSet.csv", row.names = FALSE)
complicationSet <- read.csv(file = "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/complicationSet.csv")
complicationSet[is.na(complicationSet)] <- NaN

#function that deletes rows with more than 3 nas
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) < 3,]
}

#creating the first version of the reference set
#the first version of the set consists of patients' vital signs
#outside complications intervals
referenceSet1 = anti_join(vitals, complicationSet1[, -"V1"])
#referenceSet1
delete.na(referenceSet1)
write.csv(referenceSet1, "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/referenceSet_v1.csv", row.names = FALSE)
#referenceSet1 <- read.csv(fiel = "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/referenceSet_v1.csv", row.names = FALSE)

#creating the second version of the reference set
#the second version of the set consists of patients' vital signs
#that didn't have any complication at all
differentID = setdiff(vitalsPatients$subjectID, complicationData$patientID)
referenceSet2 <- vitalsPatients[vitalsPatients$subjectID %in% differentID, ]
referenceSet2
delete.na(referenceSet2)
write.csv(referenceSet2, "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/referenceSet_v2.csv", row.names = FALSE)
