complicationSet <- read.csv(file = "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/complicationSet.csv")
complicationSet[is.na(complicationSet)] <- NaN
referenceSet <- read.csv(file = "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/referenceSet_v1.csv")

library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)
require(chron)

temp <- list()
j <- 1 #index used to add new elements to the list of lists
for (i in 1:length(complicationSet))
{ #create a pair of (id, complicationTime, compl_type)
  temp[[j]] <- list(complicationSet[i, "subjectID"], complicationSet[i, "T"], complicationSet[i, "V1"])
  j <- j + 1
}

temp <- rbindlist(temp)
write.csv(temp, "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/temp.csv", row.names = FALSE)
temp <- read.csv(file = "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/temp.csv")

verify_vitals <- function(HR, RR, TEMP) {
  compl_warning = 0
  if(!is.na(HR) & HR >= 130){
    compl_warning = 1
  }
  else if(!is.na(RR) & RR >= 30){
    compl_warning = 1
  }
  #else if (TEMP)
  return(compl_warning)
}

observedVitalsList <- data.frame()
critical_time <- ""
before_time <- ""
lasting_time <- ""
imp_index = 0
compl_type = ""
critical_flag = 0
lasting_flag = 0
z <- 1

for (i in 1:nrow(referenceSet))
{ 
  index <- match(referenceSet[i, "subjectID"], temp$V1)
  if(!is.na(index)){ 
    if(imp_index == 0){
      imp_index = index
    }
    else if (imp_index != index){
      path = paste('C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/temp/obsSet', z, '_v2', '.csv', sep = "")
      write.csv(observedVitalsList, path, row.names = FALSE)
      print("Done")
      observedVitalsList <- data.frame()
      critical_time <- ""
      before_time <- ""
      lasting_time <- ""
      imp_index = index
      critical_flag = 0
      lasting_flag = 0
      z <- z+1
    }
    compl_time = as.POSIXlt(strptime(as.character(temp[index, "V2"]), format = "%d-%b-%Y %H:%M:%S"))
    compl_time <- format(compl_time, "%d/%m/%Y %H:%M:%S")
    compl_time <- as.chron(compl_time, "%d/%m/%Y %H:%M:%S")
    reference_time = as.POSIXlt(strptime(as.character(referenceSet[i, "T"]), format = "%d-%b-%Y %H:%M:%S"))
    reference_time <- format(reference_time, "%d/%m/%Y %H:%M:%S")
    other_reference_time <- as.chron(reference_time, "%d/%m/%Y %H:%M:%S")
    t1 = difftime(compl_time, other_reference_time, units = "hours") 
    if(abs(t1) <= 24){
      if(verify_vitals(referenceSet[i, "HR_processed"], referenceSet[i, "RR_processed"], referenceSet[i, "TEMP_processed"]) == 1){
        if(critical_flag == 0){
          critical_time <- reference_time
          other_critical_time = as.POSIXlt(strptime(as.character(critical_time), format = "%d/%m/%Y %H:%M:%S"))
          other_critical_time <- format(other_critical_time, "%d/%m/%Y %H:%M:%S")
          other_critical_time <- as.chron(other_critical_time, "%d/%m/%Y %H:%M:%S")
          
          before_time <- abs(difftime(compl_time, other_critical_time, units = "hours"))
          critical_flag = 1
        }
      }
      else{
        if(critical_flag == 1 & lasting_flag == 0){
          lasting_time <- abs(difftime(other_critical_time, other_reference_time, units = "hours"))
          lasting_flag == 1
        }
        
      }
      referenceSet[i, "lasting_time"] <- lasting_time 
      referenceSet[i, "before_time"] <- before_time 
      referenceSet[i, "critical_time"] <- critical_time 
      referenceSet[i, "compl_time"] <- temp[index, "V2"]
      referenceSet[i, "compl_type"] <- temp[index, "V3"]
      observedVitalsList <- rbind(observedVitalsList, referenceSet[i, ])
      
    }
    
  }
}

