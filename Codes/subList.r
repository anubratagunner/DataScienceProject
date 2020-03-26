subInfo <- read.csv(file = "/Users/anubratabhowmick/Desktop/UTwente Masters/Courses/Quartile 2/Data Science/Project/MoViSign (Data Science course)/Data/subject_info.csv", header=TRUE)

subInfo

subInfo <- data.frame(subjectId = subInfo$subjectID, age = subInfo$age, gender = subInfo$gender, length = subInfo$length, weight = subInfo$weight, ASA = subInfo$ASA)

subInfo <- mutate(subInfo, ASA = ifelse(ASA > 3, "risk", "not_at_risk"))

commonID = intersect(complicationData$patientID, subInfo$subjectId)

subjects <- subInfo[subInfo$subjectId %in% commonID, ]

write.csv(subjects, "/Users/anubratabhowmick/Desktop/UTwente Masters/Courses/Quartile 2/Data Science/Project/subjectList.csv", row.names = TRUE)