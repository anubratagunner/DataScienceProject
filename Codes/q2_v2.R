complicationSet <- read.csv(file = "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/complicationSet.csv")
complicationSet[is.na(complicationSet)] <- NaN
referenceSet <- read.csv(file = "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/referenceSet_v1.csv")

library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)
require(chron)
require(gridExtra)
library(scales)

plot_v1 <- function(obsSet, compl_type, compl_time, critical_time) {
  obsSet$T = as.POSIXlt(strptime(as.character(obsSet$T), format = "%d-%b-%Y %H:%M:%S"))
  obsSet$T <- format(obsSet$T, "%d/%m/%Y %H:%M:%S")
  obsSet$T <- as.POSIXct(strptime(as.character(obsSet$T), format = "%d/%m/%Y %H:%M:%S"))
  
  compl_time = as.POSIXlt(strptime(as.character(compl_time), format = "%d-%b-%Y %H:%M:%S"))
  compl_time <- format(compl_time, "%d/%m/%Y %H:%M:%S")
  compl_time <- as.POSIXct(strptime(as.character(compl_time), format = "%d/%m/%Y %H:%M:%S"))
  
  critical_time = as.POSIXct(strptime(as.character(critical_time), format = "%d/%m/%Y %H:%M:%S"))
  
  p1 <- ggplot(obsSet, aes(x = obsSet$T, y = obsSet$HR_processed)) +
    geom_point(colour = "black", size = 0.5) + geom_line(aes(group=1), color='black', size=0.8) +
    geom_vline(xintercept = critical_time, linetype="solid", color = "red", size=1.5) +
    geom_vline(xintercept = compl_time, linetype="solid", color = "blue", size=1.5) +
    scale_x_datetime(labels=date_format("%d/%m %H:%M"), breaks = date_breaks("6 hours")) +
    ggtitle(paste(compl_type, 'HR', sep = "_")) +
    xlab("Time") + ylab("HR") 
  p2 <- ggplot(obsSet, aes(x = obsSet$T, y = obsSet$RR_processed)) +
    geom_point(colour = "black", size = 0.5) + geom_line(aes(group=1), color='black', size=0.8) + 
    geom_vline(xintercept = critical_time, linetype="solid", color = "red", size=1.5) +
    geom_vline(xintercept = compl_time, linetype="solid", color = "blue", size=1.5) +
    scale_x_datetime(labels=date_format("%d/%m %H:%M"), breaks = date_breaks("6 hours")) +
    ggtitle(paste(compl_type, 'RR', sep = "_")) +
    xlab("Time") + ylab("RR") 
  p3 <- ggplot(obsSet, aes(x = obsSet$T, y = obsSet$TEMP_processed)) +   
    geom_point(colour = "black", size = 0.5) + geom_line(aes(group=1), color='black', size=0.8) + 
    geom_vline(xintercept = critical_time, linetype="solid", color = "red", size=1.5) +
    geom_vline(xintercept = compl_time, linetype="solid", color = "blue", size=1.5) +
    scale_x_datetime(labels=date_format("%d/%m %H:%M"), breaks = date_breaks("6 hours")) +
    ggtitle(paste(compl_type, 'TEMP', sep = "_")) +
    xlab("Time") + ylab("TEMP") 
  
  print(p1)
  filename = paste(i, compl_type, 'HR', '_v2','.jpeg', sep = "")
  ggsave(filename, plot = last_plot(), device = "jpeg", path = "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/graphs")
  
  print(p2)
  filename = paste(i, compl_type, 'RR', '_v2','.jpeg', sep = "")
  ggsave(filename, plot = last_plot(), device = "jpeg", path = "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/graphs")
  
  print(p3)
  filename = paste(i, compl_type, 'TEMP', '_v2','.jpeg', sep = "")
  ggsave(filename, plot = last_plot(), device = "jpeg", path = "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/graphs")
  
  #print(grid.arrange(p1, p2, p3, nrow = 1))
  #filename = paste(complicationSet[i, "V1"], i,'.jpeg', sep = "")
  #ggsave(filename, plot = grid.arrange(p1, p2, p3, nrow = 1), device = "jpeg", path = "C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/graphs")
}


for(i in 1:9){
  path = paste('C:/Users/mitra/Desktop/Q2/DS/DM/Project_DM/temp/obsSet', i, '_v2', '.csv', sep = "")
  obsSet = read.csv(file = path)
  idx <- min(which(obsSet[,13] !=""))
  critical_time = obsSet[idx,13]
  plot_v1(obsSet, obsSet[1, "compl_type"], obsSet[1, "compl_time"], critical_time)
  
}

