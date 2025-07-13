#### Student Deer Diel Activity Assessment
### Data from CLBS camera Array

## Setup
# Libraries
library(lubridate)
library(dplyr)
library(overlap)
library(circular)
library(CircStats)

# Data
Detections <- read.csv("Student Detections.csv") #import dataset
for(i in 1:nrow(Detections)){ #add 0 to single digit times
  if(nchar(Detections$Time[i]) < 5){Detections$Time[i] <- paste0("0", Detections$Time[i])}
}
Detections$Time <- paste0(Detections$Time, ":00") #add seconds to time
#Detections$Date <- gsub("/", "-", Detections$Date) #replace / with - in dates
Detections$Timestamp <- as.character(paste0(Detections$Date, " ", Detections$Time)) #combine dates and times into one column
Detections$Timestamp <- ymd_hms(Detections$Timestamp) #convert time to Posit format

# Cyclical time
#in order to analyze diel activity, it must be formatted to a 24-h circular format
Detections$Time <- substr(Detections$Timestamp, start = 12, stop = 19) #extract just time of day from timestamp
Detections = mutate(Detections, fractime = hms(Time) / hms("24:00:00"), RadTime = fractime*2*pi) #generate fractional time as the time / 24hrs (fractime) and convert it to radians by multiplying by 2pi (radtime)

#select deer as the only species
Deer <- subset(Detections, Detections$Species %in% c("Deer-Female", "Deer-Juvenile", "Deer-Unknown", "Deer-Male"))

#subset just male and female deer
Females <- subset(Deer, Deer$Species == "Deer-Female")
Males <- subset(Deer, Deer$Species == "Deer-Male")

## Male vs Female Diel Activity
# Diel Activity
#< rug = TRUE > displays data points used to generate function across bottom of graph
#< adjust = 0.8 > is the recommended setting for small sample sizes
densityPlot(Females$RadTime, rug = TRUE, adjust=0.8)
densityPlot(Males$RadTime, rug = TRUE, adjust = 0.8)

# Temporal overlap
overlapPlot(Females$RadTime, Males$RadTime, main="")
legend("topleft", c("Females", "Males"), lty=c(1,2), col = c(1,4), bty="n")

# Watson U2 test
#you can ignore warnings
watson.two.test(Females$RadTime, Males$RadTime)
