#Calculate total alkalinity using potentiometric titrations
#Uses a for loop to read in data exported as a titration file and calculate Total alkalinity
#At the end it exports your data as a .csv file. Comment the last line out if your don't want that.

### Files needed ######
# 1. pHCalibration.csv in your "Data" folder
#Inside the Data folder You must have a subfolder for each data set. In each subfolder there is:
#2. the mass file for your run  
#3. a subfolder named "TodaysDate" (where all of your titration files are) directly exported from LabX.
# 

#Created by Nyssa Silbiger 03/28/2014
#modified 20180627 Hollie Putnam
#new acid bottle 20180712 Emma Strand
#line 68 changed SampleID to Sample.ID - Sam Gurr 20180713
#
#------------------------------------------------------------
rm(list=ls())

#set working directory---------------------------------------------------------------------------------------------

setwd("C:/Users/samjg/Documents/My_Projects/Geoduck_transgen_offspring_OA/RAnalysis")
#setwd("~/MyProjects/UW_TA_2016/RAnalysis/")
main<-getwd()

#load libraries----------------------------------------------
library(seacarb) #used to calculate TA
library(tidyverse)

#CHANGE THESE VALUES EVERY DAY----------------------------------------------
path<-"Data/TA/20190408" #the location of all your titration files
massfile<-"20190408mass_RUN2.csv" # name of your file with masses
titrationfile<-'20190408_RUN2.csv'# name of the last titration file run


# Date that the data were run
date<-'20190408'


#DO NOT CHANGE ANYTHING BELOW THIS LINE UNLESS A NEW BOTTLE OF ACID IS USED
#load Data---------------------------------------------------
#load Mass Data
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 

#### pH Calibration #####
pHCal<-read.csv('Data/pHCalibration.csv') # read in the pH Calibration file

#select the calibration for the correct date
pHData<-pHCal[pHCal$Date==date,]

# calculate pH 3 and 3.5 based on the slope and intercept from pH 4, 7, and 10 calibration
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# print a plot of the relationship between pH and mV
#png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
lines(c(4,7,10), predict(mod.pH))
R2<-summary(mod.pH)$r.squared
legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
#dev.off()

# Select the mV for pH=3 and pH=3.5 based on your probe calibration
pH35<-mod.pH$coefficients[1]+mod.pH$coefficients[2]*3.5
pH3<-mod.pH$coefficients[1]+mod.pH$coefficients[2]*3

##### titration###########
#create an empty matrix to put the TA values in
nrows<-nrow(Mass) # number of rows in a mass file
TA <- data.frame(matrix(nrow = nrows, ncol = 3))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("Sample.ID",'TA','Mass') # changed SampleID to Sample.ID

#run a for loop to bring in the titration files one at a time and calculate TA
# read in the mega concatenated titration results file
filename<-file.path(path,titrationfile)
AllData<-read.csv(filename, sep=",", na.string="NA",as.is=T, skip=12)[ ,1:5] 
#AllData <- AllData[-1,]
# Identifies rows starting with zero seconds "0" in column 2
sample_name_positions <- c(1,grep("^0", AllData[,2]), nrow(AllData))
sample_name_positions <- sample_name_positions[-1] #remove first report of duplicated 1

## parse through all the data in the one file ###
sample_names<-Mass$sample
# create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}


# fill the list with the data from each sample
for (i in 1:nrows){
sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}


for(i in 1:nrows) {
#  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
 # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  Data<-sample_names_list[[i]]
  # everything was brought in as a character because of the second line, converts back to numeric
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced through coercion
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature)) ## supress the warnings since NA will be produced through coercion
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume)) ## supress the warnings since NA will be produced through coercion
  #name of the file without .csv
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  name<-sample_names[i]
  
  #calculates the index of values between pH 2 and 3.5 
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  #Bottle A15 20190218 SJG  - acid titrant# , 
  #density of your titrant: change every time acid is changed
  
  d<-1.02901 -(0.0001233*mean(Data$Temperature[mV], na.rm=T)) - (0.0000037*(mean(Data$Temperature[mV], na.rm=T)^2)) 
  #Batch A15
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  c<-0.100146 #Batch A15 first used by SJG on 20190218 NEW 
  
  #------------------------------------------------------------------------------
  
  #Salinity of your samples
  s<-Mass[Mass$sample==name,3]
  #s<-Mass[name,2]
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$sample==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name
  TA[i,2]<-1000000*at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass
  #TA[i,4]<-sample.index
}
TA[,2:3]<-sapply(TA[,2:3], as.numeric) # make sure the appropriate columns are numeric
#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Data/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

write.table(update.data,"Data/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)
