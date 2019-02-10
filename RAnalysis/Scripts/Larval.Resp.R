#Title: Respiration Calculations
#Author: Sam Gurr & HM Putnam
#Edited by: Sam Gurr
#Date Last Modified: 20190209
#See Readme file for details

rm(list=ls()) #clears workspace 

## install packages if you dont already have them in your library
if ("devtools" %in% rownames(installed.packages()) == 'FALSE') install.packages('devtools') 
library(devtools)
if ("segmented" %in% rownames(installed.packages()) == 'FALSE') install.packages('segmented') 
if ("plotrix" %in% rownames(installed.packages()) == 'FALSE') install.packages('plotrix') 
if ("gridExtra" %in% rownames(installed.packages()) == 'FALSE') install.packages('gridExtra') 
if ("LoLinR" %in% rownames(installed.packages()) == 'FALSE') install_github('colin-olito/LoLinR') 
if ("lubridate" %in% rownames(installed.packages()) == 'FALSE') install.packages('lubridate') 
if ("chron" %in% rownames(installed.packages()) == 'FALSE') install.packages('chron') 
if ("plyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('plyr') 
if ("dplyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('dplyr') 


#Read in required libraries
##### Include Versions of libraries
#install_github('colin-olito/LoLinR')
library("ggplot2")
library("segmented")
library("plotrix")
library("gridExtra")
library("LoLinR")
library("lubridate")
library("chron")
library('plyr')
library('dplyr')

#Required Data files

# Set Working Directory:
# setwd("~/MyProjects/Geoduck_Conditioning/RAnalysis/") #set working
setwd("C:/Users/samjg/Documents/My_Projects/Geoduck_transgen_offspring_OA/RAnalysis/")

# CHANGE THE FOLLOWING ..THEN CONTROL A + ENTER
# Respiration File
path.p<-"Data/SDR_data/All_resp_data" #the location of all your respirometry files 
a <- 0.4
ouputNAME<-"Data/SDR_data/Cumulative_resp_alpha0.4.csv" 


# bring in the respiration file names
file.names<-basename(list.files(path = path.p, pattern = "csv$", recursive = TRUE)) #list all csv file names in the folder and subfolders
#basename above removes the subdirectory name from the file
#add file names that include the subdirectory name
file.names.full<-list.files(path = path.p, pattern = "csv$", recursive = TRUE) #list all csv file names in the folder and subfolders

#generate a 3 column dataframe with specific column names
#Resp.R <- data.frame(matrix(NA, nrow=length(file.names)*2, ncol=3))
#colnames(Resp.R) <- c("Sample.ID","Intercept", "umol.L.sec")

#Load Sample Info
Sample.Info <- read.csv(file="Data/SDR_data/REFERENCE_number.individuals_shell.size.csv", header=T) #read sample.info data

#includes treatment, tank, chamber volume, animal size/number etc for normalization
df_total <- data.frame() # start dataframe 
resp.table <- data.frame(matrix(nrow = 1, ncol = 7)) # create dataframe to save cumunalitively during for loop
colnames(resp.table)<-c('Date', 'RUN', 'SDR_position', 'Lpc', 'Leq' , 'Lz', 'alpha') # names for comuns in the for loop

for(i in 1:length(file.names)) { # for every file in list start at the first and run this following function
  Resp.Data <-read.table(file.path(path.p,file.names[i]), skip = 56, header=T, sep=",", na.string="NA", fill = TRUE, as.is=TRUE, fileEncoding="latin1") #reads in the data files
  Resp.Data$Time.Min. <- seq.int(0.017, (nrow(Resp.Data))*0.25, by=0.25) #set time in min
  #Resp.Data[Resp.Data[,] == "No Sensor"] <- as.numeric(runif(nrow(Resp.Data), min=0, max=300)) #convert any vials with no data
  Resp.Data <- Resp.Data[,2:27] #use only res values - 24 total in the 24 well plate (SDR SensorDish)
  

  for(j in 2:(ncol(Resp.Data)-1)){
    model <- rankLocReg(
      xall=Resp.Data$Time.Min., yall=as.numeric(Resp.Data[, j]),
      alpha=a, method="pc", verbose=TRUE) # run the LoLin script
    
    sum.table<-summary(model)
    resp.table$Date <- substr(file.names[i], 1,8) # all files have date in the form of yyyymmdd at the start of each csv name
    resp.table$RUN <- substr(file.names[i], 15,15) # assign the run to the number in the title for the trials completed that day
    resp.table$SDR_position <- colnames(Resp.Data[j]) # assign the vial position - this will be related to contents (blank or individuals) later in script
    resp.table$alpha <- a # set at start of script - reresents the proportion of data for final estimate of slopes (Lpc, Leq, Lz)
    resp.table$Lpc <-sum.table$Lcompare[3,6] # Lpc slope 
    resp.table$Leq <-sum.table$Lcompare[2,6] # Leq slope 
    resp.table$Lz <-sum.table$Lcompare[1,6]  # Lz slope 
    #resp.table$ci.Lz<-sum.table$Lcompare[1,9]
    #resp.table$ci.Leq<-sum.table$Lcompare[2,9]
    #resp.table$ci.Lpc<-sum.table$Lcompare[3,9]
    
    
    df <- data.frame(resp.table) # name dataframe for this singl e row
    df_total <- rbind(df_total,df) #bind to a cumulative list dataframe
    print(df_total) # print to monitor progress
  
    # save plots every inside loop and name by date_run_vialposition
  pdf(paste0("C:/Users/samjg/Documents/My_Projects/Geoduck_transgen_offspring_OA/RAnalysis/Data/SDR_data/All_resp_data/plots/",substr(file.names[i], 1,8),"_", "RUN",substr(file.names[i], 15,15),"_",colnames(Resp.Data[j]),"_regression.pdf"))
  plot(model)
  dev.off()
} # end of inside for loop
} # end of outside for loop


write.table(df_total,ouputNAME,sep=",", row.names=FALSE)  # write out to the path names outputNAME

x <- merge(df_total, Sample.Info, by=c("Date","SDR_position", "RUN")) # merge the individual info (size, estimate number of larvae) by common columns 
x
names(x)

JUVresp_all <- x %>% 
  filter((substr(x$Notes, 1,9)) == "juveniles") # call only resp values of juveniles
JUVresp_blanks <- JUVresp_all %>%  filter(JUVresp_all$Tank.ID == "Blank") # call only blanks
JUVresp_blankMEANS <- JUVresp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

JUVresp_geoduck <- JUVresp_all %>% 
  filter(!is.na(length_number.individuals))

JUVresp_geoduck$Resp_rate_ug.mol <-
  ((((((abs(JUVresp_geoduck$Lpc)) - (JUVresp_blankMEANS$mean_Lpc))*(4/1000))*(60))*31.998)/(JUVresp_geoduck$length_number.individuals))
JUVresp_geoduck$Resp_rate_ug.mol


JUVresp_table_treatments_ALL <- JUVresp_geoduck %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
JUVresp_table_treatments_ALL # view table

JUVresp_table_treatments_INITIAL <- JUVresp_geoduck %>%
  group_by(Treat.initial) %>% #group the dataset by INITIAL TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
JUVresp_table_treatments_INITIAL # view table

JUVresp_table_treatments_SECONDARY<- JUVresp_geoduck %>%
  group_by(Treat.Secondary) %>% #group the dataset by SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
JUVresp_table_treatments_SECONDARY # view table

JUVresp.mod  <- aov(Resp_rate_ug.mol~Treat.initial*Treat.Secondary, data = JUVresp_geoduck)
anova(JUVresp.mod)
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
shapiro.test((residuals(JUVresp.mod)))
hist(residuals(JUVresp.mod)) #plot histogram of residuals
boxplot(residuals(JUVresp.mod)) #plot boxplot of residuals
plot(fitted(JUVresp.mod),residuals(JUVresp.mod))
library(Rmisc)
sum_JUVresp_means <- summarySE(JUVresp_geoduck, 
                             measurevar="Resp_rate_ug.mol", 
                             groupvars=c("Treat.Secondary")) # summarize previous table for overall treatment 
sum_JUVresp_means # view the table
percentdiff <- ((sum_JUVresp_means[1,3] - sum_JUVresp_means[2,3])/sum_JUVresp_means[1,3])*100 # calculate percent difference
percentdiff # 5.8% greater shell length from animals initally exposed to low pH in initial exp trial
