#Title: Respiration Calculations
#Author: Sam Gurr & HM Putnam
#Edited by: Sam Gurr
#Date Last Modified: 20190306
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
#Load Sample Info
Sample.Info <- read.csv(file="Data/SDR_data/REFERENCE_number.individuals_shell.size.csv", header=T) #read sample.info data


# CHANGE THE FOLLOWING ..THEN CONTROL A + ENTER
# Respiration File
path.p<-"Data/SDR_data/All_resp_data" #the location of all your respirometry files 
a <- 0.4
ouputNAME<-"Data/SDR_data/Cumulative_resp_alpha0.4.csv" 


# bring in the respiration file names
file.names.full<-basename(list.files(path = path.p, pattern = "csv$", recursive = TRUE)) #list all csv file names in the folder and subfolders
file.names <- file.names.full[c(14:15)] # call the files you want to analyze and rbind to the current cumunaltive file

#generate a 3 column dataframe with specific column names
#Resp.R <- data.frame(matrix(NA, nrow=length(file.names)*2, ncol=3))
#colnames(Resp.R) <- c("Sample.ID","Intercept", "umol.L.sec")

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
  pdf(paste0("C:/Users/samjg/Documents/My_Projects/Geoduck_transgen_offspring_OA/RAnalysis/Data/SDR_data/All_resp_data/plots_alpha0.4/",substr(file.names[i], 1,8),"_", "RUN",substr(file.names[i], 15,15),"_",colnames(Resp.Data[j]),"_regression.pdf"))
  plot(model)
  dev.off()
} # end of inside for loop
} # end of outside for loop

# merge with the preexisiting table?
cumulative_resp_table <- read.csv(file="Data/SDR_data/Cumulative_resp_alpha0.4.csv", header=T) #call the pre exisiting cumulative table
new_table <- rbind(cumulative_resp_table, df_total) # bind the new table from the for loop to the pre exisiting table
write.table(new_table,ouputNAME,sep=",", row.names=FALSE)  # write out to the path names outputNAME

# X = the cumulative summary table of all Lolin outputs 
# (1) merge witht he individual number or size data after you make the cumulative table
#x <- merge(df_total, Sample.Info, by=c("Date","SDR_position", "RUN")) # merge the individual info (size, estimate number of larvae) by common columns 
# (2) instead of making this table (renders through many csv files for hours) - open the csv of the finished table itself
# call the cumulative resp table of Lolin raw outputs
cumulative_resp_table <- read.csv(file="Data/SDR_data/Cumulative_resp_alpha0.4.csv", header=T) #read sample.info data
# call the sample info of size and number of individuals per trial ( sample info called above)
x <- merge(cumulative_resp_table, Sample.Info, by=c("Date","SDR_position", "RUN"))

# analysis of LARVAE resp - from 20190131
LARVAEresp_all <- x %>% 
  filter((substr(x$Date, 1,9)) == "20190131") # call only resp values of juveniles
LARVAEresp_blanks <- LARVAEresp_all %>%  filter(LARVAEresp_all$Tank.ID == "Blank") # call only blanks
LARVAEresp_blankMEANS <- LARVAEresp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

LARVAEresp_geoduck <- LARVAEresp_all %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

LARVAEresp_geoduck$Resp_rate_ug.mol <-
  ((((((abs(LARVAEresp_geoduck$Lpc)) - (LARVAEresp_blankMEANS$mean_Lpc))*(4/1000))*(60))*31.998)/(LARVAEresp_geoduck$length_number.individuals))
LARVAEresp_geoduck$Resp_rate_ug.mol

# RESPIRATION SUMMARY TABLE
LARVAEresp_table_treatments_ALL <- LARVAEresp_geoduck %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
LARVAEresp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks

# analysis of LARVAE resp - from 20190131
LARVAEresp_all <- x %>% 
  filter((substr(x$Date, 1,9)) == "20190207") # call only resp values of juveniles
LARVAEresp_blanks <- LARVAEresp_all %>%  filter(LARVAEresp_all$Tank.ID == "Blank") # call only blanks
LARVAEresp_blankMEANS <- LARVAEresp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

LARVAEresp_geoduck <- LARVAEresp_all %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

LARVAEresp_geoduck$Resp_rate_ug.mol <-
  ((((((abs(LARVAEresp_geoduck$Lpc)) - (LARVAEresp_blankMEANS$mean_Lpc))*(4/1000))*(60))*31.998)/(LARVAEresp_geoduck$length_number.individuals))
LARVAEresp_geoduck$Resp_rate_ug.mol

# RESPIRATION SUMMARY TABLE
LARVAEresp_table_treatments_ALL <- LARVAEresp_geoduck %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
LARVAEresp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks



# analysis of juvenile resp 150d post summer experiments
JUVresp_all <- x %>% 
  filter((substr(x$Notes, 1,9)) == "juveniles") # call only resp values of juveniles
JUVresp_blanks <- JUVresp_all %>%  filter(JUVresp_all$Tank.ID == "Blank") # call only blanks
JUVresp_blankMEANS <- JUVresp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

JUVresp_geoduck <- JUVresp_all %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

JUVresp_geoduck$Resp_rate_ug.mol <-
  ((((((abs(JUVresp_geoduck$Lpc)) - (JUVresp_blankMEANS$mean_Lpc))*(4/1000))*(60))*31.998)/(JUVresp_geoduck$length_number.individuals))
JUVresp_geoduck$Resp_rate_ug.mol

# RESPIRATION SUMMARY TABLE
JUVresp_table_treatments_ALL <- JUVresp_geoduck %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
JUVresp_table_treatments_ALL # view table
# RESP: INITIAL TREATMENT sUMMRAY TABLE
JUVresp_table_treatments_INITIAL <- JUVresp_geoduck %>%
  group_by(Treat.initial) %>% #group the dataset by INITIAL TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
JUVresp_table_treatments_INITIAL # view table
# RESP: SECODNARY TREATMENT SUMMARY TABLE
JUVresp_table_treatments_SECONDARY<- JUVresp_geoduck %>%
  group_by(Treat.Secondary) %>% #group the dataset by SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
JUVresp_table_treatments_SECONDARY # view table
# RUN A TWO WAY ANOVA ON INITIAL AND SECONDARY TREATMENT ON RESP 
JUVresp.mod  <- aov(Resp_rate_ug.mol~Treat.initial*Treat.Secondary, data = JUVresp_geoduck)
anova(JUVresp.mod)
TukeyHSD(JUVresp.mod) # tukey HSD of the entire model shows a signifcant difference between the E×E and E×A treatments 
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
percentdiff # 57.58% greater shell met activity from animals initally exposed to ambient under intiial trial


# SHELL SIZE SUMMARY TABLE
JUVsize_table_treatments_ALL <- JUVresp_geoduck %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_size = mean(length_number.individuals),
            min_size = min(length_number.individuals),
            sd_size = sd(length_number.individuals),
            SEM = ((sd(length_number.individuals))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_size)) # makes table in descending order 
JUVsize_table_treatments_ALL # view table - IMPORTANT - looks like initial exposure exposure has the greatest shell size!
# SIZE: INITIAL TREATMENT sUMMRAY TABLE
JUVsize_table_treatments_INITIAL <- JUVresp_geoduck %>%
  group_by(Treat.initial) %>% #group the dataset by INITIAL TREATMENT
  summarise(mean_size = mean(length_number.individuals),
            min_size = min(length_number.individuals),
            sd_size = sd(length_number.individuals),
            SEM = ((sd(length_number.individuals))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_size)) # makes table in descending order 
JUVsize_table_treatments_INITIAL # view table - further exemplifies the difference in shell size from the initial treatment
# SIZE: SECODNARY TREATMENT SUMMARY TABLE
JUVsize_table_treatments_SECONDARY<- JUVresp_geoduck %>%
  group_by(Treat.Secondary) %>% #group the dataset by SECONDARY TREATMENT
  summarise(mean_size = mean(length_number.individuals),
            min_size = min(length_number.individuals),
            sd_size = sd(length_number.individuals),
            SEM = ((sd(length_number.individuals))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_size)) # makes table in descending order - however secodary exposure has a shorter shell length
JUVsize_table_treatments_SECONDARY # view table
# RUN A TWO WAY ANOVA ON TREATMENT AND SIZE 150 D POST EXPERIMENT
JUVsize.mod  <- aov(length_number.individuals~Treat.initial*Treat.Secondary, data = JUVresp_geoduck)
anova(JUVsize.mod) # *significant difference under intial treatment and a marginal difference from secondary treatment
TukeyHSD(JUVsize.mod) # tukey HSD of the entire model shows a signifcant difference between the A×E and E×A treatments 
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
shapiro.test((residuals(JUVsize.mod))) # normal distributed via shapiro wilk test
hist(residuals(JUVsize.mod)) #plot histogram of residuals
boxplot(residuals(JUVsize.mod)) #plot boxplot of residuals
plot(fitted(JUVsize.mod),residuals(JUVsize.mod))
library(Rmisc)
sum_JUVsize_means <- summarySE(JUVsize.mod, 
                               measurevar="length_number.individuals", 
                               groupvars=c("Treat.Secondary")) # summarize previous table for overall treatment 
sum_JUVsize_means # view the table
percentdiff <- ((sum_JUVsize_means[1,3] - sum_JUVsize_means[2,3])/sum_JUVsize_means[1,3])*100 # calculate percent difference
percentdiff # 5.8% greater shell length from animals initally exposed to low pH in initial exp trial

# PLOTS
par(mfrow=c(2,1))
par(mar=c(1,1))
plot_size <- ggplot(JUVsize_table_treatments_ALL, aes(x=Treatment, y=mean_size)) + 
  geom_errorbar(aes(ymin=mean_size-SEM , ymax=mean_size+SEM ), width=.1) +
  geom_line() +
  geom_point() +
  theme_classic() #Set the background color
plot_size  

plot_resp <- ggplot(JUVresp_table_treatments_ALL, aes(x=Treatment, y=mean_resp)) + 
  geom_errorbar(aes(ymin=mean_resp-SEM , ymax=mean_resp+SEM ), width=.1) +
  geom_line() +
  geom_point() +
  theme_classic() #Set the background color
plot_resp 
