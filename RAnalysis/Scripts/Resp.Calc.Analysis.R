#Title: Respiration Calculations
#Author: Sam Gurr 
#Edited by: Sam Gurr
#Date Last Modified: 20190329
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

# Set Working Directory:
# setwd("~/MyProjects/Geoduck_Conditioning/RAnalysis/") #set working
setwd("C:/Users/samjg/Documents/My_Projects/Geoduck_transgen_offspring_OA/RAnalysis/")
#Load Sample Info
Sample.Info <- read.csv(file="Data/SDR_data/REFERENCE_number.individuals_shell.size.csv", header=T) #read sample.info data

# X = the cumulative summary table of all Lolin outputs 
# (1) merge witht he individual number or size data after you make the cumulative table
#x <- merge(df_total, Sample.Info, by=c("Date","SDR_position", "RUN")) # merge the individual info (size, estimate number of larvae) by common columns 
# (2) instead of making this table (renders through many csv files for hours) - open the csv of the finished table itself
# call the cumulative resp table of Lolin raw outputs
cumulative_resp_table <- read.csv(file="Data/SDR_data/Cumulative_resp_alpha0.4.csv", header=T) #read sample.info data
# call the sample info of size and number of individuals per trial ( sample info called above)
x <- merge(cumulative_resp_table, Sample.Info, by=c("Date","SDR_position", "RUN"))


# D-hinge second drop respiration 20190329 #----------------------------------------------

Dhingeresp_190329 <- x %>% 
  filter((substr(x$Date, 1,9)) == "20190329") # call only resp values of juveniles

# T0 --------------- not enogh larvae alive to quanitfy for trial - T0 ended today 20190329

# Run 1 T1 ---------------
Dhingeresp_T1 <- Dhingeresp_190329 %>%
  filter((Dhingeresp_190329$RUN) == 1)# call only resp values of juveniles
DhingeT1resp_blanks <- Dhingeresp_T1 %>%  filter(Dhingeresp_T1$Tank.ID == "Blank") # call only blanks
DhingeT1resp_blankMEANS <- DhingeT1resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT1resp_geoduck <- Dhingeresp_T1 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT1resp_geoduck$Resp_rate_ug.mol <-
  ((((((abs(DhingeT1resp_geoduck$Lpc)) - (DhingeT1resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT1resp_geoduck$length_number.individuals))
DhingeT1resp_geoduck$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT1resp_table_treatments_ALL <- DhingeT1resp_geoduck %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
DhingeT1resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks

# Run 2 T2 ---------------
Dhingeresp_T2 <- Dhingeresp_190329 %>%
  filter((Dhingeresp_190329$RUN) == 2)# call only resp values of juveniles
DhingeT2resp_blanks <- Dhingeresp_T2 %>%  filter(Dhingeresp_T2$Tank.ID == "Blank") # call only blanks
DhingeT2resp_blankMEANS <- DhingeT2resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT2resp_geoduck <- Dhingeresp_T2 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT2resp_geoduck$Resp_rate_ug.mol <-
  ((((((abs(DhingeT2resp_geoduck$Lpc)) - (DhingeT2resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT2resp_geoduck$length_number.individuals))
DhingeT2resp_geoduck$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT2resp_table_treatments_ALL <- DhingeT2resp_geoduck %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
DhingeT2resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks

# Run 3 T3 ---------------
Dhingeresp_T3 <- Dhingeresp_190329 %>%
  filter((Dhingeresp_190329$RUN) == 3)# call only resp values of juveniles
DhingeT3resp_blanks <- Dhingeresp_T3 %>%  filter(Dhingeresp_T3$Tank.ID == "Blank") # call only blanks
DhingeT3resp_blankMEANS <- DhingeT3resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT3resp_geoduck <- Dhingeresp_T3 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT3resp_geoduck$Resp_rate_ug.mol <-
  ((((((abs(DhingeT3resp_geoduck$Lpc)) - (DhingeT3resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT3resp_geoduck$length_number.individuals))
DhingeT3resp_geoduck$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT3resp_table_treatments_ALL <- DhingeT3resp_geoduck %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
DhingeT3resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks




# D-hinge first drop respiration 20190325 #----------------------------------------------

Dhingeresp_190325 <- x %>% 
  filter((substr(x$Date, 1,9)) == "20190325") # call only resp values of juveniles

# Run 1 T0 ---------------
Dhingeresp_T0 <- Dhingeresp_190325 %>%
  filter((Dhingeresp_190325$RUN) == 1)# call only resp values of juveniles
DhingeT0resp_blanks <- Dhingeresp_T0 %>%  filter(Dhingeresp_T0$Tank.ID == "Blank") # call only blanks
DhingeT0resp_blankMEANS <- DhingeT0resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT0resp_geoduck <- Dhingeresp_T0 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT0resp_geoduck$Resp_rate_ug.mol <-
  ((((((abs(DhingeT0resp_geoduck$Lpc)) - (DhingeT0resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT0resp_geoduck$length_number.individuals))
DhingeT0resp_geoduck$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT0resp_table_treatments_ALL <- DhingeT0resp_geoduck %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
DhingeT0resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks


# Run 2 T1 ---------------
Dhingeresp_T1 <- Dhingeresp_190325 %>%
  filter((Dhingeresp_190325$RUN) == 2)# call only resp values of juveniles
DhingeT1resp_blanks <- Dhingeresp_T1 %>%  filter(Dhingeresp_T1$Tank.ID == "Blank") # call only blanks
DhingeT1resp_blankMEANS <- DhingeT1resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT1resp_geoduck <- Dhingeresp_T1 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT1resp_geoduck$Resp_rate_ug.mol <-
  ((((((abs(DhingeT1resp_geoduck$Lpc)) - (DhingeT1resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT1resp_geoduck$length_number.individuals))
DhingeT1resp_geoduck$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT1resp_table_treatments_ALL <- DhingeT1resp_geoduck %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
DhingeT1resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks

# Run 3 T2 ---------------
Dhingeresp_T2 <- Dhingeresp_190325 %>%
  filter((Dhingeresp_190325$RUN) == 3)# call only resp values of juveniles
DhingeT2resp_blanks <- Dhingeresp_T2 %>%  filter(Dhingeresp_T2$Tank.ID == "Blank") # call only blanks
DhingeT2resp_blankMEANS <- DhingeT2resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT2resp_geoduck <- Dhingeresp_T2 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT2resp_geoduck$Resp_rate_ug.mol <-
  ((((((abs(DhingeT2resp_geoduck$Lpc)) - (DhingeT2resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT2resp_geoduck$length_number.individuals))
DhingeT2resp_geoduck$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT2resp_table_treatments_ALL <- DhingeT2resp_geoduck %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
DhingeT2resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks

# Run 4 T3 ---------------
Dhingeresp_T3 <- Dhingeresp_190325 %>%
  filter((Dhingeresp_190325$RUN) == 4)# call only resp values of juveniles
DhingeT3resp_blanks <- Dhingeresp_T3 %>%  filter(Dhingeresp_T3$Tank.ID == "Blank") # call only blanks
DhingeT3resp_blankMEANS <- DhingeT3resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT3resp_geoduck <- Dhingeresp_T3 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT3resp_geoduck$Resp_rate_ug.mol <-
  ((((((abs(DhingeT3resp_geoduck$Lpc)) - (DhingeT3resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT3resp_geoduck$length_number.individuals))
DhingeT3resp_geoduck$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT3resp_table_treatments_ALL <- DhingeT3resp_geoduck %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
DhingeT3resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks



# UMBONE LARVAE -----------------------------------------------------------------
UMBONEresp_all <- x %>% 
  filter((substr(x$Date, 1,9)) == "20190322") # call only resp values of juveniles
UMBONEresp_blanks <- UMBONEresp_all %>%  filter(UMBONEresp_all$Tank.ID == "Blank") # call only blanks
UMBONEresp_blankMEANS <- UMBONEresp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

UMBONEresp_geoduck <- UMBONEresp_all %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

UMBONEresp_geoduck$Resp_rate_ug.mol <-
  ((((((abs(UMBONEresp_geoduck$Lpc)) - (UMBONEresp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(UMBONEresp_geoduck$length_number.individuals))
UMBONEresp_geoduck$Resp_rate_ug.mol # units in ug O2/hr/individual

UMBONEresp_table_treatments_ALL <- UMBONEresp_geoduck %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
UMBONEresp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks



# Dhinge LARVAE on 20190207
Dhingeresp_all <- x %>% 
  filter((substr(x$Date, 1,9)) == "20190207")

# run 1 
Dhingeresp_RUN_1 <- Dhingeresp_all %>%
  filter((Dhingeresp_all$RUN) == 1)# call 
Dhingeresp_blanks_RUN_1 <- Dhingeresp_RUN_1 %>%  filter(Dhingeresp_RUN_1$Tank.ID == "Blank") # call only blanks
Dhingeresp_blankMEANS_RUN_1 <- Dhingeresp_blanks_RUN_1 %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

Dhingeresp_geoduck_RUN_1 <- Dhingeresp_RUN_1 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

Dhingeresp_geoduck_RUN_1$Resp_rate_ug.mol <-
  ((((((abs(Dhingeresp_geoduck_RUN_1$Lpc)) - (Dhingeresp_blankMEANS_RUN_1$mean_Lpc))*(0.08/1000))*(60))*31.998)/(Dhingeresp_geoduck_RUN_1$length_number.individuals))
Dhingeresp_geoduck_RUN_1$Resp_rate_ug.mol # units in ug O2/hr/individual

Dhingeresp_table_treatments_RUN_1 <- Dhingeresp_geoduck_RUN_1 %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
Dhingeresp_table_treatments_RUN_1 # view table - looks like the LARVAE resp was no different from the blanks

# run 2
Dhingeresp_RUN_2 <- Dhingeresp_all %>%
  filter((Dhingeresp_all$RUN) == 2)# call only resp values of juveniles
Dhingeresp_blanks_RUN_2 <- Dhingeresp_RUN_2 %>%  filter(Dhingeresp_RUN_2$Tank.ID == "Blank") # call only blanks
Dhingeresp_blankMEANS_RUN_2 <- Dhingeresp_blanks_RUN_2 %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

Dhingeresp_geoduck_RUN_2 <- Dhingeresp_RUN_2 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

Dhingeresp_geoduck_RUN_2$Resp_rate_ug.mol <-
  ((((((abs(Dhingeresp_geoduck_RUN_2$Lpc)) - (Dhingeresp_blankMEANS_RUN_2$mean_Lpc))*(0.08/1000))*(60))*31.998)/(Dhingeresp_geoduck_RUN_2$length_number.individuals))
Dhingeresp_geoduck_RUN_2$Resp_rate_ug.mol # units in ug O2/hr/individual

Dhingeresp_table_treatments_RUN_2 <- Dhingeresp_geoduck_RUN_2 %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
Dhingeresp_table_treatments_RUN_2 # view table - looks like the LARVAE resp was no different from the blanks


# EMBRYOS --
# analysis of EMBRYOS resp - from 20190131
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
