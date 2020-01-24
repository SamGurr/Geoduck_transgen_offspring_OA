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
if ("lmtest" %in% rownames(installed.packages()) == 'FALSE') install.packages('lmtest') 
if ("car" %in% rownames(installed.packages()) == 'FALSE') install.packages('car') 

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
library('car')
library('lmtest')

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




# Pediveliger fifth drop respiration 20190418 #----------------------------------------------

Pediveligerresp_20190418<- x %>% 
  filter((substr(x$Date, 1,9)) == "20190418") # call only resp values of juveniles

# Run 1 T1_T3 ---------------
Pediveligerresp_T1_T3 <- Pediveligerresp_20190418 %>%
  filter((Pediveligerresp_20190418$RUN) == 1)# call only resp values of juveniles
PediveligerT1_T3resp_blanks <- Pediveligerresp_T1_T3 %>%  filter(Pediveligerresp_T1_T3$Tank.ID == "Blank") # call only blanks
PediveligerT1_T3resp_blankMEANS <- PediveligerT1_T3resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

PediveligerT1_T3resp_geoduck_7 <- Pediveligerresp_T1_T3 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

PediveligerT1_T3resp_geoduck_7$Resp_rate_ug.mol <-
  ((((((abs(PediveligerT1_T3resp_geoduck_7$Lpc)) - (PediveligerT1_T3resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(PediveligerT1_T3resp_geoduck_7$length_number.individuals))
PediveligerT1_T3resp_geoduck_7$Resp_rate_ug.mol # units in ug O2/hr/individual

PediveligerT1_T3resp_geoduck_7 <- PediveligerT1_T3resp_geoduck_7 %>%
  filter(PediveligerT1_T3resp_geoduck_7$Resp_rate_ug.mol > 0)

PediveligerT1_T3resp_table_treatments_ALL <- PediveligerT1_T3resp_geoduck_7 %>%
  #filter(PediveligerT1_T3resp_geoduck$Resp_rate_ug.mol > 0) %>% #filter out all negative rates
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
PediveligerT1_T3resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks

# SUMMARY DATA FROM TANK DROP 7
Day32_20190418 <- PediveligerT1_T3resp_geoduck_7
Day32_20190418$Day <- "32"



# D-hinge fifth drop respiration 20190410 #----------------------------------------------

Dhingeresp_20190410<- x %>% 
  filter((substr(x$Date, 1,9)) == "20190410") # call only resp values of juveniles

# Run 1 T1 ---------------
Dhingeresp_T1 <- Dhingeresp_20190410 %>%
  filter((Dhingeresp_20190410$RUN) == 1)# call only resp values of juveniles
DhingeT1resp_blanks <- Dhingeresp_T1 %>%  filter(Dhingeresp_T1$Tank.ID == "Blank") # call only blanks
DhingeT1resp_blankMEANS <- DhingeT1resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT1resp_geoduck_5 <- Dhingeresp_T1 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT1resp_geoduck_5$Resp_rate_ug.mol <-
  ((((((abs(DhingeT1resp_geoduck_5$Lpc)) - (DhingeT1resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT1resp_geoduck_5$length_number.individuals))
DhingeT1resp_geoduck_5$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT1resp_geoduck_5 <- DhingeT1resp_geoduck_5 %>%
  filter(DhingeT1resp_geoduck_5$Resp_rate_ug.mol > 0)

DhingeT1resp_table_treatments_ALL <- DhingeT1resp_geoduck_5 %>%
  filter(DhingeT1resp_geoduck$Resp_rate_ug.mol > 0) %>% #filter out all negative rates
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
DhingeT1resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks


# Run 2 T3 --------------- (did not do T2 becasue there was a low yield of larvae)
Dhingeresp_t3 <- Dhingeresp_20190410 %>%
  filter((Dhingeresp_20190410$RUN) == 2)# call only resp values of juveniles
Dhinget3resp_blanks <- Dhingeresp_t3 %>%  filter(Dhingeresp_t3$Tank.ID == "Blank") # call only blanks
Dhinget3resp_blankMEANS <- Dhinget3resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT3resp_geoduck_5 <- Dhingeresp_t3 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT3resp_geoduck_5$Resp_rate_ug.mol <-
  ((((((abs(DhingeT3resp_geoduck_5$Lpc)) - (Dhinget3resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT3resp_geoduck_5$length_number.individuals))
DhingeT3resp_geoduck_5$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT3resp_geoduck_5 <- DhingeT3resp_geoduck_5 %>%
  filter(DhingeT3resp_geoduck_5$Resp_rate_ug.mol > 0)

Dhinget3resp_table_treatments_ALL <- DhingeT3resp_geoduck_5 %>%
  group_by(Treatment) %>% 
  filter(Dhinget3resp_geoduck$Resp_rate_ug.mol > 0) %>% #filter out all negative rates
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
Dhinget3resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks

# SUMMARY DATA FROM TANK DROP 5
Day24_20190410 <- rbind(DhingeT1resp_geoduck_5, DhingeT3resp_geoduck_5)
Day24_20190410$Day <- "24"

###############################################
# D-hinge fourth drop respiration 20190406 #----------------------------------------------
###############################################

Dhingeresp_20190406<- x %>% 
  filter((substr(x$Date, 1,9)) == "20190406") # call only resp values of juveniles

# Run 1 T1 ---------------
Dhingeresp_T1 <- Dhingeresp_20190406 %>%
  filter((Dhingeresp_20190406$RUN) == 1)# call only resp values of juveniles
DhingeT1resp_blanks <- Dhingeresp_T1 %>%  filter(Dhingeresp_T1$Tank.ID == "Blank") # call only blanks
DhingeT1resp_blankMEANS <- DhingeT1resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT1resp_geoduck_4 <- Dhingeresp_T1 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT1resp_geoduck_4$Resp_rate_ug.mol <-
  ((((((abs(DhingeT1resp_geoduck_4$Lpc)) - (DhingeT1resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT1resp_geoduck_4$length_number.individuals))
DhingeT1resp_geoduck$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT1resp_geoduck_4 <- DhingeT1resp_geoduck_4 %>%
  filter(DhingeT1resp_geoduck_4$Resp_rate_ug.mol > 0)

DhingeT1resp_table_treatments_ALL <- DhingeT1resp_geoduck_4 %>%
  filter(DhingeT1resp_geoduck_4$Resp_rate_ug.mol > 0) %>% #filter out all negative rates
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
Dhingeresp_T2 <- Dhingeresp_20190406 %>%
  filter((Dhingeresp_20190406$RUN) == 2)# call only resp values of juveniles
DhingeT2resp_blanks <- Dhingeresp_T2 %>%  filter(Dhingeresp_T2$Tank.ID == "Blank") # call only blanks
DhingeT2resp_blankMEANS <- DhingeT2resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT2resp_geoduck_4 <- Dhingeresp_T2 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT2resp_geoduck_4$Resp_rate_ug.mol <-
  ((((((abs(DhingeT2resp_geoduck_4$Lpc)) - (DhingeT2resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT2resp_geoduck_4$length_number.individuals))
DhingeT2resp_geoduck_4$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT2resp_geoduck_4 <- DhingeT2resp_geoduck_4 %>%
  filter(DhingeT2resp_geoduck_4$Resp_rate_ug.mol > 0)

DhingeT2resp_table_treatments_ALL <- DhingeT2resp_geoduck_4 %>%
  group_by(Treatment) %>% 
  filter(DhingeT2resp_geoduck_4$Resp_rate_ug.mol > 0) %>% #filter out all negative rates
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
DhingeT2resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks

# Run 3 T3 ---------------
Dhingeresp_T3 <- Dhingeresp_20190406 %>%
  filter((Dhingeresp_20190406$RUN) == 3)# call only resp values of juveniles
DhingeT3resp_blanks <- Dhingeresp_T3 %>%  filter(Dhingeresp_T3$Tank.ID == "Blank") # call only blanks
DhingeT3resp_blankMEANS <- DhingeT3resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT3resp_geoduck_4 <- Dhingeresp_T3 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT3resp_geoduck_4$Resp_rate_ug.mol <-
  ((((((abs(DhingeT3resp_geoduck_4$Lpc)) - (DhingeT3resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT3resp_geoduck_4$length_number.individuals))
DhingeT3resp_geoduck_4$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT3resp_geoduck_4 <- DhingeT3resp_geoduck_4 %>%
  filter(DhingeT3resp_geoduck_4$Resp_rate_ug.mol > 0)

DhingeT3resp_table_treatments_ALL <- DhingeT3resp_geoduck_4 %>%
  group_by(Treatment) %>% 
  filter(DhingeT3resp_geoduck_4$Resp_rate_ug.mol > 0) %>% #filter out all negative rates
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
DhingeT3resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks

# SUMMARY DATA FROM TANK DROP 5
Day20_20190406 <- rbind(DhingeT1resp_geoduck_4, DhingeT3resp_geoduck_4)
Day20_20190406$Day <- "20"

#######################################################
# D-hinge third drop respiration 20190402 #----------------------------------------------
#######################################################

Dhingeresp_20190402<- x %>% 
  filter((substr(x$Date, 1,9)) == "20190402") # call only resp values of juveniles

# Run 1 T1 ---------------
Dhingeresp_T1 <- Dhingeresp_20190402 %>%
  filter((Dhingeresp_20190402$RUN) == 1)# call only resp values of juveniles
DhingeT1resp_blanks <- Dhingeresp_T1 %>%  filter(Dhingeresp_T1$Tank.ID == "Blank") # call only blanks
DhingeT1resp_blankMEANS <- DhingeT1resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT1resp_geoduck_3 <- Dhingeresp_T1 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT1resp_geoduck_3$Resp_rate_ug.mol <-
  ((((((abs(DhingeT1resp_geoduck_3$Lpc)) - (DhingeT1resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT1resp_geoduck_3$length_number.individuals))
DhingeT1resp_geoduck_3$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT1resp_geoduck_3 <- DhingeT1resp_geoduck_3 %>%
  filter(DhingeT1resp_geoduck_3$Resp_rate_ug.mol > 0)

DhingeT1resp_table_treatments_ALL <- DhingeT1resp_geoduck_3 %>%
  filter(DhingeT1resp_geoduck_3$Resp_rate_ug.mol > 0) %>% #filter out all negative rates
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
Dhingeresp_T2 <- Dhingeresp_20190402 %>%
  filter((Dhingeresp_20190402$RUN) == 2)# call only resp values of juveniles
DhingeT2resp_blanks <- Dhingeresp_T2 %>%  filter(Dhingeresp_T2$Tank.ID == "Blank") # call only blanks
DhingeT2resp_blankMEANS <- DhingeT2resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT2resp_geoduck_3 <- Dhingeresp_T2 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT2resp_geoduck_3$Resp_rate_ug.mol <-
  ((((((abs(DhingeT2resp_geoduck_3$Lpc)) - (DhingeT2resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT2resp_geoduck_3$length_number.individuals))
DhingeT2resp_geoduck_3$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT2resp_geoduck_3 <- DhingeT2resp_geoduck_3 %>%
  filter(DhingeT2resp_geoduck_3$Resp_rate_ug.mol > 0)

DhingeT2resp_table_treatments_ALL <- DhingeT2resp_geoduck_3 %>%
  group_by(Treatment) %>% 
  filter(DhingeT2resp_geoduck_3$Resp_rate_ug.mol > 0) %>% #filter out all negative rates
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
DhingeT2resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks

# Run 3 T3 ---------------
Dhingeresp_T3 <- Dhingeresp_20190402 %>%
  filter((Dhingeresp_20190402$RUN) == 3)# call only resp values of juveniles
DhingeT3resp_blanks <- Dhingeresp_T3 %>%  filter(Dhingeresp_T3$Tank.ID == "Blank") # call only blanks
DhingeT3resp_blankMEANS <- DhingeT3resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT3resp_geoduck_3 <- Dhingeresp_T3 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT3resp_geoduck_3$Resp_rate_ug.mol <-
  ((((((abs(DhingeT3resp_geoduck_3$Lpc)) - (DhingeT3resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT3resp_geoduck_3$length_number.individuals))
DhingeT3resp_geoduck_3$Resp_rate_ug.mol # units in ug O2/hr/individual


DhingeT3resp_geoduck_3 <- DhingeT3resp_geoduck_3 %>%
  filter(DhingeT3resp_geoduck_3$Resp_rate_ug.mol > 0)

DhingeT3resp_table_treatments_ALL <- DhingeT3resp_geoduck_3 %>%
  group_by(Treatment) %>% 
  filter(DhingeT3resp_geoduck_3$Resp_rate_ug.mol > 0) %>% #filter out all negative rates
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
DhingeT3resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks

# SUMMARY DATA FROM TANK DROP 5
Day16_20190402 <- rbind(DhingeT1resp_geoduck_3, DhingeT3resp_geoduck_3)
Day16_20190402$Day <- "16"

#######################################################
# D-hinge second drop respiration 20190329 #----------------------------------------------
#######################################################

Dhingeresp_190329 <- x %>% 
  filter((substr(x$Date, 1,9)) == "20190329") # call only resp values of juveniles

# T0 --------------- not enogh larvae alive to quanitfy for trial - T0 ended today 20190329

# Run 1 T1 ---------------
Dhingeresp_T1 <- Dhingeresp_190329 %>%
  filter((Dhingeresp_190329$RUN) == 1)# call only resp values of juveniles
DhingeT1resp_blanks <- Dhingeresp_T1 %>%  filter(Dhingeresp_T1$Tank.ID == "Blank") # call only blanks
DhingeT1resp_blankMEANS <- DhingeT1resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT1resp_geoduck_2 <- Dhingeresp_T1 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT1resp_geoduck_2$Resp_rate_ug.mol <-
  ((((((abs(DhingeT1resp_geoduck_2$Lpc)) - (DhingeT1resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT1resp_geoduck_2$length_number.individuals))
DhingeT1resp_geoduck_2$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT1resp_geoduck_2 <- DhingeT1resp_geoduck_2 %>%
  filter(DhingeT1resp_geoduck_2$Resp_rate_ug.mol > 0)

DhingeT1resp_table_treatments_ALL <- DhingeT1resp_geoduck_2 %>%
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

DhingeT2resp_geoduck_2 <- Dhingeresp_T2 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT2resp_geoduck_2$Resp_rate_ug.mol <-
  ((((((abs(DhingeT2resp_geoduck_2$Lpc)) - (DhingeT2resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT2resp_geoduck_2$length_number.individuals))
DhingeT2resp_geoduck_2$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT2resp_geoduck_2 <- DhingeT2resp_geoduck_2 %>%
  filter(DhingeT2resp_geoduck_2$Resp_rate_ug.mol > 0)

DhingeT2resp_table_treatments_ALL <- DhingeT2resp_geoduck_2 %>%
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

DhingeT3resp_geoduck_2 <- Dhingeresp_T3 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT3resp_geoduck_2$Resp_rate_ug.mol <-
  ((((((abs(DhingeT3resp_geoduck_2$Lpc)) - (DhingeT3resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT3resp_geoduck_2$length_number.individuals))
DhingeT3resp_geoduck_2$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT3resp_geoduck_2 <- DhingeT3resp_geoduck_2 %>%
  filter(DhingeT3resp_geoduck_2$Resp_rate_ug.mol > 0)

DhingeT3resp_table_treatments_ALL <- DhingeT3resp_geoduck_2 %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            max_resp = max(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
DhingeT3resp_table_treatments_ALL # view table - looks like the LARVAE resp was no different from the blanks

# SUMMARY DATA FROM TANK DROP 2
Day12_20190329 <- rbind(DhingeT1resp_geoduck_2, DhingeT3resp_geoduck_2)
Day12_20190329$Day <- "12"

####################################################
# D-hinge first drop respiration 20190325 #----------------------------------------------
####################################################

Dhingeresp_190325 <- x %>% 
  filter((substr(x$Date, 1,9)) == "20190325") # call only resp values of juveniles

# Run 1 T0 ---------------
Dhingeresp_T0 <- Dhingeresp_190325 %>%
  filter((Dhingeresp_190325$RUN) == 1)# call only resp values of juveniles
DhingeT0resp_blanks <- Dhingeresp_T0 %>%  filter(Dhingeresp_T0$Tank.ID == "Blank") # call only blanks
DhingeT0resp_blankMEANS <- DhingeT0resp_blanks %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

DhingeT0resp_geoduck_1 <- Dhingeresp_T0 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT0resp_geoduck_1$Resp_rate_ug.mol <-
  ((((((abs(DhingeT0resp_geoduck_1$Lpc)) - (DhingeT0resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT0resp_geoduck_1$length_number.individuals))
DhingeT0resp_geoduck_1$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT0resp_geoduck_1 <- DhingeT0resp_geoduck_1 %>%
  filter(DhingeT0resp_geoduck_1$Resp_rate_ug.mol > 0)

DhingeT0resp_table_treatments_ALL <- DhingeT0resp_geoduck_1 %>%
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

DhingeT1resp_geoduck_1 <- Dhingeresp_T1 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT1resp_geoduck_1$Resp_rate_ug.mol <-
  ((((((abs(DhingeT1resp_geoduck_1$Lpc)) - (DhingeT1resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT1resp_geoduck_1$length_number.individuals))
DhingeT1resp_geoduck_1$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT1resp_geoduck_1 <- DhingeT1resp_geoduck_1 %>%
  filter(DhingeT1resp_geoduck_1$Resp_rate_ug.mol > 0)

DhingeT1resp_table_treatments_ALL <- DhingeT1resp_geoduck_1 %>%
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

DhingeT2resp_geoduck_1 <- Dhingeresp_T2 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT2resp_geoduck_1$Resp_rate_ug.mol <-
  ((((((abs(DhingeT2resp_geoduck_1$Lpc)) - (DhingeT2resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT2resp_geoduck_1$length_number.individuals))
DhingeT2resp_geoduck_1$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT2resp_geoduck_1 <- DhingeT2resp_geoduck_1 %>%
  filter(DhingeT2resp_geoduck_1$Resp_rate_ug.mol > 0)

DhingeT2resp_table_treatments_ALL <- DhingeT2resp_geoduck_1 %>%
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

DhingeT3resp_geoduck_1 <- Dhingeresp_T3 %>% 
  filter(!is.na(length_number.individuals)) # remove the NAs from the number of individuals (these are the blanks)

DhingeT3resp_geoduck_1$Resp_rate_ug.mol <-
  ((((((abs(DhingeT3resp_geoduck_1$Lpc)) - (DhingeT3resp_blankMEANS$mean_Lpc))*(0.08/1000))*(60))*31.998)/(DhingeT3resp_geoduck_1$length_number.individuals))
DhingeT3resp_geoduck_1$Resp_rate_ug.mol # units in ug O2/hr/individual

DhingeT3resp_geoduck_1 <- DhingeT3resp_geoduck_1 %>%
  filter(DhingeT3resp_geoduck_1$Resp_rate_ug.mol > 0)

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

# SUMMARY DATA FROM TANK DROP 1
Day8_20190325 <- rbind(DhingeT1resp_geoduck_1, DhingeT3resp_geoduck_1)
Day8_20190325$Day <- "8"


#PLOTS OF AMBIENT AND ELEVATED FROM JUST T1 AND T3
# RBIND ALL DATA

ALL_DATA <- rbind(Day8_20190325, Day12_20190329, Day16_20190402, Day20_20190406, Day24_20190410, Day32_20190418)

ALL_DATA$Resp_rate_ng.mol <- ALL_DATA$Resp_rate_ug.mol*1000

# resp fig for all data
RESP_FIG_T1_T3 <- ggplot(ALL_DATA, aes(x = factor(Date), y = Resp_rate_ng.mol, fill = Treatment)) +
  theme_classic() +
  scale_fill_manual(values=c("white", "grey3"), 
                    labels=c("P_ambient","P_elevated")) +
  geom_boxplot(alpha = 0.5, # color hue
               width=0.6, # boxplot width
               outlier.size=0, # make outliers small
               position = position_dodge(preserve = "single")) + 
  geom_point(pch = 19, position = position_jitterdodge(.05), size=1) +
  stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
               width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
  scale_x_discrete(labels = c(8,12,16,20,24,32)) +
  theme(legend.position = c(0.55,0.96), legend.direction="horizontal", legend.title=element_blank()) +
  ylim(0,10) + 
  labs(y=expression("Respiration rate"~ng~O[2]*hr^{-1}*individual^{-1}), x=expression("Age (days)"))
RESP_FIG_T1_T3.FINAL <- RESP_FIG_T1_T3  + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
        axis.line = element_line(color = 'black'),
        axis.ticks.length=unit(0.2, "cm"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) 
RESP_FIG_T1_T3.FINAL # view the plot

# resp fig for days 8 - 24 
ALL_DATA_8.24 <- ALL_DATA %>% 
  filter(!(Day == 32)) # make new dataset without day 32 data

RESP_FIG_DAYS8.24 <- ggplot(ALL_DATA_8.24, aes(x = factor(Date), y = (Resp_rate_ng.mol), fill = Treatment)) +
  theme_classic() +
scale_fill_manual(values=c("blue", "orange"), 
                  labels=c("ambient","var.pH")) +
  geom_boxplot(alpha = 0.5, # color hue
               width=0.6, # boxplot width
               outlier.size=0, # make outliers small
               position = position_dodge(preserve = "single")) + 
  geom_point(pch = 19, position = position_jitterdodge(.05), size=1) +
  stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
               width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
  scale_x_discrete(labels = c(8,12,16,20,24,32)) +
  theme(legend.position = c(0.55,0.96), legend.direction="horizontal", legend.title=element_blank()) +
  ylim(0,10) + 
  labs(y=expression("sqrt(Respiration rate)"~ng~O[2]*hr^{-1}*individual^{-1}), x=expression("Age"))
RESP_FIG_DAYS8.24_FINAL <- RESP_FIG_DAYS8.24  + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
        axis.line = element_line(color = 'black'),
        axis.ticks.length=unit(0.2, "cm"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) 
RESP_FIG_DAYS8.24_FINAL # view the plot



# TWO WAY ANOVA MODEL
hist(ALL_DATA_8.24$Resp_rate_ng.mol) # histogram shows positibe skew
# model on transformed respiration data
RESP_aov <- aov(Resp_rate_ng.mol~Treatment*Day, data = ALL_DATA_8.24) # run two way anova of parental treatment*time
summary(RESP_aov) # significant effect of treatment and time
# diagnostic tests and plots of model residuals (not transformed)
# Shapiro test
shapiro.test(residuals(RESP_aov)) # residuals are non-normal - p = 2.381e-05
# hist qq residual diagnostic
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(RESP_aov)) #plot histogram of residuals
boxplot(residuals(RESP_aov)) #plot boxplot of residuals
plot(fitted(RESP_aov),residuals(RESP_aov)) 
qqnorm(residuals(RESP_aov)) # qqplot

# TRANSFORM VIA SQRT AND RUN MODEL AGAIN - TEST SHAPRIO.WILK FOR NORMALITY ASSUMPTIONS
# model on transformed respiration data
ALL_DATA_8.24$Resp_rate_ng.mol_SQRT <- sqrt(ALL_DATA_8.24$Resp_rate_ng.mol) # new column for sqrt of the data
RESP_aov.sqrt <- aov(Resp_rate_ng.mol_SQRT~Treatment*Day, data = ALL_DATA_8.24) # run two way anova of parental treatment*time
summary(RESP_aov.sqrt) # looses the interaction term effect but keeps the effect of time and treatment seperately
# Shapiro test
shapiro.test(residuals(RESP_aov.sqrt)) # residuals are normal  - p =0.1048 
# post-hoc
library(lsmeans)        # Version: 2.27-62, Date/Publication: 2018-05-11, Depends: methods, R (>= 3.2)
# effect of Treatment ------------------------------- #
resp.ph <- lsmeans(RESP_aov.sqrt, pairwise ~ Treatment)# pariwise Tukey Post-hoc test between repeated treatments
resp.ph # view post hoc summary
E1.pairs.RESP.05 <- cld(resp.ph, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E1.pairs.RESP.05 # Ambient > Elevated ( Elevated  1.320509) ( Ambient   1.784314)
# effect of Day ------------------------------- #
resp.day <- lsmeans(RESP_aov.sqrt, pairwise ~ Day)# pariwise Tukey Post-hoc test between repeated treatments
resp.day # view post hoc summary
resp.day.RESP.05 <- cld(resp.day, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
resp.day.RESP.05 # day 16 least resp rate all others non sig diff




#RESP_FIG_DAYS8.24_FINAL <- 
 # RESP_FIG_DAYS8.24_FINAL + 
 # annotate("text", x=0.85, y=5.56, label = "de", size = 4) + 
# annotate("text", x=1.2, y=4, label = "abc", size = 4) + 
# annotate("text", x=1.9, y=10.0, label = "e", size = 4) + 
# annotate("text", x=2.2, y=4, label = "abc", size = 4) + 
# annotate("text", x=2.85, y=3.5, label = "ab", size = 4) + 
# annotate("text", x=3.2, y=2.2, label = "a", size = 4) + 
# annotate("text", x=3.85, y=8, label = "cde", size = 4) + 
# annotate("text", x=4.2, y=4.8, label = "ab", size = 4) + 
# annotate("text", x=4.85, y=8.6, label = "bcde", size = 4) + 
# annotate("text", x=5.2, y=5.0, label = "bcd", size = 4) 
# RESP_FIG_DAYS8.24_FINAL

#save file to output
ggsave(file="Output/Larvae.resp.plot.sqrt.pdf", RESP_FIG_DAYS8.24_FINAL, width = 12, height = 8, units = c("in"))

ALL_DATA_table <- ALL_DATA %>%
  group_by(Day,Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ng.mol),
            max_resp = max(Resp_rate_ng.mol),
            min_resp = min(Resp_rate_ng.mol),
            sd_resp = sd(Resp_rate_ng.mol),
            SEM = ((sd(Resp_rate_ng.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
ALL_DATA_table # view table - looks like the LARVAE resp was no different from the blanks


###################################################  TEST LARVAE FROM OTHER POOLS #################################
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
