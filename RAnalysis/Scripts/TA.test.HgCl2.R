#Title: TA HgCl2 test
#Author: Sam Gurr 
#Edited by: Sam Gurr
#Date Last Modified: 20190209
#See Readme file for details

rm(list=ls()) #clears workspace 

if ("dplyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('dplyr') 
library('dplyr')

setwd("C:/Users/samjg/Documents/My_Projects/Geoduck_transgen_offspring_OA/RAnalysis/")
#Load Carb chem test file
Carb.chem.file<- read.csv(file="Data/TA_HgCl2_test/20190209_Carb_chem_output.csv", header=T) #read sample.info data
Carb.chem.file

# Elevated treatment
elevated <- Carb.chem.file %>% 
  filter(Carb.chem.file$Treatment == "Elevated")
# Conicals
test_conicals_elevated <- elevated %>%
  filter((substr(elevated$Tank, 10,10)) == "T")
# test Conicals
t.test(TA~Notes, data=test_conicals_elevated)
# Broodstock tanks
test_broodstock_elevated <- elevated %>%
  filter((substr(elevated$Tank, 11,11)) == "B")
# test broodstock tanks
t.test(TA~Notes, data=test_broodstock_elevated)


# Ambient treatment
Ambient <- Carb.chem.file %>% 
  filter(Carb.chem.file$Treatment == "Ambient")
# Conicals
test_conicals_Ambient <- Ambient %>%
  filter((substr(Ambient$Tank, 10,10)) == "T")
# test Conicals
t.test(TA~Notes, data=test_conicals_Ambient)
# Broodstock tanks
test_broodstock_Ambient <- Ambient %>%
  filter((substr(Ambient$Tank, 11,11)) == "B")
# test broodstock tanks
t.test(TA~Notes, data=test_broodstock_Ambient)
