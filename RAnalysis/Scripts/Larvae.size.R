#Title: Larvae.size 
#Author: Sam Gurr 
#Edited by: Sam Gurr
#Date Last Modified: 20190628
#See Readme file for details

rm(list=ls()) #clears workspace 

## install packages if you dont already have them in your library
if ("devtools" %in% rownames(installed.packages()) == 'FALSE') install.packages('devtools') 
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
# Load packages and pacage version/date/import/depends info
library(dplyr)          # Version 0.7.6, Packaged: 2018-06-27, Depends: R (>= 3.1.2)Imports: assertthat (>= 0.2.0), bindrcpp (>= 0.2.0.9000), glue (>=1.1.1), magrittr (>= 1.5), methods, pkgconfig (>= 2.0.1), R6(>= 2.2.2), Rcpp (>= 0.12.15), rlang (>= 0.2.0), tibble (>=1.3.1), tidyselect (>= 0.2.3), utils
library(ggplot2)        # Version 2.2.1, Packaged: 2016-12-30, Depends: R (>= 3.1)Imports: digest, grid, gtable (>= 0.1.1), MASS, plyr (>= 1.7.1),reshape2, scales (>= 0.4.1), stats, tibble, lazyeval
library(ggpubr)         # Version: 0.1.8 Date: 2018-08-30, Depends: R (>= 3.1.0), ggplot2, magrittrImports: ggrepel, grid, ggsci, stats, utils, tidyr, purrr, dplyr(>=0.7.1), cowplot, ggsignif, scales, gridExtra, glue, polynom
library(Rmisc)          # Version: 1.5 Packaged: 2013-10-21, Depends: lattice, plyr
library(plotrix)        # Version: 3.7-4, Date/Publication: 2018-10-03
library(lsmeans)        # Version: 2.27-62, Date/Publication: 2018-05-11, Depends: methods, R (>= 3.2)
library(gridExtra)      # Version: 2.3, Date/Publication: 2017-09-09, Imports: gtable, grid, grDevices, graphics, utils
library(reshape)        # Version: 0.8.7, Date/Publication: 2017-08-06, Depends: R (>= 2.6.1) Imports: plyr
library(multcompView)   # Version: 0.1-7, Date/Publication: 2015-07-31, Imports: grid
library(Rmisc)
library(lmtest)
library(car)

#Required Data files

# Set Working Directory:
# setwd("~/MyProjects/Geoduck_Conditioning/RAnalysis/") #set working
setwd("C:/Users/samjg/Documents/My_Projects/Geoduck_transgen_offspring_OA/RAnalysis/")
#Load Sample Info
Size.data <- read.csv(file="Data/Size_data/Size_larvae.csv", header=T) #read sample.info data

# Analysis
names(Size.data)
# filter data
Size.data_2 <- Size.data %>% 
  filter(!(Age == "28_days")) #%>% # filter out day 28 becasue no samples for T3 (variable pH)
  #filter(Tank_ID %in% c("T1", "T3")) # select only T1 and T3 (T0 and T2 failed midway through experiment)
# run the model
aov.mod_1 <- aov(length_um ~ Parental_treatment*Age, data = Size.data_2)
anova(aov.mod_1)
# diagnostic tests and plots of model residuals (not transformed)
# Shapiro test
shapiro.test(residuals(aov.mod_1)) # residuals are normal - 0.05555
# hist qq residual diagnostic
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(aov.mod_1)) #plot histogram of residuals - normal bell curve
boxplot(residuals(aov.mod_1)) #plot boxplot of residuals
plot(fitted(aov.mod_1),residuals(aov.mod_1)) 
qqnorm(residuals(aov.mod_1)) # qqplot - good qqnorm plot

# post-hoc analysis
TukeyHSD(aov.mod_1) # tukey of the model
ID.size.ANOVA <- lsmeans(aov.mod_1, pairwise ~ Parental_treatment*Age)# pariwise Tukey Post-hoc test between repeated treatments
ID.size.ANOVA # view post hoc summary
Treatment.pairs.05 <- cld(ID.size.ANOVA, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
Treatment.pairs.05 #view results

# plot data
Larvae.size.plot <- ggplot(Size.data_2, aes(x = factor(Age), y = length_um, fill = Parental_treatment)) +
  theme_classic() +
  scale_fill_manual(values=c("blue", "orange", "blue", "orange", "blue", "orange", "blue", "orange")) +
                    #labels=c("Ambient","Ambient � Ambient","Ambient � Elevated","Elevated","Elevated � Ambient","Elevated � Elevated")
  geom_boxplot(alpha = 0.5, # color hue
               width=0.6, # boxplot width
               outlier.size=0, # make outliers small
               position = position_dodge(preserve = "single")) + 
  geom_point(pch = 19, position = position_jitterdodge(.3), size=1) +
  stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
               width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
  theme(legend.position = c(0.55,0.96), legend.direction="horizontal", legend.title=element_blank()) +
  labs(y=expression("Size"~(�m)), x=expression("Age"))
Larvae.size.plot # view plot

Larvae.size.plot_FINAL <- 
  Larvae.size.plot + 
  annotate("text", x=0.7, y=140, label = "a", size = 5) + 
  annotate("text", x=1.3, y=145, label = "ab", size = 5) + 
  annotate("text", x=1.7, y=150, label = "b", size = 5) + 
  annotate("text", x=2.3, y=160, label = "c", size = 5) + 
  annotate("text", x=2.7, y=180, label = "cd", size = 5) + 
  annotate("text", x=3.3, y=175, label = "d", size = 5) + 
  annotate("text", x=3.7, y=200, label = "e", size = 5) + 
  annotate("text", x=4.3, y=210, label = "e", size = 5) + 
  annotate("text", x=4.7, y=260, label = "e", size = 5) + 
  annotate("text", x=5.3, y=220, label = "f", size = 5) +
  annotate("text", x=2, y=185, label = "*", size = 5) +
  annotate("text", x=5, y=270, label = "*", size = 5) +
  annotate("segment", x = 2.25, xend = 1.75, y = 180, yend = 180, colour = "black") +
  annotate("segment", x = 5.25, xend = 4.75, y = 265, yend = 265, colour = "black")
Larvae.size.plot_FINAL


#save file to output
ggsave(file="Output/Larvae.size.plot.pdf", Larvae.size.plot_FINAL, width = 12, height = 8, units = c("in"))
