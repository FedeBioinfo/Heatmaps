## script for repeated measures correlation

## I set the language to ENG
Sys.setenv(LANG = "en")

# I install the package rmcorr - this only the 1st time
install.packages("rmcorr")

## setwd on institute's PC
setwd("C:/Users/COMPUTER/Nextcloud/Data/Flytrap Ines/Submission Plant J/Resubmission Plant J/Repeated measures corr Rwd")

## setwd on home's computer
setwd("C:/Users/Fede/Nextcloud/Data/Flytrap Ines/Submission Plant J/Resubmission Plant J/Repeated measures corr Rwd")

## Load the required libraries
library("rmcorr") 
library("ggplot2") 
library("cowplot") 
library("RColorBrewer")
library("dplyr")

# I first check one of the example datasets to see how it is formatted
?bland1995

#bland1995 {rmcorr}	R Documentation
#Repeated measurements of intramural pH and PaCO2
#Description
#A dataset containing the repeated measurements of intramural pH and PaCO2 for eight subjects, 
#from Bland & Altman (1995).

# Usage
# bland1995
# Format
# A data frame with 47 rows and 3 variables

#[,1]	Subject	Unique identifer
#[,2]	pH	Potential of hydrogen, acidity to base
#[,3]	PaCO2	Partial pressure of carbon dioxide

# to load the data, bland1995 is a dataset of the package rmcorr
bland1995<- bland1995

# to visualise the first rows
head(bland1995)

# now, I can format my data in the same way as bland1995

## Load the data
inputData <- read.delim2("dataset_rmcorr.txt",
                         header = TRUE,
                         sep = '\t',
                         quote = '',
                         check.names = TRUE,
                         dec = '.')

## this is to visualise up to 1000 columns when I select "view" on an object/dataframe 
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

## and now I see all columns of inputData
View(inputData)

## Get sample size
n <- length(unique(inputData$Condition))

?rmcorr

as.matrix(inputData)

############Calculate rmcorr using selected columns##############

############Asparagine##################
my.rmc.Asparagine <- rmcorr(participant = Condition,
                 measure1 = Trap_Asparagine,
                 measure2 = Petiole_Asparagine,
                 dataset = inputData,
                 CI.level = 0.95,
                 CIs = "analytic",
                 nreps = 100,
                 bstrap.out = FALSE)

## If I select boostrap, only CI interval changes, but not the p-value or r
my.rmc.Asparagine.bootstrap <- rmcorr(participant = Condition,
                            measure1 = Trap_Asparagine,
                            measure2 = Petiole_Asparagine,
                            dataset = inputData,
                            CI.level = 0.95,
                            CIs = "bootstrap",
                            nreps = 100,
                            bstrap.out = FALSE)

## And plot the data
plotData <- inputData
plotData <- na.omit(select(plotData,all_of(c("Condition", "Trap_Asparagine", "Petiole_Asparagine")))) 
ggplot(plotData, aes(x = Trap_Asparagine, y = Petiole_Asparagine,   
                     group = factor(Condition), color = factor(Condition))) +
  geom_point(aes(colour = factor(Condition))) +
  geom_line(aes(y = my.rmc$model$fitted.values), linetype = 1) +
  ggtitle("Asparagine") +
  ylab("Petiole_Asparagine") +
  xlab("Trap_Asparagine") +
  theme_cowplot() +
  scale_shape_identity() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0, hjust = 0, vjust = 0)) +
  scale_colour_manual(values = colorRampPalette(brewer.pal(11, "BrBG"))(n))

############Aspartate##################
my.rmc.Aspartate <- rmcorr(participant = Condition,
                 measure1 = Trap_Aspartate,
                 measure2 = Petiole_Aspartate,
                 dataset = inputData,
                 CI.level = 0.95,
                 CIs = "analytic",
                 nreps = 100,
                 bstrap.out = FALSE)

## And plot the data
plotData <- inputData
plotData <- na.omit(select(plotData,all_of(c("Condition", "Trap_Aspartate", "Petiole_Aspartate")))) 
ggplot(plotData, aes(x = Trap_Aspartate, y = Petiole_Aspartate,   
                     group = factor(Condition), color = factor(Condition))) +
  geom_point(aes(colour = factor(Condition))) +
  geom_line(aes(y = my.rmc$model$fitted.values), linetype = 1) +
  ggtitle("Aspartate") +
  ylab("Petiole_Aspartate") +
  xlab("Trap_Aspartate") +
  theme_cowplot() +
  scale_shape_identity() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0, hjust = 0, vjust = 0)) +
  scale_colour_manual(values = colorRampPalette(brewer.pal(11, "BrBG"))(n))

############Alanine##################
my.rmc.Alanine <- rmcorr(participant = Condition,
                           measure1 = Trap_Alanine,
                           measure2 = Petiole_Alanine,
                           dataset = inputData,
                           CI.level = 0.95,
                           CIs = "analytic",
                           nreps = 100,
                           bstrap.out = FALSE)

my.rmc.Alanine <- rmcorr(Condition, Trap_Alanine, Petiole_Alanine, inputData,
                         CI.level = 0.95,
                         CIs = "analytic",
                         nreps = 100,
                         bstrap.out = FALSE)

###################GABA###############
my.rmc.GABA <- rmcorr(Condition, Trap_GABA, Petiole_GABA, inputData,
                           CI.level = 0.95,
                           CIs = "analytic",
                           nreps = 100,
                           bstrap.out = FALSE)

###################Glutamate###############
my.rmc.Glutamate <- rmcorr(Condition, Trap_Glutamate, Petiole_Glutamate, inputData,
                           CI.level = 0.95,
                           CIs = "analytic",
                           nreps = 100,
                           bstrap.out = FALSE)

###################Glutamine###############
my.rmc.Glutamine <- rmcorr(Condition, Trap_Glutamine, Petiole_Glutamine, inputData,
                         CI.level = 0.95,
                         CIs = "analytic",
                         nreps = 100,
                         bstrap.out = FALSE)

###################Isoleucine###############
my.rmc.Isoleucine <- rmcorr(Condition, Trap_Isoleucine, Petiole_Isoleucine, inputData,
                           CI.level = 0.95,
                           CIs = "analytic",
                           nreps = 100,
                           bstrap.out = FALSE)

###################Leucine###############
my.rmc.Leucine <- rmcorr(Condition, Trap_Leucine, Petiole_Leucine, inputData,
                            CI.level = 0.95,
                            CIs = "analytic",
                            nreps = 100,
                            bstrap.out = FALSE)

###################Proline###############
my.rmc.Proline <- rmcorr(Condition, Trap_Proline, Petiole_Proline, inputData,
                         CI.level = 0.95,
                         CIs = "analytic",
                         nreps = 100,
                         bstrap.out = FALSE)

###################Threonine###############
my.rmc.Threonine <- rmcorr(Condition, Trap_Threonine, Petiole_Threonine, inputData,
                         CI.level = 0.95,
                         CIs = "analytic",
                         nreps = 100,
                         bstrap.out = FALSE)