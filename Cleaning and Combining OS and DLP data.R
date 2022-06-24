#Setting directory so that the code is replicable

#Note: Reminder that the experiment was conducted overtime, as the responses were gathered 

setwd("C:/Users/eda/Desktop/Thesis_R_Sesame")

#Importing all subject responses gathered with OpenSesame 

subject0.OSdata <- read.csv(file = 'subject_0_OS.csv')
subject1.OSdata <- read.csv (file = 'subject_1_OS.csv')
subject2.OSdata <- read.csv (file = 'subject_2_OS.csv')
subject3.OSdata <- read.csv (file = 'subject_3_OS.csv')
subject4.OSdata <- read.csv (file = 'subject_4_OS.csv')
subject5.OSdata <- read.csv (file = 'subject_5_OS.csv')

#Defining the variables I want to keep from the large subject dataframe

myOSvars <- c( "spelling", "colorhex", "subject_nr", "condition", "response_time_stimulus_response", "lexicality", "OLD20", "allchars", "subject_parity")

#Cleaning subjects' large OS data

subject0.cleanedOSdata <- subject0.OSdata[myOSvars]
subject1.cleanedOSdata <- subject1.OSdata[myOSvars]
subject2.cleanedOSdata <- subject2.OSdata[myOSvars]
subject3.cleanedOSdata <- subject3.OSdata[myOSvars]
subject4.cleanedOSdata <- subject4.OSdata[myOSvars]
subject5.cleanedOSdata <- subject5.OSdata[myOSvars]

#Discarding the practice runs

OS.trial.only.0 <- subject0.cleanedOSdata[c(31:390),]
OS.trial.only.1 <- subject1.cleanedOSdata[c(31:390),]
OS.trial.only.2 <- subject2.cleanedOSdata[c(31:390),]
OS.trial.only.3 <- subject3.cleanedOSdata[c(31:390),]
OS.trial.only.4 <- subject4.cleanedOSdata[c(31:390),]
OS.trial.only.5 <- subject5.cleanedOSdata[c(31:390),]

#Seperating the control squares from the text stimuli

squares_OS_0 <- OS.trial.only.0[c(0:60, 301:360),]
squares_OS_1 <- OS.trial.only.1[c(0:60, 301:360),]
squares_OS_2 <- OS.trial.only.2[c(0:60, 301:360),]
squares_OS_3 <- OS.trial.only.3[c(0:60, 301:360),]
squares_OS_4 <- OS.trial.only.4[c(0:60, 301:360),]
squares_OS_5 <- OS.trial.only.5[c(0:60, 301:360),]


#Joining all the combined square datasets together for final square dataframe

all.squares_OS <- rbind(squares_OS_0,squares_OS_1,squares_OS_2,squares_OS_3,squares_OS_4,squares_OS_5)


#Now the strings

strings_OS_0 <- OS.trial.only.0[c(61:300),]
strings_OS_1 <- OS.trial.only.1[c(61:300),]
strings_OS_2 <- OS.trial.only.2[c(61:300),]
strings_OS_3 <- OS.trial.only.3[c(61:300),]
strings_OS_4 <- OS.trial.only.4[c(61:300),]
strings_OS_5 <- OS.trial.only.5[c(61:300),]

#manually adding trial counts

strings_OS_0$trial <- c(1:240)
strings_OS_1$trial <- c(1:240)
strings_OS_2$trial <- c(1:240)
strings_OS_3$trial <- c(1:240)
strings_OS_4$trial <- c(1:240)
strings_OS_5$trial <- c(1:240)

#dropping unnecessarily repeated colorhex from the string data frames

strings_OS_0$colorhex <- NULL 
strings_OS_1$colorhex <- NULL 
strings_OS_2$colorhex <- NULL 
strings_OS_3$colorhex <- NULL 
strings_OS_4$colorhex <- NULL 
strings_OS_5$colorhex <- NULL 

#Importing the previously crafted Joint DLP dataset

jointDLP.large <-  read.csv(file = 'jointdlp.csv')


#Defining variables I want from the DLP dataset

smallDLPvars <- c("spelling", "celex.frequency", "celex.cd", "summed.bigram", "subtlex.frequency", "subtlex.cd", "subtlex.log10.frequency", "subtlex.log10.frequency", "subtlex.log10.cd", "nsyl")

#extracting the defined variables

jointDLP.small <- jointDLP.large[smallDLPvars]

#As it is a large file, at this point you can remove the larger dlp dataset from the R environment to clear some workspace using "rm(jointDLP.large)"

#Merging the subjects' data with the variables I want from DLP

subject0.DLPOS <- unique(merge(strings_OS_0, jointDLP.small, by = 'spelling', all.x= TRUE))
subject1.DLPOS <- unique(merge(strings_OS_1, jointDLP.small, by = 'spelling', all.x= TRUE))
subject2.DLPOS <- unique(merge(strings_OS_2, jointDLP.small, by = 'spelling', all.x= TRUE))
subject3.DLPOS <- unique(merge(strings_OS_3, jointDLP.small, by = 'spelling', all.x= TRUE))
subject4.DLPOS <- unique(merge(strings_OS_4, jointDLP.small, by = 'spelling', all.x= TRUE))
subject5.DLPOS <- unique(merge(strings_OS_5, jointDLP.small, by = 'spelling', all.x= TRUE))

#Joining all the combined DLPOS datasets together for the final DLPOS dataframe

all.subjects.DLPOS <- rbind(subject0.DLPOS,subject1.DLPOS,subject2.DLPOS,subject3.DLPOS,subject4.DLPOS,subject5.DLPOS)

#Changing some variable names for easier modelling

colnames(all.subjects.DLPOS)[4] <- "RT"

#Subseting the data by condition for a dataframe of only words and pseudowords

all.no.pluses <- subset(all.subjects.DLPOS, all.subjects.DLPOS$condition == "A1" | all.subjects.DLPOS$condition == "A2" )

#Adding log10 values of CELEX values to avoid scale issues during modeling

all.no.pluses$celex.log10.freq <- log10(all.no.pluses$celex.frequency)
all.no.pluses$celex.log10.cd <- log10(all.no.pluses$celex.cd)

#Seperating into two dataframes to observe lexicality effects individually

all.words.only <- subset(all.no.pluses, all.no.pluses$condition == "A1")
all.nonwords.only <- subset(all.no.pluses, all.no.pluses$condition == "A2")

#End of Processing OS data
