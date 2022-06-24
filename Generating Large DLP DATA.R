
#Creating the joint dataset for DLP from the DLP index data, freely avaliable online

#setting directory to import necessary data

setwd("C:/Users/eda/Desktop/Thesis_R_Sesame")

df.all <- read.csv(“data/jointdlp.csv.r”) #all dlp data

dlp.items <- read.table(file = "dlp-items.txt",sep="\t",header=T)

dlp.stimuli <- read.table(file = "dlp-stimuli.txt",sep="\t",header=T)

dlp.trials <- read.table(file = "dlp-trials.txt",sep="\t",header=T)



jointdlp <- merge(dlp.trials, dlp.stimuli, by = 'spelling')

jointdlp <- merge(jointdlp, dlp.items, by = 'spelling')

#Resulting dataset can be subsetted as wished

#End of Generating Large DLP DATA