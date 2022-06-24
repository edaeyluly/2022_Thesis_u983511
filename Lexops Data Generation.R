
#install.packages("LexOPS")
#library(LexOPS)

#setting directory
setwd("C:/Users/Desktop/")

#Importing the preproccesed dlp data generated earlier, found in files

fastdlpnew <- read.csv(file = 'fastdlpnew.csv')

#-------------------------------------------------------------------------------

#General formula of creating the dataframes

#It should be noted that some variables,
#like plus signs of matching lenghts, were added outside of R, the output data was then
#called back in and manipulated as necessary, all the files used are accesible in the zip folder.

type5each <- fastdlpnew %>%
  set_options(id_col = "spelling") %>%
  split_by(lexicality, "W"~"N") %>%                       #setting lexops parameters
  control_for(allchars, -0:0) %>%
  control_for(OLD20, -0.1:0.1) %>%
  generate(n=5, seed = 13, "inclusive")


long5each <- long_format(type5each)

write.csv(long5each,"C:\\Users\\eda\\type5each.csv", row.names = FALSE)

string15exp <- (rexp(15, 1/700) + 700)                      #exp distributed stimuli duration

OS15Strings_1$new_duration <- string15exp

OS15Strings_1 <- OS15Strings_1[sample(nrow(OS15Strings_1)), ]  #shuffling the data

OS15Strings_2 <- OS15Strings_1[sample(nrow(OS15Strings_1)), ]  #shuffling the data

write.csv(OS15Strings_1,"C:\\Users\\eda\\OS15_Strings_1.csv", row.names = FALSE)

#---------------------------------------------------------------------------
#Using Lexops to generate 5 words and 5 non words for the practice blocks

seededshort15dlplex <- fastdlpnew %>%
  set_options(id_col = "spelling") %>%
  split_by(lexicality, "W"~"N") %>%
  control_for(allchars, -0:0) %>%
  control_for(OLD20, -0.1:0.1) %>%
  generate(n=5, seed = 13, "inclusive")

seededlong15dlplex <- long_format(seededshort15dlplex)

#Pluses of machingmlenghts were added in excel 

#Creating durations for 15 strings

string15exp <- (rexp(15, 1/700) + 700)

seededlong15dlplex$newduration <- string15exp

#Creating 2 versions of the lists

set.seed(13)
OS15list1 <- seededlong15dlplex[sample(nrow(seededlong15dlplex)), ]
set.seed(14)
OS15list2 <- seededlong15dlplex[sample(nrow(seededlong15dlplex)), ]

write.csv(OS15list1 ,"C:\\Users\\eda\\OS15list1.csv", row.names = FALSE) 
write.csv(OS15list2 ,"C:\\Users\\eda\\OS15list2.csv", row.names = FALSE)


#importing square dataframe

#rexp for 15 squares durations

square15exp <- (rexp(15, 1/700) + 700)

squares15csv$square_duration <- square15exp

set.seed(13)
random15list1 <- OS15longlistplus[sample(nrow(OS15longlistplus)), ]

set.seed(14)
random15list2 <- OS15longlistplus[sample(nrow(OS15longlistplus)), ]

#---------------------------------------------------------------------------


#Using Lexops to generate 80 words and 80 non words with the specified values for the trial blocks

seededshortdlplex <- fastdlpnew %>%
  set_options(id_col = "spelling") %>%
  split_by(lexicality, "W"~"N") %>%
  control_for(allchars, -0:0) %>%
  control_for(OLD20, -0.1:0.1) %>%
  generate(n=80, seed = 13, "inclusive")

#Larger table of the output

seededlongdlplex <- long_format(seededshortdlplex)

#importing the datasets with pluses added

#setting rexp durations for 240 strings

string240exp <- (rexp(240, 1/700) + 700)

X240stringsfinal$string_duration <- string240exp

#subsetting the data again

set.seed(20)
string240random1 <- X240stringsfinal[sample(nrow(X240stringsfinal)), ]

set.seed(21)
string240random2 <- X240stringsfinal[sample(nrow(X240stringsfinal)), ]

string240_block2_1 <- string240random1[0:60,]
string240_block3_1 <- string240random1[61:120,]
string240_block4_1 <- string240random1[121:180,]
string240_block5_1 <- string240random1[181:240,]

write.csv(string240_block2_1,"C:\\Users\\eda\\OSblock2_1.csv", row.names = FALSE)
write.csv(string240_block3_1,"C:\\Users\\eda\\OSblock3_1.csv", row.names = FALSE)
write.csv(string240_block4_1,"C:\\Users\\eda\\OSblock4_1.csv", row.names = FALSE)
write.csv(string240_block5_1,"C:\\Users\\eda\\OSblock5_1.csv", row.names = FALSE)

string240_block2_2 <- string240random2[0:60,]
string240_block3_2 <- string240random2[61:120,]
string240_block4_2 <- string240random2[121:180,]
string240_block5_2 <- string240random2[181:240,]

write.csv(string240_block2_2,"C:\\Users\\eda\\OSblock2_2.csv", row.names = FALSE)
write.csv(string240_block3_2,"C:\\Users\\eda\\OSblock3_2.csv", row.names = FALSE)
write.csv(string240_block4_2,"C:\\Users\\eda\\OSblock4_2.csv", row.names = FALSE)
write.csv(string240_block5_2,"C:\\Users\\eda\\OSblock5_2.csv", row.names = FALSE)



#Extracting the data

#write.csv(seededlongdlplex,"C:\\Users\\eda\\Desktop\\OSlonglist.csv", row.names = FALSE)
#write.csv(randomlist1,"C:\\Users\\eda\\Desktop\\OSlist1.csv", row.names = FALSE)
#write.csv(randomlist2,"C:\\Users\\eda\\Desktop\\OSlist2.csv", row.names = FALSE)

write.csv(random15list1,"C:\\Users\\eda\\Desktop\\OS15list1.csv", row.names = FALSE)
write.csv(random15list2,"C:\\Users\\eda\\Desktop\\OS15list2.csv", row.names = FALSE)

#---------------------------------------------------------------------------


#back to squares

set.seed(17)
squares15csv <- squares15csv[sample(nrow(squares15csv)), ]

write.csv(squares15csv,"C:\\Users\\eda\\OS15squares.csv", row.names = FALSE)

#rexp for 60 squares, trial shapes

square60exp <- (rexp(60, 1/700) + 700)

X60squarescsv$square_duration <- square60exp

set.seed(15)
X60squarescsv1 <- X60squarescsv[sample(nrow(X60squarescsv)), ]
X60squarescsv2 <- X60squarescsv[sample(nrow(X60squarescsv)), ]

write.csv(X60squarescsv1,"C:\\Users\\eda\\OS60squares1_0.csv", row.names = FALSE)
write.csv(X60squarescsv1,"C:\\Users\\eda\\OS60squares1_1.csv", row.names = FALSE)

#rexp for 60 squares, trial 2 shapes

square60exp2 <- (rexp(60, 1/700) + 700)

X60squarescsv2$square_duration <- square60exp2

set.seed(16)
X60squarescsv3 <- X60squarescsv[sample(nrow(X60squarescsv)), ]
X60squarescsv4 <- X60squarescsv[sample(nrow(X60squarescsv)), ]

write.csv(X60squarescsv3,"C:\\Users\\eda\\OS60squares2_0.csv", row.names = FALSE)
write.csv(X60squarescsv4,"C:\\Users\\eda\\OS60squares2_1.csv", row.names = FALSE)

#End of Lexops data generation