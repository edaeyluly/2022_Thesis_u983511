#Answering the research questions

#library(lme4)
# library(afex)
# library(tidyverse) #for all data wrangling
# library(cowplot) #for manuscript ready figures
# library(sjPlot) #for plotting lmer 
# library(sjmisc) 
# library(effects)
# library(sjstats)
#library(corrplot)
#library(ggplot2)
#library(lmerTest)
#library(MuMIn)

#-------------------------------------------------------------------------------------------


#RQ1 Q1) Is there a systematic difference in reaction times to words and non-words when participants are asked to simply react to stimuli? 

#Defining the model

rq1.test <- lmer(RT ~ (lexicality * subject_parity) + trial + (1+trial|subject_nr) + (1|spelling), data = all.no.pluses)


#R squares, AIC score and sumamry of the model

r.squaredGLMM(rq1.test)

AIC(rq1.test)

summary(rq1.test)  


#Creating correlation matrix for RQ1 model

rq1corr <- cov2cor(vcov(rq1.test))

#Converting from dpoMatrix to matrix for correlation plot

rq1mat <- as.matrix(rq1corr)

#Correlation Plot

corrplot(rq1mat,  method = 'color',  type = 'lower',addCoef.col ='black', tl.col= "#444444",  number.cex = 0.8, order = 'alphabet', diag=TRUE)

#Plotting the effects of RQ1 model

#Raw Plot of the RQ1 model

sjPlot::plot_model(rq1.test)

#Labeled Plot of the RQ1 model

sjPlot::plot_model(rq1.test, 
                   axis.labels=c("Lexicality [W] and Subejct Parity [odd] Interaction Effect", "Trial Effect", 
                                 "Subject Parity Effect [odd]","Lexicality Effect [W]"),
                   show.values=TRUE, show.p=FALSE,
                   title="Effect of lexicality on Reaction Times") 

#Table of the RQ1 model

sjPlot::tab_model(rq1.test, 
                  show.re.var= TRUE, show.p = FALSE,
                  pred.labels =c("Intercept", "Lexicality Effect [W]", "Subject Parity Effect [odd]", "Trial Effect", 
                                 "Lexicality [W] and Subejct Parity [odd] Interaction Effect"),
                  dv.labels= "Effect of lexicality on Reaction Times")

#----------------------------------------------------------------------------------
  
  
#Q2) Do the lexical features of words and non-words, such as length, OLD20 or others 
#affect the reaction times of participants in a simple reaction time experiment? 
  
#At this point, subject Parity was dropped from the models as it demonstrated more of a difference between individuals 
#rather than an even divide as the sample size was too small

#Defining the model

rq2.test <- lmer(RT ~  (allchars * lexicality) + (OLD20 * lexicality) + (OLD20 * allchars * lexicality) + trial + (1+trial|subject_nr) + (1|spelling), data = all.no.pluses)

#Creating correlation matrix for RQ2 model

rq2corr <- cov2cor(vcov(rq2.test))

rq2mat <- as.matrix(rq2corr)

corrplot(rq2mat,  method = 'color',  type = 'lower',addCoef.col ='black', tl.col= "#444444",  number.cex = 0.8, order = 'AOE', diag=TRUE)

#R squared, AIC score and summary of the RQ2 model

AIC(rq2.test)
r.squaredGLMM(rq1.test)
summary(rq2.test)

#Plotting the effects of RQ2 model

sjPlot::plot_model(rq2.test, show.values=TRUE, show.p = FALSE)

#Table of the RQ2 model

sjPlot::tab_model(rq2.test, 
                  show.re.var= TRUE, show.p = FALSE,
                  pred.labels =c("Intercept", "String Length", "Lexicality Effect [W]", "OLD20 Effect", "Trial Effect", "Length and Lexicality [W] Interaction Effect",
                                 "Lexicality [W] and OLD20 Interaction Effect", "Length and OLD20 Interaction Effect", "Combined Interaction Effect" ),
                  dv.labels= "Effects of Mutual Lexical Feautures on Reaction Times")



----------------------------------------------------------------------------------
  
#Q3) Do the lexical features of words such as frequency, length and contextual diversity affect the 
#reaction times of participants for words only, when participants are asked to react to a simple reaction time experiment?
  
#getting rid of non-finite values as the model requires it
  
all.words.only$celex.log10.freq[!is.finite(all.words.only$celex.log10.freq)] <- 0
all.words.only$celex.log10.cd[!is.finite(all.words.only$celex.log10.cd)] <- 0

#Rescaling bigram counts

#commented as they might be already rescaled in the work space, uncomment for initial execution, summed bigram should be in range (0-550)
#all.words.only$summed.bigram <- all.words.only$summed.bigram / 1000 


#Defining the model



rq3.test <- lmer(RT ~  (subtlex.log10.cd * subtlex.log10.frequency) + (celex.log10.cd * celex.log10.freq) +  
                   ( OLD20 * allchars ) + summed.bigram + trial + (1+trial|subject_nr) + (1|spelling), data = all.words.only)

#R squared, AIC score and summary of the RQ2 model

AIC(rq3.test2)
summary(rq3.test)
r.squaredGLMM(rq3.test)

#Plotting correlations

rq3corr <- cov2cor(vcov(rq3.test))

rq3mat <- as.matrix(rq3corr)

corrplot(rq3mat,  method = 'color',  type = 'lower',addCoef.col ='black', tl.col= "#444444",  number.cex = 0.8, order = 'alphabet', diag=TRUE)

#Plotting the effects of RQ2 model

sjPlot::tab_model(rq3.test)

sjPlot::plot_model(rq3.test, show.values=TRUE, show.p = TRUE)

#Table of the RQ2 model

sjPlot::tab_model(rq3.test, show.re.var= TRUE, show.p = FALSE,
                  pred.labels =c("Intercept", "Subtlex CD Effect", "Subtlex Frequency Effect", "CELEX CD Effect", "CELEX Frequency Effect",
                                "OLD20 Effect", "String Length Effect","Bigram Effect", "Trial Effect", "Subtlex CD and Frequency Interaction Effect", 
                                "CELEX CD and Frequency Interaction Effect", "OLD20 and Length Interaction Effect"), 
                  dv.labels= "Effects of Lexical features on Reaction Times of Words")



#End Of Research Question Models
