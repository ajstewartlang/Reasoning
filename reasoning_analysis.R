#load the appropriate packages

library (lme4)
library (lmerTest)
library (lsmeans)
library (readr)
library (ggplot2)

#in total there are 8 conditions
#the second sentence is identical for the first 4 versions, and for the last 4 versions
#the third sentence is identical across all conditions
#for example:

#1MP
#If there is a black square then there is a yellow circle. 
#There is a black square.
#There is a yellow circle.

#1AC
#If there is a yellow circle then there is a black square. 
#There is a black square.
#There is a yellow circle.

#1CB
#There is a black square and there is a yellow circle. 
#There is a black square.
#There is a yellow circle.

#1DB
#There is a yellow circle or there is a black square.
#There is a black square.
#There is a yellow circle.

#1MPF
#If there is a black square then there is a yellow circle.
#There is not a black square.
#There is a yellow circle.

#1ACF
#If there is a yellow circle then there is a black square.
#There is not a black square.
#There is a yellow circle.

#1CF
#There is a yellow circle and there is a black square.
#There is not a black square.
#There is a yellow circle.

#1DF
#There is a black square or there is a yellow circle. 
#There is not a black square.
#There is a yellow circle.

#FP = First Pass - the sum of all fixations within a region before the eye exits to the left or to the right
#RP = Regressions Path - the sum of all fixations within a region (incl. re-reading of previous regions) before the eye exits to the right
#RO = Regressions out of a region as a binary variable (1=regression out, 0=no regression out)
#RI = Regressions in to a region as a binary variable (1=regression in, 0=no regression in)
#TT = Total Time - the sum of all fixations within a region

#Run the First Pass analysis
#read in data file
FP <- read_csv("~/Desktop/Air Work/R analyses/Reasoning/FP.csv")

#MP and AC analysis - all reading times (even when people get the conclusion wrong)
#select MP and AC conditions
index <- FP$CondType == "MP" | FP$CondType == "AC"
MPACdataFP <- as.data.frame (FP[index,])

#make CondType a factor
MPACdataFP$CondType <- as.factor (MPACdataFP$CondType)

#set up contrasts
contrasts (MPACdataFP$CondType) <- matrix (c(.5, -.5)) 

#Don't examine antecedent and consequent times as text differs - remember to look at % regression in later

#Examine FP on the minor premise
model.MPACFPpr <- lmer (Premise ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPACdataFP, REML=TRUE)
summary (model.MPACFPpr)
lsmeans (model.MPACFPpr, pairwise ~ CondType, adjust="none")

#Examine FP on conclusion
model.MPACFP <- lmer (Conclusion ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPACdataFP, REML=TRUE)
summary (model.MPACFP)

#Run the Regressions out analyses
RO <- read_csv("~/Desktop/Air Work/R analyses/Reasoning/RO.csv")

#select MP and MPF conditions
index <- RO$CondType == "MP" | RO$CondType == "AC"
MPACdataRO <- as.data.frame (RO[index,])

#make CondType a factor
MPACdataRO$CondType <- as.factor (MPACdataRO$CondType)

#set up contrasts
contrasts (MPACdataRO$CondType) <- matrix (c(.5, -.5)) 

#Examine RO on the conclusion
model.MPACROpr <- glmer (Conclusion ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPACdataRO, family=binomial)
summary (model.MPACROpr)
lsmeans (model.MPACROpr, pairwise ~ CondType, adjust="none", type="response")


#Run the Regression in analysis
RI <- read_csv("~/Desktop/Air Work/R analyses/Reasoning/RI.csv")

#select MP and AC conditions
index <- RI$CondType == "MP" | RI$CondType == "AC"
MPACdataRI <- as.data.frame (RI[index,])

#make CondType a factor
MPACdataRI$CondType <- as.factor (MPACdataRI$CondType)

#set up contrasts
contrasts (MPACdataRI$CondType) <- matrix (c(.5, -.5)) 

#Examine RI on the antecedent
model.MPACRIpr <- glmer (Antecedent ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPACdataRI, family=binomial)
summary (model.MPACRIpr)
lsmeans (model.MPACRIpr, pairwise ~ CondType, adjust="none", type="response")

#Examine RI on the minor premise
model.MPACRIpr <- glmer (Premise ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPACdataRI, family=binomial)
summary (model.MPACRIpr)
lsmeans (model.MPACRIpr, pairwise ~ CondType, adjust="none", type="response")

#Run the Regression Path analysis
#read in data file
RP <- read_csv("~/Desktop/Air Work/R analyses/Reasoning/RP.csv")

#select MP and AC conditions
index <- RP$CondType == "MP" | RP$CondType == "AC"
MPACdataRP <- as.data.frame (RP[index,])

#make CondType a factor
MPACdataRP$CondType <- as.factor (MPACdataRP$CondType)

#set up contrasts
contrasts (MPACdataRP$CondType) <- matrix (c(.5, -.5)) 

#Don't examine antecedent and consequent times as text differs - remember to look at % regressions in later 

#Examine RP on the minor premise
model.MPACRPpr <- lmer (Premise ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPACdataRP, REML=TRUE)
summary (model.MPACRPpr)
lsmeans (model.MPACRPpr, pairwise ~ CondType, adjust="none")

#Examine RP on conclusion
model.MPACRP <- lmer (Conclusion ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPACdataRP, REML=TRUE)
summary (model.MPACRP)
lsmeans (model.MPACRP, pairwise ~ CondType, adjust="none")


#Run the Total Time analysis
#read in the data file
TT <- read_csv("~/Desktop/Air Work/R analyses/Reasoning/TT.csv")

#select MP and AC conditions
index <- TT$CondType == "MP" | TT$CondType == "AC"
MPACdataTT <- as.data.frame (TT[index,])

#make CondType a factor
MPACdataTT$CondType <- as.factor (MPACdataTT$CondType)

#set up contrasts
contrasts (MPACdataTT$CondType) <- matrix (c(.5, -.5)) 

#Don't examine antecedent and consequent times as text differs - remember to look at % regressions later

#Examine TT on the minor premise
model.MPACTTpr <- lmer (Premise ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPACdataTT, REML=TRUE)
summary (model.MPACTTpr)
lsmeans (model.MPACTTpr, pairwise ~ CondType, adjust="none")

#Examine TT on conclusion
model.MPACTT <- lmer (Conclusion ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPACdataTT, REML=TRUE)
summary (model.MPACTT)
lsmeans (model.MPACTT, pairwise ~ CondType, adjust="none")



#This is now analysing a different two conditions
#MP and MPF analysis (all reading times even when people get the conclusion wrong)
#Run the First Pass analysis
#read in the data file
FP <- read_csv("~/Desktop/Air Work/R analyses/Reasoning/FP.csv")

#select MP and MPF conditions
index <- FP$CondType == "MP" | FP$CondType == "MPF"
MPMPFdataFP <- as.data.frame (FP[index,])

#make CondType a factor
MPMPFdataFP$CondType <- as.factor (MPMPFdataFP$CondType)

#set up contrasts
contrasts (MPMPFdataFP$CondType) <- matrix (c(.5, -.5)) 

#Examine FP on the conditional consequent
model.MPMPFFPpr <- lmer (Consequent ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPMPFdataFP, REML=TRUE)
summary (model.MPMPFFPpr)
lsmeans (model.MPMPFFPpr, pairwise ~ CondType, adjust="none")

#Don't examine minor premise times as text differs - remember to look at % regression in later

#Examine FP on conclusion
model.MPMPFFP <- lmer (Conclusion ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPMPFdataFP, REML=TRUE)
summary (model.MPMPFFP)

#Run the Regressions out analyses
RO <- read_csv("~/Desktop/Air Work/R analyses/Reasoning/RO.csv")

#select MP and MPF conditions
index <- RO$CondType == "MP" | RO$CondType == "MPF"
MPMPFdataRO <- as.data.frame (RO[index,])

#make CondType a factor
MPMPFdataRO$CondType <- as.factor (MPMPFdataRO$CondType)

#set up contrasts
contrasts (MPMPFdataRO$CondType) <- matrix (c(.5, -.5)) 

#Examine RO on the conclusion
model.MPMPFROpr <- glmer (Conclusion ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPMPFdataRO, family=binomial)
summary (model.MPMPFROpr)
lsmeans (model.MPMPFROpr, pairwise ~ CondType, adjust="none", type="response")

#Run the Regression in analysis
RI <- read_csv("~/Desktop/Air Work/R analyses/Reasoning/RI.csv")

#select MP and MPF conditions
index <- RI$CondType == "MP" | RI$CondType == "MPF"
MPMPFdataRI <- as.data.frame (RI[index,])

#make CondType a factor
MPMPFdataRI$CondType <- as.factor (MPMPFdataRI$CondType)

#set up contrasts
contrasts (MPMPFdataRI$CondType) <- matrix (c(.5, -.5)) 

#Examine RI on the antecedent
model.MPMPFRIpr <- glmer (Antecedent ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPMPFdataRI, family=binomial)
summary (model.MPMPFRIpr)
lsmeans (model.MPMPFRIpr, pairwise ~ CondType, adjust="none", type="response")

#Examine RI on the premise
model.MPMPFRIpr <- glmer (Premise ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPMPFdataRI, family=binomial)
summary (model.MPMPFRIpr)
lsmeans (model.MPMPFRIpr, pairwise ~ CondType, adjust="none", type="response")

#Run the Regression Path analysis
#read in the data file
RP <- read_csv("~/Desktop/Air Work/R analyses/Reasoning/RP.csv")

#select MP and MPF conditions
index <- RP$CondType == "MP" | RP$CondType == "MPF"
MPMPFdataRP <- as.data.frame (RP[index,])

#make CondType a factor
MPMPFdataRP$CondType <- as.factor (MPMPFdataRP$CondType)

#set up contrasts
contrasts (MPMPFdataRP$CondType) <- matrix (c(.5, -.5)) 

#Examine RP on the conditional consequent
model.MPMPFRPpr <- lmer (Consequent ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPMPFdataRP, REML=TRUE)
summary (model.MPMPFRPpr)
lsmeans (model.MPMPFRPpr, pairwise ~ CondType, adjust="none")

#Don't examine minor premise times as text differs - remember to look at % regression in later

#Examine RP on conclusion
model.MPMPFRP <- lmer (Conclusion ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPMPFdataRP, REML=TRUE)
summary (model.MPMPFRP)


#Run the Total Time analysis
#read in the data file
TT <- read_csv("~/Desktop/Air Work/R analyses/Reasoning/TT.csv")

#select MP and MPF conditions
index <- TT$CondType == "MP" | TT$CondType == "MPF"
MPMPFdataTT <- as.data.frame (TT[index,])

#make CondType a factor
MPMPFdataTT$CondType <- as.factor (MPMPFdataTT$CondType)

#set up contrasts
contrasts (MPMPFdataTT$CondType) <- matrix (c(.5, -.5)) 

#Examine TT on the conditional consequent
model.MPMPFTTpr <- lmer (Consequent ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPMPFdataTT, REML=TRUE)
summary (model.MPMPFTTpr)
lsmeans (model.MPMPFTTpr, pairwise ~ CondType, adjust="none")

#Don't examine minor premise times as text differs - remember to look at % regression in later

#Examine TT on conclusion
model.MPMPFTT <- lmer (Conclusion ~ CondType + (1+CondType|P.s) + (1+CondType|Item), data=MPMPFdataTT, REML=TRUE)
summary (model.MPMPFTT)

