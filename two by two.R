#load the appropriate packages
library (readr)
library (ggplot2)
library (lme4)
library (lmerTest)
library (lsmeans) 
library (pbkrtest)
library(dplyr)
library(tidyr)

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

#This is a 2 x 2 LMM with factors Validity (Valid vs Invalid) and 
#Major Premise (Conditional vs Non-conditional).

add_conditions <- function(data) {
  data$CondType <- factor(data$CondType, levels = c("MP", "AC", "MPF", "ACF", "CB", "DB", "CF", "DF"))
  data$inference <- car::recode(data$CondType, 
                                "c('MP', 'MPF')='MP';
                                c('AC', 'ACF')='AC';
                                c('CB', 'CF')='conj';
                                c('DB', 'DF')='disj'", 
                                levels = c("MP", "AC", "conj", "disj"))
  data$polarity_premise <- car::recode(data$CondType, 
                                       "c('MP', 'AC', 'CB', 'DB')='positive'; 
                                       c('MPF', 'ACF', 'CF', 'DF')='negative'", 
                                       levels = c("positive", "negative"))
  data$validity <- factor(ifelse(data$CondType %in% c("MP", "CB", "DF"), "valid", 
                                 ifelse(data$CondType %in% c("AC", "ACF", "DB", "CF"), "possible", 
                                        "impossible")), 
                          levels = c("valid", "possible", "impossible"))
  data$quasi_validity <- car::recode(data$CondType, 
                                     "c('MP',  'CB', 'DF')='quasi_valid'; 
                                     c('MPF', 'AC', 'CF', 'DB')='quasi_invalid'", 
                                     levels = c("quasi_valid", "quasi_invalid"))
  data$major_premise <- factor(ifelse(data$CondType %in% c("MP", "AC", "MPF", "ACF"), "conditional", "conjunction-disjunction"), levels = c( "conditional", "conjunction-disjunction"))
  data
}


#Run the First Pass analysis
#read in data file
d_fp <- read_csv("~/Desktop/Air Work/R analyses/Reasoning/FP.csv")
d_fp <- add_conditions(d_fp)
attr(d_fp, "spec") <- NULL
str(d_fp)

index <- d_fp$CondType == "MP" | d_fp$CondType == "AC" | d_fp$CondType == "CB"| d_fp$CondType == "DB"
d_fp <- d_fp[index,]

d_fp$P.s <- as.factor (d_fp$P.s)
d_fp$Item <- as.factor (d_fp$Item)

contrasts (d_fp$quasi_validity) <- matrix (c(.5, -.5)) 
contrasts (d_fp$major_premise) <- matrix (c(.5, -.5)) 

#build the LMM for first pass times on the minor premise
modelfp_premise <- lmer (Premise ~ quasi_validity*major_premise + (1 + quasi_validity*major_premise| P.s)  + (1 + quasi_validity*major_premise| Item), data = d_fp, REML=TRUE)
summary (modelfp_premise)
lsmeans (modelrp_premise, pairwise ~ quasi_validity*major_premise, adjust="none")

#build the LMM for first pass times on the conclusion
modelfp_concl <- lmer (Conclusion ~ quasi_validity*major_premise + (1 + quasi_validity*major_premise| P.s)  + (1 + quasi_validity*major_premise| Item), data = d_fp, REML=TRUE)
summary (modelfp_concl)

#Plots
d_fp_a <- d_fp %>% 
  gather("dv", "rt", Premise:Conclusion) %>% 
  group_by(CondType, dv) %>% 
  summarise(mean_fp = mean(rt, na.rm = TRUE))

xyplot(mean_fp ~ CondType|dv, d_fp_a, type = "o")

#Run the Regression Path analysis
#read in data file
d_rp <- read_csv("~/Desktop/Air Work/R analyses/Reasoning/RP.csv")
d_rp <- add_conditions(d_rp)
attr(d_rp, "spec") <- NULL
str(d_rp)

index <- d_rp$CondType == "MP" | d_rp$CondType == "AC" | d_rp$CondType == "CB"| d_rp$CondType == "DB"
d_rp <- d_rp[index,]

d_rp$P.s <- as.factor (d_rp$P.s)
d_rp$Item <- as.factor (d_rp$Item)

contrasts (d_rp$quasi_validity) <- matrix (c(.5, -.5)) 
contrasts (d_rp$major_premise) <- matrix (c(.5, -.5)) 

#build the LMM for regression path times on the minor premise
modelrp_premise <- lmer (Premise ~ quasi_validity*major_premise + (1 + quasi_validity*major_premise| P.s)  + (1 + quasi_validity*major_premise| Item), data = d_rp, REML=TRUE)
summary (modelrp_premise)

#build the LMM for regression path times on the conclusion
modelrp_concl <- lmer (Conclusion ~ quasi_validity*major_premise + (1 + quasi_validity*major_premise| P.s)  + (1 + quasi_validity*major_premise| Item), data = d_rp, REML=TRUE)
summary (modelrp_concl)

#Plots
d_rp_a <- d_rp %>% 
  gather("dv", "rt", Premise:Conclusion) %>% 
  group_by(CondType, dv) %>% 
  summarise(mean_rp = mean(rt, na.rm = TRUE))

xyplot(mean_rp ~ CondType|dv, d_rp_a, type = "o")

#Run the Total Time analaysis
#read in data file
d_tt <- read_csv("~/Desktop/Air Work/R analyses/Reasoning/TT.csv")
d_tt <- add_conditions(d_tt)
attr(d_tt, "spec") <- NULL
str(d_tt)

index <- d_tt$CondType == "MP" | d_tt$CondType == "AC" | d_tt$CondType == "CB"| d_tt$CondType == "DB"
d_tt <- d_tt[index,]

d_tt$P.s <- as.factor (d_tt$P.s)
d_tt$Item <- as.factor (d_tt$Item)

contrasts (d_tt$quasi_validity) <- matrix (c(.5, -.5)) 
contrasts (d_tt$major_premise) <- matrix (c(.5, -.5)) 

#build the LMM for total  times on the minor premise - note, item random effects need to be simplified
modeltt_premise <- lmer (Premise ~ quasi_validity*major_premise + (1 + quasi_validity*major_premise| P.s)  + (1 + quasi_validity+major_premise| Item), data = d_tt, REML=TRUE)
summary (modeltt_premise)
lsmeans (modeltt_premise, pairwise ~ quasi_validity*major_premise, adjust="none")

#build the LMM for total times on the conclusion
modeltt_concl <- lmer (Conclusion ~ quasi_validity*major_premise + (1 + quasi_validity*major_premise| P.s)  + (1 + quasi_validity*major_premise| Item), data = d_tt, REML=TRUE)
summary (modeltt_concl)
lsmeans (modeltt_concl, pairwise~quasi_validity*major_premise, adjust = "none")

#Plots
d_tt_a <- d_tt %>% 
  gather("dv", "rt", Premise:Conclusion) %>% 
  group_by(CondType, dv) %>% 
  summarise(mean_tt = mean(rt, na.rm = TRUE))

xyplot(mean_tt ~ CondType|dv, d_tt_a, type = "o")

#Run the Regressions Out analaysis
#read in data file
d_ro <- read_csv("~/Desktop/Air Work/R analyses/Reasoning/RO.csv")
d_ro <- add_conditions(d_ro)
aror(d_ro, "spec") <- NULL
str(d_ro)

index <- d_ro$CondType == "MP" | d_ro$CondType == "AC" | d_ro$CondType == "CB"| d_ro$CondType == "DB"
d_ro <- d_ro[index,]

d_ro$P.s <- as.factor (d_ro$P.s)
d_ro$Item <- as.factor (d_ro$Item)

contrasts (d_ro$quasi_validity) <- matrix (c(.5, -.5)) 
contrasts (d_ro$major_premise) <- matrix (c(.5, -.5)) 

#build the LMM for total  times on the minor premise - note, item random effects need to be simplified
modelro_premise <- glmer (Premise ~ quasi_validity*major_premise + (1 + quasi_validity*major_premise| P.s)  + (1 + quasi_validity*major_premise| Item), family=binomial, data = d_ro, REML=TRUE)
summary (modelro_premise)

#build the LMM for total  times on the conclusion
modelro_concl <- glmer (Conclusion ~ quasi_validity*major_premise + (1 + quasi_validity*major_premise| P.s)  + (1 + quasi_validity*major_premise| Item), family=binomial, data = d_ro, REML=TRUE)
summary (modelro_concl)
lsmeans (modelro_concl, pairwise~quasi_validity*major_premise, adjust = "none")

#build separate by-subject and by-item analyses
modelrosubj_concl <- glmer (Conclusion ~ quasi_validity*major_premise + (1 + quasi_validity*major_premise| P.s), family=binomial, data = d_ro, REML=TRUE)
summary (modelrosubj_concl)

modelroitem_concl <- glmer (Conclusion ~ quasi_validity*major_premise + (1 + quasi_validity*major_premise| Item), family=binomial, data = d_ro, REML=TRUE)
summary (modelroitem_concl)

