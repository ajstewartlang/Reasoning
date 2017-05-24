#load the appropriate packages
library(afex)
lsm.options(lmer.df = "asymptotic")
library (readr)
library (ggplot2)
require(latticeExtra)
lattice.options(default.theme = standard.theme(color = FALSE))
lattice.options(default.args = list(as.table = TRUE))
require(dplyr)
require(tidyr)

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
                                      "c('MP', 'AC', 'CB', 'DF')='quasi_valid'; 
                                      c('MPF', 'ACF', 'CF', 'DB')='quasi_invalid'", 
                                      levels = c("quasi_valid", "quasi_invalid"))
  data$major_premise <- factor(ifelse(data$CondType %in% c("MP", "AC", "MPF", "ACF"), "conditional", "conjunction-disjunction"), levels = c( "conditional", "conjunction-disjunction"))
  data
}

#Run the First Pass analysis
#read in data file
d_fp <- read_csv("FP.csv")
d_fp <- add_conditions(d_fp)
attr(d_fp, "spec") <- NULL
str(d_fp)

d_fp_a <- d_fp %>% 
  gather("dv", "rt", Antecedent:Conclusion) %>% 
  group_by(CondType, dv) %>% 
  summarise(mean_fp = mean(rt, na.rm = TRUE))

d_fp_a2 <- d_fp %>% 
  gather("dv", "rt", Antecedent:Conclusion) %>% 
  group_by(inference, polarity_premise, dv) %>% 
  summarise(mean_fp = mean(rt, na.rm = TRUE))


xyplot(mean_fp ~ CondType|dv, d_fp_a, type = "o")

xyplot(mean_fp ~ inference|dv, d_fp_a2, group = polarity_premise, type = "o", auto.key = list(lines=TRUE))


mfp_conclusion_1 <- mixed(Conclusion ~ inference*polarity_premise + (inference*polarity_premise|P.s), data=d_fp, control = lmerControl(optCtrl = list(maxfun=1e6))) 
mfp_conclusion_1

mfp_conclusion_2 <- mixed(Conclusion ~ inference*quasi_validity + (inference*quasi_validity|P.s), data=d_fp, control = lmerControl(optCtrl = list(maxfun=1e6))) 
mfp_conclusion_2


lsmeans(mfp_conclusion_1, "polarity_premise")

mfp_conclusion_0 <- mixed(Conclusion ~ CondType + (1+CondType|P.s), data=d_fp, control = lmerControl(optCtrl = list(maxfun=1e6))) 
summary(mfp_conclusion_0)
mfp_conclusion_0

mfp_conclusion_2b <- mixed(Conclusion ~ CondType + (1+CondType||P.s), data=d_fp, expand_re = TRUE) 
summary(mfp_conclusion_2b)
mfp_conclusion_2b

lsmeans(mfp_conclusion_2, "CondType")
lsmeans(mfp_conclusion_2b, "CondType")


mfp_premise_1 <- mixed(Premise ~ inference*polarity_premise + (inference*polarity_premise|P.s), data=d_fp, control = lmerControl(optCtrl = list(maxfun=1e6))) 
mfp_premise_1
summary(mfp_premise_1)

mfp_premise_1b <- mixed(Premise ~ inference*polarity_premise + (inference*polarity_premise||P.s), data=d_fp, control = lmerControl(optCtrl = list(maxfun=1e6)), expand_re = TRUE) # + (validity*major_premise|Item)
mfp_premise_1b

lsmeans(mfp_premise_1, "polarity_premise")

mfp_premise_2 <- mixed(Premise ~ CondType + (1+CondType|P.s), data=d_fp, control = lmerControl(optCtrl = list(maxfun=1e6))) 
mfp_premise_2

lsmeans(mfp_premise_2, "CondType")


#MP and AC analysis - all reading times (even when people get the conclusion wrong)

#Run the Regressions out analyses
d_ro <- read_csv("RO.csv")
d_ro <- add_conditions(d_ro)
attr(d_ro, "spec") <- NULL

mro_conclusion_1 <- mixed(Conclusion ~ inference*polarity_premise + (inference*polarity_premise|P.s), data=d_ro, control = glmerControl(optCtrl = list(maxfun=1e6)), family = binomial, method = "LRT") 
mro_conclusion_1

lsmeans(mro_conclusion_1, c("inference", "polarity_premise"), type = "response")
