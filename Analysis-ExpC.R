################ README #############################
#
# This script contains code used to analyze Experiments C1 an C2 in Moeng (2018)'s dissertation (Chapter 5)
#
# There were 3 names used for each experiment: the original name when I was running the experiments, the analysisName (used in analysis scripts and some early dissertation drafts), and the final name in the dissertation. 
# 
# original | analysisName | Dissertation name
#------------------------------------------------------
# B1       | Exp C1       | Exp C1
# B2       | Exp C2       | Exp C2
#
#####################################################

library(readxl)
library(lme4)
library(multcomp)
library(ggplot2)

################ READ IN DATA #######################
## Open file: CHOOSE ONE OF THE FOLLOWING....

# data_expB <- read_excel("C:/Users/Emily/Google Drive/Research/Dissertation/Stored on GitHub/data-C1 (origB1).xlsx","day1")
# data_expB <- read_excel("C:/Users/Emily/Google Drive/Research/Dissertation/Stored on GitHub/data-C1 (origB1).xlsx","allDays")
# data_expB <- read_excel("C:/Users/Emily/Google Drive/Research/Dissertation/Stored on GitHub/data-C2 (origB2).xlsx","allDays")


#####################################################

data_expB <- data_expB[c("Subject","Day","SleepQuality","SleepAmount","Condition","Subcondition","TrialType","VowelType","Item","Referent","CorrectAnswerBySubcondition","PairType","Response","GotCorrect")]


data_expB$ContPt <- data_expB$Item
data_expB$ContPt[grepl("G1",data_expB$Item)] <- "Pt1"
data_expB$ContPt[grepl("G8",data_expB$Item)] <- "Pt8"
data_expB$ContPt[grepl("C1",data_expB$Item)] <- "Pt1"
data_expB$ContPt[grepl("C8",data_expB$Item)] <- "Pt8"

data_expB$Response = as.factor(data_expB$Response)
data_expB$Subject = as.factor(data_expB$Subject)
data_expB$Day = as.factor(data_expB$Day)
data_expB$Condition = as.factor(data_expB$Condition)
data_expB$Subcondition = as.factor(data_expB$Subcondition)
data_expB$TrialType = as.factor(data_expB$TrialType)
data_expB$PairType = as.factor(data_expB$PairType)
data_expB$Item = as.factor(data_expB$Item)
data_expB$ContPt = as.factor(data_expB$ContPt)


table(data_expB$Response)
factor(data_expB$Response)

# table(data_expB$SleepAmount,data_expB$Condition)/800 
# 		Divide by 720 for C1 bimodal percentages, 760 for C1 mono, 1080 for c2 bi, 800 for c2 mono

data_expB$SleepAmount <- factor(data_expB$SleepAmount)
levels(data_expB$SleepAmount) <- list(More=c("10+ hours", "8-9 hours"), Less=c("6-7 hours", "4-5 hours", "Less than 4 hours"))


# Subset overall dataset into critical and control trials
data_expB_crit <- data_expB[which(data_expB$TrialType=="crit"),]
data_expB_control <- data_expB[which(data_expB$TrialType=="control"),]


###################################################
#                                                 #
#                  ALL DAYS ANALYSIS              #
#                                                 #
###################################################


################ TREATMENT/DUMMY CODING ##############
# CHOOSE ONE:
# 1. ADAPTIVE GAUSS-HERMITE QUADRATURE ALGORITHM
#glmer_expB_crit_treatment <- glmer(Response ~ Condition*PairType*Day + (1+Day*PairType|Subject) + (1+Day*Condition|Item), family="binomial", data=data_expB_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),nAGQ=0)
# 2. (DEFAULT) LAPLACE APPROXIMATION ALGORITHM
#glmer_expB_crit_treatment <- glmer(Response ~ Condition*PairType*Day + (1+Day*PairType|Subject) + (1+Day*Condition|Item), family="binomial", data=data_expB_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
# 3. (DEFAULT) LAPLACE APPROXIMATION ALGORITHM SIMPLIFIED RANDOM EFFECTS STRUCTURE
#glmer_expB_crit_treatment <- glmer(Response ~ Condition*PairType*Day + (1+Day+PairType|Subject) + (1+Day+Condition|Item), family="binomial", data=data_expB_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
# 4. ADAPTIVE GAUSS-HERMITE QUADRATURE ALGORITHM SIMPLIFIED RANDOM EFFECTS STRUCTURE
glmer_expB_crit_treatment <- glmer(Response ~ Condition*PairType*Day + (1+PairType+Day|Subject) + (1+Condition+Day|Item), family="binomial", data=data_expB_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),nAGQ=0)


#glmer_expB_crit_treatment <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expB_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),nAGQ=0)


# CHOOSE ONE:
# 1. ADAPTIVE GAUSS-HERMITE QUADRATURE ALGORITHM
#glmer_expB_control_treatment <- glmer(Response ~ Condition*PairType*Day + (1+Day*PairType|Subject) + (1+Day*Condition|Item), family="binomial", data=data_expB_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),nAGQ=0)
# 2. (DEFAULT) LAPLACE APPROXIMATION ALGORITHM
#glmer_expB_control_treatment <- glmer(Response ~ Condition*PairType*Day + (1+Day*PairType|Subject) + (1+Day*Condition|Item), family="binomial", data=data_expB_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
# 3. (DEFAULT) LAPLACE APPROXIMATION ALGORITHM SIMPLIFIED RANDOM EFFECTS STRUCTURE
#glmer_expB_control_treatment <- glmer(Response ~ Condition*PairType*Day + (1+Day+PairType|Subject) + (1+Day+Condition|Item), family="binomial", data=data_expB_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
# 4. ADAPTIVE GAUSS-HERMITE QUADRATURE ALGORITHM SIMPLIFIED RANDOM EFFECTS STRUCTURE
#glmer_expB_control_treatment <- glmer(Response ~ Condition*PairType*Day + (1+PairType+Day|Subject) + (1+Condition+Day|Item), family="binomial", data=data_expB_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),nAGQ=0)


glmer_expB_control_treatment <- glmer(Response ~ Condition*PairType*Day + (1+PairType+Day|Subject) + (1+Condition+Day|Item), family="binomial", data=data_expB_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),nAGQ=0)

summary(glmer_expB_crit_treatment)
summary(glmer_expB_control_treatment)



# Order of fixedef is...
# 1.Intercept, 2.Mono, 3.NotMatchPair, 4.Two, 5.Three, 6.Mono:NotMatchPair, 7.Mono:Two, 8.Mono:Three, 9.NotMatchPair:Two, 10.NotMatchPair:Three, 11.Mono:NotMatchPair:Two, 12.Mono:NotMatchPair:Three


bi.match.1      <- c(1,0,0,0,0,0,0,0,0,0,0,0)
mono.match.1    <- c(1,1,0,0,0,0,0,0,0,0,0,0)
bi.notMatch.1   <- c(1,0,1,0,0,0,0,0,0,0,0,0)
mono.notMatch.1 <- c(1,1,1,0,0,1,0,0,0,0,0,0)

bi.match.2      <- c(1,0,0,1,0,0,0,0,0,0,0,0)
mono.match.2    <- c(1,1,0,1,0,0,1,0,0,0,0,0)
bi.notMatch.2   <- c(1,0,1,1,0,0,0,0,1,0,0,0)
mono.notMatch.2 <- c(1,1,1,1,0,1,1,0,1,0,1,0)

bi.match.3      <- c(1,0,0,0,1,0,0,0,0,0,0,0)
mono.match.3    <- c(1,1,0,0,1,0,0,1,0,0,0,0)
bi.notMatch.3   <- c(1,0,1,0,1,0,0,0,0,1,0,0)
mono.notMatch.3 <- c(1,1,1,0,1,1,0,1,0,1,0,1)


############ main effects ###############

# Define the specific contrasts
mainEffects1_matrix <- matrix((bi.match.1+bi.notMatch.1-mono.match.1-mono.notMatch.1)/2,1)
mainEffects2_matrix <- matrix((bi.match.2+bi.notMatch.2-mono.match.2-mono.notMatch.2)/2,1)
mainEffects3_matrix <- matrix((bi.match.3+bi.notMatch.3-mono.match.3-mono.notMatch.3)/2,1)

# main effects critical
crit1_hyp <- glht(glmer_expB_crit_treatment,linfct=mainEffects1_matrix)
crit2_hyp <- glht(glmer_expB_crit_treatment,linfct=mainEffects2_matrix)
crit3_hyp <- glht(glmer_expB_crit_treatment,linfct=mainEffects3_matrix)


# main effects control
control1_hyp <- glht(glmer_expB_control_treatment,linfct=mainEffects1_matrix)
control2_hyp <- glht(glmer_expB_control_treatment,linfct=mainEffects2_matrix)
control3_hyp <- glht(glmer_expB_control_treatment,linfct=mainEffects3_matrix)


############# interactions ##############

# Define the specific contrasts
interaction1_matrix <- matrix(bi.notMatch.1-bi.match.1-mono.notMatch.1+mono.match.1,1)
interaction2_matrix <- matrix(bi.notMatch.2-bi.match.2-mono.notMatch.2+mono.match.2,1)
interaction3_matrix <- matrix(bi.notMatch.3-bi.match.3-mono.notMatch.3+mono.match.3,1)

# interaction critical
interaction_crit1_hyp <- glht(glmer_expB_crit_treatment,linfct=interaction1_matrix)
interaction_crit2_hyp <- glht(glmer_expB_crit_treatment,linfct=interaction2_matrix)
interaction_crit3_hyp <- glht(glmer_expB_crit_treatment,linfct=interaction3_matrix)

# interaction control
interaction_control1_hyp <- glht(glmer_expB_control_treatment,linfct=interaction1_matrix)
interaction_control2_hyp <- glht(glmer_expB_control_treatment,linfct=interaction2_matrix)
interaction_control3_hyp <- glht(glmer_expB_control_treatment,linfct=interaction3_matrix)


# summaries

summary(crit1_hyp)
summary(crit2_hyp)
summary(crit3_hyp)

summary(control1_hyp)
summary(control2_hyp)
summary(control3_hyp)

summary(interaction_crit1_hyp)
summary(interaction_crit2_hyp)
summary(interaction_crit3_hyp)

summary(interaction_control1_hyp)
summary(interaction_control2_hyp)
summary(interaction_control3_hyp)




###################################################
#                                                 #
#                  SLEEP QUALITY                  #
#                  (Dissertation "sleep tests")   #
#                                                 #
###################################################

glmer_sleep_crit <- glmer(Response ~ Condition*PairType*SleepAmount + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expB_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

glmer_sleep_control <- glmer(Response ~ Condition*PairType*SleepAmount + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expB_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

summary(glmer_sleep_crit)
summary(glmer_sleep_control)


# Order of fixedef is...
# 1.Intercept, 2.Mono, 3.NotMatchPair, 4.Less, 5.Mono:NotMatchPair, 6.Mono:Less, 7.NotMatchPair:Less, 8.Mono:NotMatchPair:Less


#                       1 2 3 4 5 6 7 8 
bi.match.more      <- c(1,0,0,0,0,0,0,0)
mono.match.more    <- c(1,1,0,0,0,0,0,0)
bi.notMatch.more   <- c(1,0,1,0,0,0,0,0)
mono.notMatch.more <- c(1,1,1,0,1,0,0,0)

bi.match.less      <- c(1,0,0,1,0,0,0,0)
mono.match.less    <- c(1,1,0,1,0,1,0,0)
bi.notMatch.less   <- c(1,0,1,1,0,0,1,0)
mono.notMatch.less <- c(1,1,1,1,1,1,1,1)


############ interaction sleep x condition ###############

# Interaction sleep x condition  = Ave(Bi.Less) + Ave(Mono.More) - Ave(Bi.More) - Ave(Mono.Less)
#                       = ((bi.match.less + bi.notMatch.less)/2)
#                         + ((mono.match.more + mono.notMatch.more)/2)
#                         - ((bi.match.more + bi.notMatch.more)/2)
#                         - ((mono.match.less + mono.notMatch.less)/2) 

# Define the specific contrasts
interaction_matrix_sleep.condition <- matrix(((bi.match.less + bi.notMatch.less)/2)+((mono.match.more + mono.notMatch.more)/2)-((bi.match.more + bi.notMatch.more)/2)-((mono.match.less + mono.notMatch.less)/2) ,1)


# interaction sleep x condition critical
crit_hyp_sleep.condition <- glht(glmer_sleep_crit,linfct=interaction_matrix_sleep.condition)

# interaction sleep x condition critical
control_hyp_sleep.condition <- glht(glmer_sleep_control,linfct=interaction_matrix_sleep.condition)

summary(crit_hyp_sleep.condition)
summary(control_hyp_sleep.condition)

############ main effects ###############

# Main effect of sleep  = Ave (MoreSleep) - Ave (LessSleep)
#                       = (bi.match.more+mono.match.more+bi.notMatch.more+mono.notMatch.more)/4
#                         -(bi.match.less+mono.match.less+bi.notMatch.less+mono.notMatch.less)/4

# Define the specific contrasts
mainEffects_matrix_sleep <- matrix(((bi.match.more+mono.match.more+bi.notMatch.more+mono.notMatch.more)/4)-((bi.match.less+mono.match.less+bi.notMatch.less+mono.notMatch.less)/4),1)


# main effects critical
crit_hyp_sleep <- glht(glmer_sleep_crit,linfct=mainEffects_matrix_sleep)

# main effects control
control_hyp_sleep <- glht(glmer_sleep_control,linfct=mainEffects_matrix_sleep)

summary(crit_hyp_sleep)
summary(control_hyp_sleep)

############# interactions ##############

# Define the specific contrasts
interaction1_matrix <- matrix(bi.notMatch.1-bi.match.1-mono.notMatch.1+mono.match.1,1)
interaction2_matrix <- matrix(bi.notMatch.2-bi.match.2-mono.notMatch.2+mono.match.2,1)
interaction3_matrix <- matrix(bi.notMatch.3-bi.match.3-mono.notMatch.3+mono.match.3,1)

# interaction critical
interaction_crit1_hyp <- glht(glmer_expB_crit_treatment,linfct=interaction1_matrix)
interaction_crit2_hyp <- glht(glmer_expB_crit_treatment,linfct=interaction2_matrix)
interaction_crit3_hyp <- glht(glmer_expB_crit_treatment,linfct=interaction3_matrix)

# interaction control
interaction_control1_hyp <- glht(glmer_expB_control_treatment,linfct=interaction1_matrix)
interaction_control2_hyp <- glht(glmer_expB_control_treatment,linfct=interaction2_matrix)
interaction_control3_hyp <- glht(glmer_expB_control_treatment,linfct=interaction3_matrix)


# summaries

summary(crit1_hyp)
summary(crit2_hyp)
summary(crit3_hyp)

summary(control1_hyp)
summary(control2_hyp)
summary(control3_hyp)

summary(interaction_crit1_hyp)
summary(interaction_crit2_hyp)
summary(interaction_crit3_hyp)

summary(interaction_control1_hyp)
summary(interaction_control2_hyp)
summary(interaction_control3_hyp)