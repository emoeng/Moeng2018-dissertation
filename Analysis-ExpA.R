################ README #############################
#
# This script contains code used to analyze the "A" set of experiments in Moeng (2018)'s dissertation (Chapter 3)
#
# There were 3 names used for each experiment: the original name used when I was running the experiments, the analysisName (used in analysis scripts and some early dissertation drafts), and the final name as stated in the dissertation. Since analysis scripts may make use of earlier names that did not make it into the dissertation, all names are provided below for reference.
# 
# original | analysisName | Dissertation name
#------------------------------------------------------
# Data3    | Exp A1-1     | Pilot experiment
# Data6    | Exp A1-2     | Exp A1
# Data4    | Exp A2-2     | Exp A2
# Data8    | Exp A3-2     | Exp A3
# Data5    | Exp A2-1     | Exp A2-Tone
# Data9    | Exp A3-1     | Exp A3-Tone
#
#####################################################

library(readxl)
library(lme4)

################ READ IN DATA #######################

data_expA11 <- read_excel("C:/Users/Emily/Google Drive/Research/Dissertation/Stored on GitHub/data-pilot (data3).xlsx")
data_expA12 <- read_excel("C:/Users/Emily/Google Drive/Research/Dissertation/Stored on GitHub/data-A1 (data6).xlsx")
data_expA21 <- read_excel("C:/Users/Emily/Google Drive/Research/Dissertation/Stored on GitHub/data-A2-Tone (data5).xlsx")
data_expA22 <- read_excel("C:/Users/Emily/Google Drive/Research/Dissertation/Stored on GitHub/data-A2 (data4).xlsx")
data_expA31 <- read_excel("C:/Users/Emily/Google Drive/Research/Dissertation/Stored on GitHub/data-A3-Tone (data9).xlsx")
data_expA32 <- read_excel("C:/Users/Emily/Google Drive/Research/Dissertation/Stored on GitHub/data-A3 (data8).xlsx")

data_expA11 <- data_expA11[c(1:8)]
data_expA12 <- data_expA12[c(1:8)]
data_expA21 <- data_expA21[c(1:8)]
data_expA22 <- data_expA22[c(1:8)]
data_expA31 <- data_expA31[c(1:8)]
data_expA32 <- data_expA32[c(1:8)]

# Pilot experiment (not reported in dissertation) does not analyze test pairs including continuum points 3,4,5,6
data_expA11 <- data_expA11[which(data_expA11$CorrectAnswer!="ignore"),]

colnames(data_expA11)[4] <- "PairType"
colnames(data_expA12)[4] <- "PairType"
colnames(data_expA21)[4] <- "PairType"
colnames(data_expA22)[4] <- "PairType"

colnames(data_expA11)[6] <- "Item"
colnames(data_expA12)[6] <- "Item"
colnames(data_expA21)[6] <- "Item"
colnames(data_expA22)[6] <- "Item"
colnames(data_expA31)[6] <- "Item"
colnames(data_expA32)[6] <- "Item"

data_expA11$Subject = as.factor(data_expA11$Subject)
data_expA12$Subject = as.factor(data_expA12$Subject)
data_expA21$Subject = as.factor(data_expA21$Subject)
data_expA22$Subject = as.factor(data_expA22$Subject)
data_expA31$Subject = as.factor(data_expA31$Subject)
data_expA32$Subject = as.factor(data_expA32$Subject)

data_expA11$Response = as.factor(data_expA11$Response)
data_expA12$Response = as.factor(data_expA12$Response)
data_expA21$Response = as.factor(data_expA21$Response)
data_expA22$Response = as.factor(data_expA22$Response)
data_expA31$Response = as.factor(data_expA31$Response)
data_expA32$Response = as.factor(data_expA32$Response)

data_expA11$Condition = as.factor(data_expA11$Condition)
data_expA12$Condition = as.factor(data_expA12$Condition)
data_expA21$Condition = as.factor(data_expA21$Condition)
data_expA22$Condition = as.factor(data_expA22$Condition)
data_expA31$Condition = as.factor(data_expA31$Condition)
data_expA32$Condition = as.factor(data_expA32$Condition)

data_expA11$TrialType = as.factor(data_expA11$TrialType)
data_expA12$TrialType = as.factor(data_expA12$TrialType)
data_expA21$TrialType = as.factor(data_expA21$TrialType)
data_expA22$TrialType = as.factor(data_expA22$TrialType)
data_expA31$TrialType = as.factor(data_expA31$TrialType)
data_expA32$TrialType = as.factor(data_expA32$TrialType)

data_expA11$PairType = as.factor(data_expA11$PairType)
data_expA12$PairType = as.factor(data_expA12$PairType)
data_expA21$PairType = as.factor(data_expA21$PairType)
data_expA22$PairType = as.factor(data_expA22$PairType)
data_expA31$PairType = as.factor(data_expA31$PairType)
data_expA32$PairType = as.factor(data_expA32$PairType)

data_expA11$Item = as.factor(data_expA11$Item)
data_expA12$Item = as.factor(data_expA12$Item)
data_expA21$Item = as.factor(data_expA21$Item)
data_expA22$Item = as.factor(data_expA22$Item)
data_expA31$Item = as.factor(data_expA31$Item)
data_expA32$Item = as.factor(data_expA32$Item)

data_expA11$Response <- relevel(data_expA11$Response, ref = "s")
data_expA12$Response <- relevel(data_expA12$Response, ref = "s")
data_expA21$Response <- relevel(data_expA21$Response, ref = "s")
data_expA22$Response <- relevel(data_expA22$Response, ref = "s")
data_expA31$Response <- relevel(data_expA31$Response, ref = "s")
data_expA32$Response <- relevel(data_expA32$Response, ref = "s")

#View(data_expA11)
#View(data_expA12)
#View(data_expA21)
#View(data_expA22)
#View(data_expA31)
#View(data_expA32)

#
#
#####################################################



######################################################
#                                                   ##
#                GLMER MODELS                       ##
#                                                   ##
######################################################
#
#
#
# STEP 1: SUBSET DATA INTO CRIT AND CONTROL TRIALS
# STEP 2: RUN GLMER ON EACH
#   CHOOSE ONE OF THE TWO EQUATIONS BELOW FOR GLMER_expA_crit
#   (Second equation below uses the default Laplace Approximation algorithm with 10,000 evaluations. If it fails after 10,000 evaluations, use first equation, which uses the Adaptive Gauss-Hermite Quadrature algorithm)
#
#     CHOOSE ONE:
#     1. ADAPTIVE GAUSS-HERMITE QUADRATURE ALGORITHM
#     glmer_expA_crit_treatment <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),nAGQ=0)
#     2. (DEFAULT) LAPLACE APPROXIMATION ALGORITHM
#     glmer_expA_crit_treatment <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
#     summary(glmer_expA_crit_treatment)
#
# STEP 3: DO FOLLOW-UP CONTRASTS



#################################
# SUBSET DATA
#################################
data_expA11_crit <- data_expA11[which(data_expA11$TrialType=="G"),]
data_expA12_crit <- data_expA12[which(data_expA12$TrialType=="G"),]
data_expA21_crit <- data_expA21[which(data_expA21$TrialType=="G"),]
data_expA22_crit <- data_expA22[which(data_expA22$TrialType=="G"),]
data_expA31_crit <- data_expA31[which(data_expA31$TrialType=="crit"),]
data_expA32_crit <- data_expA32[which(data_expA32$TrialType=="crit"),]

data_expA11_control <- data_expA11[which(data_expA11$TrialType=="control"),]
data_expA12_control <- data_expA12[which(data_expA12$TrialType=="control"),]
data_expA21_control <- data_expA21[which(data_expA21$TrialType=="control"),]
data_expA22_control <- data_expA22[which(data_expA22$TrialType=="control"),]
data_expA31_control <- data_expA31[which(data_expA31$TrialType=="control"),]
data_expA32_control <- data_expA32[which(data_expA32$TrialType=="control"),]

#View(data_expA11_crit)
#View(data_expA11_control)



#################################
# GLMER (TREATMENT CODING)
#################################

glmer_expA11_crit <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA11_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
glmer_expA12_crit <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA12_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
#glmer_expA21_crit <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA21_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
glmer_expA22_crit <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA22_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
#glmer_expA31_crit <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA31_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
glmer_expA32_crit <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA32_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

# Default LaPlace fails to converge for A21 and A31 control trials, so use Adaptive Gauss-Hermite Quadrature instead
glmer_expA21_crit <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA21_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),nAGQ=0)
glmer_expA31_crit <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA31_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),nAGQ=0)




glmer_expA11_control <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA11_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
glmer_expA12_control <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA12_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
#glmer_expA21_control <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA21_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
glmer_expA22_control <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA22_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
#glmer_expA31_control <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA31_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
glmer_expA32_control <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA32_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

# Default LaPlace fails to converge for A21 and A31 control trials, so use Adaptive Gauss-Hermite Quadrature instead
glmer_expA21_control <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA21_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),nAGQ=0)
glmer_expA31_control <- glmer(Response ~ Condition*PairType + (1+PairType|Subject) + (1+Condition|Item), family="binomial", data=data_expA31_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),nAGQ=0)

summary(glmer_expA11_crit)
summary(glmer_expA12_crit)
summary(glmer_expA21_crit)  # Adaptive Gauss-Hermite Quadrature
summary(glmer_expA22_crit)
summary(glmer_expA31_crit)  # Adaptive Gauss-Hermite Quadrature
summary(glmer_expA32_crit)

summary(glmer_expA11_control)
summary(glmer_expA12_control)
summary(glmer_expA21_control)  # Adaptive Gauss-Hermite Quadrature
summary(glmer_expA22_control)
summary(glmer_expA31_control)  # Adaptive Gauss-Hermite Quadrature
summary(glmer_expA32_control)



#################################
# FOLLOW-UP HYPOTHESIS TESTING
#################################


########################## GET MAIN EFFECT ###########################
#   
#   The equivalent of doing contrast coding (coding levels of independent factors as -.5 and .5 rather than default dummy coding, which codes levels as 0's and 1's)
#
#   Get stats for a specific contrast, in this case, main effect of Condition
#
#   Main effect of Condition is the average of PairType diff and PairType same
#
# 
#               |   Condition=Bimodal   |  Condition=Monomodal
# --------------|-----------------------|-----------------------
# PairType=Diff | intercept             | intercept
#               |                       |     +ConditionMonomodal
# --------------|-----------------------|-----------------------
# PairType=Same | intercept             | intercept
#               |     +PairTypeSame     |     +ConditionMonomodal
#               |                       |     +PairTypeSame
#               |                       |     +ConditionMonomodal:PairTypeSame
#
#
# Main effect of Condition = (Average of Monomodal Diff and Monomodal Same) - (Average of Bimodal Diff and Bimodal Same)
#   = ((intercept+ConditionMono)+(intercept+ConditionMono+PairTypeSame+ConditionMono:PairTypeSame))/2 - ((intercept)+(intercept+PairTypeSame))/2
#   = (intercept+ConditonMono+.5*PairTypeSame+.5*ConditonMono:PairTypeSame)-(intercept+.5*PairTypeSame)
#   = ConditionMono+.5*ConditionMono:PairTypeSame
#
# se=sqrt(diag(vcov(glmer_expA_crit_treatment)))
# fixedef=fixef(glmer_expA_crit_treatment)
#
# Order of fixedef is Intercept, ConditionMono, PairTypeSame, ConditionMono:PairTypeSame
# Since we want 0 Intercept + 1 ConditionMono + 0 PairTypeSame + .5 ConditionMono:PairTypeSame, make matrix (0,1,0,.5)
# 
#
#
#
####### Main effects

library(multcomp)
h <- matrix(c(0,1,0,.5),1)    # Define the specific contrasts
htest_critA11 <- glht(glmer_expA11_crit,linfct=h)
htest_critA12 <- glht(glmer_expA12_crit,linfct=h)
htest_critA21 <- glht(glmer_expA21_crit,linfct=h)
htest_critA22 <- glht(glmer_expA22_crit,linfct=h)
htest_critA31 <- glht(glmer_expA31_crit,linfct=h)
htest_critA32 <- glht(glmer_expA32_crit,linfct=h)

htest_controlA11 <- glht(glmer_expA11_control,linfct=h)
htest_controlA12 <- glht(glmer_expA12_control,linfct=h)
htest_controlA21 <- glht(glmer_expA21_control,linfct=h)
htest_controlA22 <- glht(glmer_expA22_control,linfct=h)
htest_controlA31 <- glht(glmer_expA31_control,linfct=h)
htest_controlA32 <- glht(glmer_expA32_control,linfct=h)

summary(htest_critA11)
summary(htest_critA12)
summary(htest_critA21)
summary(htest_critA22)
summary(htest_critA31)
summary(htest_critA32)

summary(htest_controlA11)
summary(htest_controlA12)
summary(htest_controlA21)
summary(htest_controlA22)
summary(htest_controlA31)
summary(htest_controlA32)

#intercept_contrastCoding_crit <- summary(htest_crit)$test$coefficients
#se_contrastCoding_crit <- summary(htest_crit)$test$sigma
#pvalue_contrastCoding_crit <- summary(htest_crit)$test$pvalue
