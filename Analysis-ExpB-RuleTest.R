################ README #############################
#
# This script contains code used to analyze the Rule Test in Experiment B in Moeng (2018)'s dissertation (Chapter 4)
#
# There were 3 names used for each experiment: the original name used when I was running the experiments, the analysisName (used in analysis scripts and some early dissertation drafts), and the final name as stated in the dissertation. Since analysis scripts may make use of earlier names that did not make it into the dissertation, all names are provided below for reference.
# 
# original | analysisName | Dissertation name
#------------------------------------------------------
# C3       | Exp C        | Exp B
#
#####################################################

library(readxl)
library(lme4)
library(plyr)
library(multcomp)

################# READ IN, PREP DATA ###################

data_expCRule <- read_excel("C:/Users/Emily/Google Drive/Research/Dissertation/Stored on GitHub/data-B (origC3).xlsx","RawNumbers")

colnames(data_expCRule)[7] <- "Subject"
colnames(data_expCRule)[9] <- "Condition"
colnames(data_expCRule)[10] <- "Subcondition"
colnames(data_expCRule)[11] <- "Timepoint"
colnames(data_expCRule)[12] <- "Item"
colnames(data_expCRule)[13] <- "Response"
colnames(data_expCRule)[14] <- "PairType"
colnames(data_expCRule)[15] <- "TrialType"
colnames(data_expCRule)[16] <- "TestType"

data_expCRule <- data_expCRule[which(data_expCRule$TestType=="rule"),]

# A future experiment had been planned. Experiment B included a pilot of that future experiment, so some participants were tested on a fourth timepoint (about half the number as in other timepoints). Their results are included in the data file, but not discussed in the dissertation. For those wishing to test longer exposure times, I would suggest splitting this over multiple days, as those in the third timepoint seemed to show effects of fatigue (see discussion in dissertation)
data_expCRule <- data_expCRule[which(data_expCRule$Timepoint!="four"),]

data_expCRule$Subject = as.factor(data_expCRule$Subject)
data_expCRule$Condition = as.factor(data_expCRule$Condition)
data_expCRule$Subcondition = as.factor(data_expCRule$Subcondition)
data_expCRule$Timepoint = as.factor(data_expCRule$Timepoint)
data_expCRule$Item = as.factor(data_expCRule$Item)
data_expCRule$Response = as.factor(data_expCRule$Response)
data_expCRule$PairType = as.factor(data_expCRule$PairType)
data_expCRule$TrialType = as.factor(data_expCRule$TrialType)
data_expCRule$TestType = as.factor(data_expCRule$TestType)


table(data_expCRule$Timepoint)
levels(data_expCRule$Timepoint)
levels(data_expCRule$Timepoint) <- c("Aone", "Cthree", "Btwo")
table(data_expCRule$Timepoint)


levels(data_expCRule$Response)      # "0" = 'yes, it's allowed, "1" = 'no, it's not allowed
levels(data_expCRule$Response) <-c("y","n")    

data_expCRule$Correct <- ifelse(data_expCRule$Response=="y" & data_expCRule$PairType=="yes", "Bcorrect",
                                ifelse(data_expCRule$Response=="y" & data_expCRule$PairType=="no", "Aincorrect",
                                       ifelse(data_expCRule$Response=="n" & data_expCRule$PairType=="yes", "Aincorrect",
                                              ifelse(data_expCRule$Response=="n" & data_expCRule$PairType=="no", "Bcorrect",
                                                     NA  )))) # all other values map to NA
data_expCRule$Correct = as.factor(data_expCRule$Correct)

#View(data_expCRule)

data_expCRule$Response <- relevel(data_expCRule$Response, ref="n")


######################################################
#                                                   
#                GLMER MODELS                   
#                                               
######################################################

# Subset overall dataset into critical and control trials
data_expC_old <- data_expCRule[which(data_expCRule$TrialType=="old"),]
data_expC_new <- data_expCRule[which(data_expCRule$TrialType=="new"),]
data_expC_control <- data_expCRule[which(data_expCRule$TrialType=="control"),]


glmer_expC_old_treatment <- glmer(Response ~ Condition*PairType*Timepoint + (1|Subject) + (1|Item), family="binomial", data=data_expC_old, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

glmer_expC_new_treatment <- glmer(Response ~ Condition*PairType*Timepoint + (1|Subject) + (1|Item), family="binomial", data=data_expC_new, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

summary(glmer_expC_old_treatment)
summary(glmer_expC_new_treatment)


# Order of fixedef is...
# 1.Intercept, 2.BiNon, 3.Mono, 4.Yes, 5.Three, 6.Two, 7.BiNon:Yes, 8.Mono:Yes, 9.BiNon:Three, 10.Mono:Three, 11.BiNon:Two, 12.Mono:Two, 13.Yes:Three, 14.Yes:Two, 15.BiNon:Yes:Three, 16.Mono:Yes:Three, 17.BiNon:Yes:Two, 18.Mono:Yes:Two


#                c(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8)
biComp.no.1   <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
biNon.no.1    <- c(1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mono.no.1     <- c(1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

#                c(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8)
biComp.no.2   <- c(1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
biNon.no.2    <- c(1,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0)
mono.no.2     <- c(1,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0)

#                c(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8)
biComp.no.3   <- c(1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
biNon.no.3    <- c(1,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0)
mono.no.3     <- c(1,0,1,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0)

#                 c(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8)
biComp.yes.1   <- c(1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
biNon.yes.1    <- c(1,1,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0)
mono.yes.1     <- c(1,0,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0)

#                 c(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8)
biComp.yes.2   <- c(1,0,0,1,0,1,0,0,0,0,0,0,0,1,0,0,0,0)
biNon.yes.2    <- c(1,1,0,1,0,1,1,0,0,0,1,0,0,1,0,0,1,0)
mono.yes.2     <- c(1,0,1,1,0,1,0,1,0,0,0,1,0,1,0,0,0,1)

#                 c(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8)
biComp.yes.3   <- c(1,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0)
biNon.yes.3    <- c(1,1,0,1,1,0,1,0,1,0,0,0,1,0,1,0,0,0)
mono.yes.3     <- c(1,0,1,1,1,0,0,1,0,1,0,0,1,0,0,1,0,0)


########## matrices defining specific contrasts for specific hypotheses ##############
# Define the specific contrasts

# for sensitivity means
sensitivity.biNon.1_matrix <- matrix(biNon.yes.1-biNon.no.1,1)    
sensitivity.biComp.1_matrix <- matrix(biComp.yes.1-biComp.no.1,1)
sensitivity.mono.1_matrix <- matrix(mono.yes.1-mono.no.1,1)

sensitivity.biNon.2_matrix <- matrix(biNon.yes.2-biNon.no.2,1)
sensitivity.biComp.2_matrix <- matrix(biComp.yes.2-biComp.no.2,1) 
sensitivity.mono.2_matrix <- matrix(mono.yes.2-mono.no.2,1)

sensitivity.biNon.3_matrix <- matrix(biNon.yes.3-biNon.no.3,1)    # Define the specific contrasts
sensitivity.biComp.3_matrix <- matrix(biComp.yes.3-biComp.no.3,1)    # Define the specific contrasts
sensitivity.mono.3_matrix <- matrix(mono.yes.3-mono.no.3,1)    # Define the specific contrasts


# for sensitivity comparisons
sensitivity.biNon.mono.1_matrix <- matrix(sensitivity.biNon.1_matrix-sensitivity.mono.1_matrix,1)    
sensitivity.biComp.mono.1_matrix <- matrix(sensitivity.biComp.1_matrix-sensitivity.mono.1_matrix,1)
sensitivity.biComp.biNon.1_matrix <- matrix(sensitivity.biComp.1_matrix-sensitivity.biNon.1_matrix,1)

sensitivity.biNon.mono.2_matrix <- matrix(sensitivity.biNon.2_matrix-sensitivity.mono.2_matrix,1)
sensitivity.biComp.mono.2_matrix <- matrix(sensitivity.biComp.2_matrix-sensitivity.mono.2_matrix,1)
sensitivity.biComp.biNon.2_matrix <- matrix(sensitivity.biComp.2_matrix-sensitivity.biNon.2_matrix,1)

sensitivity.biNon.mono.3_matrix <- matrix(sensitivity.biNon.3_matrix-sensitivity.mono.3_matrix,1)
sensitivity.biComp.mono.3_matrix <- matrix(sensitivity.biComp.3_matrix-sensitivity.mono.3_matrix,1)
sensitivity.biComp.biNon.3_matrix <- matrix(sensitivity.biComp.3_matrix-sensitivity.biNon.3_matrix,1)




############ test specific hypotheses ###################
######## get sensitivity means
### old
# time 1
biNon.1.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biNon.1_matrix)
biNon.1.old_intercept <- summary(biNon.1.old_hyp)$test$coefficients
biNon.1.old_se <- summary(biNon.1.old_hyp)$test$sigma

biComp.1.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biComp.1_matrix)
biComp.1.old_intercept <- summary(biComp.1.old_hyp)$test$coefficients
biComp.1.old_se <- summary(biNon.1.old_hyp)$test$sigma

mono.1.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.mono.1_matrix)
mono.1.old_intercept <- summary(mono.1.old_hyp)$test$coefficients
mono.1.old_se <- summary(biNon.1.old_hyp)$test$sigma

# time 2
biNon.2.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biNon.2_matrix)
biNon.2.old_intercept <- summary(biNon.2.old_hyp)$test$coefficients
biNon.2.old_se <- summary(biNon.2.old_hyp)$test$sigma

biComp.2.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biComp.2_matrix)
biComp.2.old_intercept <- summary(biComp.2.old_hyp)$test$coefficients
biComp.2.old_se <- summary(biNon.2.old_hyp)$test$sigma

mono.2.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.mono.2_matrix)
mono.2.old_intercept <- summary(mono.2.old_hyp)$test$coefficients
mono.2.old_se <- summary(biNon.2.old_hyp)$test$sigma

# time 3
biNon.3.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biNon.3_matrix)
biNon.3.old_intercept <- summary(biNon.3.old_hyp)$test$coefficients
biNon.3.old_se <- summary(biNon.3.old_hyp)$test$sigma

biComp.3.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biComp.3_matrix)
biComp.3.old_intercept <- summary(biComp.3.old_hyp)$test$coefficients
biComp.3.old_se <- summary(biNon.3.old_hyp)$test$sigma

mono.3.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.mono.3_matrix)
mono.3.old_intercept <- summary(mono.3.old_hyp)$test$coefficients
mono.3.old_se <- summary(biNon.3.old_hyp)$test$sigma


### new
# time 1
biNon.1.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biNon.1_matrix)
biNon.1.new_intercept <- summary(biNon.1.new_hyp)$test$coefficients
biNon.1.new_se <- summary(biNon.1.new_hyp)$test$sigma

biComp.1.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biComp.1_matrix)
biComp.1.new_intercept <- summary(biComp.1.new_hyp)$test$coefficients
biComp.1.new_se <- summary(biNon.1.new_hyp)$test$sigma

mono.1.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.mono.1_matrix)
mono.1.new_intercept <- summary(mono.1.new_hyp)$test$coefficients
mono.1.new_se <- summary(biNon.1.new_hyp)$test$sigma

# time 2
biNon.2.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biNon.2_matrix)
biNon.2.new_intercept <- summary(biNon.2.new_hyp)$test$coefficients
biNon.2.new_se <- summary(biNon.2.new_hyp)$test$sigma

biComp.2.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biComp.2_matrix)
biComp.2.new_intercept <- summary(biComp.2.new_hyp)$test$coefficients
biComp.2.new_se <- summary(biNon.2.new_hyp)$test$sigma

mono.2.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.mono.2_matrix)
mono.2.new_intercept <- summary(mono.2.new_hyp)$test$coefficients
mono.2.new_se <- summary(biNon.2.new_hyp)$test$sigma

# time 3
biNon.3.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biNon.3_matrix)
biNon.3.new_intercept <- summary(biNon.3.new_hyp)$test$coefficients
biNon.3.new_se <- summary(biNon.3.new_hyp)$test$sigma

biComp.3.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biComp.3_matrix)
biComp.3.new_intercept <- summary(biComp.3.new_hyp)$test$coefficients
biComp.3.new_se <- summary(biNon.3.new_hyp)$test$sigma

mono.3.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.mono.3_matrix)
mono.3.new_intercept <- summary(mono.3.new_hyp)$test$coefficients
mono.3.new_se <- summary(biNon.3.new_hyp)$test$sigma


######## make sensitivity comparisons (interaction btwn pairtype and distribution) for each pair of distributions
### interaction for old
# time 1
biNon.mono.1.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biNon.mono.1_matrix)
biNon.mono.1.old_intercept <- summary(biNon.mono.1.old_hyp)$test$coefficients
biNon.mono.1.old_se <- summary(biNon.mono.1.old_hyp)$test$sigma

biComp.mono.1.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biComp.mono.1_matrix)
biComp.mono.1.old_intercept <- summary(biComp.mono.1.old_hyp)$test$coefficients
biComp.mono.1.old_se <- summary(biComp.mono.1.old_hyp)$test$sigma

biComp.biNon.1.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biComp.biNon.1_matrix)
biComp.biNon.1.old_intercept <- summary(biComp.biNon.1.old_hyp)$test$coefficients
biComp.biNon.1.old_se <- summary(biComp.biNon.1.old_hyp)$test$sigma

# time 2
biNon.mono.2.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biNon.mono.2_matrix)
biNon.mono.2.old_intercept <- summary(biNon.mono.2.old_hyp)$test$coefficients
biNon.mono.2.old_se <- summary(biNon.mono.2.old_hyp)$test$sigma

biComp.mono.2.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biComp.mono.2_matrix)
biComp.mono.2.old_intercept <- summary(biComp.mono.2.old_hyp)$test$coefficients
biComp.mono.2.old_se <- summary(biComp.mono.2.old_hyp)$test$sigma

biComp.biNon.2.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biComp.biNon.2_matrix)
biComp.biNon.2.old_intercept <- summary(biComp.biNon.2.old_hyp)$test$coefficients
biComp.biNon.2.old_se <- summary(biComp.biNon.2.old_hyp)$test$sigma

# time 3
biNon.mono.3.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biNon.mono.3_matrix)
biNon.mono.3.old_intercept <- summary(biNon.mono.3.old_hyp)$test$coefficients
biNon.mono.3.old_se <- summary(biNon.mono.3.old_hyp)$test$sigma

biComp.mono.3.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biComp.mono.3_matrix)
biComp.mono.3.old_intercept <- summary(biComp.mono.3.old_hyp)$test$coefficients
biComp.mono.3.old_se <- summary(biComp.mono.3.old_hyp)$test$sigma

biComp.biNon.3.old_hyp <- glht(glmer_expC_old_treatment,linfct=sensitivity.biComp.biNon.3_matrix)
biComp.biNon.3.old_intercept <- summary(biComp.biNon.3.old_hyp)$test$coefficients
biComp.biNon.3.old_se <- summary(biComp.biNon.3.old_hyp)$test$sigma

### interaction for new
# time 1
biNon.mono.1.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biNon.mono.1_matrix)
biNon.mono.1.new_intercept <- summary(biNon.mono.1.new_hyp)$test$coefficients
biNon.mono.1.new_se <- summary(biNon.mono.1.new_hyp)$test$sigma

biComp.mono.1.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biComp.mono.1_matrix)
biComp.mono.1.new_intercept <- summary(biComp.mono.1.new_hyp)$test$coefficients
biComp.mono.1.new_se <- summary(biComp.mono.1.new_hyp)$test$sigma

biComp.biNon.1.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biComp.biNon.1_matrix)
biComp.biNon.1.new_intercept <- summary(biComp.biNon.1.new_hyp)$test$coefficients
biComp.biNon.1.new_se <- summary(biComp.biNon.1.new_hyp)$test$sigma

# time 2
biNon.mono.2.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biNon.mono.2_matrix)
biNon.mono.2.new_intercept <- summary(biNon.mono.2.new_hyp)$test$coefficients
biNon.mono.2.new_se <- summary(biNon.mono.2.new_hyp)$test$sigma

biComp.mono.2.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biComp.mono.2_matrix)
biComp.mono.2.new_intercept <- summary(biComp.mono.2.new_hyp)$test$coefficients
biComp.mono.2.new_se <- summary(biComp.mono.2.new_hyp)$test$sigma

biComp.biNon.2.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biComp.biNon.2_matrix)
biComp.biNon.2.new_intercept <- summary(biComp.biNon.2.new_hyp)$test$coefficients
biComp.biNon.2.new_se <- summary(biComp.biNon.2.new_hyp)$test$sigma

# time 3
biNon.mono.3.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biNon.mono.3_matrix)
biNon.mono.3.new_intercept <- summary(biNon.mono.3.new_hyp)$test$coefficients
biNon.mono.3.new_se <- summary(biNon.mono.3.new_hyp)$test$sigma

biComp.mono.3.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biComp.mono.3_matrix)
biComp.mono.3.new_intercept <- summary(biComp.mono.3.new_hyp)$test$coefficients
biComp.mono.3.new_se <- summary(biComp.mono.3.new_hyp)$test$sigma

biComp.biNon.3.new_hyp <- glht(glmer_expC_new_treatment,linfct=sensitivity.biComp.biNon.3_matrix)
biComp.biNon.3.new_intercept <- summary(biComp.biNon.3.new_hyp)$test$coefficients
biComp.biNon.3.new_se <- summary(biComp.biNon.3.new_hyp)$test$sigma


summary(biNon.mono.1.old_hyp)
summary(biComp.mono.1.old_hyp)
summary(biComp.biNon.1.old_hyp)

summary(biNon.mono.2.old_hyp)
summary(biComp.mono.2.old_hyp)
summary(biComp.biNon.2.old_hyp)

summary(biNon.mono.3.old_hyp)
summary(biComp.mono.3.old_hyp)
summary(biComp.biNon.3.old_hyp)


summary(biNon.mono.1.new_hyp)
summary(biComp.mono.1.new_hyp)
summary(biComp.biNon.1.new_hyp)

summary(biNon.mono.2.new_hyp)
summary(biComp.mono.2.new_hyp)
summary(biComp.biNon.2.new_hyp)

summary(biNon.mono.3.new_hyp)
summary(biComp.mono.3.new_hyp)
summary(biComp.biNon.3.new_hyp)