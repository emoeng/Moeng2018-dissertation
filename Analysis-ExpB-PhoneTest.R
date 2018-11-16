################ README #############################
#
# This script contains code used to analyze the Phone Test in Experiment B in Moeng (2018)'s dissertation (Chapter 4)
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
library(multcomp)
library(plyr)


################# READ IN, PREP DATA ###################

data_expC <- read_excel("C:/Users/Emily/Google Drive/Research/Dissertation/Stored on GitHub/data-B (origC3).xlsx","RawNumbers")

colnames(data_expC)[7] <- "Subject"
colnames(data_expC)[9] <- "Condition"
colnames(data_expC)[10] <- "Subcondition"
colnames(data_expC)[11] <- "Timepoint"
colnames(data_expC)[12] <- "Item"
colnames(data_expC)[13] <- "Response"
colnames(data_expC)[14] <- "PairType"
colnames(data_expC)[15] <- "TrialType"
colnames(data_expC)[16] <- "TestType"

data_expC <- data_expC[which(data_expC$TestType=="phon"),]

# A future experiment had been planned. Experiment B included a pilot of that future experiment, so some participants were tested on a fourth timepoint (about half the number as in other timepoints). Their results are included in the data file, but not discussed in the dissertation. For those wishing to test longer exposure times, I would suggest splitting this over multiple days, as those in the third timepoint seemed to show effects of fatigue (see discussion in dissertation)
data_expC <- data_expC[which(data_expC$Timepoint!="four"),]

data_expC$Subject = as.factor(data_expC$Subject)
data_expC$Condition = as.factor(data_expC$Condition)
data_expC$Subcondition = as.factor(data_expC$Subcondition)
data_expC$Timepoint = as.factor(data_expC$Timepoint)
data_expC$Item = as.factor(data_expC$Item)
data_expC$Response = as.factor(data_expC$Response)
data_expC$PairType = as.factor(data_expC$PairType)
data_expC$TrialType = as.factor(data_expC$TrialType)
data_expC$TestType = as.factor(data_expC$TestType)

# Rename factors
table(data_expC$Timepoint)
levels(data_expC$Timepoint)
levels(data_expC$Timepoint) <- c("Aone", "Cthree", "Btwo")
table(data_expC$Timepoint)

table(data_expC$Response)
levels(data_expC$Response)
levels(data_expC$Response) <- c("s", "d")
table(data_expC$Response)

######################################################
#                                 
#                GLMER MODELS     
#                                 
######################################################

# Subset overall dataset into critical and control trials
data_expC_crit <- data_expC[which(data_expC$TrialType=="critical"),]
data_expC_control <- data_expC[which(data_expC$TrialType=="control"),]

################ TREATMENT/DUMMY CODING ##############
glmer_expC_crit_treatment <- glmer(Response ~ Condition*PairType*Timepoint + (1|Subject) + (1|Item), family="binomial", data=data_expC_crit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

glmer_expC_control_treatment <- glmer(Response ~ Condition*PairType*Timepoint + (1|Subject) + (1|Item), family="binomial", data=data_expC_control, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

summary(glmer_expC_crit_treatment)
summary(glmer_expC_control_treatment)





biComp.diff.1   <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
biNon.diff.1    <- c(1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mono.diff.1     <- c(1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

biComp.diff.2   <- c(1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
biNon.diff.2    <- c(1,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0)
mono.diff.2     <- c(1,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0)

biComp.diff.3   <- c(1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
biNon.diff.3    <- c(1,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0)
mono.diff.3     <- c(1,0,1,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0)

biComp.same.1   <- c(1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
biNon.same.1    <- c(1,1,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0)
mono.same.1     <- c(1,0,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0)

biComp.same.2   <- c(1,0,0,1,0,1,0,0,0,0,0,0,0,1,0,0,0,0)
biNon.same.2    <- c(1,1,0,1,0,1,1,0,0,0,1,0,0,1,0,0,1,0)
mono.same.2     <- c(1,0,1,1,0,1,0,1,0,0,0,1,0,1,0,0,0,1)

biComp.same.3   <- c(1,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0)
biNon.same.3    <- c(1,1,0,1,1,0,1,0,1,0,0,0,1,0,1,0,0,0)
mono.same.3     <- c(1,0,1,1,1,0,0,1,0,1,0,0,1,0,0,1,0,0)



# timepoint 1
# main effects critical
biNon.mono.1.crit_matrix <- matrix((biNon.same.1+biNon.diff.1-mono.same.1-mono.diff.1)/2,1)    # Define the specific contrasts
biNon.mono.1.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=biNon.mono.1.crit_matrix)
biNon.mono.1.crit_intercept <- summary(biNon.mono.1.crit_hyp)$test$coefficients
biNon.mono.1.crit_se <- summary(biNon.mono.1.crit_hyp)$test$sigma

biComp.mono.1.crit_matrix <- matrix((biComp.same.1+biComp.diff.1-mono.same.1-mono.diff.1)/2,1)    # Define the specific contrasts
biComp.mono.1.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=biComp.mono.1.crit_matrix)
biComp.mono.1.crit_intercept <- summary(biComp.mono.1.crit_hyp)$test$coefficients
biComp.mono.1.crit_se <- summary(biComp.mono.1.crit_hyp)$test$sigma

biComp.biNon.1.crit_matrix <- matrix((biComp.same.1+biComp.diff.1-biNon.same.1-biNon.diff.1)/2,1)    # Define the specific contrasts
biComp.biNon.1.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=biComp.biNon.1.crit_matrix)
biComp.biNon.1.crit_intercept <- summary(biComp.biNon.1.crit_hyp)$test$coefficients
biComp.biNon.1.crit_se <- summary(biComp.biNon.1.crit_hyp)$test$sigma

# main effects controls
biNon.mono.1.control_matrix <- matrix((biNon.same.1+biNon.diff.1-mono.same.1-mono.diff.1)/2,1)    # Define the specific contrasts
biNon.mono.1.control_hyp <- glht(glmer_expC_control_treatment,linfct=biNon.mono.1.control_matrix)
biNon.mono.1.control_intercept <- summary(biNon.mono.1.control_hyp)$test$coefficients
biNon.mono.1.control_se <- summary(biNon.mono.1.control_hyp)$test$sigma

biComp.mono.1.control_matrix <- matrix((biComp.same.1+biComp.diff.1-mono.same.1-mono.diff.1)/2,1)    # Define the specific contrasts
biComp.mono.1.control_hyp <- glht(glmer_expC_control_treatment,linfct=biComp.mono.1.control_matrix)
biComp.mono.1.control_intercept <- summary(biComp.mono.1.control_hyp)$test$coefficients
biComp.mono.1.control_se <- summary(biComp.mono.1.control_hyp)$test$sigma

biComp.biNon.1.control_matrix <- matrix((biComp.same.1+biComp.diff.1-biNon.same.1-biNon.diff.1)/2,1)    # Define the specific contrasts
biComp.biNon.1.control_hyp <- glht(glmer_expC_control_treatment,linfct=biComp.biNon.1.control_matrix)
biComp.biNon.1.control_intercept <- summary(biComp.biNon.1.control_hyp)$test$coefficients
biComp.biNon.1.control_se <- summary(biComp.biNon.1.control_hyp)$test$sigma


# timepoint 2
# main effects critical
biNon.mono.2.crit_matrix <- matrix((biNon.same.2+biNon.diff.2-mono.same.2-mono.diff.2)/2,1)    # Define the specific contrasts
biNon.mono.2.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=biNon.mono.2.crit_matrix)
biNon.mono.2.crit_intercept <- summary(biNon.mono.2.crit_hyp)$test$coefficients
biNon.mono.2.crit_se <- summary(biNon.mono.2.crit_hyp)$test$sigma

biComp.mono.2.crit_matrix <- matrix((biComp.same.2+biComp.diff.2-mono.same.2-mono.diff.2)/2,1)    # Define the specific contrasts
biComp.mono.2.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=biComp.mono.2.crit_matrix)
biComp.mono.2.crit_intercept <- summary(biComp.mono.2.crit_hyp)$test$coefficients
biComp.mono.2.crit_se <- summary(biComp.mono.2.crit_hyp)$test$sigma

biComp.biNon.2.crit_matrix <- matrix((biComp.same.2+biComp.diff.2-biNon.same.2-biNon.diff.2)/2,1)    # Define the specific contrasts
biComp.biNon.2.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=biComp.biNon.2.crit_matrix)
biComp.biNon.2.crit_intercept <- summary(biComp.biNon.2.crit_hyp)$test$coefficients
biComp.biNon.2.crit_se <- summary(biComp.biNon.2.crit_hyp)$test$sigma

# main effects controls
biNon.mono.2.control_matrix <- matrix((biNon.same.2+biNon.diff.2-mono.same.2-mono.diff.2)/2,1)    # Define the specific contrasts
biNon.mono.2.control_hyp <- glht(glmer_expC_control_treatment,linfct=biNon.mono.2.control_matrix)
biNon.mono.2.control_intercept <- summary(biNon.mono.2.control_hyp)$test$coefficients
biNon.mono.2.control_se <- summary(biNon.mono.2.control_hyp)$test$sigma

biComp.mono.2.control_matrix <- matrix((biComp.same.2+biComp.diff.2-mono.same.2-mono.diff.2)/2,1)    # Define the specific contrasts
biComp.mono.2.control_hyp <- glht(glmer_expC_control_treatment,linfct=biComp.mono.2.control_matrix)
biComp.mono.2.control_intercept <- summary(biComp.mono.2.control_hyp)$test$coefficients
biComp.mono.2.control_se <- summary(biComp.mono.2.control_hyp)$test$sigma

biComp.biNon.2.control_matrix <- matrix((biComp.same.2+biComp.diff.2-biNon.same.2-biNon.diff.2)/2,1)    # Define the specific contrasts
biComp.biNon.2.control_hyp <- glht(glmer_expC_control_treatment,linfct=biComp.biNon.2.control_matrix)
biComp.biNon.2.control_intercept <- summary(biComp.biNon.2.control_hyp)$test$coefficients
biComp.biNon.2.control_se <- summary(biComp.biNon.2.control_hyp)$test$sigma

# timepoint 3
# main effects critical
biNon.mono.3.crit_matrix <- matrix((biNon.same.3+biNon.diff.3-mono.same.3-mono.diff.3)/2,1)    # Define the specific contrasts
biNon.mono.3.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=biNon.mono.3.crit_matrix)
biNon.mono.3.crit_intercept <- summary(biNon.mono.3.crit_hyp)$test$coefficients
biNon.mono.3.crit_se <- summary(biNon.mono.3.crit_hyp)$test$sigma

biComp.mono.3.crit_matrix <- matrix((biComp.same.3+biComp.diff.3-mono.same.3-mono.diff.3)/2,1)    # Define the specific contrasts
biComp.mono.3.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=biComp.mono.3.crit_matrix)
biComp.mono.3.crit_intercept <- summary(biComp.mono.3.crit_hyp)$test$coefficients
biComp.mono.3.crit_se <- summary(biComp.mono.3.crit_hyp)$test$sigma

biComp.biNon.3.crit_matrix <- matrix((biComp.same.3+biComp.diff.3-biNon.same.3-biNon.diff.3)/2,1)    # Define the specific contrasts
biComp.biNon.3.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=biComp.biNon.3.crit_matrix)
biComp.biNon.3.crit_intercept <- summary(biComp.biNon.3.crit_hyp)$test$coefficients
biComp.biNon.3.crit_se <- summary(biComp.biNon.3.crit_hyp)$test$sigma

# main effects controls
biNon.mono.3.control_matrix <- matrix((biNon.same.3+biNon.diff.3-mono.same.3-mono.diff.3)/2,1)    # Define the specific contrasts
biNon.mono.3.control_hyp <- glht(glmer_expC_control_treatment,linfct=biNon.mono.3.control_matrix)
biNon.mono.3.control_intercept <- summary(biNon.mono.3.control_hyp)$test$coefficients
biNon.mono.3.control_se <- summary(biNon.mono.3.control_hyp)$test$sigma

biComp.mono.3.control_matrix <- matrix((biComp.same.3+biComp.diff.3-mono.same.3-mono.diff.3)/2,1)    # Define the specific contrasts
biComp.mono.3.control_hyp <- glht(glmer_expC_control_treatment,linfct=biComp.mono.3.control_matrix)
biComp.mono.3.control_intercept <- summary(biComp.mono.3.control_hyp)$test$coefficients
biComp.mono.3.control_se <- summary(biComp.mono.3.control_hyp)$test$sigma

biComp.biNon.3.control_matrix <- matrix((biComp.same.3+biComp.diff.3-biNon.same.3-biNon.diff.3)/2,1)    # Define the specific contrasts
biComp.biNon.3.control_hyp <- glht(glmer_expC_control_treatment,linfct=biComp.biNon.3.control_matrix)
biComp.biNon.3.control_intercept <- summary(biComp.biNon.3.control_hyp)$test$coefficients
biComp.biNon.3.control_se <- summary(biComp.biNon.3.control_hyp)$test$sigma


############# interactions ##############

# timepoint 1
# interaction critical
interaction.biNon.mono.1.crit_matrix <- matrix((biNon.diff.1-biNon.same.1)-(mono.diff.1-mono.same.1),1)    # Define the specific contrasts
interaction.biNon.mono.1.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=interaction.biNon.mono.1.crit_matrix)
interaction.biNon.mono.1.crit_intercept <- summary(interaction.biNon.mono.1.crit_hyp)$test$coefficients
interaction.biNon.mono.1.crit_se <- summary(interaction.biNon.mono.1.crit_hyp)$test$sigma

interaction.biComp.mono.1.crit_matrix <- matrix((biComp.diff.1-biComp.same.1)-(mono.diff.1-mono.same.1),1)    # Define the specific contrasts
interaction.biComp.mono.1.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=interaction.biComp.mono.1.crit_matrix)
interaction.biComp.mono.1.crit_intercept <- summary(interaction.biComp.mono.1.crit_hyp)$test$coefficients
interaction.biComp.mono.1.crit_se <- summary(interaction.biComp.mono.1.crit_hyp)$test$sigma

interaction.biComp.biNon.1.crit_matrix <- matrix((biComp.diff.1-biComp.same.1)-(biNon.diff.1-biNon.same.1),1)    # Define the specific contrasts
interaction.biComp.biNon.1.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=interaction.biComp.biNon.1.crit_matrix)
interaction.biComp.biNon.1.crit_intercept <- summary(interaction.biComp.biNon.1.crit_hyp)$test$coefficients
interaction.biComp.biNon.1.crit_se <- summary(interaction.biComp.biNon.1.crit_hyp)$test$sigma

# interaction control
interaction.biNon.mono.1.control_matrix <- matrix((biNon.diff.1-biNon.same.1)-(mono.diff.1-mono.same.1),1)    # Define the specific contrasts
interaction.biNon.mono.1.control_hyp <- glht(glmer_expC_control_treatment,linfct=interaction.biNon.mono.1.control_matrix)
interaction.biNon.mono.1.control_intercept <- summary(interaction.biNon.mono.1.control_hyp)$test$coefficients
interaction.biNon.mono.1.control_se <- summary(interaction.biNon.mono.1.control_hyp)$test$sigma

interaction.biComp.mono.1.control_matrix <- matrix((biComp.diff.1-biComp.same.1)-(mono.diff.1-mono.same.1),1)    # Define the specific contrasts
interaction.biComp.mono.1.control_hyp <- glht(glmer_expC_control_treatment,linfct=interaction.biComp.mono.1.control_matrix)
interaction.biComp.mono.1.control_intercept <- summary(interaction.biComp.mono.1.control_hyp)$test$coefficients
interaction.biComp.mono.1.control_se <- summary(interaction.biComp.mono.1.control_hyp)$test$sigma

interaction.biComp.biNon.1.control_matrix <- matrix((biComp.diff.1-biComp.same.1)-(biNon.diff.1-biNon.same.1),1)    # Define the specific contrasts
interaction.biComp.biNon.1.control_hyp <- glht(glmer_expC_control_treatment,linfct=interaction.biComp.biNon.1.control_matrix)
interaction.biComp.biNon.1.control_intercept <- summary(interaction.biComp.biNon.1.control_hyp)$test$coefficients
interaction.biComp.biNon.1.control_se <- summary(interaction.biComp.biNon.1.control_hyp)$test$sigma


# timepoint 2
# interaction critical
interaction.biNon.mono.2.crit_matrix <- matrix((biNon.diff.2-biNon.same.2)-(mono.diff.2-mono.same.2),1)    # Define the specific contrasts
interaction.biNon.mono.2.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=interaction.biNon.mono.2.crit_matrix)
interaction.biNon.mono.2.crit_intercept <- summary(interaction.biNon.mono.2.crit_hyp)$test$coefficients
interaction.biNon.mono.2.crit_se <- summary(interaction.biNon.mono.2.crit_hyp)$test$sigma

interaction.biComp.mono.2.crit_matrix <- matrix((biComp.diff.2-biComp.same.2)-(mono.diff.2-mono.same.2),1)    # Define the specific contrasts
interaction.biComp.mono.2.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=interaction.biComp.mono.2.crit_matrix)
interaction.biComp.mono.2.crit_intercept <- summary(interaction.biComp.mono.2.crit_hyp)$test$coefficients
interaction.biComp.mono.2.crit_se <- summary(interaction.biComp.mono.2.crit_hyp)$test$sigma

interaction.biComp.biNon.2.crit_matrix <- matrix((biComp.diff.2-biComp.same.2)-(biNon.diff.2-biNon.same.2),1)    # Define the specific contrasts
interaction.biComp.biNon.2.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=interaction.biComp.biNon.2.crit_matrix)
interaction.biComp.biNon.2.crit_intercept <- summary(interaction.biComp.biNon.2.crit_hyp)$test$coefficients
interaction.biComp.biNon.2.crit_se <- summary(interaction.biComp.biNon.2.crit_hyp)$test$sigma

# interaction control
interaction.biNon.mono.2.control_matrix <- matrix((biNon.diff.2-biNon.same.2)-(mono.diff.2-mono.same.2),1)    # Define the specific contrasts
interaction.biNon.mono.2.control_hyp <- glht(glmer_expC_control_treatment,linfct=interaction.biNon.mono.2.control_matrix)
interaction.biNon.mono.2.control_intercept <- summary(interaction.biNon.mono.2.control_hyp)$test$coefficients
interaction.biNon.mono.2.control_se <- summary(interaction.biNon.mono.2.control_hyp)$test$sigma

interaction.biComp.mono.2.control_matrix <- matrix((biComp.diff.2-biComp.same.2)-(mono.diff.2-mono.same.2),1)    # Define the specific contrasts
interaction.biComp.mono.2.control_hyp <- glht(glmer_expC_control_treatment,linfct=interaction.biComp.mono.2.control_matrix)
interaction.biComp.mono.2.control_intercept <- summary(interaction.biComp.mono.2.control_hyp)$test$coefficients
interaction.biComp.mono.2.control_se <- summary(interaction.biComp.mono.2.control_hyp)$test$sigma

interaction.biComp.biNon.2.control_matrix <- matrix((biComp.diff.2-biComp.same.2)-(biNon.diff.2-biNon.same.2),1)    # Define the specific contrasts
interaction.biComp.biNon.2.control_hyp <- glht(glmer_expC_control_treatment,linfct=interaction.biComp.biNon.2.control_matrix)
interaction.biComp.biNon.2.control_intercept <- summary(interaction.biComp.biNon.2.control_hyp)$test$coefficients
interaction.biComp.biNon.2.control_se <- summary(interaction.biComp.biNon.2.control_hyp)$test$sigma

# timepoint 3
# interaction critical
interaction.biNon.mono.3.crit_matrix <- matrix((biNon.diff.3-biNon.same.3)-(mono.diff.3-mono.same.3),1)    # Define the specific contrasts
interaction.biNon.mono.3.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=interaction.biNon.mono.3.crit_matrix)
interaction.biNon.mono.3.crit_intercept <- summary(interaction.biNon.mono.3.crit_hyp)$test$coefficients
interaction.biNon.mono.3.crit_se <- summary(interaction.biNon.mono.3.crit_hyp)$test$sigma

interaction.biComp.mono.3.crit_matrix <- matrix((biComp.diff.3-biComp.same.3)-(mono.diff.3-mono.same.3),1)    # Define the specific contrasts
interaction.biComp.mono.3.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=interaction.biComp.mono.3.crit_matrix)
interaction.biComp.mono.3.crit_intercept <- summary(interaction.biComp.mono.3.crit_hyp)$test$coefficients
interaction.biComp.mono.3.crit_se <- summary(interaction.biComp.mono.3.crit_hyp)$test$sigma

interaction.biComp.biNon.3.crit_matrix <- matrix((biComp.diff.3-biComp.same.3)-(biNon.diff.3-biNon.same.3),1)    # Define the specific contrasts
interaction.biComp.biNon.3.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=interaction.biComp.biNon.3.crit_matrix)
interaction.biComp.biNon.3.crit_intercept <- summary(interaction.biComp.biNon.3.crit_hyp)$test$coefficients
interaction.biComp.biNon.3.crit_se <- summary(interaction.biComp.biNon.3.crit_hyp)$test$sigma

# interaction control
interaction.biNon.mono.3.control_matrix <- matrix((biNon.diff.3-biNon.same.3)-(mono.diff.3-mono.same.3),1)    # Define the specific contrasts
interaction.biNon.mono.3.control_hyp <- glht(glmer_expC_control_treatment,linfct=interaction.biNon.mono.3.control_matrix)
interaction.biNon.mono.3.control_intercept <- summary(interaction.biNon.mono.3.control_hyp)$test$coefficients
interaction.biNon.mono.3.control_se <- summary(interaction.biNon.mono.3.control_hyp)$test$sigma

interaction.biComp.mono.3.control_matrix <- matrix((biComp.diff.3-biComp.same.3)-(mono.diff.3-mono.same.3),1)    # Define the specific contrasts
interaction.biComp.mono.3.control_hyp <- glht(glmer_expC_control_treatment,linfct=interaction.biComp.mono.3.control_matrix)
interaction.biComp.mono.3.control_intercept <- summary(interaction.biComp.mono.3.control_hyp)$test$coefficients
interaction.biComp.mono.3.control_se <- summary(interaction.biComp.mono.3.control_hyp)$test$sigma

interaction.biComp.biNon.3.control_matrix <- matrix((biComp.diff.3-biComp.same.3)-(biNon.diff.3-biNon.same.3),1)    # Define the specific contrasts
interaction.biComp.biNon.3.control_hyp <- glht(glmer_expC_control_treatment,linfct=interaction.biComp.biNon.3.control_matrix)
interaction.biComp.biNon.3.control_intercept <- summary(interaction.biComp.biNon.3.control_hyp)$test$coefficients
interaction.biComp.biNon.3.control_se <- summary(interaction.biComp.biNon.3.control_hyp)$test$sigma


################### means, se ##################

####### critical

sensitivity.biNon.1.crit_matrix <- matrix(biNon.diff.1-biNon.same.1,1)    # Define the specific contrasts
sensitivity.biNon.1.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=sensitivity.biNon.1.crit_matrix)
sensitivity.biNon.1.crit_intercept <- summary(sensitivity.biNon.1.crit_hyp)$test$coefficients
sensitivity.biNon.1.crit_se <- summary(sensitivity.biNon.1.crit_hyp)$test$sigma

sensitivity.mono.1.crit_matrix <- matrix(mono.diff.1-mono.diff.1,1)    # Define the specific contrasts
sensitivity.mono.1.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=sensitivity.mono.1.crit_matrix)
sensitivity.mono.1.crit_intercept <- summary(sensitivity.mono.1.crit_hyp)$test$coefficients
sensitivity.mono.1.crit_se <- summary(sensitivity.mono.1.crit_hyp)$test$sigma

sensitivity.biComp.1.crit_matrix <- matrix(biComp.diff.1-biComp.same.1,1)    # Define the specific contrasts
sensitivity.biComp.1.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=sensitivity.biComp.1.crit_matrix)
sensitivity.biComp.1.crit_intercept <- summary(sensitivity.biComp.1.crit_hyp)$test$coefficients
sensitivity.biComp.1.crit_se <- summary(sensitivity.biComp.1.crit_hyp)$test$sigma

sensitivity.biNon.2.crit_matrix <- matrix(biNon.diff.2-biNon.same.2,1)    # Define the specific contrasts
sensitivity.biNon.2.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=sensitivity.biNon.2.crit_matrix)
sensitivity.biNon.2.crit_intercept <- summary(sensitivity.biNon.2.crit_hyp)$test$coefficients
sensitivity.biNon.2.crit_se <- summary(sensitivity.biNon.2.crit_hyp)$test$sigma

sensitivity.mono.2.crit_matrix <- matrix(mono.diff.2-mono.diff.2,1)    # Define the specific contrasts
sensitivity.mono.2.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=sensitivity.mono.2.crit_matrix)
sensitivity.mono.2.crit_intercept <- summary(sensitivity.mono.2.crit_hyp)$test$coefficients
sensitivity.mono.2.crit_se <- summary(sensitivity.mono.2.crit_hyp)$test$sigma

sensitivity.biComp.2.crit_matrix <- matrix(biComp.diff.2-biComp.same.2,1)    # Define the specific contrasts
sensitivity.biComp.2.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=sensitivity.biComp.2.crit_matrix)
sensitivity.biComp.2.crit_intercept <- summary(sensitivity.biComp.2.crit_hyp)$test$coefficients
sensitivity.biComp.2.crit_se <- summary(sensitivity.biComp.2.crit_hyp)$test$sigma

sensitivity.biNon.3.crit_matrix <- matrix(biNon.diff.3-biNon.same.3,1)    # Define the specific contrasts
sensitivity.biNon.3.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=sensitivity.biNon.3.crit_matrix)
sensitivity.biNon.3.crit_intercept <- summary(sensitivity.biNon.3.crit_hyp)$test$coefficients
sensitivity.biNon.3.crit_se <- summary(sensitivity.biNon.3.crit_hyp)$test$sigma

sensitivity.mono.3.crit_matrix <- matrix(mono.diff.3-mono.diff.3,1)    # Define the specific contrasts
sensitivity.mono.3.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=sensitivity.mono.3.crit_matrix)
sensitivity.mono.3.crit_intercept <- summary(sensitivity.mono.3.crit_hyp)$test$coefficients
sensitivity.mono.3.crit_se <- summary(sensitivity.mono.3.crit_hyp)$test$sigma

sensitivity.biComp.3.crit_matrix <- matrix(biComp.diff.3-biComp.same.3,1)    # Define the specific contrasts
sensitivity.biComp.3.crit_hyp <- glht(glmer_expC_crit_treatment,linfct=sensitivity.biComp.3.crit_matrix)
sensitivity.biComp.3.crit_intercept <- summary(sensitivity.biComp.3.crit_hyp)$test$coefficients
sensitivity.biComp.3.crit_se <- summary(sensitivity.biComp.3.crit_hyp)$test$sigma


######### control
sensitivity.biNon.1.control_matrix <- matrix(biNon.diff.1-biNon.same.1,1)    # Define the specific contrasts
sensitivity.biNon.1.control_hyp <- glht(glmer_expC_control_treatment,linfct=sensitivity.biNon.1.control_matrix)
sensitivity.biNon.1.control_intercept <- summary(sensitivity.biNon.1.control_hyp)$test$coefficients
sensitivity.biNon.1.control_se <- summary(sensitivity.biNon.1.control_hyp)$test$sigma

sensitivity.mono.1.control_matrix <- matrix(mono.diff.1-mono.diff.1,1)    # Define the specific contrasts
sensitivity.mono.1.control_hyp <- glht(glmer_expC_control_treatment,linfct=sensitivity.mono.1.control_matrix)
sensitivity.mono.1.control_intercept <- summary(sensitivity.mono.1.control_hyp)$test$coefficients
sensitivity.mono.1.control_se <- summary(sensitivity.mono.1.control_hyp)$test$sigma

sensitivity.biComp.1.control_matrix <- matrix(biComp.diff.1-biComp.same.1,1)    # Define the specific contrasts
sensitivity.biComp.1.control_hyp <- glht(glmer_expC_control_treatment,linfct=sensitivity.biComp.1.control_matrix)
sensitivity.biComp.1.control_intercept <- summary(sensitivity.biComp.1.control_hyp)$test$coefficients
sensitivity.biComp.1.control_se <- summary(sensitivity.biComp.1.control_hyp)$test$sigma

sensitivity.biNon.2.control_matrix <- matrix(biNon.diff.2-biNon.same.2,1)    # Define the specific contrasts
sensitivity.biNon.2.control_hyp <- glht(glmer_expC_control_treatment,linfct=sensitivity.biNon.2.control_matrix)
sensitivity.biNon.2.control_intercept <- summary(sensitivity.biNon.2.control_hyp)$test$coefficients
sensitivity.biNon.2.control_se <- summary(sensitivity.biNon.2.control_hyp)$test$sigma

sensitivity.mono.2.control_matrix <- matrix(mono.diff.2-mono.diff.2,1)    # Define the specific contrasts
sensitivity.mono.2.control_hyp <- glht(glmer_expC_control_treatment,linfct=sensitivity.mono.2.control_matrix)
sensitivity.mono.2.control_intercept <- summary(sensitivity.mono.2.control_hyp)$test$coefficients
sensitivity.mono.2.control_se <- summary(sensitivity.mono.2.control_hyp)$test$sigma

sensitivity.biComp.2.control_matrix <- matrix(biComp.diff.2-biComp.same.2,1)    # Define the specific contrasts
sensitivity.biComp.2.control_hyp <- glht(glmer_expC_control_treatment,linfct=sensitivity.biComp.2.control_matrix)
sensitivity.biComp.2.control_intercept <- summary(sensitivity.biComp.2.control_hyp)$test$coefficients
sensitivity.biComp.2.control_se <- summary(sensitivity.biComp.2.control_hyp)$test$sigma

sensitivity.biNon.3.control_matrix <- matrix(biNon.diff.3-biNon.same.3,1)    # Define the specific contrasts
sensitivity.biNon.3.control_hyp <- glht(glmer_expC_control_treatment,linfct=sensitivity.biNon.3.control_matrix)
sensitivity.biNon.3.control_intercept <- summary(sensitivity.biNon.3.control_hyp)$test$coefficients
sensitivity.biNon.3.control_se <- summary(sensitivity.biNon.3.control_hyp)$test$sigma

sensitivity.mono.3.control_matrix <- matrix(mono.diff.3-mono.diff.3,1)    # Define the specific contrasts
sensitivity.mono.3.control_hyp <- glht(glmer_expC_control_treatment,linfct=sensitivity.mono.3.control_matrix)
sensitivity.mono.3.control_intercept <- summary(sensitivity.mono.3.control_hyp)$test$coefficients
sensitivity.mono.3.control_se <- summary(sensitivity.mono.3.control_hyp)$test$sigma

sensitivity.biComp.3.control_matrix <- matrix(biComp.diff.3-biComp.same.3,1)    # Define the specific contrasts
sensitivity.biComp.3.control_hyp <- glht(glmer_expC_control_treatment,linfct=sensitivity.biComp.3.control_matrix)
sensitivity.biComp.3.control_intercept <- summary(sensitivity.biComp.3.control_hyp)$test$coefficients
sensitivity.biComp.3.control_se <- summary(sensitivity.biComp.3.control_hyp)$test$sigma

# crit
summary(biNon.mono.1.crit_hyp)
summary(biComp.mono.1.crit_hyp)
summary(biComp.biNon.1.crit_hyp)

summary(biNon.mono.2.crit_hyp)
summary(biComp.mono.2.crit_hyp)
summary(biComp.biNon.2.crit_hyp)

summary(biNon.mono.3.crit_hyp)
summary(biComp.mono.3.crit_hyp)
summary(biComp.biNon.3.crit_hyp)

summary(interaction.biNon.mono.1.crit_hyp)
summary(interaction.biComp.mono.1.crit_hyp)
summary(interaction.biComp.biNon.1.crit_hyp)

summary(interaction.biNon.mono.2.crit_hyp)
summary(interaction.biComp.mono.2.crit_hyp)
summary(interaction.biComp.biNon.2.crit_hyp)

summary(interaction.biNon.mono.3.crit_hyp)
summary(interaction.biComp.mono.3.crit_hyp)
summary(interaction.biComp.biNon.3.crit_hyp)

#controls
summary(biNon.mono.1.control_hyp)
summary(biComp.mono.1.control_hyp)
summary(biComp.biNon.1.control_hyp)

summary(biNon.mono.2.control_hyp)
summary(biComp.mono.2.control_hyp)
summary(biComp.biNon.2.control_hyp)

summary(biNon.mono.3.control_hyp)
summary(biComp.mono.3.control_hyp)
summary(biComp.biNon.3.control_hyp)

summary(interaction.biNon.mono.1.control_hyp)
summary(interaction.biComp.mono.1.control_hyp)
summary(interaction.biComp.biNon.1.control_hyp)

summary(interaction.biNon.mono.2.control_hyp)
summary(interaction.biComp.mono.2.control_hyp)
summary(interaction.biComp.biNon.2.control_hyp)

summary(interaction.biNon.mono.3.control_hyp)
summary(interaction.biComp.mono.3.control_hyp)
summary(interaction.biComp.biNon.3.control_hyp)