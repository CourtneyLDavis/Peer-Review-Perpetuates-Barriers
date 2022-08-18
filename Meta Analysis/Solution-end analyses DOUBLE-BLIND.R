#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Analysis of double-blind vs. single-blind data
# Olivia Smith, with data formatting code by Wendy Leuenberger
# Date first created: April 2022
# Date last checked: 11-14 August 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

############ Set-up
############
### Load packages
library(here)
library(scales)
library(plotrix)
library(glmmTMB)
library(boot)
library(performance)
library(dplyr)
library(DHARMa)
library(bbmle)
library(emmeans)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(RColorBrewer)
library(magrittr)
library(tidyverse)

##set up sjplot to look better later on
theme_set(theme_sjplot())

set_theme( # make sjplot look better
  geom.outline.color = "antiquewhite4", 
  geom.outline.size = 1.5, 
  geom.label.size = 3,
  geom.label.color = "black",
  title.color = "black", 
  title.size = 1.5, 
  axis.angle.x = 0, 
  axis.textcolor = "black", 
  base = theme_classic()
)

require(multcomp)

#set up multcomp to work with glmmTMB pacakge
glht_glmmTMB <- function (model, ..., component="cond") { 
  glht(model, ..., 
       coef. = function(x) fixef(x)[[component]], 
       vcov. = function(x) vcov(x)[[component]],
       df = NULL)
} 
modelparm.glmmTMB <- function (model, coef. = function(x) fixef(x)[[component]], 
                               vcov. = function(x) vcov(x)[[component]], 
                               df = NULL, component="cond", ...) { 
  multcomp:::modelparm.default(model, coef. = coef., vcov. = vcov., 
                               df = df, ...) 
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Changing unit of replication from study to paper within study
# Collaborative Research Group (CRG)
# Wendy Leuenberger
# Date created: 05/06/2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###############
###############

####################Initial decision##################

# Load data
initialdatog <- read.csv(here("Meta Analysis", "Solution-end data", "Double-blind",  "Initial decision solution double blind.csv"))

# For each line of data, duplicate based on Rejected and 
# Went.through.to.next.stage

# Function! ####
StudyToPaper <- function(Data, 
                         ColumnNames = c('Rejected', 
                                         'Went.through.to.next.stage'), 
                         NamesTo = 'Outcome', 
                         ValuesTo = 'NumPapers',
                         Binary0 = 'Rejected'){
  # pivot so that rejected and next stage are in different rows
  LongData <- Data %>% 
    pivot_longer(# Columns for rejected and next stage
      cols = ColumnNames, 
      # Name the column with the previous column names
      names_to = NamesTo,  
      # Name the column with the number of papers 
      values_to = ValuesTo)
  
  # Make a column with a 0/1 response for the logistic regression
  # Binary0 is the value in NamesTo that becomes a 0
  LongData %<>% 
    mutate(OutcomeBinary = ifelse(LongData[[NamesTo]] == Binary0, 0, 1))
  
  # Replicate rows of data base on the ValuesTo column (NumPapers)
  SuperLongData <- LongData %>% 
    lapply(rep, LongData[[ValuesTo]]) %>% 
    data.frame
  
  # Make an error if the final number of rows is not the number of papers
  # Wouldn't work if >2 columns, shouldn't be a problem
  if(dim(SuperLongData)[1] != sum(LongData[[ValuesTo]])){
    stop(error = 'Number of rows does not match sum of papers')
  }
  
  return(SuperLongData)
}

# Make the conversion
initialdat <- initialdatog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

# QC ####
Original <- initialdatog
New <- initialdat

dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])

# Load data
postinitialreviewdatog <- read.csv(here("Meta Analysis", "Solution-end data", "Double-blind", "Post initial review decision solution double blind.csv"))

# For each line of data, duplicate based on Rejected and 
# Went.through.to.next.stage

# Function! ####
StudyToPaper <- function(Data, 
                         ColumnNames = c('Rejected', 
                                         'Went.through.to.next.stage'), 
                         NamesTo = 'Outcome', 
                         ValuesTo = 'NumPapers',
                         Binary0 = 'Rejected'){
  # pivot so that rejected and next stage are in different rows
  LongData <- Data %>% 
    pivot_longer(# Columns for rejected and next stage
      cols = ColumnNames, 
      # Name the column with the previous column names
      names_to = NamesTo,  
      # Name the column with the number of papers 
      values_to = ValuesTo)
  
  # Make a column with a 0/1 response for the logistic regression
  # Binary0 is the value in NamesTo that becomes a 0
  LongData %<>% 
    mutate(OutcomeBinary = ifelse(LongData[[NamesTo]] == Binary0, 0, 1))
  
  # Replicate rows of data base on the ValuesTo column (NumPapers)
  SuperLongData <- LongData %>% 
    lapply(rep, LongData[[ValuesTo]]) %>% 
    data.frame
  
  # Make an error if the final number of rows is not the number of papers
  # Wouldn't work if >2 columns, shouldn't be a problem
  if(dim(SuperLongData)[1] != sum(LongData[[ValuesTo]])){
    stop(error = 'Number of rows does not match sum of papers')
  }
  
  return(SuperLongData)
}

#########################Post-initial review###################

# Make the conversion
postinitialreviewdat <- postinitialreviewdatog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

# QC ####
Original <- postinitialreviewdatog
New <- postinitialreviewdat

dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])

####################Final decision###################

# Load data

finaldatog <- read.csv(here("Meta Analysis", "Solution-end data", "Double-blind", "Final decision solution double blind.csv"))

# For each line of data, duplicate based on Rejected and 
# Went.through.to.next.stage

# Function! ####
StudyToPaper <- function(Data, 
                         ColumnNames = c('Rejected', 
                                         'Went.through.to.next.stage'), 
                         NamesTo = 'Outcome', 
                         ValuesTo = 'NumPapers',
                         Binary0 = 'Rejected'){
  # pivot so that rejected and next stage are in different rows
  LongData <- Data %>% 
    pivot_longer(# Columns for rejected and next stage
      cols = ColumnNames, 
      # Name the column with the previous column names
      names_to = NamesTo,  
      # Name the column with the number of papers 
      values_to = ValuesTo)
  
  # Make a column with a 0/1 response for the logistic regression
  # Binary0 is the value in NamesTo that becomes a 0
  LongData %<>% 
    mutate(OutcomeBinary = ifelse(LongData[[NamesTo]] == Binary0, 0, 1))
  
  # Replicate rows of data base on the ValuesTo column (NumPapers)
  SuperLongData <- LongData %>% 
    lapply(rep, LongData[[ValuesTo]]) %>% 
    data.frame
  
  # Make an error if the final number of rows is not the number of papers
  # Wouldn't work if >2 columns, shouldn't be a problem
  if(dim(SuperLongData)[1] != sum(LongData[[ValuesTo]])){
    stop(error = 'Number of rows does not match sum of papers')
  }
  
  return(SuperLongData)
}

# Make the conversion
finaldat <- finaldatog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

# QC ####
Original <- finaldatog
New <- finaldat

dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])

#################Overall decision###################
# Load data
overalldatog <- read.csv(here("Meta Analysis", "Solution-end data", "Double-blind", "Overall decision solution double blind.csv"))

# For each line of data, duplicate based on Rejected and 
# Went.through.to.next.stage

# Function! ####
StudyToPaper <- function(Data, 
                         ColumnNames = c('Rejected', 
                                         'Went.through.to.next.stage'), 
                         NamesTo = 'Outcome', 
                         ValuesTo = 'NumPapers',
                         Binary0 = 'Rejected'){
  # pivot so that rejected and next stage are in different rows
  LongData <- Data %>% 
    pivot_longer(# Columns for rejected and next stage
      cols = ColumnNames, 
      # Name the column with the previous column names
      names_to = NamesTo,  
      # Name the column with the number of papers 
      values_to = ValuesTo)
  
  # Make a column with a 0/1 response for the logistic regression
  # Binary0 is the value in NamesTo that becomes a 0
  LongData %<>% 
    mutate(OutcomeBinary = ifelse(LongData[[NamesTo]] == Binary0, 0, 1))
  
  # Replicate rows of data base on the ValuesTo column (NumPapers)
  SuperLongData <- LongData %>% 
    lapply(rep, LongData[[ValuesTo]]) %>% 
    data.frame
  
  # Make an error if the final number of rows is not the number of papers
  # Wouldn't work if >2 columns, shouldn't be a problem
  if(dim(SuperLongData)[1] != sum(LongData[[ValuesTo]])){
    stop(error = 'Number of rows does not match sum of papers')
  }
  
  return(SuperLongData)
}

# Make the conversion
overalldat <- overalldatog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

# QC ####
Original <- overalldatog
New <- overalldat

dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])


#####################################################
#####################################################
##Analyses
#####################################################
##INITIAL DECISION
#####################################################

initialdat$Demographic.category<- as.factor(initialdat$Demographic.category)
initialdat$Category<- as.factor(initialdat$Category)
initialdat$Subcategory<- as.factor(initialdat$Subcategory)
initialdat$Study<- as.factor(initialdat$Study)

####################Subset initial data by demographic and position##################

###################################
###################################
### Gender for corresponding author

initialdat.gender.corresponding<- subset(initialdat, Demographic=="Gender" & Author.position == "Corresponding" )
initialdat.gender.corresponding$mean_year<-(initialdat.gender.corresponding$Year - 1988.75)

##this dataset only has 1 study so don't need JIF/year (since they're the same within study)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Blinding.format
                , data = initialdat.gender.corresponding, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and double-blind
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Blinding.format
                , data = initialdat.gender.corresponding, family = binomial)
##Likelihood Ratio Test
anova(mod1, mod2) ##review model*gender important: 8.12, 0.0044

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Institutional prestige for corresponding author
initialdat.Prestige_institution.corresponding<- subset(initialdat, Demographic=="Prestige_institution" & Author.position == "Corresponding" )
initialdat.Prestige_institution.corresponding$mean_year<-(initialdat.Prestige_institution.corresponding$Year - 1988.75)

##this dataset only has 1 study so don't need JIF/year (since they're the same within study)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Blinding.format
                , data = initialdat.Prestige_institution.corresponding, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and double-blind
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Blinding.format #just one study
                , data = initialdat.Prestige_institution.corresponding, family = binomial)
##Likelihood Ratio Test
anova(mod1, mod2) ##6.26, 0.044: review model*prestige does matter

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
#####################################################
#####################################################
########################################################
##POST-INITIAL REVIEW DECISION
#####################################################

postinitialreviewdat$Demographic.category<- as.factor(postinitialreviewdat$Demographic.category)
postinitialreviewdat$Category<- as.factor(postinitialreviewdat$Category)
postinitialreviewdat$Subcategory<- as.factor(postinitialreviewdat$Subcategory)
postinitialreviewdat$Study<- as.factor(postinitialreviewdat$Study)

####################Subset post initial review data by demographic and position##################

###################################
###################################
### Gender for first author
postinitialreviewdat.gender.first<- subset(postinitialreviewdat, Demographic=="Gender" & Author.position == "First" )
postinitialreviewdat.gender.first$mean_year<-(postinitialreviewdat.gender.first$Year - 1988.75)

##this dataset only has 2 studies but one is Squazzoni et al. that have >100,000 manuscripts and 79 journals
##JIF provided at 1 unit intervals. Include JIF and study as fixed effects 
##run model with interaction term with blinding format
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Blinding.format #two studies but Squazzoni reps
                + Journal.impact.factor + Study, data = postinitialreviewdat.gender.first, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and double-blind
##remove interaction for comparison model
##run model with no interaction term to check for multicollinearity issue
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Blinding.format #two studies but Squazzoni reps
                + Journal.impact.factor + Study, data = postinitialreviewdat.gender.first, family = binomial)
#Check for multicollinearity issues
check_collinearity(mod2) ##fine - highest VIF = 1.19

##Likelihood Ratio Test
anova(mod1, mod2) ##review model*gender important: 49.42, < 0.0001

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Gender for corresponding author

postinitialreviewdat.gender.corresponding<- subset(postinitialreviewdat, Demographic=="Gender" & Author.position == "Corresponding" )
postinitialreviewdat.gender.corresponding$mean_year<-(postinitialreviewdat.gender.corresponding$Year - 1988.75)

##this dataset only has 1 study so don't need JIF/year (since they're the same within study)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Blinding.format #just one study
                , data = postinitialreviewdat.gender.corresponding, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and double-blind
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Blinding.format #just one study
                , data = postinitialreviewdat.gender.corresponding, family = binomial)
##Likelihood Ratio Test
anova(mod1, mod2) ##review model*gender not important: 0.070, 0.79

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Gender for last author

postinitialreviewdat.gender.last<- subset(postinitialreviewdat, Demographic=="Gender" & Author.position == "Last" )
postinitialreviewdat.gender.last$mean_year<-(postinitialreviewdat.gender.last$Year - 1988.75)

##this dataset only has 2 studies but one is Squazzoni et al. that have >100,000 manuscripts and 79 journals
##JIF provided at 1 unit intervals. Include JIF and study as fixed effects 
##run model with interaction term with blinding format
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Blinding.format #two studies but Squazzoni reps
                + Journal.impact.factor + Study, data = postinitialreviewdat.gender.last, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and double-blind
##remove interaction for comparison model
##run model with no interaction term to check for multicollinearity issue
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Blinding.format #two studies but Squazzoni reps
                + Journal.impact.factor + Study, data = postinitialreviewdat.gender.last, family = binomial)
#Check for multicollinearity issues
check_collinearity(mod2) ##fine - highest VIF = 1.20
##Likelihood Ratio Test
anova(mod1, mod2) ##review model*gender important: 54.32, < 0.0001

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Author prestige for first author

postinitialreviewdat.Prestige_author.first<- subset(postinitialreviewdat, Demographic=="Prestige_author" & Author.position == "First" )
postinitialreviewdat.Prestige_author.first$mean_year<-(postinitialreviewdat.Prestige_author.first$Year - 1988.75)

##this dataset only has 1 study so don't need JIF/year (since they're the same within study)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Blinding.format #just one study
                , data = postinitialreviewdat.Prestige_author.first, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and double-blind
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Blinding.format #just one study
                , data = postinitialreviewdat.Prestige_author.first, family = binomial)
##Likelihood Ratio Test
anova(mod1, mod2) ##prestige * blinding format not significant: 0.0013, 0.97

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Author prestige for corresponding author

postinitialreviewdat.Prestige_author.corresponding<- subset(postinitialreviewdat, Demographic=="Prestige_author" & Author.position == "Corresponding" )
postinitialreviewdat.Prestige_author.corresponding$mean_year<-(postinitialreviewdat.Prestige_author.corresponding$Year - 1988.75)

##this dataset only has 1 study so don't need JIF/year (since they're the same within study)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Blinding.format #just one study
                , data = postinitialreviewdat.Prestige_author.corresponding, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and double-blind
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Blinding.format #just one study
                , data = postinitialreviewdat.Prestige_author.corresponding, family = binomial)
##Likelihood Ratio Test
anova(mod1, mod2) ##prestige * blinding format borderline significant: 3.39, 0.066.

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Author prestige for last author

postinitialreviewdat.Prestige_author.last<- subset(postinitialreviewdat, Demographic=="Prestige_author" & Author.position == "Last" )
postinitialreviewdat.Prestige_author.last$mean_year<-(postinitialreviewdat.Prestige_author.last$Year - 1988.75)

##this dataset only has 1 study so don't need JIF/year (since they're the same within study)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Blinding.format #just one study
                , data = postinitialreviewdat.Prestige_author.last, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and double-blind
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Blinding.format #just one study
                , data = postinitialreviewdat.Prestige_author.last, family = binomial)
##Likelihood Ratio Test
anova(mod1, mod2) ####review model*prestige not important: 0.44, 0.51

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
#####################################################
#####################################################
### Load the final decision data 
finaldat$Demographic.category<- as.factor(finaldat$Demographic.category)
finaldat$Category<- as.factor(finaldat$Category)
finaldat$Subcategory<- as.factor(finaldat$Subcategory)
finaldat$Study<- as.factor(finaldat$Study)

###################################
###################################
### Gender for first author

finaldat.gender.first<- subset(finaldat, Demographic=="Gender" & Author.position == "First" )
finaldat.gender.first$mean_year<-(finaldat.gender.first$Year - 1988.75)

##this dataset only has 1 study but it is Squazzoni et al. that have >100,000 manuscripts and 79 journals
##JIF provided at 1 unit intervals. Include JIF as fixed effect
##run model with no interaction term to check for multicollinearity issue
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Blinding.format + Journal.impact.factor
               #just one study 
                ,data = finaldat.gender.first, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and double-blind
##remove interaction for comparison model
##run model with no interaction term to check for multicollinearity issue
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Blinding.format + Journal.impact.factor #just one study
                , data = finaldat.gender.first, family = binomial)
#Check for multicollinearity issues 
check_collinearity(mod2) #fine - max VIF = 1.19

##Likelihood Ratio Test
anova(mod1, mod2) ##review model*gender important: 29.11, < 0.0001

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Gender for last author

finaldat.gender.last<- subset(finaldat, Demographic=="Gender" & Author.position == "Last" )
finaldat.gender.last$mean_year<-(finaldat.gender.last$Year - 1988.75)

##this dataset only has 1 study but it is Squazzoni et al. that have >100,000 manuscripts and 79 journals
##JIF provided at 1 unit intervals. Include JIF as fixed effect
##run model with no interaction term to check for multicollinearity issue
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Blinding.format + Journal.impact.factor
                #just one study 
                ,data = finaldat.gender.last, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and double-blind
##remove interaction for comparison model
##run model with no interaction term to check for multicollinearity issue
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Blinding.format + Journal.impact.factor #just one study
                , data = finaldat.gender.last, family = binomial)
#Check for multicollinearity issues
check_collinearity(mod2) ##fine - highest VIF = 1.19

##Likelihood Ratio Test
anova(mod1, mod2) ##review model*gender important: 40.04, < 0.0001

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
#####################################################
#####################################################
### Load the overall decision data 

overalldat$Demographic.category<- as.factor(overalldat$Demographic.category)
overalldat$Category<- as.factor(overalldat$Category)
overalldat$Subcategory<- as.factor(overalldat$Subcategory)
overalldat$Study<- as.factor(overalldat$Study)

####################Subset overall data by demographic and position##################

###################################
###################################
### CORRESPONDING AUTHOR gender
overalldat.gender.corresponding<- subset(overalldat, Demographic=="Gender" & Author.position == "Corresponding" )
overalldat.gender.corresponding$mean_year<-(overalldat.gender.corresponding$Year - 1988.75)

##this dataset only has 1 study so don't need JIF/year (since they're the same within study)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Blinding.format #just one study
                , data = overalldat.gender.corresponding, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and double-blind
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Blinding.format #just one study
                , data = overalldat.gender.corresponding, family = binomial)
##Likelihood Ratio Test
anova(mod1, mod2) ##review model*gender marginally important: 3.52, 0.061.

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### CORRESPONDING AUTHOR continent

####Continent
overalldat.Continent.corresponding<- subset(overalldat, Demographic=="Continent" & Author.position == "Corresponding" )
overalldat.Continent.corresponding$mean_year<-(overalldat.Continent.corresponding$Year - 1988.75)

##this dataset only has 1 study so don't need JIF/year (since they're the same within study)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Blinding.format #just one study
                , data = overalldat.Continent.corresponding, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and double-blind
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Blinding.format #just one study
                , data = overalldat.Continent.corresponding, family = binomial)
##Likelihood Ratio Test
anova(mod1, mod2) ####review model*continent not important: 5.86, 0.32

##estimates for figures
##we removed the CI for Africa. There are no acceptances for double-blind for Africa, so it is causing
##the large (0,1) 95% CIs
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### CORRESPONDING AUTHOR English primary language (all countries)

overalldat.ESL_all.corresponding<- subset(overalldat, Demographic=="ESL_all" & Author.position == "Corresponding" )
overalldat.ESL_all.corresponding$mean_year<-(overalldat.ESL_all.corresponding$Year - 1988.75)

##this dataset only has 1 study so don't need JIF/year (since they're the same within study)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Blinding.format #just one study
                , data = overalldat.ESL_all.corresponding, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and double-blind
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Blinding.format #just one study
                , data = overalldat.ESL_all.corresponding, family = binomial)
##Likelihood Ratio Test
anova(mod1, mod2) ##review model*ESL_all not important: 1.45, 0.23

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### CORRESPONDING AUTHOR English primary language for countries with similar HDI

overalldat.ESL_similarHDI.corresponding<- subset(overalldat, Demographic=="ESL_similarHDI" & Author.position == "Corresponding" )
overalldat.ESL_similarHDI.corresponding$mean_year<-(overalldat.ESL_similarHDI.corresponding$Year - 1988.75)

##this dataset only has 1 study so don't need JIF/year (since they're the same within study)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Blinding.format #just one study
                , data = overalldat.ESL_similarHDI.corresponding, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and double-blind
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Blinding.format #just one study
                , data = overalldat.ESL_similarHDI.corresponding, family = binomial)
##Likelihood Ratio Test
anova(mod1, mod2) ##review model*ESL_similarHDI not important: 0.28, 0.59

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


####################################
####################################
####################################
#####OVERALL DECISIONS COUNTRY

### Load the overall decision data 
overalldatog <- read.csv(here("Meta Analysis", "Solution-end data", "Double-blind", "Overall decision solution double blind COUNTRY.csv"))
head(overalldatog)

# For each line of data, duplicate based on Rejected and 
# Went.through.to.next.stage

# Function! ####
StudyToPaper <- function(Data, 
                         ColumnNames = c('Rejected', 
                                         'Went.through.to.next.stage'), 
                         NamesTo = 'Outcome', 
                         ValuesTo = 'NumPapers',
                         Binary0 = 'Rejected'){
  # pivot so that rejected and next stage are in different rows
  LongData <- Data %>% 
    pivot_longer(# Columns for rejected and next stage
      cols = ColumnNames, 
      # Name the column with the previous column names
      names_to = NamesTo,  
      # Name the column with the number of papers 
      values_to = ValuesTo)
  
  # Make a column with a 0/1 response for the logistic regression
  # Binary0 is the value in NamesTo that becomes a 0
  LongData %<>% 
    mutate(OutcomeBinary = ifelse(LongData[[NamesTo]] == Binary0, 0, 1))
  
  # Replicate rows of data base on the ValuesTo column (NumPapers)
  SuperLongData <- LongData %>% 
    lapply(rep, LongData[[ValuesTo]]) %>% 
    data.frame
  
  # Make an error if the final number of rows is not the number of papers
  # Wouldn't work if >2 columns, shouldn't be a problem
  if(dim(SuperLongData)[1] != sum(LongData[[ValuesTo]])){
    stop(error = 'Number of rows does not match sum of papers')
  }
  
  return(SuperLongData)
}

# Make the conversion
overalldat <- overalldatog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

# QC ####
Original <- overalldatog
New <- overalldat

dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])

####################################

overalldat$Country<- as.factor(overalldat$Country)
overalldat$Study<- as.factor(overalldat$Study)
overalldat$Continent<- as.factor(overalldat$Continent)
overalldat$Language<- as.factor(overalldat$Language)
overalldat$Category<- as.factor(overalldat$Category)

####################################
########Corresponding author COUNTRY

overalldat.country.corresponding<- subset(overalldat, Author.position == "Corresponding" )
overalldat.country.corresponding$mean_year<-(overalldat.country.corresponding$Year - 1988.75)

##Just one study so don't need to have JIF/mean year since all levels are the same for all manuscripts
##Try model with all interactions between blinding format and continent/HDI/language
mod1 <- glmmTMB(OutcomeBinary ~ Continent * Blinding.format + HDI * Blinding.format + Language * Blinding.format, data = overalldat.country.corresponding, family = binomial)
summary(mod1) ##Model convergence problem; singular convergence (7). See vignette('troubleshooting')

##Start with fully additive model to check for multicollinearity issues
##Also used in Likelihood Ratio Tests below to test for interactions
mod1 <- glmmTMB(OutcomeBinary ~ Continent + Blinding.format + HDI + Language, data = overalldat.country.corresponding, family = binomial)
check_collinearity(mod1) #fine - highest VIF is 4.09

##try running sequentially with 2 interactions and see if those models converge
modcont <- glmmTMB(OutcomeBinary ~ Continent + Blinding.format + HDI * Blinding.format + Language * Blinding.format, data = overalldat.country.corresponding, family = binomial)
modhdi <- glmmTMB(OutcomeBinary ~ Continent * Blinding.format + HDI + Blinding.format + Language * Blinding.format, data = overalldat.country.corresponding, family = binomial)
##Model convergence problem; singular convergence (7). See vignette('troubleshooting')
modlang <- glmmTMB(OutcomeBinary ~ Continent * Blinding.format + HDI * Blinding.format + Language + Blinding.format, data = overalldat.country.corresponding, family = binomial)
#Model convergence problem; singular convergence (7). See vignette('troubleshooting')

#####With convergence issues, move into using a stepwise approach instead, considering interaction between
##blinding format and each of continent, HDI, and language, while accounting for the two variables not in the interaction

###Interaction between continent and blinding format, while accounting for other country covariates 
mod1cont <- glmmTMB(OutcomeBinary ~ Continent * Blinding.format + HDI + Language, data = overalldat.country.corresponding, family = binomial)
summary(mod1cont)

##Likelihood Ratio Test
anova(mod1cont, mod1) ##continent not important predictor: 7.91, 0.16

##visualize model
plot_model(mod1cont, type = "pred", terms = c("Continent", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###Interaction between HDI and blinding format, while accounting for other country covariates 
mod1hdi <- glmmTMB(OutcomeBinary ~ HDI * Blinding.format + Continent + Language, data = overalldat.country.corresponding, family = binomial)
summary(mod1hdi)

##Likelihood Ratio Test
anova(mod1hdi, mod1) ##different slope by HDI and blinding format: 6.57, 0.010

##Make figure looking at HDI*blinding. Exports to your working directory. Axis beautification done in PowerPoint post-export
tiff("hdi overall blind.tiff", width = 3.7, height = 2.2, units = 'in', res = 600, compression = 'lzw')
plot_model(mod1hdi, type = "pred", terms = c("HDI", "Blinding.format"),  colors = c("olivedrab3", "gold2"), 
                         value.offset = 0.2, value.size = 8,
                         dot.size = 3, line.size = 1, vline.color = "black", width = 0)
dev.off()

###Interaction between language and blinding format, while accounting for other country covariates 
mod1lang <- glmmTMB(OutcomeBinary ~ Language * Blinding.format + Continent + HDI, data = overalldat.country.corresponding, family = binomial)
summary(mod1lang)

##Likelihood Ratio Test
anova(mod1lang, mod1) ##language*blinding format significant: 4.53, 0.033

##Make figure looking at language*blinding. Exports to your working directory. Axis beautification done in PowerPoint post-export
tiff("language overall blind.tiff", width = 3.7, height = 2.2, units = 'in', res = 600, compression = 'lzw')
plot_model(mod1hdi, type = "pred", terms = c("Language", "Blinding.format"),  colors = c("olivedrab3", "gold2"), 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)
dev.off()


#####################################################
#####################################################
#####################################################
#####################################################
### Load the review score data 
reviewscoredat <- read.csv(here("Meta Analysis", "Solution-end data", "Double-blind", "Review scores solution double blind.csv"), fileEncoding="UTF-8-BOM")
head(reviewscoredat)

reviewscoredat$Demographic.category<- as.factor(reviewscoredat$Demographic.category)
reviewscoredat$Category<- as.factor(reviewscoredat$Category)
reviewscoredat$Subcategory<- as.factor(reviewscoredat$Subcategory)
reviewscoredat$Study<- as.factor(reviewscoredat$Study)

###################################
###################################
### FIRST AUTHOR gender
reviewscoredat.gender.first<- subset(reviewscoredat, Demographic=="Gender" & Author.position == "First" )
reviewscoredat.gender.first$mean_year<-(reviewscoredat.gender.first$Year - 1988.75)

##see the general distribution
hist(reviewscoredat.gender.first$Reviewer_rescaled)

##look at gaussian and beta distributions
##this dataset only has 1 study but it is Squazzoni et al. that has >100,000 manuscripts and 79 journals
##JIF provided at 1 unit intervals. Include JIF as fixed effect
##run model with interaction term with blinding format
modgaus <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = reviewscoredat.gender.first, family = gaussian)
summary(modgaus)

modbeta <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = reviewscoredat.gender.first, family=beta_family(link = "logit"))
summary(modbeta) ##beta seems good

##see if beta or gaussian are better
AICctab(modgaus, modbeta, weights=TRUE)
##similar results but beta slightly better dAICc 0.4

##use beta for inference
mod1 <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = reviewscoredat.gender.first, family=beta_family(link = "logit"))
summary(mod1)

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(Reviewer_rescaled ~ Demographic.category + Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = reviewscoredat.gender.first, family=beta_family(link = "logit"))
#check for multicollinearity issue on model without interaction term
check_collinearity(mod2) ##fine - highest VIF = 1.01
##Likelihood Ratio Test
anova(mod1, mod2) ##Interaction not significant: 1.24, 0.27

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### last AUTHOR
reviewscoredat.gender.last<- subset(reviewscoredat, Demographic=="Gender" & Author.position == "Last" )
reviewscoredat.gender.last$mean_year<-(reviewscoredat.gender.last$Year - 1988.75)

##see the general distribution
hist(reviewscoredat.gender.last$Reviewer_rescaled)

##look at gaussian and beta distributions
##this dataset only has 1 study but it is Squazzoni et al. that has >100,000 manuscripts and 79 journals
##JIF provided at 1 unit intervals. Include JIF as fixed effect
##run model with interaction term with blinding format
modgaus <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = reviewscoredat.gender.last, family = gaussian)
summary(modgaus)

modbeta <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = reviewscoredat.gender.last, family=beta_family(link = "logit"))
summary(modbeta) ##beta seems good

##see if beta or gaussian are better
AICctab(modgaus, modbeta, weights=TRUE)
##similar results but beta slightly better dAICc 0.5

##use beta for inference
mod1 <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = reviewscoredat.gender.last, family=beta_family(link = "logit"))
summary(mod1)

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(Reviewer_rescaled ~ Demographic.category + Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = reviewscoredat.gender.last, family=beta_family(link = "logit"))
#check for multicollinearity issue on model without interaction term
check_collinearity(mod2) ##fine - highest VIF = 1.01
##Likelihood Ratio Test
anova(mod1, mod2) ##Interaction not significant: 0.79, 0.38

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

#####################################################
#####################################################
#####################################################
#####################################################
### Load the number revisions data 
nreviewersdat <- read.csv(here("Meta Analysis", "Solution-end data", "Double-blind", "Number reviewers solution double blind.csv"), fileEncoding="UTF-8-BOM")
head(nreviewersdat)

nreviewersdat.gender.first<- subset(nreviewersdat, Stage=="Number of reviewers" & Author.position == "First" )
nreviewersdat.gender.last<- subset(nreviewersdat, Stage=="Number of reviewers" & Author.position == "Last" )

######################
#####First author gender number reviewers

##look at general distribution
hist(nreviewersdat.gender.first$N.reviewers)

#single study for single year so 
#remove the year and study random effect since all levels are the same
#maintain JIF because the data come from Squazzoni that cover 79 journals across JIFs
mod1 <- glmmTMB(N.reviewers ~ Demographic.category * Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = nreviewersdat.gender.first, family = gaussian)
summary(mod1)

#take out interaction for VIF test
modvif <- glmmTMB(N.reviewers ~ Demographic.category + Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = nreviewersdat.gender.first, family = gaussian)
##check assumptions
check_collinearity(modvif) #fine - highest VIF = 1.01
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) #quantile deviations detected but deviation not significant; dispersion not significant
#proceed with caution 

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(N.reviewers ~ Demographic.category + Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = nreviewersdat.gender.first, family = gaussian)
##Likelihood Ratio Test
anova(mod1, mod2) #interaction not significant: 0.0070, 0.93

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

######################
#####Last author gender number reviewers

##look at general distribution
hist(nreviewersdat.gender.last$N.reviewers)

#single study for single year so 
#remove the year and study random effect since all levels are the same
#maintain JIF because the data come from Squazzoni that cover 79 journals across JIFs
mod1 <- glmmTMB(N.reviewers ~ Demographic.category * Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = nreviewersdat.gender.last, family = gaussian)
summary(mod1)

#take out interaction for VIF test
modvif <- glmmTMB(N.reviewers ~ Demographic.category + Blinding.format +
                    Journal.impact.factor #just one study Squazzoni
                  , data = nreviewersdat.gender.last, family = gaussian)
##check assumptions
check_collinearity(modvif) #fine - highest VIF = 1.01
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) #no problems detected 

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(N.reviewers ~ Demographic.category + Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = nreviewersdat.gender.last, family = gaussian)
##Likelihood Ratio Test
anova(mod1, mod2) #interaction not significant: 0.022, 0.88

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
#####################################################
#####################################################
### Load the number revisions data 
nrevisionsdat <- read.csv(here("Meta Analysis", "Solution-end data", "Double-blind", "Number revisions solution double blind.csv"), fileEncoding="UTF-8-BOM")
head(nrevisionsdat)

nrevisionsdat.gender.first<- subset(nrevisionsdat, Stage=="Number of revisions" & Author.position == "First" )
nrevisionsdat.gender.last<- subset(nrevisionsdat, Stage=="Number of revisions" & Author.position == "Last" )

######################
#####First author gender number revisions

##see the general distribution
hist(nrevisionsdat.gender.first$N.revisions)

#single study for single year so 
#remove the year and study random effect since all levels are the same
#maintain JIF because the data come from Squazzoni that cover 79 journals across JIFs
mod1 <- glmmTMB(N.revisions ~ Demographic.category * Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = nrevisionsdat.gender.first, family = gaussian)
summary(mod1)

#take out interaction for VIF test
modvif <- glmmTMB(N.revisions ~ Demographic.category + Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = nrevisionsdat.gender.first, family = gaussian)
##check assumptions
check_collinearity(modvif) #fine - highest VIF = 1.01
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) #no problems detected 

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(N.revisions ~ Demographic.category + Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = nrevisionsdat.gender.first, family = gaussian)
##Likelihood Ratio Test
anova(mod1, mod2) #interaction not significant: 0.27, 0.60

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

######################
#####Last author gender number revisions

##see the general distribution
hist(nrevisionsdat.gender.last$N.revisions)

#single study for single year so 
#remove the year and study random effect since all levels are the same
#maintain JIF because the data come from Squazzoni that cover 79 journals across JIFs
mod1 <- glmmTMB(N.revisions ~ Demographic.category * Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = nrevisionsdat.gender.last, family = gaussian)
summary(mod1)

#take out interaction for VIF test
modvif <- glmmTMB(N.revisions ~ Demographic.category + Blinding.format +
                    Journal.impact.factor #just one study Squazzoni
                  , data = nrevisionsdat.gender.last, family = gaussian)
##check assumptions
check_collinearity(modvif) #fine - highest VIF = 1.01
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) #no problems detected 

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(N.revisions ~ Demographic.category + Blinding.format +
                  Journal.impact.factor #just one study Squazzoni
                , data = nrevisionsdat.gender.last, family = gaussian)
##Likelihood Ratio Test
anova(mod1, mod2) #interaction not significant: 0.31, 0.58

##estimates for figures
emmeans(mod1, c("Demographic.category", "Blinding.format"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Blinding.format"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)
