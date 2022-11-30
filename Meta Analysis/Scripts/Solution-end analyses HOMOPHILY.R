#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Analysis of homophily data (editor and reviewer "match" to author) 
# Olivia Smith, with data formatting code by Wendy Leuenberger
# Date first created: April 2022
# Date last fully checked: 22 Nov 2022
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

#################INITIAL DATA EDITOR homophily

# Load data
initialdatEDog <- read.csv(here("Meta Analysis", "Solution-end data", "Homophily", "Initial decision solution editor diversity.csv"), fileEncoding="UTF-8-BOM")

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
initialdatED <- initialdatEDog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

# QC ####
Original <- initialdatEDog
New <- initialdatED

dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])

#################POST-INITIAL EDITOR homophily

# Load data
postinitialreviewdatEDog <- read.csv(here("Meta Analysis", "Solution-end data", "Homophily", "Post initial review solution editor diversity.csv"),  fileEncoding="UTF-8-BOM")

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
postinitialreviewdatED <- postinitialreviewdatEDog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

# QC ####
Original <- postinitialreviewdatEDog
New <- postinitialreviewdatED

dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])

################FINAL DATA EDITOR homophily

# Load data
finaldatEDog <- read.csv(here("Meta Analysis", "Solution-end data", "Homophily", "Final decision solution editor diversity.csv"), fileEncoding="UTF-8-BOM")

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
finaldatED <- finaldatEDog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

# QC ####
Original <- finaldatEDog
New <- finaldatED

dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])


################OVERALL DATA EDITOR homophily

overalldatEDog <- read.csv(here("Meta Analysis", "Solution-end data", "Homophily", "Overall decision solution editor diversity.csv"), fileEncoding="UTF-8-BOM")

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
overalldatED <- overalldatEDog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

# QC ####
Original <- overalldatEDog
New <- overalldatED

dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])

##################POST INITIAL REVIEWER homophily

# Load data
postinitialreviewdatREVog <- read.csv(here("Meta Analysis", "Solution-end data", "Homophily", "Post initial review solution reviewer diversity.csv"), fileEncoding="UTF-8-BOM")

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
postinitialreviewdatREV <- postinitialreviewdatREVog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

# QC ####
Original <- postinitialreviewdatREVog
New <- postinitialreviewdatREV

dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])

################FINAL DATA REVIEWER homophily

# Load data
finaldatREVog <- read.csv(here("Meta Analysis", "Solution-end data", "Homophily", "Final decision solution reviewer diversity.csv"), fileEncoding="UTF-8-BOM")

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
finaldatREV <- finaldatREVog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

# QC ####
Original <- finaldatREVog
New <- finaldatREV

dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])

########OVERALL REVIEWER#########

# Load data
overalldatREVog <- read.csv(here("Meta Analysis", "Solution-end data", "Homophily", "Overall decision solution reviewer diversity.csv"), fileEncoding="UTF-8-BOM")

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
overalldatREV <- overalldatREVog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

# QC ####
Original <- overalldatREVog
New <- overalldatREV

dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])

#####################################################
#####################################################
##Analyses
#####################################################
##EDITOR HOMOPHILY

#####################################################
##INITIAL DECISION 
#####################################################

initialdatED$Demographic.category<- as.factor(initialdatED$Demographic.category)
initialdatED$Study<- as.factor(initialdatED$Study)
initialdatED$Editor.demographic<- as.factor(initialdatED$Editor.demographic)

###################################
###################################
### FIRST AUTHOR gender
### Editor homophily
initialdatED.gender.first<- subset(initialdatED, Demographic=="Gender" & Author.position == "First" )
initialdatED.gender.first$mean_year<-(initialdatED.gender.first$Year - 1988.75)

##This category has 2 studies so all levels of JIF and mean year are the same as their respective
##studies. Include study as fixed effect
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Editor.demographic + Study #two studies
                , data = initialdatED.gender.first, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and editor "match
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Editor.demographic + Study #two studies
                , data = initialdatED.gender.first, family = binomial)
check_collinearity(mod2) ##fine VIF = 1.01
##Likelihood Ratio Test
anova(mod1, mod2)##demographic category * editor demographic not significant: 0.17, 0.68

##estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### CORRESPONDING AUTHOR gender 
### Editor homophily
initialdatED.gender.corresponding<- subset(initialdatED, Demographic=="Gender" & Author.position == "Corresponding" )
initialdatED.gender.corresponding$mean_year<-(initialdatED.gender.corresponding$Year - 1988.75)

##3 studies in this category so include mean study year, JIF, and study random effect
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Editor.demographic + mean_year + #3 studies
               Journal.impact.factor + (1|Study) , data = initialdatED.gender.corresponding, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and editor "match"
##remove interaction for comparison model
##remove interaction to test for multicollinearity
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Editor.demographic + mean_year + #3 studies
                  Journal.impact.factor + (1|Study), data = initialdatED.gender.corresponding, family = binomial)
check_collinearity(mod2) ##fine - max VIF = 1.23
##Likelihood Ratio Test
anova(mod1, mod2) ##demographic category * editor demographic not significant: 0.52, 0.47

##estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### LAST AUTHOR gender 
### Editor homophily
initialdatED.gender.last<- subset(initialdatED, Demographic=="Gender" & Author.position == "Last" )
initialdatED.gender.last$mean_year<-(initialdatED.gender.last$Year - 1988.75)

##This category has 2 studies so all levels of JIF and mean year are the same as their respective
##studies. Include study as fixed effect
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Editor.demographic + #two studies
                Study, data = initialdatED.gender.last, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and editor "match"
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Editor.demographic + #two studies
                  Study, data = initialdatED.gender.last, family = binomial)
check_collinearity(mod2) ##fine VIF = 1.01
anova(mod1, mod2) ##demographic category * editor demographic not significant: 1.64, 0.20

##estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

#####################################################
##POST-INITIAL REVIEW DECISION 
#####################################################

postinitialreviewdatED$Demographic.category<- as.factor(postinitialreviewdatED$Demographic.category)
postinitialreviewdatED$Study<- as.factor(postinitialreviewdatED$Study)
postinitialreviewdatED$Editor.demographic<- as.factor(postinitialreviewdatED$Editor.demographic)

###################################
###################################
### FIRST AUTHOR gender
### Editor homophily
postinitialreviewdatED.gender.first<- subset(postinitialreviewdatED, Demographic=="Gender" & Author.position == "First" )
postinitialreviewdatED.gender.first$mean_year<-(postinitialreviewdatED.gender.first$Year - 1988.75)

##4 studies in this category so include mean study year, JIF, and study random effect
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Editor.demographic + Journal.impact.factor + #4 studies
                mean_year + (1|Study), data = postinitialreviewdatED.gender.first, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and editor "match'
##remove interaction for comparison model
##run model with no interaction term to check for multicollinearity issue
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Editor.demographic + Journal.impact.factor + #4 studies
                  mean_year + (1|Study), data = postinitialreviewdatED.gender.first, family = binomial)
#Check for multicollinearity issues
check_collinearity(mod2) #highest VIF = 2.91
#Likelihood Ratio Test
anova(mod1, mod2) ##demographic category * editor demographic not significant: 0.73, 0.39

##estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### CORRESPONDING AUTHOR gender
### Editor homophily
postinitialreviewdatED.gender.corresponding<- subset(postinitialreviewdatED, Demographic=="Gender" & Author.position == "Corresponding" )
postinitialreviewdatED.gender.corresponding$mean_year<-(postinitialreviewdatED.gender.corresponding$Year - 1988.75)

##This category has 2 studies so all levels of JIF and mean year are the same as their respective
##studies. Include study as fixed effect
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Editor.demographic + #2 studies
                  Study, data = postinitialreviewdatED.gender.corresponding, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and editor "match"
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Editor.demographic + #2 studies
                  Study, data = postinitialreviewdatED.gender.corresponding, family = binomial)
##check multicollinearity
check_collinearity(mod2) #Fine - VIF = 1.02
##Likelihood Ratio Test
anova(mod1, mod2) ##demographic category * editor demographic not significant: 0.079, 0.78

##estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### LAST AUTHOR gender
### Editor homophily
postinitialreviewdatED.gender.last<- subset(postinitialreviewdatED, Demographic=="Gender" & Author.position == "Last" )
postinitialreviewdatED.gender.last$mean_year<-(postinitialreviewdatED.gender.last$Year - 1988.75)

##3 studies in this category so include mean study year, JIF, and study random effect
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Editor.demographic + Journal.impact.factor + #3 studies
                  mean_year + (1|Study), data = postinitialreviewdatED.gender.last, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and editor "match'
##remove interaction for comparison model
##run model with no interaction term to check for multicollinearity issue
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Editor.demographic + Journal.impact.factor + #3 studies
                  mean_year + (1|Study), data = postinitialreviewdatED.gender.last, family = binomial)
##check for multicollinearity
check_collinearity(mod2) #fine - highest VIF 1.21
##Likelihood Ratio Test
anova(mod1, mod2) ##demographic category * editor demographic not significant: 0.13, 0.72

##estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

#####################################################
##FINAL DECISION 
#####################################################

finaldatED$Demographic.category<- as.factor(finaldatED$Demographic.category)
finaldatED$Study<- as.factor(finaldatED$Study)
finaldatED$Editor.demographic<- as.factor(finaldatED$Editor.demographic)

###################################
###################################
### FIRST AUTHOR gender
### Editor homophily
finaldatED.gender.first<- subset(finaldatED, Demographic=="Gender" & Author.position == "First" )
finaldatED.gender.first$mean_year<-(finaldatED.gender.first$Year - 1988.75)

##This category has 2 studies so all levels of JIF and mean year are the same as their respective
##studies. Include study as fixed effect
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Editor.demographic + #2 studies
                  Study, data = finaldatED.gender.first, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and editor "match'
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Editor.demographic + #2 studies
                  Study, data = finaldatED.gender.first, family = binomial)
##Check for multicollinearity
check_collinearity(mod2) # fine - highest VIF = 1.09
##Likelihood Ratio Test
anova(mod1, mod2) ##demographic category * editor demographic not significant: 0.62, 0.43

##estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### CORRESPONDING AUTHOR gender
### Editor homophily
finaldatED.gender.corresponding<- subset(finaldatED, Demographic=="Gender" & Author.position == "Corresponding" )
finaldatED.gender.corresponding$mean_year<-(finaldatED.gender.corresponding$Year - 1988.75)

##just 1 study in this category so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Editor.demographic #1 study
                , data = finaldatED.gender.corresponding, family = binomial)
summary(mod1)

###Use a Likelihood Ratio Test to see if interaction is supported between demographic category and editor "match'
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Editor.demographic #one study
                , data = finaldatED.gender.corresponding, family = binomial)
##Likelihood Ratio Test
anova(mod1, mod2) ##demographic category * editor demographic SIGNIFICANT: 6.49, 0.011*

##estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### LAST AUTHOR gender
### Editor homophily
finaldatED.gender.last<-subset(finaldatED, Demographic=="Gender" & Author.position == "Last" )
finaldatED.gender.last$mean_year<-(finaldatED.gender.last$Year - 1988.75)

##just 1 study in this category so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Editor.demographic #1 study
                , data = finaldatED.gender.last, family = binomial)
summary(mod1)

###Use a Likelihood Ratio Test to see if interaction is supported between demographic category and editor "match'
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Editor.demographic #one study
                , data = finaldatED.gender.last, family = binomial)
##Likelihood Ratio Test
anova(mod1, mod2) ##demographic category * editor demographic not significant: 0.15, 0.70

##estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

#####################################################
##OVERALL DECISION 
#####################################################

overalldatED$Demographic.category<- as.factor(overalldatED$Demographic.category)
overalldatED$Category<- as.factor(overalldatED$Category)
overalldatED$Subcategory<- as.factor(overalldatED$Subcategory)
overalldatED$Study<- as.factor(overalldatED$Study)
overalldatED$Editor.demographic<- as.factor(overalldatED$Editor.demographic)


####################Subset overall data by demographic and position##################

###################################
###################################
### FIRST AUTHOR gender
### Editor homophily
overalldatED.gender.first<- subset(overalldatED, Demographic=="Gender" & Author.position == "First" )
overalldatED.gender.first$mean_year<-(overalldatED.gender.first$Year - 1988.75)

##This category has 2 studies so all levels of JIF and mean year are the same as their respective
##studies. Include study as fixed effect
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Editor.demographic + Study #two studies
                , data = overalldatED.gender.first, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and editor "match'
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Editor.demographic + Study #two studies
                , data = overalldatED.gender.first, family = binomial)
check_collinearity(mod2) #fine - highest VIF = 1.07
##Likelihood Ratio Test
anova(mod1, mod2) ##demographic category * editor demographic not significant: 1.68, 0.19

##estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### CORRESPONDING AUTHOR gender
### Editor homophily
overalldatED.gender.corresponding<- subset(overalldatED, Demographic=="Gender" & Author.position == "Corresponding" )
overalldatED.gender.corresponding$mean_year<-(overalldatED.gender.corresponding$Year - 1988.75)

##just 1 study in this category so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Editor.demographic #just one study
                , data = overalldatED.gender.corresponding, family = binomial)
summary(mod1)

###Use a Likelihood Ratio test to see if interaction is supported between demographic category and editor "match'
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Editor.demographic #just one study
                , data = overalldatED.gender.corresponding, family = binomial)
check_collinearity(mod2) #Fine - max VIF = 1.00
##Likelihood Ratio Test
anova(mod1, mod2) ##demographic category * editor demographic MARGINALLY significant: 3.46, 0.063.

##estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### LAST AUTHOR gender
### Editor homophily
overalldatED.gender.last<- subset(overalldatED, Demographic=="Gender" & Author.position == "Last" )
overalldatED.gender.last$mean_year<-(overalldatED.gender.last$Year - 1988.75)

##just 1 study in this category so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Editor.demographic #one study
                , data = overalldatED.gender.last, family = binomial)
summary(mod1)

###Use a Likelihood Ratio Test to see if interaction is supported between demographic category and editor "match'
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Editor.demographic #just one study
                , data = overalldatED.gender.last, family = binomial)
check_collinearity(mod2) #Fine - max VIF = 1.00
##Likelihood Ratio Test
anova(mod1, mod2) ##demographic category * editor demographic not significant: 0.064, 0.80

##estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
##NUMBER REVIEWERS 
#####################################################

#####################################################
#####################################################
#####################################################
#####################################################
### Load the number reviewers data 
nreviewersdat <- read.csv(here("Meta Analysis", "Solution-end data", "Homophily", "Number reviewers solution editor diversity.csv"), fileEncoding="UTF-8-BOM")
head(nreviewersdat)

nreviewersdat$Study<- as.factor(nreviewersdat$Study)
nreviewersdat$Demographic.category<- as.factor(nreviewersdat$Demographic.category)
nreviewersdat$Editor.demographic<- as.factor(nreviewersdat$Editor.demographic)

nreviewersdat.gender.first<- subset(nreviewersdat, Author.position == "First" )
nreviewersdat.gender.corresponding<- subset(nreviewersdat, Author.position == "Corresponding" )
nreviewersdat.gender.last<- subset(nreviewersdat, Author.position == "Last" )

###################################
###################################
### FIRST AUTHOR gender
### Editor homophily

nreviewersdat.gender.first$mean_year<-(nreviewersdat.gender.first$Year - 1988.75)

##look at the general distribution
hist(nreviewersdat.gender.first$N.reviewers)

##4 studies so include JIF (fixed), mean year (fixed), and study (random)
mod1 <- glmmTMB(N.reviewers ~ Demographic.category * Editor.demographic + Journal.impact.factor +
                  mean_year + (1|Study), data = nreviewersdat.gender.first, family = gaussian)
summary(mod1)

#take out interaction for variance inflation factor test
modvif <- glmmTMB(N.reviewers ~ Demographic.category + Editor.demographic + Journal.impact.factor +
                  mean_year + (1|Study), data = nreviewersdat.gender.first, family = gaussian)
##check assumptions
check_collinearity(modvif) #fine - max VIF = 1.01
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) #fine
#on check on 11/22/2022 got error qu = 0.25, log(sigma) = -2.353321 : outer Newton did not converge fully.

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(N.reviewers ~ Demographic.category + Editor.demographic + Journal.impact.factor +
                  mean_year + (1|Study), data = nreviewersdat.gender.first, family = gaussian)

#Likelihood Ratio Test
anova(mod1, mod2) ##demographic category * editor demographic not significant: 0.13, 0.72

#estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### CORRESPONDING AUTHOR gender
### Editor homophily

##look at the general distribution
hist(nreviewersdat.gender.corresponding$N.reviewers)

##2 studies so JIF and mean year same as respective studies; include study fixed effect
mod1 <- glmmTMB(N.reviewers ~ Demographic.category * Editor.demographic + #2 studies
                  Study, data = nreviewersdat.gender.corresponding, family = gaussian)
summary(mod1)

#take out interaction for variance inflation factor test
modvif <- glmmTMB(N.reviewers ~ Demographic.category + Editor.demographic + #2 studies
                  Study, data = nreviewersdat.gender.corresponding, family = gaussian)
##Check assumptions
check_collinearity(modvif) #fine - max VIF = 1.00
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) #no problems but does say can't calcuate quantile at 0.25 due to probably few points

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(N.reviewers ~ Demographic.category + Editor.demographic + #2 studies
                  Study, data = nreviewersdat.gender.corresponding, family = gaussian)

##Likelihood Ratio Test
anova(mod1, mod2) ##demographic category * editor demographic not significant: 0.22, 0.64

##Estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### LAST AUTHOR gender
### Editor homophily

##look at the general distribution
hist(nreviewersdat.gender.last$N.reviewers)

##2 studies so JIF and mean year same as respective studies; include study fixed effect
mod1 <- glmmTMB(N.reviewers ~ Demographic.category * Editor.demographic + # 2 studies
                  Study, data = nreviewersdat.gender.last, family = gaussian)
summary(mod1)

#take out interaction for variance inflation factor test
modvif <- glmmTMB(N.reviewers ~ Demographic.category + Editor.demographic + #2 studies
                    Study, data = nreviewersdat.gender.last, family = gaussian)
##Check assumptions
check_collinearity(modvif) #fine - max VIF = 1.00
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) ##dispersion fine, deviation fine, but getting error for quantiles
#We had to increase `err` for some of the quantiles. See fit$calibr$err
#qu = 0.5, log(sigma) = -2.179714 : outer Newton did not converge fully
#possibly due to low replication. Proceed with caution 

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(N.reviewers ~ Demographic.category + Editor.demographic + # 2 studies
                  Study , data = nreviewersdat.gender.last, family = gaussian)

##Likelihood Ratio Test
anova(mod1, mod2) ##demographic category * editor demographic not significant: 0.85, 0.36

##estimates for figures
emmeans(mod1, c("Demographic.category", "Editor.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Editor.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

#####################################################
#####################################################
##Analyses
#####################################################
##REVIEWER HOMOPHILY

#####################################################
###POST-INITIAL REVIEW DECISION
##################################################

#####################################################
#####################################################
postinitialreviewdatREV$Demographic.category<- as.factor(postinitialreviewdatREV$Demographic.category)
postinitialreviewdatREV$Category<- as.factor(postinitialreviewdatREV$Category)
postinitialreviewdatREV$Subcategory<- as.factor(postinitialreviewdatREV$Subcategory)
postinitialreviewdatREV$Study<- as.factor(postinitialreviewdatREV$Study)

####################Subset post-initial review data by demographic and position##################

###################################
###################################
### FIRST AUTHOR for gender_1
postinitialreviewdatREV.gender_1.first<- subset(postinitialreviewdatREV, Demographic=="Gender_1" & Author.position == "First" )
postinitialreviewdatREV.gender_1.first$mean_year<-(postinitialreviewdatREV.gender_1.first$Year - 1988.75)

##3 studies so include JIF (fixed), mean year (fixed), and study (random)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic + Journal.impact.factor +#three studies
                  mean_year + (1|Study), data = postinitialreviewdatREV.gender_1.first, family = binomial)

##remove interaction term to check for multicollinearity issues
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Reviewer.demographic + Journal.impact.factor +#three studies
                  mean_year + (1|Study), data = postinitialreviewdatREV.gender_1.first, family = binomial)
#check multicollinearity
check_collinearity(mod2) ##high correlation between year and JIF (JIF VIF = 15.75, year VIF = 15.90)

#run with just JIF or year
modjif <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic + Journal.impact.factor +#three studies
                    (1|Study), data = postinitialreviewdatREV.gender_1.first, family = binomial)
modyear <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic + #three studies
                     mean_year + (1|Study), data = postinitialreviewdatREV.gender_1.first, family = binomial)

#see if model with JIF or year is better
AICctab(modjif, modyear, weights=TRUE) ##the model with journal impact factor is slightly better (dAICc = 0.90)

##make inference from the model with JIF 
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic + Journal.impact.factor +#three studies
                  (1|Study), data = postinitialreviewdatREV.gender_1.first, family = binomial)
summary(mod1)

###Use a Likelihood Ratio Test to see if interaction is supported between reviewer and author demographic "match"
##remove interaction for comparison model
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Reviewer.demographic + Journal.impact.factor +#three studies
                  (1|Study), data = postinitialreviewdatREV.gender_1.first, family = binomial)
check_collinearity(mod2) ##fine - max VIF = 1.03

##Likelihood Ratio Test
anova(mod1, mod2) ##author * reviewer not significant: 0.067, 0.80

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### FIRST AUTHOR for gender_2
postinitialreviewdatREV.gender_2.first<- subset(postinitialreviewdatREV, Demographic=="Gender_2" & Author.position == "First" )
postinitialreviewdatREV.gender_2.first$mean_year<-(postinitialreviewdatREV.gender_2.first$Year - 1988.75)

##4 studies so include mean year (fixed), JIF (fixed), and study (random)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic + Journal.impact.factor + # 4 studies
                  mean_year + (1|Study), data = postinitialreviewdatREV.gender_2.first, family = binomial)
summary(mod1)

##remove interaction term to check for multicollinearity issues
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Reviewer.demographic + Journal.impact.factor + # 4 studies
                  mean_year + (1|Study), data = postinitialreviewdatREV.gender_2.first, family = binomial)
check_collinearity(mod2) #fine - max VIF = 1.01

#Likelihood Ratio Test
anova(mod1, mod2) ##author * reviewer not significant: 0.20, 0.65

#estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### corresponding AUTHOR for gender_2
postinitialreviewdatREV.gender_2.corresponding<- subset(postinitialreviewdatREV, Demographic=="Gender_2" & Author.position == "Corresponding" )
postinitialreviewdatREV.gender_2.corresponding$mean_year<-(postinitialreviewdatREV.gender_2.corresponding$Year - 1988.75)

##one study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic #one study
                , data = postinitialreviewdatREV.gender_2.corresponding, family = binomial)
summary(mod1)

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Reviewer.demographic  #one study
                , data = postinitialreviewdatREV.gender_2.corresponding, family = binomial)
check_collinearity(mod2) #low - 1.00

##Likelihood Ratio Test
anova(mod1, mod2) ##author * reviewer not significant: 0.084, 0.77

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### last AUTHOR for gender_1
postinitialreviewdatREV.gender_1.last<- subset(postinitialreviewdatREV, Demographic=="Gender_1" & Author.position == "Last" )
postinitialreviewdatREV.gender_1.last$mean_year<-(postinitialreviewdatREV.gender_1.last$Year - 1988.75)

##2 studies so JIF and mean year are identical within study - include study fixed effect
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic + Study #two studies
                , data = postinitialreviewdatREV.gender_1.last, family = binomial)
summary(mod1)

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Reviewer.demographic  + Study #two studies
                , data = postinitialreviewdatREV.gender_1.last, family = binomial)

##Likelihood Ratio Test
anova(mod1, mod2) ##author * reviewer not significant: 0.10, 0.75

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### last AUTHOR for gender_2
postinitialreviewdatREV.gender_2.last<- subset(postinitialreviewdatREV, Demographic=="Gender_2" & Author.position == "Last" )
postinitialreviewdatREV.gender_2.last$mean_year<-(postinitialreviewdatREV.gender_2.last$Year - 1988.75)

##4 studies so include JIF (fixed), mean year (fixed), and study (random)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic + Journal.impact.factor + #4 studies
                  mean_year + (1|Study), data = postinitialreviewdatREV.gender_2.last, family = binomial)
summary(mod1)

##remove interaction for multicollinearity check
##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Reviewer.demographic + Journal.impact.factor + #4 studies
                  mean_year + (1|Study), data = postinitialreviewdatREV.gender_2.last, family = binomial)
##check for multicollinearity
check_collinearity(mod2) #fine - max VIF = 1.01
##Likelihood Ratio Test
anova(mod1, mod2) ##author * reviewer not significant: 0.057, 0.81

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

#####################################################
#####################################################
###FINAL DECISION
##################################################

finaldatREV$Demographic.category<- as.factor(finaldatREV$Demographic.category)
finaldatREV$Category<- as.factor(finaldatREV$Category)
finaldatREV$Subcategory<- as.factor(finaldatREV$Subcategory)
finaldatREV$Study<- as.factor(finaldatREV$Study)
finaldatREV$Reviewer.demographic<- as.factor(finaldatREV$Reviewer.demographic)

####################Subset overall data by demographic and position##################

###################################
###################################
### FIRST AUTHOR for gender_1
finaldatREV.gender_1.first<- subset(finaldatREV, Demographic=="Gender_1" & Author.position == "First" )
finaldatREV.gender_1.first$mean_year<-(finaldatREV.gender_1.first$Year - 1988.75)

##two studies so all levels of JIF and mean year are identical within respective studies. Include study fixed effect
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic + Study #two studies
                , data = finaldatREV.gender_1.first, family = binomial)
summary(mod1)

##remove interaction term for Likelihood Ratio Test to test interaction
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Reviewer.demographic + Study #two studies
                , data = finaldatREV.gender_1.first, family = binomial)
check_collinearity(mod2) #fine - max VIF = 1.0

##Likelihood Ratio Test
anova(mod1, mod2) ##author * reviewer SIGNIFICANT: 4.23, 0.040

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### FIRST AUTHOR for gender_2
finaldatREV.gender_2.first<- subset(finaldatREV, Demographic=="Gender_2" & Author.position == "First" )
finaldatREV.gender_2.first$mean_year<-(finaldatREV.gender_2.first$Year - 1988.75)

##this dataset only has 2 studies but one is Squazzoni et al. that have ~85,000 manuscripts in this category from 79 journals
##JIF provided at 1 unit intervals. Include JIF and study as fixed effects 
##run model with interaction term with reviewer demographic
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic + Journal.impact.factor + 
                  # 2 studies but one is Squazzoni, which has rows by JIFs. Remove years which 
                  # had 2 levels 2013 and 2014 (same as their respective studies) and use study but keep JIF
                  Study, data = finaldatREV.gender_2.first, family = binomial)
summary(mod1)

##Remove interaction for Likelihood Ratio Test and test for multicollinearity issues
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Reviewer.demographic + Journal.impact.factor + 
                  # 2 studies but one is Squazzoni, which has rows by JIFs. Remove years which 
                  # had 2 levels 2013 and 2014 (same as their respective studies) and use study but keep JIF
                  Study, data = finaldatREV.gender_2.first, family = binomial)
##Check for multicollinearity
check_collinearity(mod2) #fine - highest 1.01 
##Likelihood Ratio Test
anova(mod1, mod2) ##author * reviewer not significant: 0.25, 0.62

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### last AUTHOR for gender_1
finaldatREV.gender_1.last<- subset(finaldatREV, Demographic=="Gender_1" & Author.position == "Last" )
finaldatREV.gender_1.last$mean_year<-(finaldatREV.gender_1.last$Year - 1988.75)

##one study so all levels of JIF and mean year are the same across manuscripts
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic
                , data = finaldatREV.gender_1.last, family = binomial)
summary(mod1)

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Reviewer.demographic
                , data = finaldatREV.gender_1.last, family = binomial)
check_collinearity(mod2) #fine - 1.00
##Likelihood Ratio Test
anova(mod1, mod2) #author * reviewer not significant: 0.54, 0.46

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### last AUTHOR for gender_1
finaldatREV.gender_2.last<- subset(finaldatREV, Demographic=="Gender_2" & Author.position == "Last" )
finaldatREV.gender_2.last$mean_year<-(finaldatREV.gender_2.last$Year - 1988.75)

##this dataset only has 2 studies but one is Squazzoni et al. that have ~82,000 manuscripts in this category from 79 journals
##JIF provided at 1 unit intervals. Include JIF and study as fixed effects 
##run model with interaction term with reviewer demographic format
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic + Journal.impact.factor +
                  Study, data = finaldatREV.gender_2.last, family = binomial)
summary(mod1)

##Remove interaction for Likelihood Ratio Test and test for multicollinearity issues
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Reviewer.demographic + Journal.impact.factor + 
                  Study, data = finaldatREV.gender_2.last, family = binomial)
##Check for multicollinearity
check_collinearity(mod2) #fine - max VIF = 1.01
##Likelihood Ratio Test
anova(mod1, mod2) ##author * reviewer not significant: 0.090, 0.76

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
###OVERALL DECISION
##################################################

overalldatREV$Demographic.category<- as.factor(overalldatREV$Demographic.category)
overalldatREV$Category<- as.factor(overalldatREV$Category)
overalldatREV$Subcategory<- as.factor(overalldatREV$Subcategory)
overalldatREV$Study<- as.factor(overalldatREV$Study)
overalldatREV$Reviewer.demographic<- as.factor(overalldatREV$Reviewer.demographic)

####################Subset overall data by demographic and position##################

###################################
###################################
### FIRST AUTHOR for gender_1
overalldatREV.gender_1.first<- subset(overalldatREV, Demographic=="Gender_1" & Author.position == "First" )
overalldatREV.gender_1.first$mean_year<-(overalldatREV.gender_1.first$Year - 1988.75)

##One study so JIF and mean year are the same for all manuscripts
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic #one study
                , data = overalldatREV.gender_1.first, family = binomial)
summary(mod1)

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Reviewer.demographic  #one study
                , data = overalldatREV.gender_1.first, family = binomial)
##Check for multicollinearity
check_collinearity(mod2) #VIF 1.00
##Likelihood Ratio Test
anova(mod1, mod2) ##author * reviewer not significant: 2.50, 0.11

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### FIRST AUTHOR for gender_1
overalldatREV.gender_2.first<- subset(overalldatREV, Demographic=="Gender_2" & Author.position == "First" )
overalldatREV.gender_2.first$mean_year<-(overalldatREV.gender_2.first$Year - 1988.75)

##Two studies so don't include year/JIF, just study 
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic + Study #two studies
                , data = overalldatREV.gender_2.first, family = binomial)
summary(mod1)

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Reviewer.demographic + Study #two studies
                , data = overalldatREV.gender_2.first, family = binomial)
##Check for multicollinearity
check_collinearity(mod2) #max VIF 1.07
##Likelihood Ratio Test
anova(mod1, mod2) ##author * reviewer not significant: 1.64, 0.20

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### last AUTHOR for gender_1
overalldatREV.gender_1.last<- subset(overalldatREV, Demographic=="Gender_1" & Author.position == "Last" )
overalldatREV.gender_1.last$mean_year<-(overalldatREV.gender_1.last$Year - 1988.75)

##1 study so all manuscripts have the same JIF/mean year
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic #one study
                , data = overalldatREV.gender_1.last, family = binomial)
summary(mod1)

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Reviewer.demographic  #one study
                , data = overalldatREV.gender_1.last, family = binomial)
##Check for multicollinearity
check_collinearity(mod2) 
##Likelihood Ratio Test
anova(mod1, mod2) ##author * reviewer not significant: 0.49, 0.49

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### last AUTHOR for gender_2
overalldatREV.gender_2.last<- subset(overalldatREV, Demographic=="Gender_2" & Author.position == "Last" )
overalldatREV.gender_2.last$mean_year<-(overalldatREV.gender_2.last$Year - 1988.75)

##1 study so all manuscripts have the same JIF/mean year
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category*Reviewer.demographic #one study
                , data = overalldatREV.gender_2.last, family = binomial)
summary(mod1)

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category+Reviewer.demographic #one study
                , data = overalldatREV.gender_2.last, family = binomial)
##Check for multicollinearity
check_collinearity(mod2)
##Likelihood Ratio Test
anova(mod1, mod2) ##author * reviewer not significant: 0.26, 0.61

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
###REVIEW SCORES FOR REVIEWER HOMOPHILY
##################################################
#####################################################

#####################################################
### Load the review score data 
reviewscoredat <- read.csv(here("Meta Analysis", "Solution-end data", "Homophily", "Review scores solution reviewer diversity.csv"), fileEncoding="UTF-8-BOM")
head(reviewscoredat)

reviewscoredat$Demographic.category<- as.factor(reviewscoredat$Demographic.category)
reviewscoredat$Category<- as.factor(reviewscoredat$Category)
reviewscoredat$Subcategory<- as.factor(reviewscoredat$Subcategory)
reviewscoredat$Study<- as.factor(reviewscoredat$Study)
reviewscoredat$Reviewer.demographic<- as.factor(reviewscoredat$Reviewer.demographic)

###################################
###################################
### FIRST AUTHOR Gender_1
reviewscoredat.gender.first<- subset(reviewscoredat, Demographic=="Gender_1" & Author.position == "First" )
reviewscoredat.gender.first$mean_year<-(reviewscoredat.gender.first$Year - 1988.75)

##look at general distribution
hist(reviewscoredat.gender.first$Reviewer_rescaled, breaks = 4)

##two studies so JIF and year are the same as study. Include study fixed effect
##look at gaussian and beta distributions
modgaus <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Reviewer.demographic + Study
                   #two studies with 8 points total
                   , data = reviewscoredat.gender.first, family = gaussian)
summary(modgaus)
##check gaussian assumptions
testDispersion(modgaus)
simulationOutput <- simulateResiduals(fittedModel = modgaus, plot = F)
plot(simulationOutput) #fine
#when re-checked on 11/22/2022, says quantile deviations detected

modbeta <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Reviewer.demographic + Study
                   #two studies with 8 points total
                   , data = reviewscoredat.gender.first, family=beta_family(link = "logit"))
summary(modbeta) ##beta seems good

##see if beta or gaussian are better
AICctab(modgaus, modbeta, weights=TRUE)
##similar results but beta better dAICc 2.1

##use beta for inference
mod1 <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Reviewer.demographic + Study
                #two studies with 8 points total
                , data = reviewscoredat.gender.first, family=beta_family(link = "logit"))
summary(mod1)

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(Reviewer_rescaled ~ Demographic.category + Reviewer.demographic + Study
                #two studies with 8 points total
                , data = reviewscoredat.gender.first, family=beta_family(link = "logit"))
check_collinearity(mod2) 
##Likelihood Ratio Test
anova(mod1, mod2) ##author * reviewer SIGNIFICANT: 5.51, 0.019

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### FIRST AUTHOR Gender_2
reviewscoredat.gender.first<- subset(reviewscoredat, Demographic=="Gender_2" & Author.position == "First" )
reviewscoredat.gender.first$mean_year<-(reviewscoredat.gender.first$Year - 1988.75)

##look at general distribution
hist(reviewscoredat.gender.first$Reviewer_rescaled, breaks = 4)

##look at gaussian and beta distributions
##this dataset only has 2 studies, but one is Squazzoni et al. that has ~85,000 manuscripts and 79 journals
##JIF provided at 1 unit intervals. Include JIF as fixed effect
##run model with interaction term with reviewer demographic
modgaus <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Reviewer.demographic + Journal.impact.factor +
                     Study #two studies but includes Squazzoni with JIF intervals
                   , data = reviewscoredat.gender.first, family = gaussian)
summary(modgaus)
##check gaussian assumptions
testDispersion(modgaus)
simulationOutput <- simulateResiduals(fittedModel = modgaus, plot = F)
plot(simulationOutput) #fine

modbeta <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Reviewer.demographic + Journal.impact.factor +
                     Study #two studies but includes Squazzoni with JIF intervals
                   , data = reviewscoredat.gender.first, family=beta_family(link = "logit"))
summary(modbeta) ##beta seems good

##see if beta or gaussian are better
AICctab(modgaus, modbeta, weights=TRUE)
##similar results; gaussian is better in this case with dAICc 2.2 but using beta for consistency 

##use beta for inference
mod1 <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Reviewer.demographic + Journal.impact.factor +
                  Study #two studies but includes Squazzoni with JIF intervals
                , data = reviewscoredat.gender.first, family=beta_family(link = "logit"))
summary(mod1)

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(Reviewer_rescaled ~ Demographic.category + Reviewer.demographic + Journal.impact.factor +
                  Study #two studies but includes Squazzoni with JIF intervals
                , data = reviewscoredat.gender.first, family=beta_family(link = "logit"))
#check for multicollinearity issue on model without interaction term
check_collinearity(mod2) ##fine - highest VIF = 1.01
##Likelihood Ratio Test
anova(mod1, mod2) ##Interaction not significant: 0.030, 0.86

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### CORRESPONDING AUTHOR Gender_1
reviewscoredat.gender.corresponding<- subset(reviewscoredat, Demographic=="Gender_1" & Author.position == "Corresponding" )
reviewscoredat.gender.corresponding$mean_year<-(reviewscoredat.gender.corresponding$Year - 1988.75)

##look at general distribution
hist(reviewscoredat.gender.corresponding$Reviewer_rescaled, breaks = 4)

##2 studies so all JIF and mean year the same as their studies. Include study fixed effect
##look at gaussian and beta distributions
modgaus <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Reviewer.demographic + Study
                   #two studies with 8 points total
                   , data = reviewscoredat.gender.corresponding, family = gaussian)
summary(modgaus)
##check gaussian assumptions
testDispersion(modgaus)
simulationOutput <- simulateResiduals(fittedModel = modgaus, plot = F)
plot(simulationOutput) #dispersion and deviation fine but unable to calculate quantile regression 
#Unable to calculate quantile regression for quantile 0.25. Possibly to few (unique) data points / predictions. Will be ommited in plots and significance calculations.
##maybe the sparse replicates in this category

modbeta <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Reviewer.demographic + Study
                   #two studies with 8 points total
                   , data = reviewscoredat.gender.corresponding, family=beta_family(link = "logit"))
summary(modbeta) 

##see if beta or gaussian are better
AICctab(modgaus, modbeta, weights=TRUE) ##gaus slightly better at 0.4 AICc
#conclusions same between 2 models; beta for consistency 

##use beta for inference
mod1 <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Reviewer.demographic + Study
                #two studies with 8 points total
                , data = reviewscoredat.gender.corresponding, family=beta_family(link = "logit"))
summary(mod1)

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(Reviewer_rescaled ~ Demographic.category + Reviewer.demographic + Study
                #two studies with 8 points total
                , data = reviewscoredat.gender.corresponding, family=beta_family(link = "logit"))
#check for multicollinearity issue on model without interaction term
check_collinearity(mod2) 
##Likelihood Ratio Test
anova(mod1, mod2) ##Interaction significant: 3.87, 0.049

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### LAST AUTHOR gender_2
reviewscoredat.gender.last<- subset(reviewscoredat, Demographic=="Gender_2" & Author.position == "Last" )
reviewscoredat.gender.last$mean_year<-(reviewscoredat.gender.last$Year - 1988.75)

##see the general distribution
hist(reviewscoredat.gender.last$Reviewer_rescaled, breaks = 4)

##look at gaussian and beta distributions
##this dataset only has 2 studies but one is Squazzoni et al. that has ~82,000 manuscripts and 79 journals
##JIF provided at 1 unit intervals. Include JIF as fixed effect
##run model with interaction term with reviewer demographic
modgaus <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Reviewer.demographic + Journal.impact.factor +
                     Study #two studies but includes Squazzoni with JIF intervals
                   , data = reviewscoredat.gender.last, family = gaussian)
summary(modgaus)
##check gaussian assumptions
testDispersion(modgaus)
simulationOutput <- simulateResiduals(fittedModel = modgaus, plot = F)
plot(simulationOutput) #fine

modbeta <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Reviewer.demographic + Journal.impact.factor +
                     Study #two studies but includes Squazzoni with JIF intervals
                   , data = reviewscoredat.gender.last, family=beta_family(link = "logit"))
summary(modbeta) ##beta seems good

##see if beta or gaussian are better
AICctab(modgaus, modbeta, weights=TRUE) ##gaus slightly better at 1.2 AICc
#conclusions same between 2 models; beta for consistency 

##use beta for inference
mod1 <- glmmTMB(Reviewer_rescaled ~ Demographic.category * Reviewer.demographic + Journal.impact.factor +
                  Study #two studies but includes Squazzoni with JIF intervals
                , data = reviewscoredat.gender.last, family=beta_family(link = "logit"))
summary(mod1)

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(Reviewer_rescaled ~ Demographic.category + Reviewer.demographic + Journal.impact.factor +
                  Study #two studies but includes Squazzoni with JIF intervals
                , data = reviewscoredat.gender.last, family=beta_family(link = "logit"))
#check for multicollinearity issue on model without interaction term
check_collinearity(mod2) ##fine - highest VIF = 1.01
##Likelihood Ratio Test
anova(mod1, mod2) ##Interaction not significant: 0.13, 0.72

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)



#####################################################
#####################################################
###REVIEW SCORES FOR REVIEWER HOMOPHILY
##################################################
#####################################################
###COUNTRY DATA

reviewscoredat <- read.csv(here("Meta Analysis", "Solution-end data", "Homophily", "Review scores solution diversity reviewer COUNTRY.csv"), fileEncoding="UTF-8-BOM")
head(reviewscoredat)

reviewscoredat$Demographic.category<- as.factor(reviewscoredat$Demographic.category)
reviewscoredat$Category<- as.factor(reviewscoredat$Category)
reviewscoredat$Subcategory<- as.factor(reviewscoredat$Subcategory)
reviewscoredat$Study<- as.factor(reviewscoredat$Study)
reviewscoredat$Reviewer.demographic<- as.factor(reviewscoredat$Reviewer.demographic)

###################################
###################################
### FIRST AUTHOR country
reviewscoredat.country.first<- subset(reviewscoredat, Author.country=="Known" & Author.position == "First" )
reviewscoredat.country.first$mean_year<-(reviewscoredat.country.first$Year - 1988.75)

##see the general distribution
hist(reviewscoredat.country.first$Reviewer_rescaled, breaks = 4)

##look at gaussian and beta distributions. Two studies so include study fixed effect
##JIF and year same as their respective studies
##unlike other homophily datasets, this is coded as "match"/"not match" ahead, so we're looking at that
##instead of an interaction. This is because of the large number of countries and few to no replicates within countries 
modgaus <- glmmTMB(Reviewer_rescaled ~ Reviewer.demographic + Study
                   #two studies with 14 points total
                   , data = reviewscoredat.country.first, family = gaussian)
summary(modgaus)
##check gaussian assumptions
check_collinearity(modgaus)
testDispersion(modgaus)
simulationOutput <- simulateResiduals(fittedModel = modgaus, plot = F)
plot(simulationOutput) #dispersion and deviation fine. 
#error: We had to increase `err` for some of the quantiles. See fit$calibr$err. 
#probably due to few reps 
#Not getting error anymore on run on 11/22/2022

modbeta <- glmmTMB(Reviewer_rescaled ~  Reviewer.demographic + Study
                   #two studies with 14 points total
                   , data = reviewscoredat.country.first, family=beta_family(link = "logit"))
summary(modbeta) ##beta seems good

##see if beta or gaussian are better
AICctab(modgaus, modbeta, weights=TRUE)
##beta much better; dAICc = 13.9

##use beta for inference
mod1 <- glmmTMB(Reviewer_rescaled ~  Reviewer.demographic + Study
                #two studies with 14 points total
                , data = reviewscoredat.country.first, family=beta_family(link = "logit"))
summary(mod1)

##estimates for figures
emmeans(mod1, c("Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### CORRESPONDING AUTHOR country
reviewscoredat.country.corresponding<- subset(reviewscoredat, Author.country=="Known" & Author.position == "Corresponding" )
reviewscoredat.country.corresponding$mean_year<-(reviewscoredat.country.corresponding$Year - 1988.75)

##look at general distribution
hist(reviewscoredat.country.corresponding$Reviewer_rescaled, breaks = 4)

##4 studies in this category so include JIF (fixed), mean year (fixed), and study (random)
modgaus <- glmmTMB(Reviewer_rescaled ~ Reviewer.demographic + Journal.impact.factor +  #4 studies
                     mean_year + (1|Study), data = reviewscoredat.country.corresponding, family = gaussian)
summary(modgaus)
##check gaussian assumptions
check_collinearity(modgaus) #fine - max VIF = 1.08
testDispersion(modgaus)
simulationOutput <- simulateResiduals(fittedModel = modgaus, plot = F)
plot(simulationOutput) #fine

##4 studies in this category so include JIF (fixed), mean year (fixed), and study (random)
modbeta <- glmmTMB(Reviewer_rescaled ~  Reviewer.demographic+ Journal.impact.factor +  #4 studies
                     mean_year + (1|Study), data = reviewscoredat.country.corresponding, family=beta_family(link = "logit"))
check_collinearity(modbeta)
summary(modbeta)

##see if beta or gaussian are better
AICctab(modgaus, modbeta, weights=TRUE)
##beta much better; dAICc = 9.4

##use beta for inference
mod1 <- glmmTMB(Reviewer_rescaled ~  Reviewer.demographic + Journal.impact.factor +  #4 studies
                  mean_year + (1|Study), data = reviewscoredat.country.corresponding, family=beta_family(link = "logit"))
summary(mod1)

#estimates for figures
emmeans(mod1, c("Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### LAST AUTHOR country
reviewscoredat.country.last<- subset(reviewscoredat, Author.country=="Known" & Author.position == "Last" )
reviewscoredat.country.last$mean_year<-(reviewscoredat.country.last$Year - 1988.75)

##look at general distribution 
hist(reviewscoredat.country.last$Reviewer_rescaled, breaks = 4)

##only 1 study in this category so JIF/mean year the same for all manuscripts
modgaus <- glmmTMB(Reviewer_rescaled ~ Reviewer.demographic # 1 study
                   , data = reviewscoredat.country.last, family = gaussian)
summary(modgaus)
testDispersion(modgaus)
simulationOutput <- simulateResiduals(fittedModel = modgaus, plot = F)
plot(simulationOutput) #dispersion and deviation fine but 
##error: Unable to calculate quantile regression for quantile 0.25. 
#Possibly to few (unique) data points / predictions. Will be ommited in plots and significance calculations.

modbeta <- glmmTMB(Reviewer_rescaled ~  Reviewer.demographic # 1 study
                   , data = reviewscoredat.country.last, family=beta_family(link = "logit"))
summary(modbeta)

##see if beta or gaussian are better
AICctab(modgaus, modbeta, weights=TRUE)
##beta much better; dAICc = 17.4

##use beta for inference
mod1 <- glmmTMB(Reviewer_rescaled ~ Reviewer.demographic # 1 study
                , data = reviewscoredat.country.last, family=beta_family(link = "logit"))
summary(mod1)

##estimates for figures
emmeans(mod1, c("Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
###NUMBER OF REVISIONS
###REVIEWER HOMOPHILY
##################################################
#####################################################

#####################################################
### Load the number revisions data 
nrevisionsdat <- read.csv(here("Meta Analysis", "Solution-end data", "Homophily", "Number revisions solution reviewer diversity.csv"))
head(nrevisionsdat)

nrevisionsdat$Study<-as.factor(nrevisionsdat$Study)
nrevisionsdat$Demographic.category<-as.factor(nrevisionsdat$Demographic.category)
nrevisionsdat$Reviewer.demographic<-as.factor(nrevisionsdat$Reviewer.demographic)

nrevisionsdat.gender.first<- subset(nrevisionsdat, Author.position == "First" )
nrevisionsdat.gender.corr<- subset(nrevisionsdat, Author.position == "Corresponding" )
nrevisionsdat.gender.last<- subset(nrevisionsdat, Author.position == "Last" )

##########################
##########################
#####FIRST AUTHOR gender_2

##see the general distribution
hist(nrevisionsdat.gender.first$N.revisions)

##This dataset has 2 studies. One is Squazzoni et al. that has ~85,000 manuscripts and 79 journals
##JIF provided at 1 unit intervals. Include JIF as fixed effect and study as fixed effect
##run model with interaction term with reviewer demographic
mod1 <- glmmTMB(N.revisions ~ Demographic.category * Reviewer.demographic + Journal.impact.factor + 
                  Study, data = nrevisionsdat.gender.first, family = gaussian)
summary(mod1)
##check assumptions
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) #No problems detected

##remove interaction for Likelihood Ratio Test and multicollinearity check
mod2 <- glmmTMB(N.revisions ~ Demographic.category + Reviewer.demographic + Journal.impact.factor + 
                  Study, data = nrevisionsdat.gender.first, family = gaussian)
##check for multicollinearity issue
check_collinearity(mod2) #fine - max VIF = 1.14
##Likelihood Ratio Test
anova(mod1, mod2) #author*reviewer not significant: 0.061, 0.80

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

##########################
##########################
#####LAST AUTHOR gender_2

##see the general distribution
hist(nrevisionsdat.gender.last$N.revisions)

##This dataset has 1 study, Squazzoni et al., that has ~82,000 manuscripts and 79 journals
##JIF provided at 1 unit intervals. Include JIF as fixed effect
##run model with interaction term with reviewer demographic
mod1 <- glmmTMB(N.revisions ~ Demographic.category * Reviewer.demographic + Journal.impact.factor  
                # 1 study - Squazzoni which has rows by JIF
                , data = nrevisionsdat.gender.last, family = gaussian)
summary(mod1)
##check model assumptions
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) #No problems detected

##remove interaction for Likelihood Ratio Test
mod2 <- glmmTMB(N.revisions ~ Demographic.category + Reviewer.demographic + Journal.impact.factor  
                # 1 study - Squazzoni which has rows by JIF
                , data = nrevisionsdat.gender.last, family = gaussian)
check_collinearity(mod2)
##Likelihood Ratio Test
anova(mod1, mod2) #author * reviewer not significant: 0.0010, 0.97

##estimates for figures
emmeans(mod1, c("Demographic.category", "Reviewer.demographic"), type = "response")

##visualize data
plot_model(mod1, type = "pred", terms = c("Demographic.category", "Reviewer.demographic"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)
