#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Analysis of bias data 
# Olivia Smith, with data formatting code by Wendy Leuenberger
# Date first created: April 2022
# Date last checked: 8-10 August 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

############ Set-up
############
### Load packages
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

# set working directory
setwd("D:/Olivia/Desktop/olivia.m.smith/Desktop/EEB CRG/Obj. 1 and 3/Analyzable data/Moving towards analyzable files/Problem end/Checked")
#change this to yours

# Load data
preinitialdatog <- read.csv('Pre initial decision problem.csv')
#preinitialdatog %>% View

# For each line of data, duplicate based on Rejected and 
# Went through to next stage

# Function! ####
StudyToPaper <- function(Data, 
                         ColumnNames = c('Rejected', 
                                         'Went through to next stage'), 
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
  # Wouldn't work if >2 columns, shouldn't be a preinitialdatog
  if(dim(SuperLongData)[1] != sum(LongData[[ValuesTo]])){
    stop(error = 'Number of rows does not match sum of papers')
  }
  
  return(SuperLongData)
}

# Make the conversion
# preinitialdat decision preinitialdatog
preinitialdat <- preinitialdatog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

##check if total papers adds up to total in datasheet
Original <- preinitialdatog
New <- preinitialdat
dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])

###############
###############

# set working directory
setwd("D:/Olivia/Desktop/olivia.m.smith/Desktop/EEB CRG/Obj. 1 and 3/Analyzable data/Moving towards analyzable files/Problem end/Checked")
#change this to yours

# Load data
initialdatog <- read.csv('Initial decision problem.csv')
#initialdatog %>% View

# For each line of data, duplicate based on Rejected and 
# Went through to next stage

# Function! ####
StudyToPaper <- function(Data, 
                         ColumnNames = c('Rejected', 
                                         'Went through to next stage'), 
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
  # Wouldn't work if >2 columns, shouldn't be a initialdatog
  if(dim(SuperLongData)[1] != sum(LongData[[ValuesTo]])){
    stop(error = 'Number of rows does not match sum of papers')
  }
  
  return(SuperLongData)
}

# Make the conversion
# initialdat decision initialdatog
initialdat <- initialdatog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

##check if total papers adds up to total in datasheet
Original <- initialdatog
New <- initialdat
dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])

###############
###############

# set working directory
setwd("D:/Olivia/Desktop/olivia.m.smith/Desktop/EEB CRG/Obj. 1 and 3/Analyzable data/Moving towards analyzable files/Problem end/Checked")
#change this to yours

# Load data
postinitialreviewdatog <- read.csv('Post initial review problem.csv')
#postinitialreviewdatog %>% View

# For each line of data, duplicate based on Rejected and 
# Went through to next stage

# Function! ####
StudyToPaper <- function(Data, 
                         ColumnNames = c('Rejected', 
                                         'Went through to next stage'), 
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
  # Wouldn't work if >2 columns, shouldn't be a postinitialreviewdatog
  if(dim(SuperLongData)[1] != sum(LongData[[ValuesTo]])){
    stop(error = 'Number of rows does not match sum of papers')
  }
  
  return(SuperLongData)
}

# Make the conversion
# postinitialreviewdat decision postinitialreviewdatog
postinitialreviewdat <- postinitialreviewdatog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

##check if total papers adds up to total in datasheet
Original <- postinitialreviewdatog
New <- postinitialreviewdat
dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])


###############
###############

# set working directory
setwd("D:/Olivia/Desktop/olivia.m.smith/Desktop/EEB CRG/Obj. 1 and 3/Analyzable data/Moving towards analyzable files/Problem end/Checked")
#change this to yours

# Load data
finaldatog <- read.csv('Final decision problem.csv')
#finaldatog %>% View

# For each line of data, duplicate based on Rejected and 
# Went through to next stage

# Function! ####
StudyToPaper <- function(Data, 
                         ColumnNames = c('Rejected', 
                                         'Went through to next stage'), 
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
  # Wouldn't work if >2 columns, shouldn't be a finaldatog
  if(dim(SuperLongData)[1] != sum(LongData[[ValuesTo]])){
    stop(error = 'Number of rows does not match sum of papers')
  }
  
  return(SuperLongData)
}

# Make the conversion
# finaldat decision finaldatog
finaldat <- finaldatog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

##check if total papers adds up to total in datasheet
Original <- finaldatog
New <- finaldat
dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])


###############
###############


# set working directory
setwd("D:/Olivia/Desktop/olivia.m.smith/Desktop/EEB CRG/Obj. 1 and 3/Analyzable data/Moving towards analyzable files/Problem end/Checked")
#change this to yours

# Load data
overalldatog <- read.csv('Overall decision problem.csv')
#overalldatog %>% View

# For each line of data, duplicate based on Rejected and 
# Went through to next stage

# Function! ####
StudyToPaper <- function(Data, 
                         ColumnNames = c('Rejected', 
                                         'Went through to next stage'), 
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
  # Wouldn't work if >2 columns, shouldn't be a overalldatog
  if(dim(SuperLongData)[1] != sum(LongData[[ValuesTo]])){
    stop(error = 'Number of rows does not match sum of papers')
  }
  
  return(SuperLongData)
}

# Make the conversion
# Overall decision overalldatog
overalldat <- overalldatog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

##check if total papers adds up to total in datasheet
Original <- overalldatog
New <- overalldat
dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])


#####################################################
#####################################################
##Analyses
#####################################################
##PRE-INITIAL DECISION
#####################################################

head(preinitialdat)

preinitialdat$Demographic.category<- as.factor(preinitialdat$Demographic.category)
preinitialdat$Category<- as.factor(preinitialdat$Category)
preinitialdat$Subcategory<- as.factor(preinitialdat$Subcategory)
preinitialdat$Study<- as.factor(preinitialdat$Study)

####################Subset preinitial data by demographic and position##################

###################################
###################################
### Gender for first author
preinitialdat.gender.first<- subset(preinitialdat, Demographic=="Gender" & Author.position == "First" )
preinitialdat.gender.first$mean_year<-(preinitialdat.gender.first$Year - 1988.75)

##this dataset only has 1 study and one comparison
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category, data = preinitialdat.gender.first, family = binomial)
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Gender for corresponding author
preinitialdat.gender.corresponding<- subset(preinitialdat, Demographic=="Gender" & Author.position == "Corresponding" )
preinitialdat.gender.corresponding$mean_year<-(preinitialdat.gender.corresponding$Year - 1988.75)

##this dataset only has 1 study and one comparison
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category, data = preinitialdat.gender.corresponding, family = binomial)
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Gender for last author
preinitialdat.gender.last<- subset(preinitialdat, Demographic=="Gender" & Author.position == "Last" )
preinitialdat.gender.last$mean_year<-(preinitialdat.gender.last$Year - 1988.75)

##this dataset only has 1 study and one comparison
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category, data = preinitialdat.gender.last, family = binomial)
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
#####################################################
#####################################################
### INITIAL DECISION

head(initialdat)

initialdat$Demographic.category<- as.factor(initialdat$Demographic.category)
initialdat$Category<- as.factor(initialdat$Category)

####################Subset initial data by demographic and position##################

###################################
###################################
### Gender for first author
initialdat.gender.first<- subset(initialdat, Demographic=="Gender" & Author.position == "First" )
initialdat.gender.first$mean_year<-(initialdat.gender.first$Year - 1988.75)

##
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category + Journal.impact.factor + mean_year +
                  + (1|Study), data = initialdat.gender.first, family = binomial)
#More than two studies so need to check for collinearity issues
check_collinearity(mod1) ##looks fine. Max 1.13
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Gender for corresponding author
initialdat.gender.corresponding<- subset(initialdat, Demographic=="Gender" & Author.position == "Corresponding" )
initialdat.gender.corresponding$mean_year<-(initialdat.gender.corresponding$Year - 1988.75)

##
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category + Journal.impact.factor + 
                  mean_year + (1|Study), data = initialdat.gender.corresponding, family = binomial)
#More than two studies so need to check for collinearity issues
check_collinearity(mod1) ##fine - max 1.38
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Gender for last author
initialdat.gender.last<- subset(initialdat, Demographic=="Gender" & Author.position == "Last" )
initialdat.gender.last$mean_year<-(initialdat.gender.last$Year - 1988.75)

##Two studies so don't include year/JIF, just study 
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category + Study, data = initialdat.gender.last, family = binomial)
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

#####################################################
#####################################################
#####################################################
#####################################################
### POST INITIAL REVIEW DATA
#####

head(postinitialreviewdat)

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

##
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category +
                  Journal.impact.factor + mean_year + (1|Study), data = postinitialreviewdat.gender.first, family = binomial)
#More than two studies so need to check for collinearity issues
check_collinearity(mod1) ##low correlation. Max = 1.00
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Gender for corresponding author
postinitialreviewdat.gender.corresponding<- subset(postinitialreviewdat, Demographic=="Gender" & Author.position == "Corresponding" )
postinitialreviewdat.gender.corresponding$mean_year<-(postinitialreviewdat.gender.corresponding$Year - 1988.75)

##
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category +
                  Journal.impact.factor + mean_year + (1|Study), data = postinitialreviewdat.gender.corresponding, family = binomial)
#More than two studies so need to check for collinearity issues
check_collinearity(mod1) ##high correlation between year and JIF (JIF VIF = 123.73, year VIF = 123.71)

#run with just JIF or year
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category +
                  Journal.impact.factor + (1|Study), data = postinitialreviewdat.gender.corresponding, family = binomial)
mod3 <- glmmTMB(OutcomeBinary ~ Demographic.category +
                  mean_year + (1|Study), data = postinitialreviewdat.gender.corresponding, family = binomial)
summary(mod2)
summary(mod3)

#see if model with JIF or year is better
AICctab(mod2, mod3, weights=TRUE) ##the model with journal impact factor is 4.8 AICc better

##make inference from the model with JIF 
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category +
                  Journal.impact.factor + (1|Study), data = postinitialreviewdat.gender.corresponding, family = binomial)
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Gender for last author
postinitialreviewdat.gender.last<- subset(postinitialreviewdat, Demographic=="Gender" & Author.position == "Last" )
postinitialreviewdat.gender.last$mean_year<-(postinitialreviewdat.gender.last$Year - 1988.75)

##
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category +
                  Journal.impact.factor + mean_year + (1|Study), data = postinitialreviewdat.gender.last, family = binomial)
#More than two studies so need to check for collinearity issues
check_collinearity(mod1) ##low correlation - max VIF = 1.00
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

#####################################################
#####################################################
#####################################################
#####################################################
### FINAL DECISION DATA

head(finaldat)

finaldat$Demographic.category<- as.factor(finaldat$Demographic.category)
finaldat$Category<- as.factor(finaldat$Category)
finaldat$Subcategory<- as.factor(finaldat$Subcategory)
finaldat$Study<- as.factor(finaldat$Study)

####################Subset final decision data by demographic and position##################

###################################
###################################
### Gender for first author
finaldat.gender.first<- subset(finaldat, Demographic=="Gender" & Author.position == "First" )
finaldat.gender.first$mean_year<-(finaldat.gender.first$Year - 1988.75)

##
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category +  
                  Journal.impact.factor + mean_year + (1|Study), data = finaldat.gender.first, family = binomial)
#More than two studies so need to check for collinearity issues
check_collinearity(mod1) ##check for collinearity issue; highest VIF = 1
summary(mod1) ##look at model
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Gender for corresponding author
finaldat.gender.corresponding<- subset(finaldat, Demographic=="Gender" & Author.position == "Corresponding" )
finaldat.gender.corresponding$mean_year<-(finaldat.gender.corresponding$Year - 1988.75)

##
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category +  
                  Journal.impact.factor + mean_year + (1|Study), data = finaldat.gender.corresponding, family = binomial)
#More than two studies so need to check for collinearity issues
check_collinearity(mod1) ##check for collinearity issue; fine: highest VIF = 1.30
summary(mod1) ##look at model
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Gender for last author
finaldat.gender.last<- subset(finaldat, Demographic=="Gender" & Author.position == "Last" )
finaldat.gender.last$mean_year<-(finaldat.gender.last$Year - 1988.75)

##
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category +  
                  Journal.impact.factor + mean_year + (1|Study), data = finaldat.gender.last, family = binomial)
#More than two studies so need to check for collinearity issues
check_collinearity(mod1) ##check for collinearity issue; fine: highest VIF = 1.00
summary(mod1) ##look at model
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)



#####################################################
#####################################################
#####################################################
#####################################################
### OVERALL DECISION
###

overalldat$Demographic.category<- as.factor(overalldat$Demographic.category)
overalldat$Category<- as.factor(overalldat$Category)
overalldat$Subcategory<- as.factor(overalldat$Subcategory)
overalldat$Study<- as.factor(overalldat$Study)

####################Subset overall data by demographic and position##################

###################################
###################################
### Gender for first author
overalldat.gender.first<- subset(overalldat, Demographic=="Gender" & Author.position == "First" )
overalldat.gender.first$mean_year<-(overalldat.gender.first$Year - 1988.75)

##
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category +
                  Journal.impact.factor + mean_year + (1|Study), data = overalldat.gender.first, family = binomial)
check_collinearity(mod1) ##check for collinearity issue; fine
summary(mod1) ##look at model
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### Gender for corresponding author
overalldat.gender.corresponding<- subset(overalldat, Demographic=="Gender" & Author.position == "Corresponding" )
overalldat.gender.corresponding$mean_year<-(overalldat.gender.corresponding$Year - 1988.75)

##
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category +
                  Journal.impact.factor + mean_year + (1|Study), data = overalldat.gender.corresponding, family = binomial)
check_collinearity(mod1) ##check for collinearity issue; fine
summary(mod1) ##look at model
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Gender for last author
overalldat.gender.last<- subset(overalldat, Demographic=="Gender" & Author.position == "Last" )
overalldat.gender.last$mean_year<-(overalldat.gender.last$Year - 1988.75)

##this dataset only has 1 study so JIF and mean year are the same values for all manuscripts
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category, data = overalldat.gender.last, family = binomial)
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
#####################################################
#####################################################
###CONTINENT
#####################################################
#####################################################
#####################################################
#####################################################
### 

####################Subset preinitial data by demographic and position##################

###################################
###################################
### Continent for first author
preinitialdat.Continent.first<- subset(preinitialdat, Demographic=="Continent" & Author.position == "First" )
preinitialdat.Continent.first$mean_year<-(preinitialdat.Continent.first$Year - 1988.75)

##just one study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category
                , data = preinitialdat.Continent.first, family = binomial)
summary(mod1)
confint(mod1)

modlrt <- glmmTMB(OutcomeBinary ~ 1 # just one study
                  , data = preinitialdat.Continent.first, family = binomial)
#likelihood ratio test for categorical variable 
anova(mod1, modlrt) #continent improves model

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Continent for corresponding author
preinitialdat.Continent.corresponding<- subset(preinitialdat, Demographic=="Continent" & Author.position == "Corresponding" )
preinitialdat.Continent.corresponding$mean_year<-(preinitialdat.Continent.corresponding$Year - 1988.75)

##just one study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category 
                , data = preinitialdat.Continent.corresponding, family = binomial)
summary(mod1)
confint(mod1)

modlrt <- glmmTMB(OutcomeBinary ~ 1 # just one study
                  , data = preinitialdat.Continent.corresponding, family = binomial)
#likelihood ratio test for categorical variable 
anova(mod1, modlrt) #continent improves model

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Continent for last author
preinitialdat.Continent.last<- subset(preinitialdat, Demographic=="Continent" & Author.position == "Last" )
preinitialdat.Continent.last$mean_year<-(preinitialdat.Continent.last$Year - 1988.75)

##just one study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category
                , data = preinitialdat.Continent.last, family = binomial)
summary(mod1)
confint(mod1)

modlrt <- glmmTMB(OutcomeBinary ~ 1 # just one study
                  , data = preinitialdat.Continent.last, family = binomial)
#likelihood ratio test for categorical variable 
anova(mod1, modlrt) #continent improves model

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

#####################################################
#####################################################
#####################################################
#####################################################
### 

####################Subset initial data by demographic and position##################

###################################
###################################
### Continent for first author
initialdat.Continent.first<- subset(initialdat, Demographic=="Continent" & Author.position == "First" )
initialdat.Continent.first$mean_year<-(initialdat.Continent.first$Year - 1988.75)

##just one study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category 
                , data = initialdat.Continent.first, family = binomial)
summary(mod1)
confint(mod1)

modlrt <- glmmTMB(OutcomeBinary ~ 1 # just one study
                  , data = initialdat.Continent.first, family = binomial)
#likelihood ratio test for categorical variable 
anova(mod1, modlrt) #continent improves model

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Continent for corresponding author
initialdat.Continent.corresponding<- subset(initialdat, Demographic=="Continent" & Author.position == "Corresponding" )
initialdat.Continent.corresponding$mean_year<-(initialdat.Continent.corresponding$Year - 1988.75)

##see if there's any multicollinearity issue
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category +  
                  Journal.impact.factor + mean_year + (1|Study), data = initialdat.Continent.corresponding, family = binomial)
check_collinearity(mod1) ##this one has high collinearity - year and JIF correlated (highest 14.31)

##see if JIF or mean year is better to include
mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category +
                  Journal.impact.factor + (1|Study), data = initialdat.Continent.corresponding, family = binomial)
mod3 <- glmmTMB(OutcomeBinary ~ Demographic.category +
                  mean_year + (1|Study), data = initialdat.Continent.corresponding, family = binomial)
summary(mod2)
summary(mod3)

AICctab(mod2, mod3, weights=TRUE) ##the model with mean year is 2.5 AICc better

##make inference from the model with mean year 
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category +
                  mean_year + (1|Study), data = initialdat.Continent.corresponding, family = binomial)
summary(mod1) 
confint(mod1)

##model without continent for LRT
modlrt <- glmmTMB(OutcomeBinary ~ mean_year + (1|Study), data = initialdat.Continent.corresponding, family = binomial)
anova(mod1, modlrt)

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize differences
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### Continent for last author
initialdat.Continent.last<- subset(initialdat, Demographic=="Continent" & Author.position == "Last" )
initialdat.Continent.last$mean_year<-(initialdat.Continent.last$Year - 1988.75)

##just one study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category 
                , data = initialdat.Continent.last, family = binomial)
summary(mod1)
confint(mod1)

modlrt <- glmmTMB(OutcomeBinary ~ 1 # just one study
                  , data = initialdat.Continent.last, family = binomial)
#likelihood ratio test for categorical variable 
anova(mod1, modlrt) #continent improves model P < 0.0001

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1) 

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
#####################################################
#####################################################
###

####################Subset post initial review data by demographic and position##################

###################################
###################################
### Continent for first author
postinitialreviewdat.Continent.first<- subset(postinitialreviewdat, Demographic=="Continent" & Author.position == "First" )
postinitialreviewdat.Continent.first$mean_year<-(postinitialreviewdat.Continent.first$Year - 1988.75)

##only two studies in this group so all levels of JIF and year are the same as study ID (include study instead)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category + #only two studies in this category
                  Study, data = postinitialreviewdat.Continent.first, family = binomial)
check_collinearity(mod1) #highest VIF = 1.03
summary(mod1)
confint(mod1)

##see if continent improves the model
modlrt <- glmmTMB(OutcomeBinary ~ #only two studies in this category
                    Study, data = postinitialreviewdat.Continent.first, family = binomial)
anova(mod1, modlrt)

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1) ##get error Warning message:
# In RET$pfunction("adjusted", ...) : Completion with error > abseps
# the error goes away if you run this with the intercept removed, and the numbers are the same to 0.001 
# we found some blogs online with the error that didn't comment on it being an issue

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize differences
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### Continent for corresponding author
postinitialreviewdat.Continent.corresponding<- subset(postinitialreviewdat, Demographic=="Continent" & Author.position == "Corresponding" )
postinitialreviewdat.Continent.corresponding$mean_year<-(postinitialreviewdat.Continent.corresponding$Year - 1988.75)

##only two studies in this group so all levels of JIF and year are the same as study ID (include study instead)
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category + #only two studies in this category
                  Study, data = postinitialreviewdat.Continent.corresponding, family = binomial)
check_collinearity(mod1) #highest VIF = 1.08
summary(mod1)
confint(mod1)

##see if continent improves the model
modlrt <- glmmTMB(OutcomeBinary ~ #only two studies in this category
                    Study, data = postinitialreviewdat.Continent.corresponding, family = binomial)
anova(mod1, modlrt)

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize differences
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### Continent for last author
postinitialreviewdat.Continent.last<- subset(postinitialreviewdat, Demographic=="Continent" & Author.position == "Last" )
postinitialreviewdat.Continent.last$mean_year<-(postinitialreviewdat.Continent.last$Year - 1988.75)

##this dataset only has 1 study and one comparison
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category, data = postinitialreviewdat.Continent.last, family = binomial)
summary(mod1) 
confint(mod1)

modlrt <- glmmTMB(OutcomeBinary ~ 1, data = postinitialreviewdat.Continent.last, family = binomial)
#likelihood ratio test for categorical variable 
anova(mod1, modlrt)

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1) ##get error Warning message:
# In RET$pfunction("adjusted", ...) : Completion with error > abseps
# the error goes away if you run this with the intercept removed, and the numbers are the same to 0.001 
# we found some blogs online with the error that didn't comment on it being an issue

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize differences
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
#####################################################
#####################################################
### 

####################Subset final decision data by demographic and position##################

###################################
###################################
### Continent for first author
finaldat.Continent.first<- subset(finaldat, Demographic=="Continent" & Author.position == "First" )
finaldat.Continent.first$mean_year<-(finaldat.Continent.first$Year - 1988.75)

##this dataset only has 1 study and one comparison so all levels of JIF and year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category, data = finaldat.Continent.first, family = binomial)
summary(mod1) 
confint(mod1)

modlrt <- glmmTMB(OutcomeBinary ~ 1 #just one study
                  , data = finaldat.Continent.first, family = binomial)
#likelihood ratio test for categorical variable 
anova(mod1, modlrt)

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize differences
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### Continent for corresponding author
finaldat.Continent.corresponding<- subset(finaldat, Demographic=="Continent" & Author.position == "Corresponding" )
finaldat.Continent.corresponding$mean_year<-(finaldat.Continent.corresponding$Year - 1988.75)

##this dataset only has 1 study and one comparison so all JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category, data = finaldat.Continent.corresponding, family = binomial)
summary(mod1) 
confint(mod1)

modlrt <- glmmTMB(OutcomeBinary ~ 1 #just one study
                  , data = finaldat.Continent.corresponding, family = binomial)
#likelihood ratio test for categorical variable 
anova(mod1, modlrt)

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize differences
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


####################Subset overall data by demographic and position##################

###################################
###################################
### Continent for first author
overalldat.Continent.first<- subset(overalldat, Demographic=="Continent" & Author.position == "First" )
overalldat.Continent.first$mean_year<-(overalldat.Continent.first$Year - 1988.75)

##this dataset only has 1 study and one comparison so all JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category # just one study
                , data = overalldat.Continent.first, family = binomial)
summary(mod1)

modlrt <- glmmTMB(OutcomeBinary ~ 1 # just one study
                  , data = overalldat.Continent.first, family = binomial)
#likelihood ratio test for categorical variable 
anova(mod1, modlrt) ##model with CONTINENT better 142.56, < 0.0001

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Continent for corresponding author
overalldat.Continent.corresponding<- subset(overalldat, Demographic=="Continent" & Author.position == "Corresponding" )
overalldat.Continent.corresponding$mean_year<-(overalldat.Continent.corresponding$Year - 1988.75)

##
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category + 
                  Journal.impact.factor + mean_year + (1|Study), data = overalldat.Continent.corresponding, family = binomial)
##5 studies so see if there's any multicollinearity issues
check_collinearity(mod1) ##fine - max VIF = 1.73
summary(mod1)

##see if continent improves fit with likelihood ratio test
modlrt <- glmmTMB(OutcomeBinary ~ Journal.impact.factor + mean_year +
                    + (1|Study), data = overalldat.Continent.corresponding, family = binomial)
anova(mod1, modlrt) ##continent improves model 1053.5, < 0.0001

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)



###############################################
###############################################
##############################################
###############################################
##############################################
##### ENGLISH PRIMARY LANGUAGE
###############################################
###############################################
##############################################
###############################################
##############################################

####################Subset initial data by demographic and position##################

###################################
###################################
### ESL_all for corresponding author
initialdat.ESL_all.corresponding<- subset(initialdat, Demographic=="ESL_all" & Author.position == "Corresponding" )
initialdat.ESL_all.corresponding$mean_year<-(initialdat.ESL_all.corresponding$Year - 1988.75)

###only one study in this category so can't do covariates since all JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category 
                , data = initialdat.ESL_all.corresponding, family = binomial)
summary(mod1)
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")


####################Subset overall data by demographic and position##################

###################################
###################################
### ESL_all for first author
overalldat.ESL_all.first<- subset(overalldat, Demographic=="ESL_all" & Author.position == "First" )
overalldat.ESL_all.first$mean_year<-(overalldat.ESL_all.first$Year - 1988.75)

###only one study in this category so can't do covariates since all JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category 
                , data = overalldat.ESL_all.first, family = binomial)
summary(mod1)
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### ESL_all for corresponding author
overalldat.ESL_all.corresponding<- subset(overalldat, Demographic=="ESL_all" & Author.position == "Corresponding" )
overalldat.ESL_all.corresponding$mean_year<-(overalldat.ESL_all.corresponding$Year - 1988.75)

##see if there's any multicollinearity issue since there are 4 studies in this category
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category + 
                  Journal.impact.factor + mean_year + (1|Study), data = overalldat.ESL_all.corresponding, family = binomial)
check_collinearity(mod1) #fine - highest VIF = 1.64
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)



####################################################
####################################################
####################################################
###########ESL similar HDI analyses

#####################################################
#####################################################
#####################################################
#####################################################
### 
####################Subset overall data by demographic and position##################

###################################
###################################
### ESL_similarHDI for first author
overalldat.ESL_similarHDI.first<- subset(overalldat, Demographic=="ESL_similarHDI" & Author.position == "First" )
overalldat.ESL_similarHDI.first$mean_year<-(overalldat.ESL_similarHDI.first$Year - 1988.75)

###only one study in this category so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category 
                , data = overalldat.ESL_similarHDI.first, family = binomial)
summary(mod1)
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### ESL_similarHDI for corresponding author
overalldat.ESL_similarHDI.corresponding<- subset(overalldat, Demographic=="ESL_similarHDI" & Author.position == "Corresponding" )
overalldat.ESL_similarHDI.corresponding$mean_year<-(overalldat.ESL_similarHDI.corresponding$Year - 1988.75)

##3 studies in this comparison so include JIF and year
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category + 
                  Journal.impact.factor + mean_year + (1|Study), data = overalldat.ESL_similarHDI.corresponding, family = binomial)
##check for multicollinearity issue
check_collinearity(mod1) #high collinearity (highest VIF = 9.66) - just 3 studies in this category

mod2 <- glmmTMB(OutcomeBinary ~ Demographic.category +
                  Journal.impact.factor + (1|Study), data = overalldat.ESL_similarHDI.corresponding, family = binomial)
mod3 <- glmmTMB(OutcomeBinary ~ Demographic.category +
                  mean_year + (1|Study), data = overalldat.ESL_similarHDI.corresponding, family = binomial)
summary(mod2)
summary(mod3)

AICctab(mod2, mod3, weights=TRUE) ##the model with jmean year 8.1 AICc better

mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category + 
                  mean_year + (1|Study), data = overalldat.ESL_similarHDI.corresponding, family = binomial)
summary(mod1)
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#########################################
###########################################
###################################
#######################################
##AUTHOR PRESTIGE
#########################################
###########################################
###################################
#######################################

####################Subset post initial review data by demographic and position##################

###################################
###################################
### Prestige_author for first author
postinitialreviewdat.Prestige_author.first<- subset(postinitialreviewdat, Demographic=="Prestige_author" & Author.position == "First" )
postinitialreviewdat.Prestige_author.first$mean_year<-(postinitialreviewdat.Prestige_author.first$Year - 1988.75)

##just two studies in this category so all levels of JIF and mean year are the same as their respective studies
##just two studies in this category so just study fixed effect 
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category + 
                  Study, data = postinitialreviewdat.Prestige_author.first, family = binomial)
check_collinearity(mod1) ##LOW - max VIF = 1.04
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Prestige_author for corresponding author
postinitialreviewdat.Prestige_author.corresponding<- subset(postinitialreviewdat, Demographic=="Prestige_author" & Author.position == "Corresponding" )
postinitialreviewdat.Prestige_author.corresponding$mean_year<-(postinitialreviewdat.Prestige_author.corresponding$Year - 1988.75)

##just two studies in this category so all levels of JIF and mean year are the same as their respective studies
##just two studies in this category so just study fixed effect 
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category + 
                  Study, data = postinitialreviewdat.Prestige_author.corresponding, family = binomial)
check_collinearity(mod1) ##LOW - max VIF = 1.04
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Prestige_author for last author
postinitialreviewdat.Prestige_author.last<- subset(postinitialreviewdat, Demographic=="Prestige_author" & Author.position == "Last" )
postinitialreviewdat.Prestige_author.last$mean_year<-(postinitialreviewdat.Prestige_author.last$Year - 1988.75)

##just one study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category,
                data = postinitialreviewdat.Prestige_author.last, family = binomial)
summary(mod1)
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
########Prestige_institution
#############################################
#####################################################
#####################################################
### 

####################Subset initial data by demographic and position##################

###################################
###################################
### Prestige_institution for corresponding author
initialdat.Prestige_institution.corresponding<- subset(initialdat, Demographic=="Prestige_institution" & Author.position == "Corresponding" )
initialdat.Prestige_institution.corresponding$mean_year<-(initialdat.Prestige_institution.corresponding$Year - 1988.75)

##just one study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category 
                , data = initialdat.Prestige_institution.corresponding, family = binomial)
summary(mod1)

modlrt <- glmmTMB(OutcomeBinary ~ 1 
                  , data = initialdat.Prestige_institution.corresponding, family = binomial)
#likelihood ratio test for categorical variable 
anova(mod1, modlrt) ##model with prestige better 1236.9, < 0.0001

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
#####################################################
#####################################################
### 

####################Subset post initial review data by demographic and position##################

###################################
###################################
### Prestige_institution for corresponding author
postinitialreviewdat.Prestige_institution.corresponding<- subset(postinitialreviewdat, Demographic=="Prestige_institution" & Author.position == "Corresponding" )
postinitialreviewdat.Prestige_institution.corresponding$mean_year<-(postinitialreviewdat.Prestige_institution.corresponding$Year - 1988.75)

##just one study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category 
                , data = postinitialreviewdat.Prestige_institution.corresponding, family = binomial)
summary(mod1)

modlrt <- glmmTMB(OutcomeBinary ~ 1 # just one study
                  , data = postinitialreviewdat.Prestige_institution.corresponding, family = binomial)
#likelihood ratio test for categorical variable 
anova(mod1, modlrt) ##model with prestige better 13.00, 0.0015

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
## All the data for overall decision for prestige_institution are from one study that is repeated 
##The study doesn't specify author position used. 
##The discussion text suggests that all papers were from single intuitions, so I repeated the rows for all authors. 

####################Subset overall data by demographic and position##################

overalldat.Prestige_institution.first<- subset(overalldat, Demographic=="Prestige_institution" & Author.position == "First" )
overalldat.Prestige_institution.first$mean_year<-(overalldat.Prestige_institution.first$Year - 1988.75)

##just one study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Demographic.category
                , data = overalldat.Prestige_institution.first, family = binomial)
summary(mod1)

modlrt <- glmmTMB(OutcomeBinary ~ 1 # just one study
                  , data = overalldat.Prestige_institution.first, family = binomial)
#likelihood ratio test for categorical variable 
anova(mod1, modlrt) ##model with prestige better 7.24, 0.027

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
#####################################################
#####################################################
### LOAD DATASET FOR JOURNAL NATIONALISM 
## SEPARATED DUE TO SLIGHTLY DIFFERENT FORMATTING BECAUSE OF THE JOURNAL LOCATION MATTERING

# Load data
alldatog <- read.csv('Journal nationalism problem binomial.csv')
#alldatog %>% View

# For each line of data, duplicate based on Rejected and 
# Went through to next stage

# Function! ####
StudyToPaper <- function(Data, 
                         ColumnNames = c('Rejected', 
                                         'Went through to next stage'), 
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
  # Wouldn't work if >2 columns, shouldn't be a alldatog
  if(dim(SuperLongData)[1] != sum(LongData[[ValuesTo]])){
    stop(error = 'Number of rows does not match sum of papers')
  }
  
  return(SuperLongData)
}

# Make the conversion
# alldat decision alldatog
alldat <- alldatog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

##check if total papers adds up to total in datasheet
Original <- alldatog
New <- alldat
dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])


###############
###############

alldat$Author.match<- as.factor(alldat$Author.match)
alldat$Study<- as.factor(alldat$Study)

#####################################################
#####################################################
#####################################################
#####################################################
### pre-initial decision

###################################
###################################
### nationalism for first author
preinitialdat.nationalism.first<- subset(alldat, Author.position == "First" & Stage == "Pre-initial decision")
preinitialdat.nationalism.first$mean_year<-(preinitialdat.nationalism.first$Year - 1988.75)

##this dataset only has 1 study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Author.match, data = preinitialdat.nationalism.first, family = binomial)
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Author.match"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Author.match"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### nationalism for corresponding author
preinitialdat.nationalism.corresponding<- subset(alldat, Author.position == "Corresponding" & Stage == "Pre-initial decision")
preinitialdat.nationalism.corresponding$mean_year<-(preinitialdat.nationalism.corresponding$Year - 1988.75)

##this dataset only has 1 study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Author.match, data = preinitialdat.nationalism.corresponding, family = binomial)
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Author.match"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Author.match"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### nationalism for last author
preinitialdat.nationalism.last<- subset(alldat, Author.position == "Last" & Stage == "Pre-initial decision")
preinitialdat.nationalism.last$mean_year<-(preinitialdat.nationalism.last$Year - 1988.75)

##this dataset only has 1 study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Author.match, data = preinitialdat.nationalism.last, family = binomial)
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Author.match"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Author.match"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

#####################################################
#####################################################
#####################################################
#####################################################
### INITIAL DECISION DATA

###################################
###################################
### nationalism for first author
initialdat.nationalism.first<- subset(alldat, Author.position == "First" & Stage == "Initial decision")
initialdat.nationalism.first$mean_year<-(initialdat.nationalism.first$Year - 1988.75)

##this dataset only has 1 study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Author.match, data = initialdat.nationalism.first, family = binomial)
summary(mod1) 
confint(mod1)

emmeans(mod1, c("Author.match"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Author.match"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### nationalism for corresponding author
initialdat.nationalism.corresponding<- subset(alldat, Author.position == "Corresponding" & Stage == "Initial decision")
initialdat.nationalism.corresponding$mean_year<-(initialdat.nationalism.corresponding$Year - 1988.75)

##this dataset only has 1 study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Author.match, data = initialdat.nationalism.corresponding, family = binomial)
summary(mod1) 
confint(mod1)
inv.logit(confint(mod1))

emmeans(mod1, c("Author.match"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Author.match"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### nationalism for last author
initialdat.nationalism.last<- subset(alldat, Author.position == "Last" & Stage == "Initial decision")
initialdat.nationalism.last$mean_year<-(initialdat.nationalism.last$Year - 1988.75)

##this dataset only has 1 study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Author.match, data = initialdat.nationalism.last, family = binomial)
summary(mod1) 
confint(mod1)
inv.logit(confint(mod1))

emmeans(mod1, c("Author.match"), type = "response")

plot_model(mod1, type = "pred", terms = c("Author.match"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
#####################################################
#####################################################
### POST-INITIAL REVIEW 

###################################
###################################
### nationalism for first author
postinitialreview.nationalism.first<- subset(alldat, Author.position == "First" & Stage == "Post initial review")
postinitialreview.nationalism.first$mean_year<-(postinitialreview.nationalism.first$Year - 1988.75)

##this dataset only has 1 study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Author.match, data = postinitialreview.nationalism.first, family = binomial)
summary(mod1) 
confint(mod1)
inv.logit(confint(mod1))

emmeans(mod1, c("Author.match"), type = "response")

plot_model(mod1, type = "pred", terms = c("Author.match"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### nationalism for corresponding author
postinitialreview.nationalism.corresponding<- subset(alldat, Author.position == "Corresponding" & Stage == "Post initial review")
postinitialreview.nationalism.corresponding$mean_year<-(postinitialreview.nationalism.corresponding$Year - 1988.75)

##this dataset only has 1 study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Author.match, data = postinitialreview.nationalism.corresponding, family = binomial)
summary(mod1) 
confint(mod1)
inv.logit(confint(mod1))

emmeans(mod1, c("Author.match"), type = "response")

plot_model(mod1, type = "pred", terms = c("Author.match"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### nationalism for last author
postinitialreview.nationalism.last<- subset(alldat, Author.position == "Last" & Stage == "Post initial review")
postinitialreview.nationalism.last$mean_year<-(postinitialreview.nationalism.last$Year - 1988.75)

##this dataset only has 1 study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Author.match, data = postinitialreview.nationalism.last, family = binomial)
summary(mod1) 
confint(mod1)
inv.logit(confint(mod1))

emmeans(mod1, c("Author.match"), type = "response")

plot_model(mod1, type = "pred", terms = c("Author.match"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### nationalism for corresponding author
overalldat.nationalism.corresponding<- subset(alldat, Author.position == "Corresponding" & Stage == "Overall decision")
overalldat.nationalism.corresponding$mean_year<-(overalldat.nationalism.corresponding$Year - 1988.75)

##3 studies so include JIF and year
mod1 <- glmmTMB(OutcomeBinary ~ Author.match + Journal.impact.factor 
                + mean_year + Journal.impact.factor + (1|Study), data = overalldat.nationalism.corresponding, family = binomial)
##see if there's any multicollinearity issue
check_collinearity(mod1) ##fine - highest VIF = 1.94
summary(mod1) 
confint(mod1)
inv.logit(confint(mod1))

emmeans(mod1, c("Author.match"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Author.match"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

####################
###################
##FORMAT DATA TO ANALYZE AT THE INDIVIDUAL COUNTRY LEVEL
###################
###################

# Load data
overalldatog <- read.csv('Overall decision problem COUNTRY.csv')
#overalldatog %>% View

# For each line of data, duplicate based on Rejected and 
# Went through to next stage

# Function! ####
StudyToPaper <- function(Data, 
                         ColumnNames = c('Rejected', 
                                         'Went through to next stage'), 
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
  # Wouldn't work if >2 columns, shouldn't be a overalldatog
  if(dim(SuperLongData)[1] != sum(LongData[[ValuesTo]])){
    stop(error = 'Number of rows does not match sum of papers')
  }
  
  return(SuperLongData)
}

# Make the conversion
# overalldat decision overalldatog
overalldat <- overalldatog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

##check if total papers adds up to total in datasheet
Original <- overalldatog
New <- overalldat
dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])

########

# set working directory
setwd("D:/Olivia/Desktop/olivia.m.smith/Desktop/EEB CRG/Obj. 1 and 3/Analyzable data/Moving towards analyzable files/Problem end/Checked")
#change this to yours

# Load data
initialdatog <- read.csv('Initial decision problem COUNTRY.csv')
#initialdatog %>% View

# For each line of data, duplicate based on Rejected and 
# Went through to next stage

# Function! ####
StudyToPaper <- function(Data, 
                         ColumnNames = c('Rejected', 
                                         'Went through to next stage'), 
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
  # Wouldn't work if >2 columns, shouldn't be a initialdatog
  if(dim(SuperLongData)[1] != sum(LongData[[ValuesTo]])){
    stop(error = 'Number of rows does not match sum of papers')
  }
  
  return(SuperLongData)
}

# Make the conversion
# initialdat decision initialdatog
initialdat <- initialdatog %>% 
  StudyToPaper(ColumnNames = c('Rejected', 'Went.through.to.next.stage'),
               NamesTo = 'Outcome', ValuesTo = 'NumPapers',
               Binary0 = 'Rejected')

##check if total papers adds up to total in datasheet
Original <- initialdatog
New <- initialdat
dim(New)[1]
sum(Original[,c('Rejected', 'Went.through.to.next.stage')])

##############

overalldat$Country<- as.factor(overalldat$Country)
overalldat$Study<- as.factor(overalldat$Study)
overalldat$Continent<- as.factor(overalldat$Continent)
overalldat$Language<- as.factor(overalldat$Language)
overalldat$Category<- as.factor(overalldat$Category)

#####################################################
#####################################################
#####################################################
#####################################################
### initial decision

###################################
###################################
### country for corresponding author
initialdat.country.corresponding<- subset(initialdat, Author.position == "Corresponding" )
initialdat.country.corresponding$mean_year<-(initialdat.country.corresponding$Year - 1988.75)

initialdat.country.corresponding$Continent<-as.factor(initialdat.country.corresponding$Continent)

##Just 1 study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Continent + Language + HDI 
                , data = initialdat.country.corresponding, family = binomial)
check_collinearity(mod1) #really high VIF 11338931192.12!

##try models with combinations of 2 of the 3
mod1 <- glmmTMB(OutcomeBinary ~ Language + HDI ##just 1 study 
                , data = initialdat.country.corresponding, family = binomial)
#see if there's any multicollinearity issue
check_collinearity(mod1) #highly correlated - VIF = 35.44

mod1 <- glmmTMB(OutcomeBinary ~ Continent + HDI ##just 1 study
                , data = initialdat.country.corresponding, family = binomial)
check_collinearity(mod1) #highly correlated - VIF = 89.89

mod1 <- glmmTMB(OutcomeBinary ~ Continent + Language ##just 1 study - not converging 
                , data = initialdat.country.corresponding, family = binomial)
check_collinearity(mod1) #warning since it didn't converge

###run individually since the models with all 3 variables or even combos of 2 won't work
mod1 <- glmmTMB(OutcomeBinary ~ Continent, data = initialdat.country.corresponding, family = binomial)
summary(mod2) ##look at model
confint(mod2)

mod2 <- glmmTMB(OutcomeBinary ~ Language, data = initialdat.country.corresponding, family = binomial)
summary(mod2) ##look at model
confint(mod2)

mod3 <- glmmTMB(OutcomeBinary ~ HDI, data = initialdat.country.corresponding, family = binomial)
summary(mod3) ##look at model
confint(mod3)

##just curious if continent, language, or HDI are more important. 
#Null model for AICc and Likelihood Ratio Test comparison
mod4 <- glmmTMB(OutcomeBinary ~ 1, data = initialdat.country.corresponding, family = binomial)

AICctab(mod1, mod2, mod3, mod4, weights=TRUE)
##mod1 with continent best by dAICc = 2.1 

##likelihood ratio tests on covariates
anova(mod1, mod4) #continent improves model 66.07, < 0.0001
anova(mod2, mod4) #language improves model 60.01, < 0.0001
anova(mod3, mod4) #HDI improves model 57.82, < 0.0001

##since continent has more than 2 categories, do Tukey HSD
mod1 <- glmmTMB(OutcomeBinary ~ Continent, data = initialdat.country.corresponding, family = binomial)
##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Continent = "Tukey")) 
summary(g1)

##visualize models
plot_model(mod1, type = "pred", terms = c("Continent"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

plot_model(mod2, type = "pred", terms = c("Language"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

plot_model(mod3, type = "pred", terms = c("HDI"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

##plot and export figure looking at HDI (x) against initial decisions (y)
##exports to working directory
###create plot used in the main text. Note: axis labels made to look better in PowerPoint
tiff("hdi initial corr.tiff", width = 2.5, height = 2.2, units = 'in', res = 600, compression = 'lzw')
plot_model(mod3, type = "pred", terms = c("HDI"),  colors = c("darkorchid4", "darkcyan"), 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)
dev.off()

#####################################################
#####################################################
#####################################################
#####################################################
### overall decision

####################Subset overall data by demographic and position##################

###################################
###################################
### country for first author
overalldat.country.first<- subset(overalldat, Author.position == "First" )
overalldat.country.first$mean_year<-(overalldat.country.first$Year - 1988.75)

##only one study so all levels of JIF and mean year are the same
mod1 <- glmmTMB(OutcomeBinary ~ Continent + Language + HDI ##just 1 study 
                , data = overalldat.country.first, family = binomial)
##check for multicollinearity issue
check_collinearity(mod1) #not too bad - max VIF is 3.51
summary(mod1)

##likelihood ratio tests on covariates. Build set of models, each leaving 1 variable out 

##continent test
modcont <- glmmTMB(OutcomeBinary ~ Language + HDI ##just 1 study 
                   , data = overalldat.country.first, family = binomial)
anova(mod1, modcont) ## continent matters: 25.17, 0.00013

##language test
modesl <- glmmTMB(OutcomeBinary ~ Continent + HDI ##just 1 study 
                  , data = overalldat.country.first, family = binomial)
anova(mod1, modesl) ## language matters: 15.35, < 0.0001

##HDI test
modhdi <- glmmTMB(OutcomeBinary ~ Continent + Language ##just 1 study 
                  , data = overalldat.country.first, family = binomial)
anova(mod1, modcont) ## HDI matters: 25.17, 0.00013

###get estimates
mod1 <- glmmTMB(OutcomeBinary ~ Continent + Language + HDI ##just 1 study 
                , data = overalldat.country.first, family = binomial)
summary(mod1) ##look at model
confint(mod1)

##Since continent has more than 2 groups, Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Continent = "Tukey")) 
summary(g1)

##visualize models with different combos of the variables

plot_model(mod1, type = "pred", terms = c("Continent", "Language"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

plot_model(mod1, type = "pred", terms = c("Continent", "HDI"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

plot_model(mod1, type = "pred", terms = c("Language", "HDI"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

plot_model(mod1, type = "pred", terms = c("HDI", "Language", "Continent"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

plot_model(mod1, type = "pred", terms = c("HDI", "Language", "Continent"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

plot_model(mod1, type = "pred", terms = c("HDI", "Continent", "Language"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

plot_model(mod1, type = "pred", terms = c("Continent", "HDI", "Language"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###create plot used in the main text. Note: axis labels made to look better in PowerPoint
tiff("hdi overall.tiff", width = 3.7, height = 2.2, units = 'in', res = 600, compression = 'lzw')
plot_model(mod1, type = "pred", terms = c("HDI", "Language"),  colors = c("darkorchid4", "darkcyan"), 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)
dev.off()


####################################
########Corresponding author

overalldat.country.corresponding<- subset(overalldat, Author.position == "Corresponding" )
overalldat.country.corresponding$mean_year<-(overalldat.country.corresponding$Year - 1988.75)

##Four underlying studies so including year and JIF in this one
mod1 <- glmmTMB(OutcomeBinary ~ Continent + Language + HDI + 
                  mean_year + Journal.impact.factor + (1|Study), data = overalldat.country.corresponding, family = binomial)
check_collinearity(mod1) #not too bad - max VIF = 4.42
summary(mod1)

##likelihood ratio tests on covariates. Build set of models, each leaving 1 variable out 

#continent test
modcont <- glmmTMB(OutcomeBinary ~ Language + HDI +
                     mean_year + Journal.impact.factor + (1|Study), data = overalldat.country.corresponding, family = binomial)
anova(mod1, modcont) ##continent matters: 101.05, < 0.0001

#language test
modesl <- glmmTMB(OutcomeBinary ~ Continent + HDI + ## 
                    mean_year + Journal.impact.factor + (1|Study), data = overalldat.country.corresponding, family = binomial)
anova(mod1, modesl) ##language matters: 43.48, < 0.0001

#HDI test
modhdi <- glmmTMB(OutcomeBinary ~ Continent + Language + ## 
                    mean_year + Journal.impact.factor + (1|Study), data = overalldat.country.corresponding, family = binomial)
anova(mod1, modcont) ##HDI matters: 101.05, < 0.0001

###get estimates
mod1 <- glmmTMB(OutcomeBinary ~ Continent + Language + HDI + 
                  mean_year + Journal.impact.factor + (1|Study), data = overalldat.country.corresponding, family = binomial)
summary(mod1) ##look at model
confint(mod1)

##Tukey HSD for continent since more than two groups to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Continent = "Tukey")) 
summary(g1) #Warning message:
#In RET$pfunction("adjusted", ...) : Completion with error > abseps

##visualize models with different combos of the variables

plot_model(mod1, type = "pred", terms = c("Continent", "Language"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

plot_model(mod1, type = "pred", terms = c("Continent", "HDI"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

plot_model(mod1, type = "pred", terms = c("Language", "HDI"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

tiff("country overall.tiff", width = 7, height = 4.5, units = 'in', res = 600, compression = 'lzw')
plot_model(mod1, type = "pred", terms = c("HDI", "Language", "Continent"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)
dev.off()

###create plot used in the main text. Note: axis labels made to look better in PowerPoint
tiff("hdi overall corr.tiff", width = 3.7, height = 2.2, units = 'in', res = 600, compression = 'lzw')
plot_model(mod1, type = "pred", terms = c("HDI", "Language"),  colors = c("darkorchid4", "darkcyan"), 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)
dev.off()
