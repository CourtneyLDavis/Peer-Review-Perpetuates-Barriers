#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Analysis of bias data 
# Olivia Smith
# Date first created: April 2022
# Date last checked: 10-14 August 2022
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
library(glmmTMB)
library(emmeans)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(RColorBrewer)

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

# set working directory
setwd("D:/Olivia/Desktop/olivia.m.smith/Desktop/EEB CRG/Obj. 1 and 3/Analyzable data/Moving towards analyzable files/Problem end/Checked")
#change this to yours

#################################################
#####################################################
#####################################################
#####################################################
### Load the review score data 
reviewscoredat <- read.csv("Reviewer scores problem.csv", fileEncoding="UTF-8-BOM")
head(reviewscoredat)

reviewscoredat$Demographic.category<- as.factor(reviewscoredat$Demographic.category)
reviewscoredat$Category<- as.factor(reviewscoredat$Category)
reviewscoredat$Subcategory<- as.factor(reviewscoredat$Subcategory)
reviewscoredat$Study<- as.factor(reviewscoredat$Study)

###################################
###################################
### Gender for first author
reviewscoredat.gender.first<- subset(reviewscoredat, Demographic=="Gender" & Author.position == "First" )
reviewscoredat.gender.first$mean_year<-(reviewscoredat.gender.first$Year - 1988.75)

##see the general distribution
hist(reviewscoredat.gender.first$Reviewer_rescaled)

##look at gaussian and beta distributions
mod1 <- glmmTMB(Reviewer_rescaled ~ Demographic.category +  
                  Journal.impact.factor + mean_year + (1|Study), data = reviewscoredat.gender.first, family = gaussian)
summary(mod1)

mod2 <- glmmTMB(Reviewer_rescaled ~ Demographic.category + Journal.impact.factor + mean_year + 
                  (1|Study), data = reviewscoredat.gender.first, family=beta_family(link = "logit"))
summary(mod2) 

##see if beta or gaussian are better
AICctab(mod1, mod2, weights=TRUE)
##similar results but beta slightly better dAICc 0.8
##use beta for inference

summary(mod2)

emmeans(mod2, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod2, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


###################################
###################################
### Gender for corresponding author
reviewscoredat.gender.corresponding<- subset(reviewscoredat, Demographic=="Gender" & Author.position == "Corresponding" )
reviewscoredat.gender.corresponding$mean_year<-(reviewscoredat.gender.corresponding$Year - 1988.75)

##see the general distribution
hist(reviewscoredat.gender.corresponding$Reviewer_rescaled)

##look at gaussian and beta distributions; only two studies so just include study fixed effect
##since JIF and mean year are the same as their respective studies
mod1 <- glmmTMB(Reviewer_rescaled ~ Demographic.category +  
                  Study, data = reviewscoredat.gender.corresponding, family = gaussian)
summary(mod1)

mod2 <- glmmTMB(Reviewer_rescaled ~ Demographic.category +  
                  Study, data = reviewscoredat.gender.corresponding, family=beta_family(link = "logit"))
summary(mod2) ##similar results and AIC 0.1 apart

##see if beta or gaussian are better
AICctab(mod1, mod2, weights=TRUE)
##similar results and dAICc 0.0
##use beta for inference

summary(mod2) 

emmeans(mod2, c("Demographic.category"), type = "response")
##Warning message:
##In qt((1 - level)/adiv, df) : NaNs produced

##visualize model
plot_model(mod2, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

###################################
###################################
### Gender for last author
reviewscoredat.gender.last<- subset(reviewscoredat, Demographic=="Gender" & Author.position == "Last" )
reviewscoredat.gender.last$mean_year<-(reviewscoredat.gender.last$Year - 1988.75)

##see the general distribution
hist(reviewscoredat.gender.last$Reviewer_rescaled)

##look at gaussian and beta distributions; only two studies so just include study fixed effect
##since JIF and mean year are the same as their respective studies
mod1 <- glmmTMB(Reviewer_rescaled ~ Demographic.category +  
                  Study, data = reviewscoredat.gender.last, family = gaussian)
summary(mod1)

mod2 <- glmmTMB(Reviewer_rescaled ~ Demographic.category +  
                  Study, data = reviewscoredat.gender.last, family=beta_family(link = "logit"))
summary(mod2) 

##see if beta or gaussian are better
AICctab(mod1, mod2, weights=TRUE)
##similar results and dAICc 0.1
##use beta for inference

summary(mod2)

emmeans(mod2, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

####################################
###################################
###################################
### Continent for first author
reviewscoredat.Continent.first<- subset(reviewscoredat, Demographic=="Continent" & Author.position == "First" )
reviewscoredat.Continent.first$mean_year<-(reviewscoredat.Continent.first$Year - 1988.75)

##see the general distribution
hist(reviewscoredat.Continent.first$Reviewer_rescaled)

##look at gaussian and beta distributions; only two studies so just include study fixed effect
##since JIF and mean year are the same as their respective studies
mod1 <- glmmTMB(Reviewer_rescaled ~ Demographic.category + 
                  Study, data = reviewscoredat.Continent.first, family = gaussian)
summary(mod1)

##look at gaussian and beta distributions; only two studies so just include study fixed effect
##since JIF and mean year are the same as their respective studies
mod2 <- glmmTMB(Reviewer_rescaled ~ Demographic.category +
                  Study, data = reviewscoredat.Continent.first, family=beta_family(link = "logit"))
summary(mod2)

##see if beta or gaussian are better
AICctab(mod1, mod2, weights=TRUE) 
##the beta distribution is better by 5.9 AICc

##use beta distribution for inference
mod1 <- glmmTMB(Reviewer_rescaled ~ Demographic.category + ##only two studies in this category
                  Study, data = reviewscoredat.Continent.first, family=beta_family(link = "logit"))
#remove continent for Likelihood Ratio Test
modlrt <- glmmTMB(Reviewer_rescaled ~  ##only two studies in this category
                    Study, data = reviewscoredat.Continent.first, family=beta_family(link = "logit"))
#Likelihood Ratio Test
anova(mod1, modlrt) #Continent matters: 27.05, < 0.0001

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

#estimates for graphs
emmeans(mod1, c("Demographic.category"), type = "response")

##visualize differences
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
#####################################################
#####################################################
### Load the journal nationalism data 

alldat <- read.csv("Journal nationalism problem nonbinomial.csv", fileEncoding="UTF-8-BOM")
head(alldat)

alldat$Author.match<- as.factor(alldat$Author.match)
alldat$Study<- as.factor(alldat$Study)

#####################################################
#####################################################
#####################################################
#####################################################
###review scores

#############          
##corresponding author
reviewscores.nationalism.corresponding<- subset(alldat, Author.position == "Corresponding" & Stage == "Reviewer scores")
reviewscores.nationalism.corresponding$mean_year<-(reviewscores.nationalism.corresponding$Year - 1988.75)

##see the general distribution
hist(reviewscores.nationalism.corresponding$Reviewer_rescaled)

##look at gaussian and beta distributions; 3 studies so including JIF, year, and (1|Study)
mod1 <- glmmTMB(Reviewer_rescaled ~ Author.match + Journal.impact.factor + mean_year + 
                  (1|Study), data = reviewscores.nationalism.corresponding, family = gaussian)
summary(mod1)

##look at gaussian and beta distributions; 3 studies so including JIF, year, and (1|Study)
mod2 <- glmmTMB(Reviewer_rescaled ~ Author.match + Journal.impact.factor + mean_year +
                  (1|Study), data = reviewscores.nationalism.corresponding, family=beta_family(link = "logit"))
summary(mod2)

##see if beta or gaussian are better
AICctab(mod1, mod2, weights=TRUE) 
##beta distribution slightly better 0.4 dAICc

##check for multicollinearity since we included JIF and mean year
check_collinearity(mod2) #collinearity not bad; max VIF = 1.75

###use beta distribution for inference
summary(mod2)
confint(mod2)

emmeans(mod2, c("Author.match"), type = "response")
#warning message:
#In qt((1 - level)/adiv, df) : NaNs produced

plot_model(mod1, type = "pred", terms = c("Author.match"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################################
#####################################################
#####################################################
#####################################################
### Load the review score data for COUNTRY level data 
reviewscoredat <- read.csv("Reviewer scores problem COUNTRY.csv")
head(reviewscoredat)

reviewscoredat$Country<- as.factor(reviewscoredat$Country)
reviewscoredat$Study<- as.factor(reviewscoredat$Study)
reviewscoredat$Continent<- as.factor(reviewscoredat$Continent)
reviewscoredat$Language<- as.factor(reviewscoredat$Language)
reviewscoredat$Category<- as.factor(reviewscoredat$Category)

####################Subset reviewscore data by demographic and position##################

###################################
###################################
### country for first author
## this study required that all authors were from that country so first/corr/last are the same data
## therefore, the values are the same for all three positions
reviewscoredat.country.first<- subset(reviewscoredat, Author.position == "First" )
reviewscoredat.country.first$mean_year<-(reviewscoredat.country.first$Year - 1988.75)

##see the general distribution
hist(reviewscoredat.country.first$Reviewer_rescaled)

##look at gaussian and beta distributions
mod1 <- glmmTMB(Reviewer_rescaled ~ HDI + Language + Continent, data = reviewscoredat.country.first, family = gaussian)
summary(mod1) 

##look at gaussian and beta distributions
mod2 <- glmmTMB(Reviewer_rescaled ~ HDI + Language + Continent, data = reviewscoredat.country.first, family=beta_family(link = "logit"))
summary(mod2) 

##see if beta or gaussian are better
AICctab(mod1, mod2, weights=TRUE) 
##models with gaussian and beta similar but gaussian 1.5 better - using beta 
##for consistency with other studies

##check for multicollinearity since we included JIF and mean year
check_collinearity(mod2) 
#collinearity not bad; max VIF = 3.31

##Likelihood Ratio tests. Sequentially remove 1 variable in each set

##language Likelihood Ratio test
modlang <- glmmTMB(Reviewer_rescaled ~ HDI + Continent, data = reviewscoredat.country.first, family=beta_family(link = "logit"))
anova(mod2, modlang) ##1.87, 0.17, languag enot improving model

##HDI Likelihood Ratio test
modhdi <- glmmTMB(Reviewer_rescaled ~ Language + Continent, data = reviewscoredat.country.first, family=beta_family(link = "logit"))
anova(mod2, modhdi) ## 0.67, 0.41 HDI not improving model

##Continent Likelihood Ratio test
modcont <- glmmTMB(Reviewer_rescaled ~ HDI + Language, data = reviewscoredat.country.first, family=beta_family(link = "logit"))
anova(mod2, modcont) ##0.15, 0.99, continent not improving model
##don't do Tukey since continent doesn't matter

##visualize models
plot_model(mod2, type = "pred", terms = c("Language"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

##recreate SI figure  showing HDI against review scores
tiff("hdi review scores.tiff", width = 2.5, height = 2.2, units = 'in', res = 600, compression = 'lzw')
plot_model(mod2, type = "pred", terms = c("HDI"),  colors = c("darkorchid4", "darkcyan"), 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)
dev.off()


#####################################################
#####################################################
#####################################################
#####################################################
### Load the number revisions data 
nrevisionsdat <- read.csv("Number revisions problem.csv", fileEncoding="UTF-8-BOM")
head(nrevisionsdat)

nrevisionsdat$Demographic.category<- as.factor(nrevisionsdat$Demographic.category)
nrevisionsdat$Category<- as.factor(nrevisionsdat$Category)
nrevisionsdat$Subcategory<- as.factor(nrevisionsdat$Subcategory)
nrevisionsdat$Study<- as.factor(nrevisionsdat$Study)

###################################
###################################
### Gender for first author
nrevisionsdat.gender.first<- subset(nrevisionsdat, Demographic=="Gender" & Author.position == "First" )
nrevisionsdat.gender.first$mean_year<-(nrevisionsdat.gender.first$Year - 1988.75)

##see the general distribution
hist(nrevisionsdat.gender.first$N.revisions, breaks = 10)

#single study for single year so 
#remove the year and study random effect since all levels are the same
#maintain JIF because the data come from Squazzoni that cover 79 journals across JIFs
mod1 <- glmmTMB(N.revisions ~ Demographic.category +  
                  Journal.impact.factor , data = nrevisionsdat.gender.first, family = gaussian)

##check model assumptions
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) #seems fine

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
nrevisionsdat.gender.last<- subset(nrevisionsdat, Demographic=="Gender" & Author.position == "Last" )
nrevisionsdat.gender.last$mean_year<-(nrevisionsdat.gender.last$Year - 1988.75)

##see the general distribution
hist(nrevisionsdat.gender.last$N.revisions, breaks = 10)

#single study for single year so 
#remove the year and study random effect since all levels are the same
#maintain JIF because the data come from Squazzoni that cover 79 journals across JIFs
mod1 <- glmmTMB(N.revisions ~ Demographic.category +  
                  Journal.impact.factor , data = nrevisionsdat.gender.last, family = gaussian)

##check model assumptions
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) #looks fine

summary(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

#####################################################
#####################################################
#####################################################
#####################################################
### Load the number reviewers data 
nreviewersdat <- read.csv("Number reviewers problem.csv")
head(nreviewersdat)

nreviewersdat$Demographic.category<- as.factor(nreviewersdat$Demographic.category)
nreviewersdat$Category<- as.factor(nreviewersdat$Category)
nreviewersdat$Subcategory<- as.factor(nreviewersdat$Subcategory)
nreviewersdat$Study<- as.factor(nreviewersdat$Study)

###################################
###################################
### Gender for first author
nreviewersdat.gender.first<- subset(nreviewersdat, Demographic=="Gender" & Author.position == "First" )
nreviewersdat.gender.first$mean_year<-(nreviewersdat.gender.first$Year - 1988.75)

##see the general distribution
hist(nreviewersdat.gender.first$N.reviewers, breaks=15)

##data from 7 studies
mod1 <- glmmTMB(N.reviewers ~ Demographic.category + mean_year +
                  Journal.impact.factor + (1|Study) , data = nreviewersdat.gender.first, family = gaussian)

##check assumptions
check_collinearity(mod1) #fine - highest VIF = 1
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) #quantile deviations detected and deviation significant 
##suspected underdispersion problem
##other options don't seem better so proceeding with caution 

summary(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

###################################
###################################
### Gender for corresponding author
nreviewersdat.gender.corresponding<- subset(nreviewersdat, Demographic=="Gender" & Author.position == "Corresponding")
nreviewersdat.gender.corresponding$mean_year<-(nreviewersdat.gender.corresponding$Year - 1988.75)

##see the general distribution
hist(nreviewersdat.gender.corresponding$N.reviewers, breaks=15)

#only two studies so just include study fixed effect 
##since JIF and mean year are the same as their respective studies
mod1 <- glmmTMB(N.reviewers ~ Demographic.category + Study 
                , data = nreviewersdat.gender.corresponding, family = gaussian)

##check model assumptions
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) #doesn't like the two study approach but values fine on what they showed

summary(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")

###################################
###################################
### Gender for last author
nreviewersdat.gender.last<- subset(nreviewersdat, Demographic=="Gender" & Author.position == "Last" )
nreviewersdat.gender.last$mean_year<-(nreviewersdat.gender.last$Year - 1988.75)

##see the general distribution
hist(nreviewersdat.gender.last$N.reviewers, breaks=15)

#four studies in this combination
mod1 <- glmmTMB(N.reviewers ~ Demographic.category +  mean_year +
                  Journal.impact.factor + (1|Study) , data = nreviewersdat.gender.last, family = gaussian)

##check model assumptions
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) #quantile deviations detected but and deviation significant 
##suspected underdispersion. Not better options so proceeding with caution. 

summary(mod1)
confint(mod1)

emmeans(mod1, c("Demographic.category"), type = "response")


#####################################################
#####################################################
#####################################################
#####################################################
### Load the number reviewers data 
nreviewersdat <- read.csv("Number reviewers problem.csv")
head(nreviewersdat)

nreviewersdat$Demographic.category<- as.factor(nreviewersdat$Demographic.category)
nreviewersdat$Category<- as.factor(nreviewersdat$Category)
nreviewersdat$Subcategory<- as.factor(nreviewersdat$Subcategory)
nreviewersdat$Study<- as.factor(nreviewersdat$Study)

###################################
###################################
### Continent for first author
nreviewersdat.Continent.first<- subset(nreviewersdat, Demographic=="Continent" & Author.position == "First" )
nreviewersdat.Continent.first$mean_year<-(nreviewersdat.Continent.first$Year - 1988.75)

##see the general distribution
hist(nreviewersdat.Continent.first$N.reviewers, breaks=15)

##two studies so JIF and mean year are the same as their respective studies
mod1 <- glmmTMB(N.reviewers ~ Demographic.category +
                  Study, data = nreviewersdat.Continent.first, family = gaussian)
##check for multicollinearity
check_collinearity(mod1) #low VIF = 1.09
##check model assumptions
testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1, plot = F)
plot(simulationOutput) #quantile deviations detected but deviation not significant; dispersion not significant

## remove continent for Likelihood Ratio Test
modlrt <- glmmTMB(N.reviewers ~ Study, data = nreviewersdat.Continent.first, family = gaussian)
anova(mod1, modlrt) #continent matters: 14.86, 0.011
summary(mod1)

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Demographic.category = "Tukey")) 
summary(g1)

emmeans(mod1, c("Demographic.category"), type = "response")

##visualize differences
plot_model(mod1, type = "pred", terms = c("Demographic.category"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)



#####################################################
#####################################################
#####################################################
#####################################################
### Load the number of submissions data
## All data are from one author questionnaire survey study: Fox and Paine (2019)
## Note that data formatting on this one is different since it's a one of a kind dataset

setwd("D:/Olivia/Desktop/olivia.m.smith/Desktop/EEB CRG/Obj. 1 and 3/Analyzable data/Moving towards analyzable files/Problem end/Checked")

nsubmissionsdat <- read.csv("Number submissions data problem.csv", fileEncoding="UTF-8-BOM")
head(nsubmissionsdat)

nsubmissionsdat$Gender<- as.factor(nsubmissionsdat$Gender)
nsubmissionsdat$English.fluency.1<- as.factor(nsubmissionsdat$English.fluency.1)
nsubmissionsdat$first_author_gender<- as.factor(nsubmissionsdat$first_author_gender)
nsubmissionsdat$corr_author_gender<- as.factor(nsubmissionsdat$corr_author_gender)
nsubmissionsdat$last_author_gender<- as.factor(nsubmissionsdat$last_author_gender)
nsubmissionsdat$First_continent<- as.factor(nsubmissionsdat$First_continent)
nsubmissionsdat$First_language<- as.factor(nsubmissionsdat$First_language)
nsubmissionsdat$Corr_continent<- as.factor(nsubmissionsdat$Corr_continent)
nsubmissionsdat$Corr_language<- as.factor(nsubmissionsdat$Corr_language)
nsubmissionsdat$Last_continent<- as.factor(nsubmissionsdat$Last_continent)
nsubmissionsdat$Last_language<- as.factor(nsubmissionsdat$Last_language)
nsubmissionsdat$Last_language_similar_HDI<- as.factor(nsubmissionsdat$Last_language_similar_HDI)
nsubmissionsdat$First_language_similar_HDI<- as.factor(nsubmissionsdat$First_language_similar_HDI)
nsubmissionsdat$Corr_language_similar_HDI<- as.factor(nsubmissionsdat$Corr_language_similar_HDI)

################################
################################
###assumed gender

######################
##first author

##see the general distribution
hist(nsubmissionsdat$Journals.submitted.to) #looks poisson

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ first_author_gender + Max.JIF.submitted.to, 
                data = nsubmissionsdat, family = poisson)
summary(mod1)

emmeans(mod1, c("first_author_gender"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("first_author_gender"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

######################
##corresponding author

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ corr_author_gender + Max.JIF.submitted.to, 
                data = nsubmissionsdat, family = poisson)
summary(mod1)

emmeans(mod1, c("corr_author_gender"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("corr_author_gender"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

######################
##last author

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ last_author_gender + Max.JIF.submitted.to, 
                data = nsubmissionsdat, family = poisson)
summary(mod1)

emmeans(mod1, c("last_author_gender"), type = "response")

plot_model(mod1, type = "pred", terms = c("last_author_gender"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

##################
##This dataset also has corresponding author's reported gender (divided into male/female/NA)
##checking to see how closely results align with assigned gender
##using assumed gender in results, though, due to consistency with other studies
mod1 <- glmmTMB(Journals.submitted.to ~ Gender + Max.JIF.submitted.to, #corr  author reported gender
                data = nsubmissionsdat, family = poisson)
summary(mod1)
##author reported gender estimate similar; I didn't include Z_ before female, so the reference is now female 

emmeans(mod1, c("Gender"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Gender"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)


#####################################
#####################################
## Made individual files for author positions for locational data to allow Likelihood Ratio Tests
## on the same subset of data 

######Continent

##First author/continent
nsubmissionsdatfirstcont <- read.csv("Number submissions data FIRST CONTINENT problem.csv", fileEncoding="UTF-8-BOM")
head(nsubmissionsdatfirstcont)

######################
##first author

nsubmissionsdatfirstcont$First_continent<- as.factor(nsubmissionsdatfirstcont$First_continent)

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ First_continent + Max.JIF.submitted.to, 
                data = nsubmissionsdatfirstcont, family = poisson)
summary(mod1)

##model without continent for Likelihood Ratio Test
mod2 <- glmmTMB(Journals.submitted.to ~ Max.JIF.submitted.to, 
                data = nsubmissionsdatfirstcont, family = poisson)
anova(mod1, mod2) #continent impacts number submissions: 35.97, < 0.0001

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(First_continent = "Tukey")) 
summary(g1)

emmeans(mod1, c("First_continent"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("First_continent"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

######################
##corresponding author

nsubmissionsdatcorrcont <- read.csv("Number submissions data CORR CONTINENT problem.csv", fileEncoding="UTF-8-BOM")
head(nsubmissionsdatcorrcont)

nsubmissionsdatcorrcont$Corr_continent<- as.factor(nsubmissionsdatcorrcont$Corr_continent)

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ Corr_continent + Max.JIF.submitted.to, 
                data = nsubmissionsdatcorrcont, family = poisson)
summary(mod1)

##model without continent for Likelihood Ratio Test
mod2 <- glmmTMB(Journals.submitted.to ~ Max.JIF.submitted.to, 
                data = nsubmissionsdatcorrcont, family = poisson)
anova(mod1, mod2) #continent important 34.81, < 0.0001

#estimates for figures
emmeans(mod1, c("Corr_continent"), type = "response")

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Corr_continent = "Tukey")) 
summary(g1)

##visualize model
plot_model(mod1, type = "pred", terms = c("Corr_continent"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

######################
##last author

nsubmissionsdatlastcont <- read.csv("Number submissions data LAST CONTINENT problem.csv", fileEncoding="UTF-8-BOM")
head(nsubmissionsdatlastcont)

nsubmissionsdatlastcont$Last_continent<- as.factor(nsubmissionsdatlastcont$Last_continent)

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ Last_continent + Max.JIF.submitted.to, 
                data = nsubmissionsdatlastcont, family = poisson)
summary(mod1)

##model without continent for Likelihood Ratio Test
mod2 <- glmmTMB(Journals.submitted.to ~ Max.JIF.submitted.to, 
                data = nsubmissionsdatlastcont, family = poisson)
anova(mod1, mod2) #continent important: 30.52, < 0.0001

#estimates for figures
emmeans(mod1, c("Last_continent"), type = "response")

##Tukey HSD to see which comparisons are different
g1<-glht(mod1, linfct = mcp(Last_continent = "Tukey")) 
summary(g1)

##visualize model
plot_model(mod1, type = "pred", terms = c("Last_continent"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

########################################
########################################

########English primary language for all countries

######################
##first author

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ First_language + Max.JIF.submitted.to, 
                data = nsubmissionsdat, family = poisson)
summary(mod1)

#estimates for figures
emmeans(mod1, c("First_language"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("First_language"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

######################
##corresponding author

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ Corr_language + Max.JIF.submitted.to, 
                data = nsubmissionsdat, family = poisson)
summary(mod1)

#estimates for figures
emmeans(mod1, c("Corr_language"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Corr_language"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

######################
##last author

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ Last_language + Max.JIF.submitted.to, 
                data = nsubmissionsdat, family = poisson)
summary(mod1)

#estimates for figures
emmeans(mod1, c("Last_language"), type = "response")

##visualize model

plot_model(mod1, type = "pred", terms = c("Last_language"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

##this dataset also surveyed corresponding authors on their English fluency. This is testing for the corresponding
##author's reported English fluency to see if the results are similar to the assignment to language
##based on country. We used the assigned language, however, to align with other studies 

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ English.fluency.1 + Max.JIF.submitted.to, #corr author
                #reported native speaker or not
                data = nsubmissionsdat, family = poisson)
summary(mod1) #estimates quite close to assigning language

emmeans(mod1, c("English.fluency.1"), type = "response") #estimates quite close to assigning language

##visualize model
plot_model(mod1, type = "pred", terms = c("English.fluency.1"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

########################################
########################################

########English primary language for countries within 0.10 HDI of the highest HDI country

######################
##first author

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ First_language_similar_HDI + Max.JIF.submitted.to, 
                data = nsubmissionsdat, family = poisson)
summary(mod1)

emmeans(mod1, c("First_language_similar_HDI"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("First_language_similar_HDI"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

######################
##corresponding author

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ Corr_language_similar_HDI + Max.JIF.submitted.to, 
                data = nsubmissionsdat, family = poisson)
summary(mod1)

emmeans(mod1, c("Corr_language_similar_HDI"), type = "response")

##visualize model
plot_model(mod1, type = "pred", terms = c("Corr_language_similar_HDI"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

######################
##last author

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ Last_language_similar_HDI + Max.JIF.submitted.to, 
                data = nsubmissionsdat, family = poisson)
summary(mod1)

emmeans(mod1, c("Last_language_similar_HDI"), type = "response")

##visualize model

plot_model(mod1, type = "pred", terms = c("Last_language_similar_HDI"),   colors = "social", 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)

#####################################
#####################################
#### Analyses at the individual country level

################################
################################

######################
##first author

nsubmissionsdatfirsthdi <- read.csv("Number submissions data FIRST COUNTRY problem.csv", fileEncoding="UTF-8-BOM")
head(nsubmissionsdatfirsthdi)

nsubmissionsdatfirsthdi$First_continent<- as.factor(nsubmissionsdatfirsthdi$First_continent)
nsubmissionsdatfirsthdi$First_language<- as.factor(nsubmissionsdatfirsthdi$First_language)

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ First_continent + First_language + First_HDI2012 + 
                  Max.JIF.submitted.to, data = nsubmissionsdatfirsthdi, family = poisson)
##check for multicollinearity
check_collinearity(mod1) #VIF = 12.85 for country; don't have all in the same model

##try combinations of 2 to see if multicollinearity issue goes away
mod1conthdi <- glmmTMB(Journals.submitted.to ~ First_continent + First_HDI2012 + 
                  Max.JIF.submitted.to, data = nsubmissionsdatfirsthdi, family = poisson)
check_collinearity(mod1conthdi) #fine: max VIF = 3.79

mod1contlang <- glmmTMB(Journals.submitted.to ~ First_continent + First_language + 
                          Max.JIF.submitted.to, data = nsubmissionsdatfirsthdi, family = poisson)
check_collinearity(mod1contlang) #fine: max VIF = 3.63

mod1langhdi <- glmmTMB(Journals.submitted.to ~ First_language + First_HDI2012 + 
                         Max.JIF.submitted.to, data = nsubmissionsdatfirsthdi, family = poisson)
check_collinearity(mod1langhdi) #fine: max VIF = 1.10

##Decision: report models for all possible combinations of two of the three

###HDI + continent tests: models without each for Likelihood Ratio Tests
mod2 <- glmmTMB(Journals.submitted.to ~ First_continent + 
                  Max.JIF.submitted.to, data = nsubmissionsdatfirsthdi, family = poisson)

mod3 <- glmmTMB(Journals.submitted.to ~ First_HDI2012 + 
                  Max.JIF.submitted.to, data = nsubmissionsdatfirsthdi, family = poisson)

##Likelihood Ratio Tests
anova(mod1conthdi, mod2) #HDI marginally important: 2.76, 0.096 .
anova(mod1conthdi, mod3) #continent important: 26.03, < 0.0001

summary(mod1conthdi)

##Tukey HSD to see which comparisons are different
g1<-glht(mod1conthdi, linfct = mcp(First_continent = "Tukey")) 
summary(g1)

##continent + language tests: models without each for Likelihood Ratio Tests
mod2 <- glmmTMB(Journals.submitted.to ~ First_continent + 
                  Max.JIF.submitted.to, data = nsubmissionsdatfirsthdi, family = poisson)

mod3 <- glmmTMB(Journals.submitted.to ~ First_language + 
                  Max.JIF.submitted.to, data = nsubmissionsdatfirsthdi, family = poisson)

##Likelihood Ratio Tests
anova(mod1contlang, mod2) #language important: 5.41, 0.020
anova(mod1contlang, mod3) #continent not important when accounting for language: 2.61, 0.76

summary(mod1contlang)

##language + HDI tests: models without each for Likelihood Ratio Tests

mod2 <- glmmTMB(Journals.submitted.to ~ First_language + 
                  Max.JIF.submitted.to, data = nsubmissionsdatfirsthdi, family = poisson)

mod3 <- glmmTMB(Journals.submitted.to ~ First_HDI2012 + 
                  Max.JIF.submitted.to, data = nsubmissionsdatfirsthdi, family = poisson)

##Likelihood Ratio Tests
anova(mod1langhdi, mod2) #HDI marginally important when accounting for language: 3.77, 0.052.
anova(mod1langhdi, mod3) #language important: 29.83, <0.0001

summary(mod1langhdi)

##of curiosity, which of three models is more informative
library(bbmle)
AICctab(mod1langhdi, mod1contlang, mod1conthdi, weights=TRUE) 
##the model with language and HDI best by a long shot: 9.2 AICc from continent + language

##create figure showing journals submitted to (y) vs. HDI (X), colored by country primary language
##exports to your working directory as a .tiff
tiff("hdi sub first.tiff", width = 3.7, height = 2.2, units = 'in', res = 600, compression = 'lzw')
plot_model(mod1langhdi, type = "pred", terms = c("First_HDI2012", "First_language"),  colors = c("darkorchid4", "darkcyan"), 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)
dev.off()

######################
##Corresponding author

nsubmissionsdatcorrhdi <- read.csv("Number submissions data CORR COUNTRY problem.csv", fileEncoding="UTF-8-BOM")
head(nsubmissionsdatcorrhdi)

nsubmissionsdatcorrhdi$Corr_continent<- as.factor(nsubmissionsdatcorrhdi$Corr_continent)
nsubmissionsdatcorrhdi$Corr_language<- as.factor(nsubmissionsdatcorrhdi$Corr_language)

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ Corr_continent + Corr_language + Corr_HDI2012 + 
                  Max.JIF.submitted.to, data = nsubmissionsdatcorrhdi, family = poisson)
##check for multicollinearity
check_collinearity(mod1) #VIF = 12.65 for country; don't have all in the same model

##try combinations of 2 to see if multicollinearity issue goes away
mod1conthdi <- glmmTMB(Journals.submitted.to ~ Corr_continent + Corr_HDI2012 + 
                         Max.JIF.submitted.to, data = nsubmissionsdatcorrhdi, family = poisson)
check_collinearity(mod1conthdi) #fine: max VIF = 3.78

mod1contlang <- glmmTMB(Journals.submitted.to ~ Corr_continent + Corr_language + 
                          Max.JIF.submitted.to, data = nsubmissionsdatcorrhdi, family = poisson)
check_collinearity(mod1contlang) #fine: max VIF = 3.54

mod1langhdi <- glmmTMB(Journals.submitted.to ~ Corr_language + Corr_HDI2012 + 
                         Max.JIF.submitted.to, data = nsubmissionsdatcorrhdi, family = poisson)
check_collinearity(mod1langhdi) #fine: max VIF = 1.09

##Decision: report models for all possible combinations of two of the three

###HDI + continent tests: models without each for Likelihood Ratio Tests
mod2 <- glmmTMB(Journals.submitted.to ~ Corr_continent + 
                  Max.JIF.submitted.to, data = nsubmissionsdatcorrhdi, family = poisson)

mod3 <- glmmTMB(Journals.submitted.to ~ Corr_HDI2012 + 
                  Max.JIF.submitted.to, data = nsubmissionsdatcorrhdi, family = poisson)

##Likelihood Ratio Tests
anova(mod1conthdi, mod2) #HDI marginally important: 2.92, 0.088 .
anova(mod1conthdi, mod3) #continent important: 25.04, 0.00014

summary(mod1conthdi)

##Tukey HSD to see which comparisons are different
g1<-glht(mod1conthdi, linfct = mcp(Corr_continent = "Tukey")) 
summary(g1)

##continent + language tests: models without each for Likelihood Ratio Tests
mod2 <- glmmTMB(Journals.submitted.to ~ Corr_continent + 
                  Max.JIF.submitted.to, data = nsubmissionsdatcorrhdi, family = poisson)

mod3 <- glmmTMB(Journals.submitted.to ~ Corr_language + 
                  Max.JIF.submitted.to, data = nsubmissionsdatcorrhdi, family = poisson)

##Likelihood Ratio Tests
anova(mod1contlang, mod2) #language important: 6.19, 0.013
anova(mod1contlang, mod3) #continent not important when accounting for language: 2.35, 0.80

summary(mod1contlang)

##language + HDI tests: models without each for Likelihood Ratio Tests

mod2 <- glmmTMB(Journals.submitted.to ~ Corr_language + 
                  Max.JIF.submitted.to, data = nsubmissionsdatcorrhdi, family = poisson)

mod3 <- glmmTMB(Journals.submitted.to ~ Corr_HDI2012 + 
                  Max.JIF.submitted.to, data = nsubmissionsdatcorrhdi, family = poisson)

##Likelihood Ratio Tests
anova(mod1langhdi, mod2) #HDI important: 4.01, 0.045
anova(mod1langhdi, mod3) #language important: 29.98, <0.0001

summary(mod1langhdi)

##of curiosity, which of three models is more informative
library(bbmle)
AICctab(mod1langhdi, mod1contlang, mod1conthdi, weights=TRUE) 
##the model with language and HDI best by a long shot: 9.7 AICc from continent + language

##recreate the figure showing journals submitted to (y) vs. HDI (X), colored by country primary language
##exports to your working directory as a .tiff
tiff("hdi sub corr.tiff", width = 3.7, height = 2.2, units = 'in', res = 600, compression = 'lzw')
plot_model(mod1langhdi, type = "pred", terms = c("Corr_HDI2012", "Corr_language"),  colors = c("darkorchid4", "darkcyan"), 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)
dev.off()

######################
##Last author

nsubmissionsdatlasthdi <- read.csv("Number submissions data LAST COUNTRY problem.csv", fileEncoding="UTF-8-BOM")
head(nsubmissionsdatlasthdi)

nsubmissionsdatlasthdi$Last_continent<- as.factor(nsubmissionsdatlasthdi$Last_continent)
nsubmissionsdatlasthdi$Last_language<- as.factor(nsubmissionsdatlasthdi$Last_language)

##one study with same mean study year, but the submissions data are linked to specific journals provided, so 
##we're using maximum impact factor submitted to
mod1 <- glmmTMB(Journals.submitted.to ~ Last_continent + Last_language + Last_HDI2012 + 
                  Max.JIF.submitted.to, data = nsubmissionsdatlasthdi, family = poisson)
##check for multicollinearity
check_collinearity(mod1) #VIF = 12.69 for country; don't have all in the same model

##try combinations of 2 to see if multicollinearity issue goes away
mod1conthdi <- glmmTMB(Journals.submitted.to ~ Last_continent + Last_HDI2012 + 
                         Max.JIF.submitted.to, data = nsubmissionsdatlasthdi, family = poisson)
check_collinearity(mod1conthdi) #fine: max VIF = 3.80

mod1contlang <- glmmTMB(Journals.submitted.to ~ Last_continent + Last_language + 
                          Max.JIF.submitted.to, data = nsubmissionsdatlasthdi, family = poisson)
check_collinearity(mod1contlang) #fine: max VIF = 3.57

mod1langhdi <- glmmTMB(Journals.submitted.to ~ Last_language + Last_HDI2012 + 
                         Max.JIF.submitted.to, data = nsubmissionsdatlasthdi, family = poisson)
check_collinearity(mod1langhdi) #fine: max VIF = 1.10

##Decision: report models for all possible combinations of two of the three

###HDI + continent tests: models without each for Likelihood Ratio Tests
mod2 <- glmmTMB(Journals.submitted.to ~ Last_continent + 
                  Max.JIF.submitted.to, data = nsubmissionsdatlasthdi, family = poisson)

mod3 <- glmmTMB(Journals.submitted.to ~ Last_HDI2012 + 
                  Max.JIF.submitted.to, data = nsubmissionsdatlasthdi, family = poisson)

##Likelihood Ratio Tests
anova(mod1conthdi, mod2) #HDI not important when accounting for continent: 0.33, 0.56
anova(mod1conthdi, mod3) #continent important: 23.63, 0.00026

summary(mod1conthdi)

##Tukey HSD to see which comparisons are different
g1<-glht(mod1conthdi, linfct = mcp(Last_continent = "Tukey")) 
summary(g1)

##continent + language tests: models without each for Likelihood Ratio Tests
mod2 <- glmmTMB(Journals.submitted.to ~ Last_continent + 
                  Max.JIF.submitted.to, data = nsubmissionsdatlasthdi, family = poisson)

mod3 <- glmmTMB(Journals.submitted.to ~ Last_language + 
                  Max.JIF.submitted.to, data = nsubmissionsdatlasthdi, family = poisson)

##Likelihood Ratio Tests
anova(mod1contlang, mod2) #language important: 5.42, 0.020
anova(mod1contlang, mod3) #continent not important when accounting for language: 2.73, 0.74

summary(mod1contlang)

##language + HDI tests: models without each for Likelihood Ratio Tests

mod2 <- glmmTMB(Journals.submitted.to ~ Last_language + 
                  Max.JIF.submitted.to, data = nsubmissionsdatlasthdi, family = poisson)

mod3 <- glmmTMB(Journals.submitted.to ~ Last_HDI2012 + 
                  Max.JIF.submitted.to, data = nsubmissionsdatlasthdi, family = poisson)

##Likelihood Ratio Tests
anova(mod1langhdi, mod2) #HDI not important when accounting for language: 1.32, 0.25
anova(mod1langhdi, mod3) #language important: 27.3, <0.0001

summary(mod1langhdi)

##of curiosity, which of three models is more informative
library(bbmle)
AICctab(mod1langhdi, mod1contlang, mod1conthdi, weights=TRUE) 
##the model with language and HDI best: 6.6 AICc from continent + language

##create figure showing journals submitted to (y) vs. HDI (X), colored by country primary language
##exports to your working directory as a .tiff
tiff("hdi sub last.tiff", width = 3.7, height = 2.2, units = 'in', res = 600, compression = 'lzw')
plot_model(mod1langhdi, type = "pred", terms = c("Last_HDI2012", "Last_language"),  colors = c("darkorchid4", "darkcyan"), 
           value.offset = 0.2, value.size = 8,
           dot.size = 3, line.size = 1, vline.color = "black", width = 0)
dev.off()
