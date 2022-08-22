####################################################
#### Plotting journal policy data by Publisher  ####
#### Created by: Courtney L Davis               #### 
#### Last modified: 18 August 2022              ####
####################################################

############ Set-up
############
# Load packages
library(ggplot2)
library(here)

# Read in the journal policy data
policy.data <- read.csv(here("Journal Policy Data Summaries","Data","Dataset S2 EcoEvo Journal Policies.csv"))


##### Code to create pie charts describing journal policy data, broken down by type of publisher ####

### Plot A: By journal statements on language requirements
# Data have already been binned accordingly:
# "No mention of English requirement" = 1
# "Journal publishes articles in more than one language (e.g., Spanish and English)" = 2
# "Journal offers FREE English language editing services (most progressive policies)" = 3
# "Offers editing but doesn't strongly suggest or recommend that people use it" = 4
# "Mentions authors need to use "good English" and points to fee-for-service editing" = 5
# "Requires or strongly advices ESL authors to pay for editing prior to submission (or have colleagues edit, etc.) (least progressive policies)" = 6
# "Other (describe in adjacent column)" = 7

# Create table summary for different values
language_df_major <- table(policy.data[policy.data$Society_publisher == "Major_Not_society", "language.requirements.binned"])
language_df_other <- table(policy.data[policy.data$Society_publisher == "Minor_Not_society", "language.requirements.binned"])
language_df_majorsociety <- table(policy.data[policy.data$Society_publisher == "Major_Society", "language.requirements.binned"])
language_df_society <- table(policy.data[policy.data$Society_publisher == "Minor_Society", "language.requirements.binned"])

# Compute percentages
language_df_major_percent = data.frame(language_df_major / sum(language_df_major))
language_df_other_percent = data.frame(language_df_other / sum(language_df_other))
language_df_majorsociety_percent = data.frame(language_df_majorsociety / sum(language_df_majorsociety))
language_df_society_percent = data.frame(language_df_society / sum(language_df_society))

# Add in the missing levels for majorsociety
language_df_majorsociety_percent <- rbind(language_df_majorsociety_percent, data.frame("Var1" = c("2","3"), "Freq" = c("0","0")))
language_df_majorsociety_percent <- language_df_majorsociety_percent[c(1,6,7,2,3,4,5),] # reorder

# Reformat Freq column to numeric
language_df_major_percent$Freq <- as.numeric(language_df_major_percent$Freq)
language_df_other_percent$Freq <- as.numeric(language_df_other_percent$Freq)
language_df_majorsociety_percent$Freq <- as.numeric(language_df_majorsociety_percent$Freq)
language_df_society_percent$Freq <- as.numeric(language_df_society_percent$Freq)

# Change the labels for legend
levels(language_df_major_percent$Var1) <- 
  levels(language_df_other_percent$Var1) <- 
  levels(language_df_majorsociety_percent$Var1) <- 
  levels(language_df_society_percent$Var1) <- list("No mention of language editing" = "1", 
                                               "Publishes in multiple languages" = "2", 
                                               "Offers free language editing" = "3",
                                               "Offers but doesn’t suggest use of editing" = "4", 
                                               "Mentions “good” English, points to editing" = "5",
                                               "Requires or strongly recommends editing" = "6",
                                               "Other" = "7")

# Create the concentric donut dataframe
concentric_df <- rbind(language_df_major_percent, language_df_other_percent, language_df_majorsociety_percent, language_df_society_percent)
concentric_df$classification <- c(rep("major",7), rep("other",7), rep("majorsociety",7), rep("society",7))
concentric_df$Freq <- as.numeric(concentric_df$Freq)
concentric_df$classification <- factor(concentric_df$classification, levels = c("society","other","majorsociety","major"))

# Create the plot
plot_a <- ggplot(concentric_df, aes(x = classification, y = Freq, fill= Var1, col = Var1)) +
  geom_col(size = 0.7) +
  coord_polar(theta="y") +
  scale_fill_viridis_d() + 
  scale_colour_viridis_d() + theme_void() +
  theme(text = element_text(size = 20), 
        plot.title = element_text(hjust = 0.5, margin=margin(t=10,b=-20)), 
        legend.margin=margin(t = 0),
        legend.box.margin=margin(0,0,0,0),
        legend.position = "bottom", 
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  ggtitle("a Language \n statement")


### Plot B: Whether authors are required to submit names of diverse reviewers
# Data have already been binned accordingly:
# Journal doesn't require that authors suggest reviewers = 1
# Journal asks for suggested reviewers but does not mention diversity of reviewers = 2
# Journal ask for suggested reviewers and explicitly mentions 1 or more axes of diversity of reviewers = 3

# Create table summary for different values
revdiversity_df_major <- table(policy.data[policy.data$Society_publisher == "Major_Not_society", "authors.suggest.diverse.reviews.binned"])
revdiversity_df_other <- table(policy.data[policy.data$Society_publisher == "Minor_Not_society", "authors.suggest.diverse.reviews.binned"])
revdiversity_df_majorsociety <- table(policy.data[policy.data$Society_publisher == "Major_Society", "authors.suggest.diverse.reviews.binned"])
revdiversity_df_society <- table(policy.data[policy.data$Society_publisher == "Minor_Society", "authors.suggest.diverse.reviews.binned"])

# Compute percentages
revdiversity_df_major_percent = data.frame(revdiversity_df_major / sum(revdiversity_df_major))
revdiversity_df_other_percent = data.frame(revdiversity_df_other / sum(revdiversity_df_other))
revdiversity_df_majorsociety_percent = data.frame(revdiversity_df_majorsociety / sum(revdiversity_df_majorsociety))
revdiversity_df_society_percent = data.frame(revdiversity_df_society / sum(revdiversity_df_society))

# Reformat Freq column to numeric
revdiversity_df_major_percent$Freq <- as.numeric(revdiversity_df_major_percent$Freq)
revdiversity_df_other_percent$Freq <- as.numeric(revdiversity_df_other_percent$Freq)
revdiversity_df_majorsociety_percent$Freq <- as.numeric(revdiversity_df_majorsociety_percent$Freq)
revdiversity_df_society_percent$Freq <- as.numeric(revdiversity_df_society_percent$Freq)

# Change the labels for legend
levels(revdiversity_df_major_percent$Var1) <- 
  levels(revdiversity_df_other_percent$Var1) <- 
  levels(revdiversity_df_majorsociety_percent$Var1) <-
  levels(revdiversity_df_society_percent$Var1) <- list("Doesn't require suggestions" = "1", 
                                                   "Required or Optional, no diversity" = "2", 
                                                   "Required or Optional, mentions diversity" = "3")

# Create the concentric donut dataframe
concentric_df <- rbind(revdiversity_df_major_percent, revdiversity_df_other_percent, revdiversity_df_majorsociety_percent, revdiversity_df_society_percent)
concentric_df$classification <- c(rep("major",3), rep("other",3), rep("majorsociety",3), rep("society",3))
concentric_df$Freq <- as.numeric(concentric_df$Freq)
concentric_df$classification <- factor(concentric_df$classification, levels = c("society","other","majorsociety","major"))

# Create the plot
plot_b <- ggplot(concentric_df, aes(x = classification, y = Freq, fill= Var1, col = Var1)) +
  geom_col(size = 0.8) +
  coord_polar(theta="y") +
  scale_fill_viridis_d() + 
  scale_colour_viridis_d() + theme_void() +
  theme(text = element_text(size = 20), 
        plot.title = element_text(hjust = 0.5, margin=margin(t=10,b=-20)), 
        legend.margin=margin(t = -50),
        legend.box.margin=margin(0,0,0,0),
        legend.position = "bottom", 
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  ggtitle("b Require suggesting \n diverse reviewers")


### Plot D: By journal blinding format
# Create table summary for different values
journal_blinding_df_major <- table(policy.data[policy.data$Society_publisher == "Major_Not_society", "blind.format"])
journal_blinding_df_other <- table(policy.data[policy.data$Society_publisher == "Minor_Not_society", "blind.format"])
journal_blinding_df_majorsociety <- table(policy.data[policy.data$Society_publisher == "Major_Society", "blind.format"])
journal_blinding_df_society <- table(policy.data[policy.data$Society_publisher == "Minor_Society", "blind.format"])

# Compute percentages
journal_blinding_df_major_percent = data.frame(journal_blinding_df_major / sum(journal_blinding_df_major))
journal_blinding_df_other_percent = data.frame(journal_blinding_df_other / sum(journal_blinding_df_other))
journal_blinding_df_majorsociety_percent = data.frame(journal_blinding_df_majorsociety / sum(journal_blinding_df_majorsociety))
journal_blinding_df_society_percent = data.frame(journal_blinding_df_society / sum(journal_blinding_df_society))

# Reformat Freq column to numeric
journal_blinding_df_major_percent$Freq <- as.numeric(journal_blinding_df_major_percent$Freq)
journal_blinding_df_other_percent$Freq <- as.numeric(journal_blinding_df_other_percent$Freq)
journal_blinding_df_majorsociety_percent$Freq <- as.numeric(journal_blinding_df_majorsociety_percent$Freq)
journal_blinding_df_society_percent$Freq <- as.numeric(journal_blinding_df_society_percent$Freq)

# Add other models to "other" and "society" journals
journal_blinding_df_major_percent <- rbind(journal_blinding_df_major_percent, data.frame("Var1" = "Other", "Freq" = "0"))
journal_blinding_df_major_percent <- journal_blinding_df_major_percent[c(1,2,3,4,6,5),] # reorder

journal_blinding_df_other_percent <- rbind(journal_blinding_df_other_percent, data.frame("Var1" = c("Double_optional","Open"), "Freq" = c("0","0")))
journal_blinding_df_other_percent <- journal_blinding_df_other_percent[c(1,5,2,6,3,4),] # reorder

journal_blinding_df_majorsociety_percent <- rbind(journal_blinding_df_majorsociety_percent, data.frame("Var1" = "Open", "Freq" = "0"))
journal_blinding_df_majorsociety_percent <- journal_blinding_df_majorsociety_percent[c(1,2,3,6,4,5),] # reorder

journal_blinding_df_society_percent <- rbind(journal_blinding_df_society_percent, data.frame("Var1" = c("Open","Double_optional"), "Freq" = c("0","0")))
journal_blinding_df_society_percent <- journal_blinding_df_society_percent[c(1,6,2,5,3,4),] # reorder

# Change the labels for legend
levels(journal_blinding_df_major_percent$Var1) <- 
  levels(journal_blinding_df_other_percent$Var1)  <- 
  levels(journal_blinding_df_majorsociety_percent$Var1)  <- 
  levels(journal_blinding_df_society_percent$Var1) <- list("Assumed single" = "Assumed_single", "Single" = "Single", 
                                                       "Double - optional" = "Double_optional", 
                                                       "Double - required" = "Double_required",
                                                       "Open" = "Open", 
                                                       "Other" = "Other")

# Create the concentric donut dataframe
concentric_df_publisher <- rbind(journal_blinding_df_major_percent, journal_blinding_df_other_percent, journal_blinding_df_majorsociety_percent, journal_blinding_df_society_percent)
concentric_df_publisher$classification <- c(rep("major",6), rep("other",6), rep("majorsociety",6),rep("society",6))
concentric_df_publisher$Freq <- as.numeric(concentric_df_publisher$Freq)
concentric_df_publisher$classification <- factor(concentric_df_publisher$classification, levels = c("society","other","majorsociety","major"))

# Create the plot
plot_d <- ggplot(concentric_df_publisher, aes(x = classification, y = Freq, fill= Var1, col = Var1)) +
  geom_col(size = 0.8,) +
  coord_polar(theta="y") +
  scale_fill_viridis_d() + 
  scale_colour_viridis_d() + theme_void() +
  theme(text = element_text(size = 20), 
        plot.title = element_text(hjust = 0.5, margin=margin(t=10,b=-20)), 
        legend.margin=margin(t = 0),
        legend.box.margin=margin(0,0,0,0),
        legend.position = "bottom", 
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  ggtitle("d Peer review \n blinding format")
  

### Plot E: Whether the journal publishes reviews upon publication of manuscript
publishrev_df_major <- table(policy.data[policy.data$Society_publisher == "Major_Not_society", "journal.publish.peer.reviews"])
publishrev_df_other <- table(policy.data[policy.data$Society_publisher == "Minor_Not_society", "journal.publish.peer.reviews"])
publishrev_df_majorsociety <- table(policy.data[policy.data$Society_publisher == "Major_Society", "journal.publish.peer.reviews"])
publishrev_df_society <- table(policy.data[policy.data$Society_publisher == "Minor_Society", "journal.publish.peer.reviews"])

# Compute percentages
publishrev_df_major_percent = data.frame(publishrev_df_major / sum(publishrev_df_major))
publishrev_df_other_percent = data.frame(publishrev_df_other / sum(publishrev_df_other))
publishrev_df_majorsociety_percent = data.frame(publishrev_df_majorsociety / sum(publishrev_df_majorsociety))
publishrev_df_society_percent = data.frame(publishrev_df_society / sum(publishrev_df_society))

# Reformat Freq column to numeric
publishrev_df_major_percent$Freq <- as.numeric(publishrev_df_major_percent$Freq)
publishrev_df_other_percent$Freq <- as.numeric(publishrev_df_other_percent$Freq)
publishrev_df_majorsociety_percent$Freq <- as.numeric(publishrev_df_majorsociety_percent$Freq)
publishrev_df_society_percent$Freq <- as.numeric(publishrev_df_society_percent$Freq)

# Change the labels for legend
levels(publishrev_df_major_percent$Var1) <- 
  levels(publishrev_df_other_percent$Var1) <- 
  levels(publishrev_df_majorsociety_percent$Var1) <- 
  levels(publishrev_df_society_percent$Var1) <- list("Publishes reviews" = "Yes", 
                                                     "Does not publish reviews" = "No")

# Create the concentric donut dataframe
concentric_df <- rbind(publishrev_df_major_percent, publishrev_df_other_percent, publishrev_df_majorsociety_percent, publishrev_df_society_percent)
concentric_df$classification <- c(rep("major",2), rep("other",2), rep("majorsociety",2), rep("society",2))
concentric_df$Freq <- as.numeric(concentric_df$Freq)
concentric_df$classification <- factor(concentric_df$classification, levels = c("society","other","majorsociety","major"))

# Create the plot
plot_e <- ggplot(concentric_df, aes(x = classification, y = Freq, fill= Var1, col = Var1)) +
  geom_col(size = 0.8) +
  coord_polar(theta="y") +
  scale_fill_viridis_d() + 
  scale_colour_viridis_d() + theme_void() +
  theme(text = element_text(size = 20), 
        plot.title = element_text(hjust = 0.5, margin=margin(t=10,b=-20)), 
        legend.margin=margin(t = -50),
        legend.box.margin=margin(0,0,0,0),
        legend.position = "bottom", 
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  ggtitle("e Publish reviews")


### Plot F: Whether the journal has reviewer guidelines that are specific to journal or publisher
# Create bins for summarizing data
policy.data$ReviewerGuidelines_Publisher_Journal_Binned <- ifelse(policy.data$guidelines.to.be.reviewer == "No", "1",
                                                                  ifelse(policy.data$guidelines.to.be.reviewer == "Yes" & policy.data$guidelines.link.publisher == "Yes", "2",
                                                                         ifelse(policy.data$guidelines.to.be.reviewer == "Yes" & policy.data$guidelines.link.publisher == "No", "3", "NA")))

# Create table summary for different values
revguidelines_df_major <- table(policy.data[policy.data$Society_publisher == "Major_Not_society", "ReviewerGuidelines_Publisher_Journal_Binned"])
revguidelines_df_other <- table(policy.data[policy.data$Society_publisher == "Minor_Not_society", "ReviewerGuidelines_Publisher_Journal_Binned"])
revguidelines_df_majorsociety <- table(policy.data[policy.data$Society_publisher == "Major_Society", "ReviewerGuidelines_Publisher_Journal_Binned"])
revguidelines_df_society <- table(policy.data[policy.data$Society_publisher == "Minor_Society", "ReviewerGuidelines_Publisher_Journal_Binned"])

# Compute percentages
revguidelines_df_major_percent = data.frame(revguidelines_df_major / sum(revguidelines_df_major))
revguidelines_df_other_percent = data.frame(revguidelines_df_other / sum(revguidelines_df_other))
revguidelines_df_majorsociety_percent = data.frame(revguidelines_df_majorsociety / sum(revguidelines_df_majorsociety))
revguidelines_df_society_percent = data.frame(revguidelines_df_society / sum(revguidelines_df_society))

# Reformat Freq column to numeric
revguidelines_df_major_percent$Freq <- as.numeric(revguidelines_df_major_percent$Freq)
revguidelines_df_other_percent$Freq <- as.numeric(revguidelines_df_other_percent$Freq)
revguidelines_df_majorsociety_percent$Freq <- as.numeric(revguidelines_df_majorsociety_percent$Freq)
revguidelines_df_society_percent$Freq <- as.numeric(revguidelines_df_society_percent$Freq)

# Change the labels for legend
levels(revguidelines_df_major_percent$Var1) <- 
  levels(revguidelines_df_other_percent$Var1) <- 
  levels(revguidelines_df_majorsociety_percent$Var1) <- 
  levels(revguidelines_df_society_percent$Var1) <- list("No guidelines" = "1", 
                                                   "Guidelines link to publisher" = "2", 
                                                   "Guidelines are journal-specific" = "3")

# Create the concentric donut dataframe
concentric_df <- rbind(revguidelines_df_major_percent, revguidelines_df_other_percent, revguidelines_df_majorsociety_percent, revguidelines_df_society_percent)
concentric_df$classification <- c(rep("major",3), rep("other",3), rep("majorsociety",3), rep("society",3))
concentric_df$Freq <- as.numeric(concentric_df$Freq)
concentric_df$classification <- factor(concentric_df$classification, levels = c("society","other","majorsociety","major"))

# Create the plot
plot_f <- ggplot(concentric_df, aes(x = classification, y = Freq, fill= Var1, col = Var1)) +
  geom_col(size = 0.8) +
  coord_polar(theta="y") +
  scale_fill_viridis_d() + 
  scale_colour_viridis_d() + theme_void() +
  theme(text = element_text(size = 20), 
        plot.title = element_text(hjust = 0.5, margin=margin(t=10,b=-20)), 
        legend.margin=margin(t = -50),
        legend.box.margin=margin(0,0,0,0),
        legend.position = "bottom", 
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  ggtitle("f Reviewer guidelines")



### Plot G: Whether journal's reviewer guidelines mention social justice
# Create bins for summarizing data
policy.data$ReviewerGuidelines_SocialJustice <- ifelse(policy.data$guidelines.to.be.reviewer == "No", "1",
                                                       ifelse(policy.data$guidelines.to.be.reviewer == "Yes" & policy.data$guidelines.social.justice == "No", "2",
                                                              ifelse(policy.data$guidelines.to.be.reviewer == "Yes" & policy.data$guidelines.social.justice == "Yes" & policy.data$guidelines.esl == "No", "3", "4")))

# Create table summary for different values
revguidelines_sj_df_major <- table(policy.data[policy.data$Society_publisher == "Major_Not_society", "ReviewerGuidelines_SocialJustice"])
revguidelines_sj_df_other <- table(policy.data[policy.data$Society_publisher == "Minor_Not_society", "ReviewerGuidelines_SocialJustice"])
revguidelines_sj_df_majorsociety <- table(policy.data[policy.data$Society_publisher == "Major_Society", "ReviewerGuidelines_SocialJustice"])
revguidelines_sj_df_society <- table(policy.data[policy.data$Society_publisher == "Minor_Society", "ReviewerGuidelines_SocialJustice"])

# Compute percentages
revguidelines_sj_df_major_percent = data.frame(revguidelines_sj_df_major / sum(revguidelines_sj_df_major))
revguidelines_sj_df_other_percent = data.frame(revguidelines_sj_df_other / sum(revguidelines_sj_df_other))
revguidelines_sj_df_majorsociety_percent = data.frame(revguidelines_sj_df_majorsociety / sum(revguidelines_sj_df_majorsociety))
revguidelines_sj_df_society_percent = data.frame(revguidelines_sj_df_society / sum(revguidelines_sj_df_society))

# Reformat Freq column to numeric
revguidelines_sj_df_major_percent$Freq <- as.numeric(revguidelines_sj_df_major_percent$Freq)
revguidelines_sj_df_other_percent$Freq <- as.numeric(revguidelines_sj_df_other_percent$Freq)
revguidelines_sj_df_majorsociety_percent$Freq <- as.numeric(revguidelines_sj_df_majorsociety_percent$Freq)
revguidelines_sj_df_society_percent$Freq <- as.numeric(revguidelines_sj_df_society_percent$Freq)

# Add in missing levels
revguidelines_sj_df_major_percent <- rbind(revguidelines_sj_df_major_percent , data.frame("Var1" = "4", "Freq" = "0"))

revguidelines_sj_df_majorsociety_percent <- rbind(revguidelines_sj_df_majorsociety_percent , data.frame("Var1" = "4", "Freq" = "0"))


# Change the labels for legend
levels(revguidelines_sj_df_major_percent$Var1) <- 
  levels(revguidelines_sj_df_other_percent$Var1) <- 
  levels(revguidelines_sj_df_majorsociety_percent$Var1) <- 
  levels(revguidelines_sj_df_society_percent$Var1) <- list("No guidelines" = "1", 
                                                           "Guidelines w/o social justice" = "2", 
                                                           "Guidelines w/ social justice" = "3",
                                                           "Guidelines mention english" = "4")

# Create the concentric donut dataframe
concentric_df <- rbind(revguidelines_sj_df_major_percent, revguidelines_sj_df_other_percent, revguidelines_sj_df_majorsociety_percent, revguidelines_sj_df_society_percent)
concentric_df$classification <- c(rep("major",4), rep("other",4), rep("majorsociety",4), rep("society",4))
concentric_df$Freq <- as.numeric(concentric_df$Freq)
concentric_df$classification <- factor(concentric_df$classification, levels = c("society","other","majorsociety","major"))

# Create the plot
plot_g <- ggplot(concentric_df, aes(x = classification, y = Freq, fill= Var1, col = Var1)) +
  geom_col(size = 0.8) +
  coord_polar(theta="y") +
  scale_fill_viridis_d() + 
  scale_colour_viridis_d() + theme_void() +
  theme(text = element_text(size = 20), 
        plot.title = element_text(hjust = 0.5, margin=margin(t=10,b=-20)), 
        legend.margin=margin(t = -50),
        legend.box.margin=margin(0,0,0,0),
        legend.position = "bottom", 
        legend.direction = "vertical",
        legend.title = element_blank()) +
  ggtitle("g Reviewer guidelines \n on social justice")



### Create bar charts for the diversity of suggested reviewers
revdiversity_major <- policy.data[policy.data$Society_publisher == "Major_Not_society", ]
revdiversity_other <- policy.data[policy.data$Society_publisher == "Minor_Not_society", ]
revdiversity_majorsociety <- policy.data[policy.data$Society_publisher == "Major_Society", ]
revdiversity_society <- policy.data[policy.data$Society_publisher == "Minor_Society", ]

# Geography, Institutions, Career, Gender, Ethnicity & Race, Other
# Society-affiliated journals
revdiversity_society_geography <- table(revdiversity_society$guidelines.multiple.countries)
revdiversity_society_geography_prop <- data.frame(revdiversity_society_geography / sum(revdiversity_society_geography))

revdiversity_society_institution <- table(revdiversity_society$guidelines.multiple.institutions)
revdiversity_society_institution_prop <- data.frame(revdiversity_society_institution / sum(revdiversity_society_institution))
revdiversity_society_institution_prop <- rbind(revdiversity_society_institution_prop, data.frame("Var1" = "Yes", "Freq" = "0"))

revdiversity_society_career <- table(revdiversity_society$guidelines.ecr)
revdiversity_society_career_prop <- data.frame(revdiversity_society_career / sum(revdiversity_society_career))

revdiversity_society_gender <- table(revdiversity_society$guidelines.gender)
revdiversity_society_gender_prop <- data.frame(revdiversity_society_gender / sum(revdiversity_society_gender))

revdiversity_society_ethnicity <- table(revdiversity_society$guidelines.another.demographic.enthnicity.or.race)
revdiversity_society_ethnicity_prop <- data.frame(revdiversity_society_ethnicity / sum(revdiversity_society_ethnicity))

revdiversity_society_other <- table(revdiversity_society$guidelines.another.demographic.other)
revdiversity_society_other_prop <- data.frame(revdiversity_society_other / sum(revdiversity_society_other))

revdiversity_society_fulldf <- rbind(revdiversity_society_geography_prop[2,], revdiversity_society_institution_prop[2,],
                                 revdiversity_society_career_prop[2,], revdiversity_society_gender_prop[2,],
                                 revdiversity_society_ethnicity_prop[2,],revdiversity_society_other_prop[2,])
revdiversity_society_fulldf$Category <- c("Geography", "Institutions", "Career", "Gender", "Ethnicity & Race", "Other")
revdiversity_society_fulldf$Publisher <- "society"

# Society-affiliated journals published by major publishers
revdiversity_majorsociety_geography <- table(revdiversity_majorsociety$guidelines.multiple.countries)
revdiversity_majorsociety_geography_prop <- data.frame(revdiversity_majorsociety_geography / sum(revdiversity_majorsociety_geography))

revdiversity_majorsociety_institution <- table(revdiversity_majorsociety$guidelines.multiple.institutions)
revdiversity_majorsociety_institution_prop <- data.frame(revdiversity_majorsociety_institution / sum(revdiversity_majorsociety_institution))

revdiversity_majorsociety_career <- table(revdiversity_majorsociety$guidelines.ecr)
revdiversity_majorsociety_career_prop <- data.frame(revdiversity_majorsociety_career / sum(revdiversity_majorsociety_career))

revdiversity_majorsociety_gender <- table(revdiversity_majorsociety$guidelines.gender)
revdiversity_majorsociety_gender_prop <- data.frame(revdiversity_majorsociety_gender / sum(revdiversity_majorsociety_gender))

revdiversity_majorsociety_ethnicity <- table(revdiversity_majorsociety$guidelines.another.demographic.enthnicity.or.race)
revdiversity_majorsociety_ethnicity_prop <- data.frame(revdiversity_majorsociety_ethnicity / sum(revdiversity_majorsociety_ethnicity))

revdiversity_majorsociety_other <- table(revdiversity_majorsociety$guidelines.another.demographic.other)
revdiversity_majorsociety_other_prop <- data.frame(revdiversity_majorsociety_other / sum(revdiversity_majorsociety_other))
revdiversity_majorsociety_other_prop <- rbind(revdiversity_majorsociety_other_prop, data.frame("Var1" = "Yes", "Freq" = "0"))

revdiversity_majorsociety_fulldf <- rbind(revdiversity_majorsociety_geography_prop[2,], revdiversity_majorsociety_institution_prop[2,],
                                 revdiversity_majorsociety_career_prop[2,], revdiversity_majorsociety_gender_prop[2,],
                                 revdiversity_majorsociety_ethnicity_prop[2,],revdiversity_majorsociety_other_prop[2,])
revdiversity_majorsociety_fulldf$Category <- c("Geography", "Institutions", "Career", "Gender", "Ethnicity & Race", "Other")
revdiversity_majorsociety_fulldf$Publisher <- "majorsociety"


# Journals published by minor publishers
revdiversity_other_geography <- table(revdiversity_other$guidelines.multiple.countries)
revdiversity_other_geography_prop <- data.frame(revdiversity_other_geography / sum(revdiversity_other_geography))

revdiversity_other_institution <- table(revdiversity_other$guidelines.multiple.institutions)
revdiversity_other_institution_prop <- data.frame(revdiversity_other_institution / sum(revdiversity_other_institution))
revdiversity_other_institution_prop <- rbind(revdiversity_other_institution_prop, data.frame("Var1" = "Yes", "Freq" = "0"))

revdiversity_other_career <- table(revdiversity_other$guidelines.ecr)
revdiversity_other_career_prop <- data.frame(revdiversity_other_career / sum(revdiversity_other_career))

revdiversity_other_gender <- table(revdiversity_other$guidelines.gender)
revdiversity_other_gender_prop <- data.frame(revdiversity_other_gender / sum(revdiversity_other_gender))

revdiversity_other_ethnicity <- table(revdiversity_other$guidelines.another.demographic.enthnicity.or.race)
revdiversity_other_ethnicity_prop <- data.frame(revdiversity_other_ethnicity / sum(revdiversity_other_ethnicity))

revdiversity_other_other <- table(revdiversity_other$guidelines.another.demographic.other)
revdiversity_other_other_prop <- data.frame(revdiversity_other_other / sum(revdiversity_other_other))

revdiversity_other_fulldf <- rbind(revdiversity_other_geography_prop[2,], revdiversity_other_institution_prop[2,],
                                  revdiversity_other_career_prop[2,], revdiversity_other_gender_prop[2,],
                                  revdiversity_other_ethnicity_prop[2,],revdiversity_other_other_prop[2,])
revdiversity_other_fulldf$Category <- c("Geography", "Institutions", "Career", "Gender", "Ethnicity & Race", "Other")
revdiversity_other_fulldf$Publisher <- "other"

# Journals published by major publishers
revdiversity_major_geography <- table(revdiversity_major$guidelines.multiple.countries)
revdiversity_major_geography_prop <- data.frame(revdiversity_major_geography / sum(revdiversity_major_geography))

revdiversity_major_institution <- table(revdiversity_major$guidelines.multiple.institutions)
revdiversity_major_institution_prop <- data.frame(revdiversity_major_institution / sum(revdiversity_major_institution))

revdiversity_major_career <- table(revdiversity_major$guidelines.ecr)
revdiversity_major_career_prop <- data.frame(revdiversity_major_career / sum(revdiversity_major_career))

revdiversity_major_gender <- table(revdiversity_major$guidelines.gender)
revdiversity_major_gender_prop <- data.frame(revdiversity_major_gender / sum(revdiversity_major_gender))

revdiversity_major_ethnicity <- table(revdiversity_major$guidelines.another.demographic.enthnicity.or.race)
revdiversity_major_ethnicity_prop <- data.frame(revdiversity_major_ethnicity / sum(revdiversity_major_ethnicity))

revdiversity_major_other <- table(revdiversity_major$guidelines.another.demographic.other)
revdiversity_major_other_prop <- data.frame(revdiversity_major_other / sum(revdiversity_major_other))
revdiversity_major_other_prop <- rbind(revdiversity_major_other_prop, data.frame("Var1" = "Yes", "Freq" = "0"))

revdiversity_major_fulldf <- rbind(revdiversity_major_geography_prop[2,], revdiversity_major_institution_prop[2,],
                                          revdiversity_major_career_prop[2,], revdiversity_major_gender_prop[2,],
                                          revdiversity_major_ethnicity_prop[2,],revdiversity_major_other_prop[2,])
revdiversity_major_fulldf$Category <- c("Geography", "Institutions", "Career", "Gender", "Ethnicity & Race", "Other")
revdiversity_major_fulldf$Publisher <- "major"



# Pull all of the data together to create bar charts
revdiversity_all_plotting_df <- rbind(revdiversity_society_fulldf, revdiversity_majorsociety_fulldf, revdiversity_other_fulldf, revdiversity_major_fulldf)
revdiversity_all_plotting_df$Freq <- as.numeric(revdiversity_all_plotting_df$Freq)

revdiversity_all_plotting_df$Publisher <- factor(revdiversity_all_plotting_df$Publisher, levels = c("society", "other", "majorsociety","major"))
levels(revdiversity_all_plotting_df$Publisher) <- list ("Society-Affiliated" = "society" , "Minor Publisher" = "other", "Society-Affilated w/ Large Publisher" = "majorsociety", "Large Publisher" = "major")

### Plot C: Diversity axes mentioned for suggest reviewers
plot_c <- ggplot(data = revdiversity_all_plotting_df, aes(x = Category, y = Freq, fill = Publisher)) + 
  geom_bar(stat="identity", color="black", position=position_dodge())+
  scale_fill_viridis_d() +
  theme_bw() + theme(panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_blank(),
                     text = element_text(size = 20),
                     legend.position = "none") + 
  coord_flip() + facet_wrap(~Publisher, nrow = 4) + ylab("Proportion of journals") + 
  xlab("") +
  ggtitle("c Diversity Axes for Suggested Reviewers")


