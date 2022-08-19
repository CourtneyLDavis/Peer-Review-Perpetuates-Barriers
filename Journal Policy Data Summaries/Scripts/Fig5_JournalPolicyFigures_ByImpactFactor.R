#################################################
#### Plotting journal policy data by JIF     ####
#### Created by: Courtney L Davis            #### 
#### Last modified: 18 August 2022           ####
#################################################

############ Set-up
############
# Load packages
library(ggplot2)
library(here)

# Read in the journal policy data
policy.data <- read.csv(here("Journal Policy Summaries","Data","JournalPolicies_Data_2022.csv"))

##### Code to create pie charts describing journal policy data, broken down by impact factor ####

### Plot A: By publisher of journal - broken down in to top 5 publishers and "other"
# Create bins for summarizing data
policy.data$Publisher_Top_Binned <- ifelse(policy.data$Publisher.from.JCR.but.binned.by.larger.company == "SPRINGER", "1",
                                           ifelse(policy.data$Publisher.from.JCR.but.binned.by.larger.company == "WILEY", "2",
                                                  ifelse(policy.data$Publisher.from.JCR.but.binned.by.larger.company == "ELSEVIER", "3",
                                                         ifelse(policy.data$Publisher.from.JCR.but.binned.by.larger.company == "TAYLOR AND FRANCIS", "4",
                                                                ifelse(policy.data$Publisher.from.JCR.but.binned.by.larger.company == "OXFORD UNIVERSITY PRESS", "5", "6")))))

# Create table summary for different values
publisher_df_low <- table(policy.data[policy.data$JIF_bin == "Low" | policy.data$JIF_bin == "NA", "Publisher_Top_Binned"])
publisher_df_med <- table(policy.data[policy.data$JIF_bin == "Mid", "Publisher_Top_Binned"])
publisher_df_high <- table(policy.data[policy.data$JIF_bin == "High", "Publisher_Top_Binned"])

# Convert table summary into a dataframe with proportions
publisher_df_low_percent = data.frame(publisher_df_low / sum(publisher_df_low))
publisher_df_med_percent = data.frame(publisher_df_med / sum(publisher_df_med))
publisher_df_high_percent = data.frame(publisher_df_high / sum(publisher_df_high))

# Convert to numeric for plotting
publisher_df_low_percent$Freq <- as.numeric(publisher_df_low_percent$Freq)
publisher_df_med_percent$Freq <- as.numeric(publisher_df_med_percent$Freq)
publisher_df_high_percent$Freq <- as.numeric(publisher_df_high_percent$Freq)

# Add "Multiple languages" to high IF journals
publisher_df_high_percent <- rbind(publisher_df_high_percent, data.frame("Var1" = "4", "Freq" = "0"))
publisher_df_high_percent <- publisher_df_high_percent[c(1,2,3,6,4,5),] # reorder

# Re-assign level names for plotting
levels(publisher_df_high_percent$Var1) <- levels(publisher_df_low_percent$Var1)  <- levels(publisher_df_med_percent$Var1) <- list("Springer" = "1", 
                                          "Wiley" = "2", 
                                          "Elsevier" = "3",
                                          "Taylor & Francis" = "4", 
                                          "Oxford University Press" = "5",
                                          "Other" = "6")

# Combine dataframes to make the concentric donut dataframe
concentric_df <- rbind(publisher_df_low_percent, publisher_df_med_percent, publisher_df_high_percent)
concentric_df$classification <- c(rep("low",6), rep("med",6), rep("high",6)) # create column with IF classification
concentric_df$Freq <- as.numeric(concentric_df$Freq) # convert to numeric for plotting
concentric_df$classification <- factor(concentric_df$classification, levels = c("low", "med","high"))

# Create the plot
plot_a <- ggplot(concentric_df, aes(x = classification, y = Freq, fill= Var1, col = Var1)) +
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
  ggtitle("a Publisher \n breakdown")


### Plot B: By journal statements on language requirements
# Data have already been binned accordingly:
# "No mention of English requirement" = 1
# "Journal publishes articles in more than one language (e.g., Spanish and English)" = 2
# "Journal offers FREE English language editing services (most progressive policies)" = 3
# "Offers editing but doesn't strongly suggest or recommend that people use it" = 4
# "Mentions authors need to use "good English" and points to fee-for-service editing" = 5
# "Requires or strongly advices ESL authors to pay for editing prior to submission (or have colleagues edit, etc.) (least progressive policies)" = 6
# "Other (describe in adjacent column)" = 7

# Create table summary for different values
language_df_low <- table(policy.data[policy.data$JIF_bin == "Low" | policy.data$JIF_bin == "NA", "language.requirements.binned"])
language_df_med <- table(policy.data[policy.data$JIF_bin == "Mid", "language.requirements.binned"])
language_df_high <- table(policy.data[policy.data$JIF_bin == "High", "language.requirements.binned"])

# Compute percentages
language_df_low_percent = data.frame(language_df_low / sum(language_df_low))
language_df_med_percent = data.frame(language_df_med / sum(language_df_med))
language_df_high_percent = data.frame(language_df_high / sum(language_df_high))

# Reformat Freq column to numeric
language_df_low_percent$Freq <- as.numeric(language_df_low_percent$Freq)
language_df_med_percent$Freq <- as.numeric(language_df_med_percent$Freq)
language_df_high_percent$Freq <- as.numeric(language_df_high_percent$Freq)

# Add "Multiple languages" to high IF journals
language_df_high_percent <- rbind(language_df_high_percent, data.frame("Var1" = "2", "Freq" = "0"))
language_df_high_percent <- language_df_high_percent[c(1,7,2,3,4,5,6),] # reorder

# Change the labels for legend
levels(language_df_high_percent$Var1) <- levels(language_df_low_percent$Var1) <- levels(language_df_med_percent$Var1) <- list("No mention of language editing" = "1", 
                                         "Publishes in multiple languages" = "2", 
                                         "Offers free language editing" = "3",
                                         "Offers but doesn’t suggest use of editing" = "4", 
                                         "Mentions “good” English, points to editing" = "5",
                                         "Requires or strongly recommends editing" = "6",
                                         "Other" = "7")

# Create the concentric donut dataframe
concentric_df <- rbind(language_df_low_percent, language_df_med_percent, language_df_high_percent)
concentric_df$classification <- c(rep("low",7), rep("med",7), rep("high",7)) # create column with IF classification
concentric_df$Freq <- as.numeric(concentric_df$Freq) # convert to numeric for plotting
concentric_df$classification <- factor(concentric_df$classification, levels = c("low", "med","high"))

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
  ggtitle("b Language \n statement")


### Plot C: Whether authors are required to submit names of diverse reviewers
# Data have already been binned accordingly:
# Journal doesn't require that authors suggest reviewers = 1
# Journal asks for suggested reviewers but does not mention diversity of reviewers = 2
# Journal ask for suggested reviewers and explicitly mentions 1 or more axes of diversity of reviewers = 3

# Create table summary for different values
revdiversity_df_low <- table(policy.data[policy.data$JIF_bin == "Low" | policy.data$JIF_bin == "NA", "authors.suggest.diverse.reviews.binned"])
revdiversity_df_med <- table(policy.data[policy.data$JIF_bin == "Mid", "authors.suggest.diverse.reviews.binned"])
revdiversity_df_high <- table(policy.data[policy.data$JIF_bin == "High", "authors.suggest.diverse.reviews.binned"])

# Compute percentages
revdiversity_df_low_percent = data.frame(revdiversity_df_low / sum(revdiversity_df_low))
revdiversity_df_med_percent = data.frame(revdiversity_df_med / sum(revdiversity_df_med))
revdiversity_df_high_percent = data.frame(revdiversity_df_high / sum(revdiversity_df_high))

# Reformat Freq column to numeric
revdiversity_df_low_percent$Freq <- as.numeric(revdiversity_df_low_percent$Freq)
revdiversity_df_med_percent$Freq <- as.numeric(revdiversity_df_med_percent$Freq)
revdiversity_df_high_percent$Freq <- as.numeric(revdiversity_df_high_percent$Freq)

# Change the labels for legend
levels(revdiversity_df_high_percent$Var1) <- levels(revdiversity_df_low_percent$Var1) <- levels(revdiversity_df_med_percent$Var1) <- list("Doesn't require suggestions" = "1", 
                                                  "Required or Optional, no diversity" = "2", 
                                                  "Required or Optional, mentions diversity" = "3")

# Create the concentric donut dataframe
concentric_df <- rbind(revdiversity_df_low_percent, revdiversity_df_med_percent, revdiversity_df_high_percent)
concentric_df$classification <- c(rep("low",3), rep("med",3), rep("high",3))
concentric_df$Freq <- as.numeric(concentric_df$Freq)
concentric_df$classification <- factor(concentric_df$classification, levels = c("low", "med","high"))

# Create the plot
plot_c <- ggplot(concentric_df, aes(x = classification, y = Freq, fill= Var1, col = Var1)) +
  geom_col(size = 0.8) +
  coord_polar(theta="y", start = 0) +
  scale_fill_viridis_d() + 
  scale_colour_viridis_d() + theme_void() + 
  theme(text = element_text(size = 20), 
        plot.title = element_text(hjust = 0.5, margin=margin(t=10,b=-20)), 
        legend.margin=margin(t = -50),
        legend.box.margin=margin(0,0,0,0),
        legend.position = "bottom", 
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  ggtitle("c Require suggesting \n diverse reviewers")



### Plot E: By journal blinding format
# Create table summary for different values
journal_blinding_df_low <- table(policy.data[policy.data$JIF_bin == "Low" | policy.data$JIF_bin == "NA", "blind.format"])
journal_blinding_df_med <- table(policy.data[policy.data$JIF_bin == "Mid", "blind.format"])
journal_blinding_df_high <- table(policy.data[policy.data$JIF_bin == "High", "blind.format"])

# Compute percentages
journal_blinding_df_low_percent = data.frame(journal_blinding_df_low / sum(journal_blinding_df_low))
journal_blinding_df_med_percent = data.frame(journal_blinding_df_med / sum(journal_blinding_df_med))
journal_blinding_df_high_percent = data.frame(journal_blinding_df_high / sum(journal_blinding_df_high))

# Reformat Freq column to numeric
journal_blinding_df_low_percent$Freq <- as.numeric(journal_blinding_df_low_percent$Freq)
journal_blinding_df_med_percent$Freq <- as.numeric(journal_blinding_df_med_percent$Freq)
journal_blinding_df_high_percent$Freq <- as.numeric(journal_blinding_df_high_percent$Freq)

# Add "Open" to low IF journals
journal_blinding_df_low_percent <- rbind(journal_blinding_df_low_percent, data.frame("Var1" = c("Open","Other"), "Freq" = c("0","0")))
journal_blinding_df_low_percent <- journal_blinding_df_low_percent[c(1,2,3,5,6,4),] # reorder

# Change the labels for legend
levels(journal_blinding_df_high_percent$Var1) <- levels(journal_blinding_df_low_percent$Var1)  <- levels(journal_blinding_df_med_percent$Var1) <- list("Assumed Single" = "Assumed_single", "Single" = "Single", 
                                                      "Double - optional" = "Double_optional", 
                                                      "Double - required" = "Double_required",
                                                      "Open" = "Open", 
                                                      "Other" = "Other")

# Create the concentric donut dataframe
concentric_df <- rbind(journal_blinding_df_low_percent, journal_blinding_df_med_percent, journal_blinding_df_high_percent)
concentric_df$classification <- c(rep("low",6), rep("med",6), rep("high",6))
concentric_df$Freq <- as.numeric(concentric_df$Freq)
concentric_df$classification <- factor(concentric_df$classification, levels = c("low", "med","high"))

# Create the plot
plot_e <- ggplot(concentric_df, aes(x = classification, y = Freq, fill= Var1, col = Var1)) +
  geom_col(size = 0.8) +
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
  ggtitle("e Peer review \n blinding format")


### Plot F: Whether the journal publishes reviews upon publication of manuscript
# Create table summary for different values
publishrev_df_low <- table(policy.data[policy.data$JIF_bin == "Low" | policy.data$JIF_bin == "NA", "journal.publish.peer.reviews"])
publishrev_df_med <- table(policy.data[policy.data$JIF_bin == "Mid", "journal.publish.peer.reviews"])
publishrev_df_high <- table(policy.data[policy.data$JIF_bin == "High", "journal.publish.peer.reviews"])

# Compute percentages
publishrev_df_low_percent = data.frame(publishrev_df_low / sum(publishrev_df_low))
publishrev_df_med_percent = data.frame(publishrev_df_med / sum(publishrev_df_med))
publishrev_df_high_percent = data.frame(publishrev_df_high / sum(publishrev_df_high))

# Reformat Freq column to numeric
publishrev_df_low_percent$Freq <- as.numeric(publishrev_df_low_percent$Freq)
publishrev_df_med_percent$Freq <- as.numeric(publishrev_df_med_percent$Freq)
publishrev_df_high_percent$Freq <- as.numeric(publishrev_df_high_percent$Freq)

# Change the labels for legend
levels(publishrev_df_high_percent$Var1) <- levels(publishrev_df_low_percent$Var1) <- levels(publishrev_df_med_percent$Var1) <- list("Publishes reviews" = "Yes", 
                                                                                                                                             "Does not publish reviews" = "No")

# Create the concentric donut dataframe
concentric_df <- rbind(publishrev_df_low_percent, publishrev_df_med_percent, publishrev_df_high_percent)
concentric_df$classification <- c(rep("low",2), rep("med",2), rep("high",2))
concentric_df$Freq <- as.numeric(concentric_df$Freq)
concentric_df$classification <- factor(concentric_df$classification, levels = c("low", "med","high"))

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
  ggtitle("f Publishes reviews")



### Plot G: Whether the journal has reviewer guidelines that are specific to journal or publisher
# Create bins for summarizing data
policy.data$ReviewerGuidelines_Publisher_Journal_Binned <- ifelse(policy.data$guidelines.to.be.reviewer == "No", "1",
                                                                  ifelse(policy.data$guidelines.to.be.reviewer == "Yes" & policy.data$guidelines.link.publisher == "Yes", "2",
                                                                         ifelse(policy.data$guidelines.to.be.reviewer == "Yes" & policy.data$guidelines.link.publisher == "No", "3", "NA")))

# Create table summary for different values
revguidelines_df_low <- table(policy.data[policy.data$JIF_bin == "Low" | policy.data$JIF_bin == "NA", "ReviewerGuidelines_Publisher_Journal_Binned"])
revguidelines_df_med <- table(policy.data[policy.data$JIF_bin == "Mid", "ReviewerGuidelines_Publisher_Journal_Binned"])
revguidelines_df_high <- table(policy.data[policy.data$JIF_bin == "High", "ReviewerGuidelines_Publisher_Journal_Binned"])

# Compute percentages
revguidelines_df_low_percent = data.frame(revguidelines_df_low / sum(revguidelines_df_low))
revguidelines_df_med_percent = data.frame(revguidelines_df_med / sum(revguidelines_df_med))
revguidelines_df_high_percent = data.frame(revguidelines_df_high / sum(revguidelines_df_high))

# Reformat Freq column to numeric
revguidelines_df_low_percent$Freq <- as.numeric(revguidelines_df_low_percent$Freq)
revguidelines_df_med_percent$Freq <- as.numeric(revguidelines_df_med_percent$Freq)
revguidelines_df_high_percent$Freq <- as.numeric(revguidelines_df_high_percent$Freq)

# Change the labels for legend
levels(revguidelines_df_high_percent$Var1) <- levels(revguidelines_df_low_percent$Var1) <- levels(revguidelines_df_med_percent$Var1) <- list("No guidelines" = "1", 
                                              "Guidelines link to publisher" = "2", 
                                              "Guidelines are journal-specific" = "3")

# Create the concentric donut dataframe
concentric_df <- rbind(revguidelines_df_low_percent, revguidelines_df_med_percent, revguidelines_df_high_percent)
concentric_df$classification <- c(rep("low",3), rep("med",3), rep("high",3))
concentric_df$Freq <- as.numeric(concentric_df$Freq)
concentric_df$classification <- factor(concentric_df$classification, levels = c("low", "med","high"))

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
  ggtitle("g Reviewer guidelines")


### Plot H: Whether journal's reviewer guidelines mention social justice
# Create bins for summarizing data
policy.data$ReviewerGuidelines_SocialJustice <- ifelse(policy.data$guidelines.to.be.reviewer == "No", "1",
                                                       ifelse(policy.data$guidelines.to.be.reviewer == "Yes" & policy.data$guidelines.social.justice == "No", "2",
                                                              ifelse(policy.data$guidelines.to.be.reviewer == "Yes" & policy.data$guidelines.social.justice == "Yes" & policy.data$guidelines.esl == "No", "3", "4")))

# Create table summary for different values
revguidelines_sj_df_low <- table(policy.data[policy.data$JIF_bin == "Low" | policy.data$JIF_bin == "NA", "ReviewerGuidelines_SocialJustice"])
revguidelines_sj_df_med <- table(policy.data[policy.data$JIF_bin == "Mid", "ReviewerGuidelines_SocialJustice"])
revguidelines_sj_df_high <- table(policy.data[policy.data$JIF_bin == "High", "ReviewerGuidelines_SocialJustice"])

# Compute percentages
revguidelines_sj_df_low_percent = data.frame(revguidelines_sj_df_low / sum(revguidelines_sj_df_low))
revguidelines_sj_df_med_percent = data.frame(revguidelines_sj_df_med / sum(revguidelines_sj_df_med))
revguidelines_sj_df_high_percent = data.frame(revguidelines_sj_df_high / sum(revguidelines_sj_df_high))

# Reformat Freq column to numeric
revguidelines_sj_df_low_percent$Freq <- as.numeric(revguidelines_sj_df_low_percent$Freq)
revguidelines_sj_df_med_percent$Freq <- as.numeric(revguidelines_sj_df_med_percent$Freq)
revguidelines_sj_df_high_percent$Freq <- as.numeric(revguidelines_sj_df_high_percent$Freq)

# Add in missing levels
revguidelines_sj_df_med_percent <- rbind(revguidelines_sj_df_med_percent , data.frame("Var1" = "4", "Freq" = "0"))
revguidelines_sj_df_high_percent <- rbind(revguidelines_sj_df_high_percent , data.frame("Var1" = "3", "Freq" = "0"))

# Change the labels for legend
levels(revguidelines_sj_df_high_percent$Var1) <- levels(revguidelines_sj_df_low_percent$Var1) <- levels(revguidelines_sj_df_med_percent$Var1) <- list("No guidelines" = "1", 
                                            "Guidelines w/o social justice" = "2", 
                                            "Guidelines w/ social justice" = "3",
                                            "Guidelines mention english" = "4")

# Create the concentric donut dataframe
concentric_df <- rbind(revguidelines_sj_df_low_percent, revguidelines_sj_df_med_percent, revguidelines_sj_df_high_percent)
concentric_df$classification <- c(rep("low",4), rep("med",4), rep("high",4))
concentric_df$Freq <- as.numeric(concentric_df$Freq)
concentric_df$classification <- factor(concentric_df$classification, levels = c("low", "med","high"))

# Create the plot
plot_h <- ggplot(concentric_df, aes(x = classification, y = Freq, fill= Var1, col = Var1)) +
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
  ggtitle("h Reviewer guidelines \n on social justice")



### Create bar charts for the diversity of suggested reviewers
revdiversity_low <- policy.data[policy.data$JIF_bin == "Low" | policy.data$JIF_bin == "NA", ]
revdiversity_med <- policy.data[policy.data$JIF_bin == "Mid", ]
revdiversity_high <- policy.data[policy.data$JIF_bin == "High", ]

# Geography, Institutions, Career, Gender, Ethnicity & Race, Other
# Low impact factor journals
revdiversity_low_geography <- table(revdiversity_low$guidelines.multiple.countries)
revdiversity_low_geography_prop <- data.frame(revdiversity_low_geography / sum(revdiversity_low_geography))

revdiversity_low_institution <- table(revdiversity_low$guidelines.multiple.institutions)
revdiversity_low_institution_prop <- data.frame(revdiversity_low_institution / sum(revdiversity_low_institution))

revdiversity_low_career <- table(revdiversity_low$guidelines.ecr)
revdiversity_low_career_prop <- data.frame(revdiversity_low_career / sum(revdiversity_low_career))

revdiversity_low_gender <- table(revdiversity_low$guidelines.gender)
revdiversity_low_gender_prop <- data.frame(revdiversity_low_gender / sum(revdiversity_low_gender))

revdiversity_low_ethnicity <- table(revdiversity_low$guidelines.another.demographic.enthnicity.or.race)
revdiversity_low_ethnicity_prop <- data.frame(revdiversity_low_ethnicity / sum(revdiversity_low_ethnicity))

revdiversity_low_other <- table(revdiversity_low$guidelines.another.demographic.other)
revdiversity_low_other_prop <- data.frame(revdiversity_low_other / sum(revdiversity_low_other))
revdiversity_low_other_prop <- rbind(revdiversity_low_other_prop, data.frame("Var1" = "Yes", "Freq" = "0"))

revdiversity_low_fulldf <- rbind(revdiversity_low_geography_prop[2,], revdiversity_low_institution_prop[2,],
                                      revdiversity_low_career_prop[2,], revdiversity_low_gender_prop[2,],
                                      revdiversity_low_ethnicity_prop[2,],revdiversity_low_other_prop[2,])
revdiversity_low_fulldf$Category <- c("Geography", "Institutions", "Career", "Gender", "Ethnicity & Race", "Other")
revdiversity_low_fulldf$IF <- "low"

# Medium impact factor journals
revdiversity_med_geography <- table(revdiversity_med$guidelines.multiple.countries)
revdiversity_med_geography_prop <- data.frame(revdiversity_med_geography / sum(revdiversity_med_geography))

revdiversity_med_institution <- table(revdiversity_med$guidelines.multiple.institutions)
revdiversity_med_institution_prop <- data.frame(revdiversity_med_institution / sum(revdiversity_med_institution))

revdiversity_med_career <- table(revdiversity_med$guidelines.ecr)
revdiversity_med_career_prop <- data.frame(revdiversity_med_career / sum(revdiversity_med_career))

revdiversity_med_gender <- table(revdiversity_med$guidelines.gender)
revdiversity_med_gender_prop <- data.frame(revdiversity_med_gender / sum(revdiversity_med_gender))

revdiversity_med_ethnicity <- table(revdiversity_med$guidelines.another.demographic.enthnicity.or.race)
revdiversity_med_ethnicity_prop <- data.frame(revdiversity_med_ethnicity / sum(revdiversity_med_ethnicity))

revdiversity_med_other <- table(revdiversity_med$guidelines.another.demographic.other)
revdiversity_med_other_prop <- data.frame(revdiversity_med_other / sum(revdiversity_med_other))
#revdiversity_med_other_prop <- rbind(revdiversity_med_other_prop, data.frame("Var1" = "Yes", "Freq" = "0"))

revdiversity_med_fulldf <- rbind(revdiversity_med_geography_prop[2,], revdiversity_med_institution_prop[2,],
                                 revdiversity_med_career_prop[2,], revdiversity_med_gender_prop[2,],
                                 revdiversity_med_ethnicity_prop[2,],revdiversity_med_other_prop[2,])
revdiversity_med_fulldf$Category <- c("Geography", "Institutions", "Career", "Gender", "Ethnicity & Race", "Other")
revdiversity_med_fulldf$IF <- "med"


# High impact factor journals
revdiversity_high_geography <- table(revdiversity_high$guidelines.multiple.countries)
revdiversity_high_geography_prop <- data.frame(revdiversity_high_geography / sum(revdiversity_high_geography))

revdiversity_high_institution <- table(revdiversity_high$guidelines.multiple.institutions)
revdiversity_high_institution_prop <- data.frame(revdiversity_high_institution / sum(revdiversity_high_institution))
revdiversity_high_institution_prop <- rbind(revdiversity_high_institution_prop, data.frame("Var1" = "Yes", "Freq" = "0"))

revdiversity_high_career <- table(revdiversity_high$guidelines.ecr)
revdiversity_high_career_prop <- data.frame(revdiversity_high_career / sum(revdiversity_high_career))

revdiversity_high_gender <- table(revdiversity_high$guidelines.gender)
revdiversity_high_gender_prop <- data.frame(revdiversity_high_gender / sum(revdiversity_high_gender))

revdiversity_high_ethnicity <- table(revdiversity_high$guidelines.another.demographic.enthnicity.or.race)
revdiversity_high_ethnicity_prop <- data.frame(revdiversity_high_ethnicity / sum(revdiversity_high_ethnicity))

revdiversity_high_other <- table(revdiversity_high$guidelines.another.demographic.other)
revdiversity_high_other_prop <- data.frame(revdiversity_high_other / sum(revdiversity_high_other))
revdiversity_high_other_prop <- rbind(revdiversity_high_other_prop, data.frame("Var1" = "Yes", "Freq" = "0"))

revdiversity_high_fulldf <- rbind(revdiversity_high_geography_prop[2,], revdiversity_high_institution_prop[2,],
                                 revdiversity_high_career_prop[2,], revdiversity_high_gender_prop[2,],
                                 revdiversity_high_ethnicity_prop[2,],revdiversity_high_other_prop[2,])
revdiversity_high_fulldf$Category <- c("Geography", "Institutions", "Career", "Gender", "Ethnicity & Race", "Other")
revdiversity_high_fulldf$IF <- "high"


# Pull all of the data together to create bar charts
revdiversity_all_plotting_df <- rbind(revdiversity_low_fulldf, revdiversity_med_fulldf, revdiversity_high_fulldf)
revdiversity_all_plotting_df$Freq <- as.numeric(revdiversity_all_plotting_df$Freq)

revdiversity_all_plotting_df$IF <- factor(revdiversity_all_plotting_df$IF, levels = c("low", "med", "high"))
levels(revdiversity_all_plotting_df$IF) <- list ("Low IF" = "low", "Medium IF" = "med", "High IF" = "high")

### Plot D: Diversity axes mentioned for suggest reviewers
plot_d <- ggplot(data = revdiversity_all_plotting_df, aes(x = Category, y = Freq, fill = IF)) + 
  geom_bar(stat="identity", color="black", position=position_dodge())+
  scale_fill_viridis_d() +
  theme_bw() + theme(panel.border = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             strip.text = element_blank(),
             text = element_text(size = 20), 
             legend.position = "none") + 
  coord_flip() + facet_wrap(~IF, nrow = 3) + ylab("Proportion of journals") + 
  xlab("") +
  ggtitle("d Diversity Axes for Suggested Reviewers")
