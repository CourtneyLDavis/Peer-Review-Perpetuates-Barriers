#################################################
#### Plotting journal policy data            ####
#### Created by: Courtney L Davis            #### 
#### Last modified: 18 August 2022           ####
#################################################

# Load in the necessary libraries
library(ggplot2)
library(here)


# Read in the journal policy data
policy.data <- read.csv(here("Journal Policy Data Summaries","Data","Dataset S2 EcoEvo Journal Policies.csv"))

##### Code to create pie charts describing journal policy data ####

### Plot A: By publisher of journal - broken down in to top 5 publishers and "other"
# Create bins for summarizing data
policy.data$Publisher_Top_Binned <- ifelse(policy.data$Publisher.from.JCR.but.binned.by.larger.company == "SPRINGER", "1",
                                           ifelse(policy.data$Publisher.from.JCR.but.binned.by.larger.company == "WILEY", "2",
                                                  ifelse(policy.data$Publisher.from.JCR.but.binned.by.larger.company == "ELSEVIER", "3",
                                                         ifelse(policy.data$Publisher.from.JCR.but.binned.by.larger.company == "TAYLOR AND FRANCIS", "4",
                                                                ifelse(policy.data$Publisher.from.JCR.but.binned.by.larger.company == "OXFORD UNIVERSITY PRESS", "5", "6")))))

# Create table summary for different values
publisher_df <- table(policy.data[, "Publisher_Top_Binned"])

# Convert table summary into a dataframe with proportions
publisher_df_percent = data.frame(publisher_df / sum(publisher_df))
publisher_df_percent$Freq <- as.numeric(publisher_df_percent$Freq) # convert to numeric for plotting

# Re-assign level names for plotting
levels(publisher_df_percent$Var1) <- list("Springer" = "1", 
                                         "Wiley" = "2", 
                                         "Elsevier" = "3",
                                         "Taylor & Francis" = "4", 
                                         "Oxford University Press" = "5",
                                         "Other" = "6")

# Create the plot
plot_a <- ggplot(publisher_df_percent, aes(x = "", y = Freq, fill= Var1, col = Var1)) +
  geom_bar(stat = "identity", width = 1) +
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
language_df <- table(policy.data[, "language.requirements.binned"])

# Convert table summary into a dataframe with proportions
language_df_percent = data.frame(language_df / sum(language_df))
language_df_percent$Freq <- as.numeric(language_df_percent$Freq) # convert to numeric for plotting

# Change the labels for legend
levels(language_df_percent$Var1) <- list("No mention of language editing" = "1", 
                                        "Publishes in multiple languages" = "2", 
                                        "Offers free language editing" = "3",
                                        "Offers but doesn’t suggest use of editing" = "4", 
                                        "Mentions “good” English, points to editing" = "5",
                                        "Requires or strongly recommends editing" = "6",
                                        "Other" = "7")

# Create the plot
plot_b <- ggplot(language_df_percent, aes(x = "", y = Freq, fill= Var1, col = Var1)) +
  geom_bar(stat = "identity", width = 1) +
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
  ggtitle("b Language \n statement")


### Plot C: Whether authors are required to submit names of diverse reviewers
# Data have already been binned accordingly:
# Journal doesn't require that authors suggest reviewers = 1
# Journal asks for suggested reviewers but does not mention diversity of reviewers = 2
# Journal ask for suggested reviewers and explicitly mentions 1 or more axes of diversity of reviewers = 3

# Create table summary for different values
suggest_df <- table(policy.data[, "authors.suggest.diverse.reviews.binned"])

# Convert table summary into a dataframe with proportions
suggest_df_percent = data.frame(suggest_df / sum(suggest_df))
suggest_df_percent$Freq <- as.numeric(suggest_df_percent$Freq) # convert to numeric for plotting

# Change the labels for legend
levels(suggest_df_percent$Var1) <- list("Doesn't require suggestions" = "1", "Required or Optional, no diversity" = "2",
                                         "Required or Optional, mentions diversity" = "3")

# Create the plot
plot_c <- ggplot(suggest_df_percent, aes(x = "", y = Freq, fill= Var1, col = Var1)) +
  geom_bar(stat = "identity", width = 1) +
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
journal_blinding_df <- table(policy.data[, "blind.format"])

# Convert table summary into a dataframe with proportions
journal_blinding_df_percent = data.frame(journal_blinding_df / sum(journal_blinding_df))
journal_blinding_df_percent$Freq <- as.numeric(journal_blinding_df_percent$Freq) # convert to numeric for plotting

# Re-order levels of factor
journal_blinding_df_percent$Var1 <- factor(journal_blinding_df_percent$Var1, levels = c("Assumed_single", "Single", "Double_optional","Double_required", "Open", "Other"))

# Change the labels for legend
levels(journal_blinding_df_percent$Var1) <- list("Assumed Single" = "Assumed_single","Single" = "Single", "Double - optional" = "Double_optional", "Double - required" = "Double_required",
                                        "Open" = "4", "Other" = "5")

# Create the plot
plot_e <- ggplot(journal_blinding_df_percent, aes(x = "", y = Freq, fill= Var1, col = Var1)) +
  geom_bar(stat = "identity", width = 1) +
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
  ggtitle("e Peer review \n blinding format")


### Plot F: Whether the journal publishes reviews upon publication of manuscript
# Create table summary for different values
publishrev_df <- table(policy.data[, "journal.publish.peer.reviews"])

# Convert table summary into a dataframe with proportions
publishrev_df_percent = data.frame(publishrev_df / sum(publishrev_df))
publishrev_df_percent$Freq <- as.numeric(publishrev_df_percent$Freq) # convert to numeric for plotting

# Change the labels for legend
levels(publishrev_df_percent$Var1) <-  list("Publishes reviews" = "Yes", 
                                          "Does not publish reviews" = "No")
# Create the plot
plot_f <- ggplot(publishrev_df_percent, aes(x = "", y = Freq, fill= Var1, col = Var1)) +
  geom_bar(stat = "identity", width = 1) +
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
  ggtitle("f Publish reviews")


### Plot G: Whether the journal has reviewer guidelines that are specific to journal or publisher
# Create bins for summarizing data
policy.data$ReviewerGuidelines_Publisher_Journal_Binned <- ifelse(policy.data$guidelines.to.be.reviewer == "No", "1",
                                                                  ifelse(policy.data$guidelines.to.be.reviewer == "Yes" & policy.data$guidelines.link.publisher == "Yes", "2",
                                                                         ifelse(policy.data$guidelines.to.be.reviewer == "Yes" & policy.data$guidelines.link.publisher == "No", "3", "NA")))

# Create table summary for different values
revguide_df <- table(policy.data[, "ReviewerGuidelines_Publisher_Journal_Binned"])

# Convert table summary into a dataframe with proportions
revguide_df_percent = data.frame(revguide_df / sum(revguide_df))
revguide_df_percent$Freq <- as.numeric(revguide_df_percent$Freq) # convert to numeric for plotting

# Change the labels for legend
levels(revguide_df_percent$Var1) <- list("No guidelines" = "1", "Guidelines link to publisher" = "2", "Guidelines are journal-specific" = "3")

# Create the plot
plot_g <- ggplot(revguide_df_percent, aes(x = "", y = Freq, fill= Var1, col = Var1)) +
  geom_bar(stat = "identity", width = 1) +
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
  ggtitle("g Reviewer guidelines")


### Plot H: Whether journal's reviewer guidelines mention social justice
# Create bins for summarizing data
policy.data$ReviewerGuidelines_SocialJustice <- ifelse(policy.data$guidelines.to.be.reviewer == "No", "1",
                                                       ifelse(policy.data$guidelines.to.be.reviewer == "Yes" & policy.data$guidelines.social.justice == "No", "2",
                                                              ifelse(policy.data$guidelines.to.be.reviewer == "Yes" & policy.data$guidelines.social.justice == "Yes" & policy.data$guidelines.esl == "No", "3", "4")))

# Create table summary for different values
revguide_sj_df <- table(policy.data[, "ReviewerGuidelines_SocialJustice"])

# Convert table summary into a dataframe with proportions
revguide_sj_df_percent = data.frame(revguide_sj_df / sum(revguide_sj_df))
revguide_sj_df_percent$Freq <- as.numeric(revguide_sj_df_percent$Freq) # convert to numeric for plotting

# Change the labels for legend
levels(revguide_sj_df_percent$Var1) <- list("No guidelines" = "1", "Guidelines w/o social justice" = "2", "Guidelines w/ social justice" = "3","Guidelines mention english" = "4")

# Create the plot
plot_h <- ggplot(revguide_sj_df_percent, aes(x = "", y = Freq, fill= Var1, col = Var1)) +
  geom_bar(stat = "identity", width = 1) +
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
  ggtitle("h Reviewer guidelines \n on social justice")




### Create bar charts for the diversity of suggested reviewers
revdiversity_geography <- table(policy.data$guidelines.multiple.countries)
revdiversity_geography_prop <- data.frame(revdiversity_geography / sum(revdiversity_geography))

revdiversity_institution <- table(policy.data$guidelines.multiple.institutions)
revdiversity_institution_prop <- data.frame(revdiversity_institution / sum(revdiversity_institution))

revdiversity_career <- table(policy.data$guidelines.ecr)
revdiversity_career_prop <- data.frame(revdiversity_career / sum(revdiversity_career))

revdiversity_gender <- table(policy.data$guidelines.gender)
revdiversity_gender_prop <- data.frame(revdiversity_gender / sum(revdiversity_gender))

revdiversity_ethnicity <- table(policy.data$guidelines.another.demographic.enthnicity.or.race)
revdiversity_ethnicity_prop <- data.frame(revdiversity_ethnicity / sum(revdiversity_ethnicity))

revdiversity_other <- table(policy.data$guidelines.another.demographic.other)
revdiversity_other_prop <- data.frame(revdiversity_other / sum(revdiversity_other))

revdiversity_fulldf <- rbind(revdiversity_geography_prop[2,], revdiversity_institution_prop[2,],
                             revdiversity_career_prop[2,], revdiversity_gender_prop[2,],
                             revdiversity_ethnicity_prop[2,],revdiversity_other_prop[2,])
revdiversity_fulldf$Category <- c("Geography", "Institutions", "Career", "Gender", "Ethnicity & Race", "Other")


# Pull all of the data together to create bar charts
revdiversity_fulldf$Freq <- as.numeric(revdiversity_fulldf$Freq)

### Plot D: Diversity axes mentioned for suggest reviewers
plot_d <- ggplot(data = revdiversity_fulldf, aes(x = Category, y = Freq)) + 
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "#440154FF")+
  theme_bw() + theme(panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     strip.background = element_blank(),
                     strip.text = element_blank(),
                     text = element_text(size = 20), 
                     legend.position = "none") + 
  coord_flip() + ylab("Proportion of journals") + 
  xlab("") +
  ggtitle("d Diversity Axes for \n Suggested Reviewers")
