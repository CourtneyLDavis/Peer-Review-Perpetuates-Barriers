
# Peer review bias perpetuates barriers for historically excluded groups
In Review 

Olivia M. Smith*, Kayla L. Davis, Riley B. Pizza, Robin Waterman, Kara C. Dobson, Brianna Foster, Julie Jarvey, Leonard N Jones, Wendy Leuenberger, Nan Nourn, Emily Conway, Cynthia Fiser, Zoe Hansen, Ani Hristova, Caitlin Mack, Alyssa N. Saunders, Olivia J. Utley, Moriah L. Young, Courtney L. Davis 

*Please contact the first author with any questions about the code or publication: Olivia M. Smith (smitho17@msu.edu)



### Key Notes
Each review stage has its own .csv file, separated by (1) bias without interventions, (2) double-blind vs. single-blind, (3) Editor homophily, and (4) reviewer homophily. Demographics and author positions are pooled in a file. The R code separates demographics/positions prior to analysis. There are three exceptions:
1. Data broken down into individual countries are separated into their own review stage files. This is because each country has additional information. These files are designated as “[REVIEW STAGE] [DATASET] COUNTRY” 

2. For journal nationalism/journal country, all stage data are combined by this demographic category into one file with binomial outcome data (“Journal nationalism problem binomial.csv”; pre-initial, initial, post-initial, final, and overall decisions) and one with non-binomial outcome data (“Journal nationalism problem nonbinomial.csv”; review scores, time of review, number reviewers, and number revisions). This is because the file has extra information pertaining to the journal location.

3. The number of submissions files have one file with all data, “Number submissions data problem.csv,” and a series of 6 files created to allow for Likelihood Ratio Tests. The authorship positions are divided into different files due to the data formatting differing from other review aspects. The six include:
i. “Number submissions data FIRST CONTINENT problem.csv” that has complete observations for the first author’s affiliation’s continent
ii. “Number submissions data CORR CONTINENT problem.csv” that has complete observations for the corresponding author’s affiliation’s continent
iii. “Number submissions data LAST CONTINENT problem.csv” that has complete observations for the last author’s affiliation’s continent
iv. “Number submissions data FIRST COUNTRY problem.csv” that has complete observations for the first author’s affiliation’s continent, language, and Human Development Index
v. “Number submissions data CORR COUNTRY problem.csv” that has complete observations for the corresponding author’s affiliation’s continent, language, and Human Development Index
vi. “Number submissions data LAST COUNTRY problem.csv” that has complete observations for the last author’s affiliation’s continent, language, and Human Development Index


Data examining bias without considering interventions is labelled “problem”; data examining impacts of possible solutions is labelled “solution”. 


### Scripts
- `Problem-end analyses for binomial data.R`: Code for analyses looking at bias in the absence of interventions are divided into review stages/aspects that are binomial (rejected, not rejected), including pre-initial, initial, post-initial review, and overall decisions
- `Problem-end analyses for NON binomial data.R`: Code for analyses looking at bias in the absence of interventions are divided into review stages/aspects that are NOT binomial
- `Solution-end analyses DOUBLE-BLIND.R`: Code for analyses examining efficacy of double-blind review
- `Solution-end analyses HOMOPHILY.R`: Code for analyses examining assessor homophily
