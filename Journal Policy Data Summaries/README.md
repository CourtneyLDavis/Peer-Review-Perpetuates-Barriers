# Peer review bias perpetuates barriers for historically excluded groups
## In Review 

Olivia M. Smith*, Kayla L. Davis, Riley B. Pizza, Robin Waterman, Kara C. Dobson, Brianna Foster, Julie Jarvey, Leonard N Jones, Wendy Leuenberger, Nan Nourn, Emily Conway, Cynthia Fiser, Zoe Hansen, Ani Hristova, Caitlin Mack, Alyssa N. Saunders, Olivia J. Utley, Moriah L. Young, Courtney L. Davis 

*Please contact the first author with any questions about the code or publication: Olivia M. Smith (smitho17@msu.edu)

&nbsp;

# Key Notes
We describe the current landscape of peer review in the subfields of ecology and evolution by collecting peer review policy data from the websites of 541 journals. For each journal included in our dataset, we collected data from the website(s) of each journal pertaining to: 
 1. the journal’s article language requirements;
 2. language editing services available to authors;
 3. journal prompts for authors to suggest diverse reviewers and what types of diversity authors were prompted to focus on in suggestions;
 4. if the journal had reviewer guidelines at all, and if so, if they were specific to the journal or linked to a general publisher website (to be counted, reviewer or referee guidelines must have been listed as such, either in a separate webpage/document or as part of the author guidelines);
 5. if reviewer guidelines mentioned social justice issues, and in particular, behaviors toward authors for whom English is a non-primary language;
 6. what country the Editor-in-Chief(s)’ primary institution was in;
 7. if the journal is affiliated with a society;
 8. what peer review model (single-, double-, or triple-blind; open; other) the journal used; and 
 9. if the journal publishes referee reports with manuscripts.

These data (Supplementary Data 2 in main text) are included in `Data` > `Dataset S2 EcoEvo Journal Policies.csv` and summarized using the R scripts detailed below. 

&nbsp;

In our journal policy data collection, Elsevier journals were the only ones to provide editor gender data on their websites (and this information was only available on 22 out of 60 journal websites; Supplementary Data 3 associated with main text). These data are included in `Data` > Dataset S3 Elsevier editor data.xlsx` but not analyzed for the main text.

&nbsp;

# Scripts
- `Journal Policies by Impact Factor.R`: R Script used to summarize policies from 541 ecology and evolution journals by journal impact factor (JIF);  corresponds to Figure 5 in main text.
- `Journal Policies.R`: R Script used to summarize policies from 541 ecology and evolution journals; corresponds to Supplementary Figure 9.
- `Journal Policies by Publisher Group.R`: R Script used to summarize policies from 541 ecology and evolution journals by publisher group (i.e., major vs. minor publisher and society-affiliated vs. not affiliated); corresponds to Supplementary Figure 10.
- `Journal EIC Count by Impact Factor.R`: R Script used to map the number of ecology and evolution journals and Editor in Chiefs (EICs) by journal impact factor (JIF); corresponds to Figure 6 in main text.
- `Journal EIC Count by Publisher Group.R`: R Script used to map the number of ecology and evolution journals and Editor in Chiefs (EICs) by publisher group (i.e., major vs. minor publisher and society-affiliated vs. not affiliated); corresponds to Supplementary Figures 11 and 12.
- `Journal EIC Count Barplots.R`: R Script used to summarize the number of ecology and evolution journals and Editor in Chiefs (EICs) per country; corresponds to Supplementary Figure 13.
- `Journal EIC Count Scaled by Pop.R`: R Script used to map the number of ecology and evolution journals and Editor in Chiefs (EICs), scaled by population size of the country; corresponds to Supplementary Figure 14.
- `Journal EIC Count by Impact Factor in Europe.R`: R Script used to map the number of ecology and evolution journals and Editor in Chiefs (EICs) in European countries by journal impact factor (JIF); corresponds to Supplementary Figure 15.
