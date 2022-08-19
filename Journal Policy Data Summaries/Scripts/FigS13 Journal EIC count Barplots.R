################################################################
#### Bar plots of journals and editors-in-chief by country  ####
#### Created by: Nan Nourn                                  #### 
#### Modified by: Julie Jarvey                              ####
#### Last modified: 16 August 2022                          ####
################################################################

############ Set-up
############
# Load packages
library(here)
library(tidyverse)
library(magrittr)
library(gridExtra)
library(tidytext)
library(magrittr)
library(janitor)


############ Format journal policy data
journals <- read_csv(here("Journal Policy Summaries", "Data", "EJournalPolicies_Data_2022.csv"))

# cleans column titles
journals %<>% clean_names()

# glimpse at tibble
journals %>% glimpse()

# rename all uppercase country names to capitalizing only the first letter
journals %<>% mutate(name = str_to_title(country_based_in_on_jcr))

############ Set global options and theme
options(scipen = 10) # use exponential notation for numbers larger than 10 digits 
theme_set(theme_minimal())


## Counting #############################################################################################################################
# Note - Singapore is included in bar plots, but is excluded in the maps since the world map base layer does not have it
# This bar plot shows the number of journals by country in the dataset.

# journals by country
j.count <- journals %>%
  group_by(name) %>%
  summarise(count = n())%>% 
  drop_na() %>%
  arrange(desc(count))

# create bar plot 
j.count.plot <- ggplot(j.count, aes(x = count, y = fct_reorder(name, desc(-count)))) +
  geom_col() +
  scale_x_continuous(expand = c(0,0)) +
  labs(title = "a. Number of EEB journals per country")+
  theme(axis.title.y = element_blank())  # remove y axis label

j.count.plot

### Editors ################################################################################################################
# The barplot shows the total number of editors-in-chief by country in the journals dataset. 
# Here, we determine (1st) editors in chief (EIC) by country in a new section because we will be joining another round of 
# dataframes/tibbles. Remember, our first join `world_journals` was based on countries + journals with the accompany geometries. 
# We have to make a new join (thus a new data frame/sf/tibble object) to have the correct polygon geometries of the corresponding 
# countries.

# create eic tibble and create name column for future join
# select jif and publisher/society columns for maps 
eic <- journals %>% 
  select(journal, name, jcr_2020_if, jif_bin, eic_01, eic_02, eic_03, eic_04, eic_05, eic_06, eic_07, eic_08, eic_09, society_publisher)

# create long format for one column of EICs denoted by rank editor
eic_long <- eic %>% 
  pivot_longer(cols = 5:13, names_to = "variable", values_to = "value")

# this rearranges the factor level of the variable column
eic_long %<>%
  mutate(variable = fct_relevel(variable, "eic_01", "eic_02", "eic_03", "eic_04", "eic_05", "eic_06",
                                "eic_07", "eic_08", "eic_09"))

# total EIC by country
eic_total_count <- eic_long %>% # use eic_long to count all eics
  count(value, sort = TRUE) %>% # use value not name, otherwise this is the same count as # journals
  drop_na()

# bar chart raw count
eic.plot <- ggplot(eic_total_count, aes(x = n, y = fct_reorder(value, desc(-n)))) +
  geom_col() +
  scale_x_continuous(expand = c(0,0)) +
  labs(title = "b. Number of EEB journal EICs per country", x ="count") +
  theme(axis.title.y = element_blank())  # remove y axis label
eic.plot

# arrange journal and eic plots side by side
counts <- grid.arrange(j.count.plot, eic.plot, ncol= 2)

# save plot 
ggsave(here("S13_journals.eic.barplots.tif"), plot = counts, device = "tiff", width = 5,
       height = 4, scale = 2, units = "in")     
