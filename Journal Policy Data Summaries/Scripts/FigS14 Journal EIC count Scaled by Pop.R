############################################################
#### Journal and EIC count maps scaled by population    ####
#### Created by: Nan Nourn                              #### 
#### Modified by: Julie Jarvey                          ####
#### Last modified: 16 August 2022                      ####
############################################################


############ Set-up
############
# Load packages
library(tidyverse)
library(sf)
library(tmap)
library(grid)
library(tidytext)
library(gt)
library(janitor)
library(magrittr)
library(here)


############ Format journal policy data
journals <- read_csv(here("Journal Policy Summaries", "Data", "JournalPolicies_Data_2022.csv"))

# cleans column titles
journals %<>% clean_names()

# glimpse at tibble
journals %>% glimpse()

# rename all uppercase country names to capitalizing only the first letter
journals %<>% mutate(name = str_to_title(country_based_in_on_jcr))

############ Set global options and theme
options(scipen = 10) # use exponential notation for numbers larger than 10 digits 
theme_set(theme_minimal())


# use world basemap from tmap
data("World")

# change relevant country names for the tmap::World basemap
# we change country names from loaded excel datafile to match names of country of Worldmap
World %<>% mutate(name = as.character(name))
World$name[41] <- "Czech Republic"
World$name[88] <- "Korea, Republic Of"

# mutate log(pop_est)
World %<>% mutate(pop_est_per_million = pop_est/10^6)  # find year data were collected

#journals$name <- as.factor(journals$name)
journals %<>% filter(name != "Singapore") # Singapore is not included in world basemap, so need to filter out so the geometry works for mapping

# join journals with World 
world_journals <- left_join(journals, World, by = "name") %>% 
  st_as_sf()

# count journal by country then plot
world_journals_count <- world_journals %>% 
  count(name, sort = TRUE)

## Journal count 
world_journals_per_pop_million <- world_journals %>% 
  count(name, pop_est_per_million, sort = TRUE) %>% 
  mutate(journals_per_pop_million = n/pop_est_per_million)


### make maps #############
b <- st_bbox(c(xmin = -160, xmax = 165, ymax = 90, ymin = -60), crs = st_crs(4326))

journals.pop <- tm_shape(World, bb = b, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(world_journals_per_pop_million,
           projection = "+proj=robin") +
  tm_fill(col = "journals_per_pop_million", title = "# of journals/1 mill", 
          alpha = 0.75, palette = "viridis", style = "jenks") +
# tm_credits("a. All journals", position = c(0.001, 0.87), size = 0.8) +
  tm_layout(legend.position  = c(0.01,0.01), main.title = "a All journals",  main.title.position = "left", main.title.size = 0.8,
            legend.title.size = 0.65, legend.text.size = 0.5)

journals.pop


b.eur <- st_bbox(c(xmin = -9, xmax = 43, ymax = 72, ymin = 34), crs = st_crs(4326))

journals.pop.eur <- tm_shape(World, bb = b.eur, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(world_journals_per_pop_million,
           projection = "+proj=robin") +
  tm_fill(col = "journals_per_pop_million", title = "# of journals/1 mill", 
          alpha = 0.75, palette = "viridis", style = "jenks") +
#  tm_credits("c. All journals", position = c(0.001, 0.89), size = 1.1) +
  tm_layout(legend.outside = FALSE, panel.show = FALSE, legend.bg.color = 'white', legend.frame = T, 
            main.title = "c All journals", main.title.position = "left", main.title.size = 0.8,
            legend.position = c(0.66,0.64), legend.title.size = 0.7, legend.text.size = 0.5)

journals.pop.eur


# EIC count ##############
# Editors
#H ere, we determine (1st) editors in chiefs (EIC) by country in a new section because we will be joining another 
# round of dataframes/tibbles. Remember, our first join `world_journals` was based on countries + journals with 
# the accompany geometries. We have to make a new join (thus a new data frame/sf/tibble object) to have the correct 
# polygon geometries of the corresponding countries.

# create eic tibble and create name column for future join
eic <- journals %>% 
  select(journal, name, jcr_2020_if, jif_bin, eic_01, eic_02, eic_03, eic_04, eic_05, eic_06, eic_07, eic_08, eic_09)

# create long format for one column of EICs denoted by rank editor
eic_long <- eic %>% 
  pivot_longer(cols = 5:13, names_to = "variable", values_to = "value")

# this rearranges the factor level of the variable column
eic_long %<>%
  mutate(variable = fct_relevel(variable, "eic_01", "eic_02", "eic_03", "eic_04", "eic_05", "eic_06",
                                "eic_07", "eic_08", "eic_09"))

eic_total_count_long <- eic_long %>%  # use value not name (name is journal country)
  count(value, sort = TRUE) %>%
  drop_na()

eic_total_count <- eic %>%
  count(name, sort = TRUE)
# we create a column that follows the first EIC and the corresponding country and call it "name" to match with basemap
# have to remove Singapore because it is not recognize in world basemaps

eic_total_count_long %<>% 
  dplyr::filter(value!="Singapore")

# join with World by name
eic_total_count_long$name <- eic_total_count_long$value

#eic.tot <- eic_total_count_long %>%
#  select(-value)
eic_total_journals <- left_join(eic_total_count_long, World, by = "name") %>%   st_as_sf()
eic_total_journals %<>% 
  mutate(eic_pop = n/pop_est,
         eic_pop_million = n/pop_est_per_million) 

## make maps ####
EIC.pop <- tm_shape(World, bb = b, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(eic_total_journals, projection = "+proj=robin") +
  tm_fill(col = "eic_pop_million", title = "# of EICs/1 mill", 
          alpha = 0.75, palette = "viridis", style = "jenks") +
  #  tm_credits("b. All editors-in-chief", position = c(0.001, 0.92), size = 0.8) +
  tm_layout(legend.position = c(0.01,0.01), main.title = "b All EICs",  main.title.position = "left", main.title.size = 0.8,
            legend.title.size = 0.65, legend.text.size = 0.5)

EIC.pop


# map for Europe
EIC.pop.eur <- tm_shape(World, bb = b.eur, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(eic_total_journals, projection = "+proj=robin") +
  tm_fill(col = "eic_pop_million", title = "# of EICs/1 mill", 
          alpha = 0.75, palette = "viridis", style = "jenks") +
 # tm_credits("d. All editors-in-chief", position = c(0.001, 0.9), size = 1) +
  tm_layout(legend.outside = FALSE, panel.show = FALSE, legend.bg.color = 'white', legend.frame = T, 
            main.title = "d All EICs", main.title.position = "left", main.title.size = 0.8,
            legend.position = c(0.67,0.64), legend.title.size = 0.7, legend.text.size = 0.5)

EIC.pop.eur

#### save plots
tiff(file="S14_journals.eic.scaled.tiff", height = 5, width = 7, units = "in", res = 300, bg = "white")
grid.newpage()
pushViewport(viewport(layout=grid.layout(
  nrow = 2,
  ncol = 2,
  heights = )))
print(journals.pop, vp = viewport(layout.pos.col = 1, layout.pos.row = 1)) #, height = 0.75, width = 0.5)) #, x = 0, y = 0.75))
print(EIC.pop, vp = viewport(layout.pos.col = 2, layout.pos.row = 1)) #, height = 0.75, width = 0.5)) #, x = 0.5, y = 0.75))
print(journals.pop.eur, vp=viewport(layout.pos.col = 1, layout.pos.row = 2)) ##, height = 0.25, width = 0.5)) #, x = 0, y = 0))
print(EIC.pop.eur, vp=viewport(layout.pos.col = 2, layout.pos.row = 2)) ##, height = 0.25, width = 0.5)) #, x = 0.5, y = 0))
dev.off()
