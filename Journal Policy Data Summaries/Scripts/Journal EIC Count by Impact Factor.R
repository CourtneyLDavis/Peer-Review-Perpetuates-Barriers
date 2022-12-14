#############################################################
#### Journal and EIC count maps by journal impact factor ####
#### Created by: Nan Nourn                               #### 
#### Modified by: Julie Jarvey, Courtney Davis           ####
#### Last checked: 30 November 2022                      ####
#############################################################


############ Set-up
############
# Load packages
library(here)
library(tidyverse)
library(magrittr)
library(sf)
library(tmap)
library(grid)
library(tidytext)
library(janitor)


############ Format journal policy data
journals <- read_csv(here("Journal Policy Data Summaries", "Data", "Dataset S2 EcoEvo Journal Policies.csv"))

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


#journals$name <- as.factor(journals$name)
journals2 <- journals %>% filter((name != "Singapore") %>% replace_na(TRUE))
journals %<>% filter(name != "Singapore") # Singapore is not included in world basemap, so need to filter out so the geometry works for mapping

#######################################################################################################


### Journal counts all ##########################################
# load world map from tmap package
# sf_use_s2(TRUE)
# join world + journal data frames

world_journals <- left_join(journals, World, by = "name") %>% 
  st_as_sf()

# count journal by country then plot

world_journals_count <- world_journals %>% 
  count(name, sort = TRUE)

# set tmap mode

tmap_mode("plot")

# plot journal counts by country for Europe

b <- st_bbox(c(xmin = -160, xmax = 165, ymax = 90, ymin = -60), crs = st_crs(4326))

journals.all <- tm_shape(World, bb=b, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(world_journals_count, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of journals", 
          alpha = 0.75, palette = "viridis", style = "jenks", labels = c("1 to 5", "6 to 12", "13 to 29", "30 to 50", "51 to 111")) +
  tm_credits("a All journals", 
             position = c(0.01, 0.9), size = 0.8) +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01,0.01), legend.title.size = 0.6, legend.text.size = 0.5,
            legend.format = list(format = "f", text.separator = "<"))  
journals.all

### JIF plots for journal counts ###################################################
# change JIFs with 'NA' to 'None' for mapping
journals$jif_bin[is.na(journals$jcr_2020_if)] <- 'No IF'

journals_if <- journals %>% 
  dplyr::select(journal, name, jcr_2020_if, jif_bin)

# here, we count but do not drop journals without an impact factor
# alternatively, we can drop the NAs if we wanted too 

journals_if_count <- journals_if %>% 
  count(name, jif_bin, sort = TRUE)

# join world + journal data frames

journals_if_count_sf <- left_join(journals_if_count, World, by = "name") %>% 
  st_as_sf()

# map of EEB journals per country faceted by IF factor 
journals_if_count_sf$jif_bin <- factor(journals_if_count_sf$jif_bin, levels = c("High", "Mid", "Low", "No IF"))

jif.map <- tm_shape(World, bb = b, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(journals_if_count_sf, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of journals", 
          alpha = 0.75, palette = "viridis", style = "jenks",  labels = c("1 to 4", "5 to 11", "12 to 21", "22 to 36", "37 to 52")) +
  tm_facets(by = "jif_bin", nrow = 4, ncol = 1,
            free.coords = FALSE) +
  tm_credits(c("c High JIF", "e Mid JIF", "g Low JIF", "i No JIF"), 
           position = c(0.001, 0.9), size = 0.65) +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01,0.01), legend.title.size = 0.6, legend.text.size = 0.5)  
jif.map


#############################################################################
## EIC Maps ####################################################################
# we create a column that follows the first EIC and the corresponding country and call it "name" to match with basemap
# have to remove Singapore because it is not recognize in world basemaps
# create eic tibble and create name column for future join
# select jif and publisher/society columns for maps 
journals2$jif_bin[is.na(journals2$jcr_2020_if)] <- 'No IF'

eic <- journals2 %>% 
  select(journal, name, jcr_2020_if, jif_bin, eic_01, eic_02, eic_03, eic_04, eic_05, eic_06, eic_07, eic_08, eic_09)

# create long format for one column of EICs denoted by rank editor

eic_long <- eic %>% 
  pivot_longer(cols = 5:13, names_to = "variable", values_to = "value")

# this rearranges the factor level of the variable column

eic_long %<>%
  mutate(variable = fct_relevel(variable, "eic_01", "eic_02", "eic_03",  "eic_04",
                                "eic_05", "eic_06", "eic_07", "eic_08", "eic_09"))
# total EIC by country
eic_total_count <- eic_long %>% # use eic_long to count all eics
  count(value, sort = TRUE) %>% # use value not name, otherwise this is the same count as # journals
  drop_na()


eic_total_count %<>% 
  dplyr::filter(value!="Singapore")

eic_total_count$name <- eic_total_count$value # change country name col to 'name' to join with world

# join with World by name

eic_total_journals <- left_join(eic_total_count, World, by = "name") %>%   st_as_sf()

# tmap 
# EIC map showing total number of EICs per country

# Editors

#Here, we determine (1st) editors in chiefs (EIC) by country in a new section because we will be joining another 
# round of dataframes/tibbles. Remember, our first join `world_journals` was based on countries + journals with 
# the accompany geometries. We have to make a new join (thus a new data frame/sf/tibble object) to have the correct 
# polygon geometries of the corresponding countries.


### EIC by IF ####################################################

# get EIC counts per country by IF levels

eic_if_count <- eic_long %>% 
  group_by(value, jif_bin) %>% # use value not name (name is journal country)
  count(value, sort = TRUE) %>%
  drop_na() %>%
  dplyr::filter(value!="Singapore")

eic_if_count$name <- eic_if_count$value

# join with World by name

eic_if <- left_join(eic_if_count, World, by = "name") %>%   st_as_sf()

### create map of total EICs by country


EIC.map <- tm_shape(World, bb=b, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(eic_total_journals, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of EICs", 
          alpha = 0.75, palette = "viridis", style = "jenks", labels = c("1 to 12", "13 to 34", "35 to 57", "58 to 83", "84 to 183")) +
  tm_credits("b All EICs",  
             position = c(0.001, 0.9), size = 0.8) +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01,0.01), legend.title.size = 0.6, legend.text.size = 0.5)  

EIC.map


# get EIC counts per country by IF levels

eic_if_count <- eic_long %>% 
  group_by(value, jif_bin) %>% # use value not name (name is journal country)
  count(value, sort = TRUE) %>%
  drop_na() %>%
  dplyr::filter(value!="Singapore")

eic_if_count$name <- eic_if_count$value

# join with World by name

eic_if <- left_join(eic_if_count, World, by = "name") %>%   st_as_sf()


## map of EICs by JIF category ####
eic_if$jif_bin <- factor(eic_if$jif_bin, levels = c("High", "Mid", "Low", "No IF"))

EIC.if.map <- tm_shape(World, bb = b, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(eic_if, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of EICs", 
          alpha = 0.75, palette = "viridis", style = "jenks", labels = c("1 to 5", "6 to 12", "13 to 22", "23 to 40", "41 to 82")) +
  tm_facets(by = "jif_bin", nrow = 4, ncol = 1,
            free.coords = FALSE) +
  tm_credits(c("d High JIF", "f Mid JIF", "h Low JIF", "j No JIF"), 
             position = c(0.001, 0.9), size = 0.65) +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01,0.01), legend.title.size = 0.6, legend.text.size = 0.5)  

EIC.if.map


### save maps #####

tiff(file="fig6.1_journal.eic.counts.tiff", height = 5, width = 8, units = "in", res = 300, bg = "white")
grid.newpage()
pushViewport(viewport(layout=grid.layout(ncol = 2)))
print(journals.all, vp = viewport(layout.pos.col = 1)) 
print(EIC.map, vp = viewport(layout.pos.col = 2))
dev.off()


tiff(file="fig6.2_journal.eic.jif.tiff", height = 10, width = 6, units = "in", res = 300, bg = "white")
grid.newpage()
pushViewport(viewport(layout=grid.layout(ncol = 2)))
print(jif.map, vp = viewport(layout.pos.col = 1)) 
print(EIC.if.map, vp = viewport(layout.pos.col = 2)) 
dev.off()
