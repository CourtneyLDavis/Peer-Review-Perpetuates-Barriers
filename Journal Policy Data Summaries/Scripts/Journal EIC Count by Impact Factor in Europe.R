#############################################################
#### Journal and EIC count maps by journal impact factor ####
#### Created by: Nan Nourn                               #### 
#### Modified by: Julie Jarvey                           ####
#### Last modified: 16 August 2022                       ####
#############################################################


############ Set-up
############
# Load packages
library(tidyverse)
library(sf)
library(tmap)
library(grid)
library(tidytext)
library(magrittr)
library(janitor)
library(here)


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
b.eur <- st_bbox(c(xmin = -9, xmax = 43, ymax = 72, ymin = 34), crs = st_crs(4326))

journals.eur <- tm_shape(World, bb=b.eur, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(world_journals_count, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of journals", 
          alpha = 0.75, palette = "viridis", style = "jenks") +
  tm_credits("a All journals", position = c(0.001, 0.87), size = 0.8) +
  tm_layout(legend.outside = FALSE, panel.show = FALSE,
            legend.bg.color = 'white', legend.frame = T,
            legend.position = c(0.7,0.57), legend.title.size = 0.7, legend.text.size = 0.5)

journals.eur


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

# map of EEB journals per country faceted by IF factor for Europe
### remake JIF plots zoomed in on Europe
b.eur <- st_bbox(c(xmin = -9, xmax = 43, ymax = 72, ymin = 34), crs = st_crs(4326))


jif.map.eur <- tm_shape(World, bb = b.eur, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(journals_if_count_sf, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of journals", 
          alpha = 0.75, palette = "viridis", style = "jenks") +
  tm_facets(by = "jif_bin", nrow = 4, ncol = 1,
            free.coords = FALSE) +
  tm_credits(c("c High JIF", "e Mid JIF", "g Low JIF", "i No JIF"), 
             position = c(0.001, 0.87), size = 0.8) +
  tm_layout(legend.outside = FALSE, panel.show = FALSE, legend.bg.color = 'white', legend.frame = T,
            legend.position = c(0.71,0.6), legend.title.size = 0.6, legend.text.size = 0.4) 

jif.map.eur


#############################################################################
## EIC Maps ####################################################################
# we create a column that follows the first EIC and the corresponding country and call it "name" to match with basemap
# have to remove Singapore because it is not recognize in world basemaps
# create eic tibble and create name column for future join
# select jif and publisher/society columns for maps 

eic <- journals %>% 
  select(journal, name, jcr_2020_if, jif_bin, eic_01, eic_02, eic_03, eic_04, eic_05, eic_06, eic_07, eic_08, eic_09)

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


eic_total_count %<>% 
  dplyr::filter(value!="Singapore")

eic_total_count$name <- eic_total_count$value # change country name col to 'name' to join with world

# join with World by name
eic_total_journals <- left_join(eic_total_count, World, by = "name") %>%   st_as_sf()

# tmap 
# EIC map showing total number of EICs per country

# Editors
# Here, we determine (1st) editors in chiefs (EIC) by country in a new section because we will be joining another 
# round of dataframes/tibbles. Remember, our first join `world_journals` was based on countries + journals with 
# the accompany geometries. We have to make a new join (thus a new data frame/sf/tibble object) to have the correct 
# polygon geometries of the corresponding countries.


### EIC by JIF ####################################################
# get EIC counts per country by JIF levels
eic_if_count <- eic_long%>% 
  group_by(value, jif_bin) %>% # use value not name (name is journal country)
  count(value, sort = TRUE) %>%
  drop_na() %>%
  dplyr::filter(value!="Singapore")

eic_if_count$name <- eic_if_count$value

# join with World by name
eic_if <- left_join(eic_if_count, World, by = "name") %>%   st_as_sf()

### remake map zoomed in on Europe
EIC.map.eur <- tm_shape(World, bb=b.eur, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(eic_total_journals, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of EICs", 
          alpha = 0.75, palette = "viridis", style = "jenks") +
  # tm_layout(legend.position = c(0.01,0.05), main.title = "b. All editors-in-chief",  main.title.position = "center",
  #          legend.title.size = 0.9, legend.text.size = 0.65, main.title.size = 1)
  tm_credits("b All EICs", position = c(0.001, 0.87), size =0.8) +
  tm_layout(legend.outside = FALSE, panel.show = FALSE,
            legend.bg.color = 'white', legend.frame = T,
            legend.position = c(0.71,0.57), legend.title.size = 0.7, legend.text.size = 0.5)

EIC.map.eur

EIC.if.map.eur <- tm_shape(World, bb = b.eur, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(eic_if, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of EICs", 
          alpha = 0.75, palette = "viridis", style = "jenks") +
  tm_facets(by = "jif_bin", nrow = 4, ncol = 1,
            free.coords = FALSE) +
  tm_credits(c("d High JIF", "f Mid JIF", "h Low JIF", "j No JIF"), 
             position = c(0.001, 0.87), size = 0.8) +
  tm_layout(legend.outside = FALSE, panel.show = FALSE, legend.bg.color = 'white', legend.frame = T,
          legend.position = c(0.75,0.6), legend.title.size = 0.6, legend.text.size = 0.4)  

EIC.if.map.eur


### save maps #####
# save total journal and eic counts #
tiff(file="S15.1_allcounts.eur.tiff", height = 7, width = 4.5, units = "in", res = 300, bg = "white")
grid.newpage()
pushViewport(viewport(layout=grid.layout(ncol = 2)))
print(journals.eur, vp = viewport(layout.pos.col = 1)) #, height = 0.75, width = 0.5)) #, x = 0, y = 0.75))
print(EIC.map.eur, vp = viewport(layout.pos.col = 2)) #, height = 0.75, width = 0.5)) #, x = 0.5, y = 0.75))
dev.off()

# save counts by jif  #
tiff(file="S15.2_jifcounts.eur.tiff", height = 7, width = 4.5, units = "in", res = 300, bg = "white")
grid.newpage()
pushViewport(viewport(layout=grid.layout(ncol = 2)))
print(jif.map.eur, vp = viewport(layout.pos.col = 1)) #, height = 0.75, width = 0.5)) #, x = 0, y = 0.75))
print(EIC.if.map.eur, vp = viewport(layout.pos.col = 2)) #, height = 0.75, width = 0.5)) #, x = 0.5, y = 0.75))
dev.off()
