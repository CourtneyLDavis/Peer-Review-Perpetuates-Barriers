############################################################
#### Journal and EIC count maps by publisher            ####
#### Created by: Nan Nourn                              #### 
#### Modified by: Julie Jarvey                          ####
#### Last modified: 16 August 2022                      ####
############################################################

############ Set-up
############
# Load packages
library(here)
library(tidyverse)
library(tidytext)
library(magrittr)
library(janitor)
library(sf)
library(tmap)
library(grid)


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


#journals$name <- as.factor(journals$name)
journals %<>% filter(name != "Singapore") # Singapore is not included in world basemap, so need to filter out so the geometry works for mapping



############ Counts by publisher/society type
#### Journal counts by soc/pub 
journals$name <- as.character(journals$name)

journals_society <- journals %>% 
  select(journal, name, jcr_2020_if, jif_bin, society_publisher)

journals_society_count <- journals_society %>% 
  count(name, society_publisher, sort = TRUE)

journals_society_count_sf <- left_join(journals_society_count, World, by = "name") %>% 
  st_as_sf()

# map
# map_mode("view")

tmap_mode("plot")

b <- st_bbox(c(xmin = -160, xmax = 165, ymax = 90, ymin = -60), crs = st_crs(4326))

journals.pub <- tm_shape(World, bb= b, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(journals_society_count_sf, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of journals", 
          alpha = 0.75, palette = "viridis", style = "jenks") +
  tm_facets(by = "society_publisher", nrow = 4, ncol = 1, 
            free.coords = FALSE) +
  tm_credits(c("a. Major, not soc.", "c. Major, soc.", "e. Minor, not soc.", "g. Minor, soc."), 
             position = c(0.001, 0.9), size = 0.65) +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01,0.01), legend.title.size = 0.6, legend.text.size = 0.5)  # saving along legend title = 1.1, legend text = 0.9

journals.pub


#### EIC counts by publisher
# Editors
# create eic tibble and create name column for future join
eic <- journals %>% 
  select(journal, name, jcr_2020_if, jif_bin, eic_01, eic_02, eic_03, eic_04, eic_05, eic_06, eic_07, eic_08, eic_09, society_publisher)

# create long format for one column of EICs denoted by rank editor
eic_long <- eic %>% 
  pivot_longer(cols = 5:13, names_to = "variable", values_to = "value")

# this rearranges the factor level of the variable column
eic_long %<>%
  mutate(variable = fct_relevel(variable, "eic_01", "eic_02", "eic_03", "eic_04", "eic_05", "eic_06",
                                "eic_07", "eic_08", "eic_09"))


# get EIC counts per country by JIF levels
eic_pub_count <- eic_long %>% 
  group_by(value, society_publisher) %>% 
  count(value, sort = TRUE) %>%
  drop_na() %>%
  dplyr::filter(value!="Singapore")

eic_pub_count$name <- eic_pub_count$value


# join with World by name
eic_pub <- left_join(eic_pub_count, World, by = "name") %>%   st_as_sf()

tmap_mode("plot")

EIC.pub.map <- tm_shape(World, bb = b, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(eic_pub, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of EICs", 
          alpha = 0.75, palette = "viridis", style = "jenks") +
  tm_facets(by = "society_publisher", nrow = 4, ncol = 1,
            free.coords = FALSE) +
  tm_credits(c("b. Major, not soc.", "d. Major, soc.", "f. Minor, not soc.", "h. Minor, soc."), 
             position = c(0.001, 0.9), size = 0.65) +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01,0.01), legend.title.size = 0.6, legend.text.size = 0.5)  # saving along legend title = 1.1, legend text = 0.9

EIC.pub.map


## arrange and save maps
pub.both <- tmap_arrange(journals.pub, EIC.pub.map)
pub.both

tmap_save(pub.both, "S11_pub.soc.maps.tiff", asp = 2)


##############################################################################
## remake maps zoomed in on Europe
b.eur <- st_bbox(c(xmin = -9, xmax = 43, ymax = 72, ymin = 34), crs = st_crs(4326))

journals.pub.eur <- tm_shape(World, bb= b.eur, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(journals_society_count_sf, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of journals", 
          alpha = 0.75, palette = "viridis", style = "jenks") +
  tm_facets(by = "society_publisher", nrow = 4, ncol = 1, 
            free.coords = FALSE) +
  tm_credits(c("a. Major, not soc.", "c. Major, soc.", "e. Minor, not soc.", "g. Minor, soc."), 
             position = c(0.001, 0.9), size = 0.65) +
  tm_layout(legend.outside = FALSE, panel.show = FALSE, legend.bg.color = 'white', legend.frame = T,
            legend.position = c(0.71,0.6), legend.title.size = 0.6, legend.text.size = 0.4)  # saving along legend title = 1.1, legend text = 0.9

journals.pub.eur


EIC.pub.map.eur <- tm_shape(World, bb = b.eur, projection = "+proj=robin") +
  tm_borders(col = "black") +
  tm_shape(eic_pub, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of EICs", 
          alpha = 0.75, palette = "viridis", style = "jenks") +
  tm_facets(by = "society_publisher", nrow = 4, ncol = 1,
            free.coords = FALSE) +
  tm_credits(c("b. Major, not soc.", "d. Major, soc.", "f. Minor, not soc.", "h. Minor, soc."), 
             position = c(0.001, 0.9), size = 0.65) +
  tm_layout(legend.outside = FALSE, panel.show = FALSE, legend.bg.color = 'white', legend.frame = T,
            legend.position = c(0.75,0.6), legend.title.size = 0.6, legend.text.size = 0.4)  # saving along legend title = 1.1, legend text = 0.9


EIC.pub.map.eur


tiff(file="S12_pub.soc.eur.tiff", height = 7, width = 4.5, units = "in", res = 300, bg = "transparent")
grid.newpage()
pushViewport(viewport(layout=grid.layout(ncol = 2)))
print(journals.pub.eur, vp = viewport(layout.pos.col = 1)) #, height = 0.75, width = 0.5)) #, x = 0, y = 0.75))
print(EIC.pub.map.eur, vp = viewport(layout.pos.col = 2)) #, height = 0.75, width = 0.5)) #, x = 0.5, y = 0.75))
dev.off()
