##########################################
##     Parks Ranked by Biodiversity     ##
##########################################

# Fisher Ankney
# January 12 2018

# Install packages
library('tidyverse')

# Import dataset
# setwd('../np_biodiversity')
# parks <- read_csv('../input/parks.csv')
# species <- read_csv('../input/species.csv')

# code
species_cleaned <- species %>%
  filter(Occurrence == "Present") %>%
  mutate(park_name = str_replace(`Park Name`, " Na.*", ""))
species_cleaned

plant_biodiv <- species_cleaned %>%
  filter(Category == "Algae" | Category == "Fungi" | Category == "Nonvascular Plant" | Category == "Vascular Plant") %>%
  group_by(park_name) %>%
  mutate(total_div = n())
plant_biodiv

plant_biodiv_mapped <- parks %>%
  left_join(plant_biodiv, by = "Park Name")
plant_biodiv_mapped

plant_biodiv_mapped %>%
  filter(State != "AK" & State != "HI") %>% 
  ggplot(aes(Longitude, Latitude)) +
  borders("state") +
  geom_point(
    aes(
      size=total_div, 
      color=total_div
    )
  ) +
  coord_quickmap() +   
  labs(title="U.S. National Parks",               
       subtitle="Plant Biodiversity",
       caption="Source: Kaggle biodiversity dataset"
  ) + 
  scale_size_continuous(
    range=c(3,9),
    guide=FALSE) +
  scale_colour_distiller(name = "Total Plant Species",
                  #       breaks=c(800, 600, 400, 200, 0),
                         palette="Spectral")
plant_biodiv_mapped