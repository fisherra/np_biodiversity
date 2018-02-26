##########################################
##      Animal Biodiversity Mapped      ##
##########################################

# Fisher Ankney
# January 12 2018

# Install packages
# library('tidyverse')

# Import dataset
# setwd('../np_biodiversity')
# parks <- read_csv('../input/parks.csv')
# species <- read_csv('../input/species.csv')

species_cleaned <- species %>%
  filter(Occurrence == "Present") %>%
  mutate(park_name = str_replace(`Park Name`, " Na.*", ""))
species_cleaned

animal_biodiv <- species_clean %>%
  filter(Category == "Amphibian" | Category == "Reptile" | Category == "Bird" | Category == "Fish" | Category ==  "Mammal") %>%
  group_by(park_name) %>%
  mutate(total_div = n())
animal_biodiv

animal_biodiv_mapped <- parks %>%
  left_join(animal_biodiv, by = "Park Name")
animal_biodiv_mapped

animal_biodiv_mapped %>%
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
       subtitle="Animal Biodiversity",
       caption="Source: Kaggle biodiversity dataset"
  ) + 
  scale_size_continuous(
    range=c(3,9),
    guide=FALSE) +
  scale_colour_distiller(name = "Total Animal Species",
                         breaks=c(800, 600, 400, 200, 0),
                         palette="Spectral")
animal_biodiv_mapped



