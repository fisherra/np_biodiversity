############################################
##    Animal Biodiversity & Park Size      ##
############################################

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

animal_bio_corr <- parks %>%
  left_join(animal_biodiv, by = "Park Name") %>%
  select(park_name, Acres, Latitude, Longitude, total_div)
animal_bio_corr <- unique(animal_bio_corr)

animal_bio_corr %>%
  ggplot(
    aes(total_div,
        Acres
    )
  ) + 
  geom_point(
    color="burlywood4",
    size=4,
    alpha = 0.8
  ) + 
  geom_smooth(method="lm", 
              color = "black",
              se = FALSE,
              linetype="dotted"
  ) + 
  labs(                   
    title="Animal Biodiversity & National Park Area", 
    y="Square Acres", 
    x="Total Number of Animal Species", 
    caption = "Source: Kaggle biodiversity dataset"
  )

animal_cor_value <- cor.test(animal_bio_corr$Acres, animal_bio_corr$total_div)
animal_cor_value