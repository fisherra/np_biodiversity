############################################
##    Plant Biodiversity & Park Size      ##
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

plant_biodiv <- species_cleaned %>%
  filter(Category == "Algae" | Category == "Fungi" | Category == "Nonvascular Plant" | Category == "Vascular Plant") %>%
  group_by(park_name) %>%
  mutate(total_div = n())
plant_biodiv

plant_bio_corr <- parks %>%
  left_join(plant_biodiv, by = "Park Name") %>%
  select(park_name, Acres, Latitude, Longitude, total_div)
plant_bio_corr <- unique(plant_bio_corr)

plant_bio_corr %>%
  ggplot(
    aes(total_div,
        Acres
        )
  ) + 
  geom_point(
      color="darkgreen",
      size=4,
      alpha = 0.8
  ) + 
  geom_smooth(method="lm", 
              color = "black",
              se = FALSE,
              linetype="dotted"
  ) + 
  labs(                   
    title="Plant Biodiversity & National Park Area", 
    y="Square Acres", 
    x="Total Number of Plant Species", 
    caption = "Source: Kaggle biodiversity dataset"
  )

plant_cor_value <- cor.test(plant_bio_corr$Acres, plant_bio_corr$total_div)
plant_cor_value