##########################################
##     Parks Ranked by Biodiversity     ##
##########################################

# Fisher Ankney
# January 12 2018

# Install packages
library('tidyverse')

# Import dataset
parks <- read_csv('parks.csv')
species <- read_csv('species.csv')

## creating biodiversity dataframes
species_cleaned <- species %>%
  filter(Occurrence == "Present") %>%
  mutate(park_name = str_replace(`Park Name`, " Na.*", ""))
species_cleaned

plant_biodiv <- species_cleaned %>%
  filter(Category == "Algae" | Category == "Fungi" | Category == "Nonvascular Plant" | Category == "Vascular Plant") %>%
  group_by(park_name) %>%
  mutate(total_div = n())
plant_biodiv

animal_biodiv <- species_cleaned %>%
  filter(Category == "Amphibian" | Category == "Reptile" | Category == "Bird" | Category == "Fish" | Category ==  "Mammal") %>%
  group_by(park_name) %>%
  mutate(total_div = n())
animal_biodiv
  

# Plotting Plant Biodiversity 
ggplot(plant_biodiv) + 
  geom_bar(
    mapping = aes(x = fct_reorder(park_name, total_div), fill = Category),    # catagories along the x axis
    width = 0.8,                       
    color = "black",                 
    alpha = 0.8
  ) + 
  labs(title="Plant Biodiversity Within U.S. National Parks",                          # labels
       caption="Source: Kaggle biodiversity dataset", 
       x = "National Park",
       y = "Number of Species"
  ) + 
  coord_flip()

# Plotting Animal Biodiversity
ggplot(animal_biodiv) + 
  geom_bar(
    mapping = aes(x = fct_reorder(park_name, total_div), fill = Category),    # catagories along the x axis
    width = 0.8,                       
    color = "black",                 
    alpha = 0.8
  ) + 
  labs(title="Animal Biodiversity Within U.S. National Parks",                          # labels
       caption="Source: Kaggle biodiversity dataset", 
       x = "National Park",
       y = "Number of Species"
  ) + coord_flip()