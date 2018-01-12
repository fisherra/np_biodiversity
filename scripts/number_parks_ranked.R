##########################################
##  Number of National Parks per State  ##
##########################################

# Fisher Ankney
# January 11 2018

# Install packages
library('tidyverse')

# Import dataset
setwd('../np_biodiversity')
parks <- read_csv('../input/parks.csv')
species <- read_csv('../input/species.csv')

# Create a table of number of national parks per state:
parks_by_state <- parks %>%
  group_by(State) %>%
  summarise(num_of_parks = n()) %>% 
  arrange(desc(num_of_parks)) %>%
  ungroup(parks_by_state)
View(parks_by_state)


# Create a graph to show the same: 
ggplot(parks_by_state) + 
  geom_bar(aes(fct_reorder(State, num_of_parks), num_of_parks),
           stat="identity",
           width = 0.8,                      
           color = "black",                   
           fill = "darkgreen",             
           alpha = 0.8                       
           ) +
  labs(title="National Parks",                          
       subtitle="States Ranked by Number of National Parks", 
       caption="Kaggle biodiversity datset", 
       y = "Number of National Parks",
       x = "State"
  ) + 
  coord_flip()

ggsave("number_parks_ranked.png")
