###############################################
##  Biodiversity in the U.S. National Parks  ##
###############################################

# Fisher Ankney
# January 11 2018

# Install packages
library('tidyverse')
library('RColorBrewer')

# Import dataset
setwd('../np_biodiversity')
parks <- read_csv('../input/parks.csv')
species <- read_csv('../input/species.csv')

# lets take a quick look at the parks 
parks_mapped <- parks %>%
  ggplot(aes(Longitude, Latitude)) +
  borders("state") +
  geom_point(aes(size=Acres)) +
  coord_quickmap()
parks_mapped
  
ggsave('parks_mapped.png')

# better map
lower_48_mapped <- parks %>% 
  filter(State != "AK" & State != "HI") %>% 
  ggplot(aes(Longitude, Latitude)) +
  borders("state") +
  geom_point(
             aes(
                 size=Acres, 
                 color=Acres
                 )
             ) +
  coord_quickmap() +   
labs(title="U.S. National Parks",               
     subtitle="Size and Location",
     caption="Source: Kaggle biodiversity dataset"
     ) + 
scale_size_continuous(
  range=c(3,9),
  guide=FALSE) + 
        #                name = "Acres",
        #                breaks=c(1e+06, 2e+06, 3e+06, 4e+06),
        #                labels=c("1 Million", "2 Million", "3 Million", "4 Million")) + 
scale_colour_distiller(name = "Square Acres",
                          breaks=c(1e+06, 2e+06, 3e+06, 4e+06),
                          labels=c("1 Million", "2 Million", "3 Million", "4 Million"),
                          palette="Spectral")  
lower_48_mapped

ggsave('lower_48_mapped.png')


## states with the most national parks

# ranked animal biodiversity of each park
# - states with the most combined biodiversity within their parks?

# mammals that are unique to a single national park 
