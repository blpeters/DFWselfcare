library(tidyverse)
library(tidycensus)
library(ggplot2)
library(Hmisc)
library(sf)
library(leaflet)
library(stringr)
library(htmlwidgets)
library(dplyr)

counties <- c(085, 'Dallas', 'Denton', 'Ellis', 'Hunt', 'Kaufman', 'Rockwall',
              'Johnson', 'Parker', 'Tarrant', 'Wise')
Variables<-c('B18106_004', # *male self care disabilities by age 
             'B18106_007', # *
             'B18106_010', # *
             'B18106_013', # *
             'B18106_016', # *
             'B18106_020', # *female self care disabilities by age
             'B18106_023', # *
             'B18106_026', # *
             'B18106_029', # *
             'B18106_032'  # *
             )
selfcare2022 <- get_acs(geography = "tract",
                        state = 'TX',
                        county = counties,
                        variables = Variables, 
                        survey = "acs5",     
                        year = 2022,   
                        geometry=T
                        )
totalpop2022 <-get_acs(geography = "tract", 
                   state = 'TX',
                   county = counties,
                   variables = 'B01003_001', 
                   survey = "acs5",     
                   year = 2022, 
                   ) 
# Group individual age groups into a total population with self-care disabilities
total2022 <- selfcare2022 %>% 
  group_by(NAME) %>% 
  summarise(self_care_pop = sum(estimate))
# Merge total population to the self care population sf object and calculate percent
df2022 <- merge(total2022, totalpop2022)
df2022$selfcare_percent <- df2022$self_care_pop / df2022$estimate * 100
# plot map
ggplot(df2022) + 
  geom_sf(aes(fill = selfcare_percent)) +
  scale_fill_viridis_c(option = 'inferno') +
  labs(title = '2022', fill = "% of Pop. with a Self-care Disability") +
  theme_void()
