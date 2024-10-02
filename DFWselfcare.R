library(tidyverse)
library(tidycensus)
library(ggplot2)
library(sf)
library(stringr)
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

get_selfcare_data <- function(year) {
  selfcare <- self_care(year)
}

get_total_pop <- function(year) {
  totalpop <- total_pop(year)
}

self_care <- function(year) {
return(get_acs(geography = "tract",
                state = 'TX',
                county = counties,
                variables = Variables, 
                survey = "acs5",     
                year = year,   
                geometry=T
                ))
}

total_pop <- function(year) {
return(get_acs(geography = "tract", 
                state = 'TX',
                county = counties,
                variables = 'B01003_001', 
                survey = "acs5",     
                year = year,
                )) 
}

# Group individual age groups into a total population with self-care disabilities
combine_ages <- function(selfcaredata) {
  total <- selfcaredata %>% 
  group_by(NAME) %>% 
  summarise(self_care_pop = sum(estimate))
  return (total) 
}

plot_selfcare <- function(df) {
  ggplot(df) + 
  geom_sf(aes(fill = selfcare_percent)) +
  scale_fill_viridis_c(option = 'inferno') +
  labs(fill = "% of Pop. with a Self-care Disability") +
  theme_void()
}


# Run Script:

selfcare <- get_selfcare_data(2021)
totalpop <- get_total_pop(2021) 
total <- combine_ages(selfcare)
df <- merge(total, totalpop)
df$selfcare_percent <- df$self_care_pop / df$estimate * 100
# plot_selfcare(df)
st_write(df, "output/selfcare2021.shp", append = FALSE)
