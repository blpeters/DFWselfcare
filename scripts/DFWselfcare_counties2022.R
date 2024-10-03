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
return(get_acs(geography = "county",
                state = 'TX',
                county = counties,
                variables = Variables, 
                survey = "acs5",     
                year = year,   
                geometry=T
                ))
}

total_pop <- function(year) {
return(get_acs(geography = "county", 
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
  scale_fill_viridis_c(option = 'viridis') +
  labs(main = 'DFW Self-Care Disabilities 2022', fill = "% of Pop. with a Self-care Disability") +
  theme_void()
}

lollipop <- function(df) {
  ggplot(df, aes(x = reorder(county, selfcare_percent), y = selfcare_percent)) +
    geom_segment(aes(x = reorder(county, selfcare_percent), xend = reorder(county, selfcare_percent), 
                     y = 0, yend = selfcare_percent),
                 color = 'black', lwd = 1) +
    geom_point(size = 4, pch = 21, bg = 4, col = 1) +
    coord_flip() +
    theme(  # Adjust axis, labels, grid, etc.
      legend.position = "none",
      axis.ticks.length = unit(0, "mm"),
      axis.text.y = element_text(hjust = 1.5),
      panel.grid = element_blank()
    ) +
    labs(title = "% Population with Self-Care Disability", 
         subtitle = "Dallas-Fort Worth Counties",
         x = "County",
         y = "% of Population with Self-Care Disability")  +
    theme_bw()
}
  
# Separate county text from texas Text:
df$county <- sub(" .*", "", df$NAME)

# Run Script:

selfcare <- get_selfcare_data(2022)
totalpop <- get_total_pop(2022) 
total <- combine_ages(selfcare)
df <- merge(total, totalpop)
df$selfcare_percent <- df$self_care_pop / df$estimate * 100
plot_selfcare(df)
lollipop(df)


ggplot(df) +
  geom_sf(aes(fill = county), fill = 'white', alpha = 0) +
  geom_sf_label(aes(label = county)) +
  theme_void()

