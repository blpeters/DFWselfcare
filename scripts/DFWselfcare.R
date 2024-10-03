library(tidyverse)
library(tidycensus)
library(ggplot2)
library(sf)
library(stringr)
library(dplyr)
library(leaflet)
library(htmlwidgets)

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


# Separate county text from texas Text:
df$tract <- sub(";.*", "", df$NAME)

# Run Script:
selfcare <- get_selfcare_data(2022)
totalpop <- get_total_pop(2022) 
total <- combine_ages(selfcare)
df <- merge(total, totalpop)
df$selfcare_percent <- df$self_care_pop / df$estimate * 100
plot_selfcare(df)
st_write(df, "output/selfcare2021.shp", append = FALSE)

# Leaflet Map

# Color palette
mypalette <- colorNumeric(
  palette = "viridis", domain = df$selfcare_percent,
  na.color = "transparent"
)
mypalette(c(45, 43))

selfcare_map <- df %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  leaflet() %>% 
  setView(lng = -96.97, lat = 32.90, zoom = 9) %>% 
  addTiles(group = "OSM (default)") %>%  
  addProviderTiles(providers$CartoDB.Positron, group = "Carto B") %>%
  addPolygons(group = "Total", 
              popup = paste(df$tract, "<br>",
                            "Total Population: ", df$estimate, "<br>",
                            "Population with Self-Care Disability: ", df$self_care_pop, "<br>",
                            "% Self Care: ", round(df$selfcare_percent,2), "<br>"),
              stroke = FALSE,
              smoothFactor = 0,
              fillColor = ~ mypalette(selfcare_percent),
              fillOpacity = 0.8,) %>% 
  addLayersControl(  #Gives you the option to turn on and off different basemaps or layers.
    baseGroups = c("OSM (default)", "Carto B"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(
    pal = mypalette, values = ~selfcare_percent, opacity = 0.9,
    title = "% Pop. with<br>self-care disability", position = 'bottomleft'
  )


selfcare_map # loads the map you just made, this will take awhile

# Saving the Html map
# first argument is the leaflet map, second is the file name and extension, and final is whether it is selfcontained
# The space needed for the self contained file is a lot which is why we reduced it down to just one layer.
saveWidget(selfcare_map, file="selfcare_map.html", selfcontained = T)

