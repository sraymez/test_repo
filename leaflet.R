# Making interactive maps with leaflet

# Directory
setwd("C:/Users/SantiagoRaymentGomez/OneDrive - London School of Hygiene and Tropical Medicine/OneZoo PhD/Data analysis")

{library(pacman)
p_load(leaflet, leaflet.extras, sf, tidyverse)}

# Senegal and Gambia shapefiles
sen_adm0 <- st_read("Shapefiles/gadm41_SEN_0.shp")
sen_adm1 <- st_read("Shapefiles/gadm41_SEN_1.shp")
sen_adm2 <- st_read("Shapefiles/gadm41_SEN_2.shp")

# Html code for labeling interactive plot
labels <- sprintf(
  "<strong>%s</strong><br/>Type: %s",
  sen_adm2$NAME_2, sen_adm2$TYPE_2
) %>% lapply(htmltools::HTML)

pal <- colorFactor(palette = "YlOrRd", domain = sen_adm2$NAME_1)

# Leaflet
leaflet(sen_adm2) %>% 
  addTiles() %>% 
  addPolygons(weight = 1, 
              fillColor = ~pal(NAME_1),
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(
                weight = 8,
                color = "#666",
                fillOpacity = 0.8,
                bringToFront = T),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px", direction = "auto")
              ) %>% 
  addLegend(pal = pal, 
            values = ~NAME_1, 
            opacity = 0.5, 
            title = "Admin Level 2",
            position = "topright") %>% 
  addDrawToolbar()

leaflet(sen_adm1) %>% 
  addTiles() %>% 
  addDrawToolbar() %>% 
  addPolygons(weight = .5, fillColor = "grey")

# species digitiser georeferencing tool ---------------------------------
library(devtools)
library(remotes)

package_url = "https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.6-7.tar.gz"

remotes::install_version("rgdal", version = "1.5-18")

install_github("ptarroso/speciesdigitizer", subdir="source")
