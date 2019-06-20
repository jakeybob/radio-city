library(tidyverse)
library(leaflet)
library(sf)
library(sp)
library(rgdal)
library(rmapshaper)

indicators <- c("Young people living in the most income deprived quintile",
                "Population within 500 metres of a derelict site",
                "Drug-related hospital stays, aged 11-25 years",
                "Alcohol-related hospital stays, aged 11-25 years",
                "Employment rate for 16-24 year olds")

# regenerate from original scotpho extract all the IZ data etc

dat <- read_rds("data/data.rds") %>% 
  filter(indicator %in% indicators,
         area_type == "Intermediate zone")


# intermediate zone choropleth
# https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search;jsessionid=7BFD03DFFDA5CD66CC065C75FBAD2172#/metadata/389787c0-697d-4824-9ca9-9ce8cb79d6f5
x <- sf::st_read("data/SG_IntermediateZoneBdry_2011") %>%
  # sf::st_simplify(preserveTopology = TRUE, dTolerance = 100) %>%
  ms_simplify(.) %>%
  ms_filter_islands(.) %>%
  sf::st_transform(crs="+proj=longlat +datum=WGS84") 

# bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
# pal <- colorBin("YlOrRd", domain = x$StdAreaKm2, bins = bins)
pal2 <- colorNumeric("YlOrRd", domain = x$StdAreaKm2)

b <- x %>% 
  # dplyr::slice(1:nrow(.)) %>%
  leaflet(options = leafletOptions(preferCanvas = F)) %>%
  setView(-3, 57, 5.5) %>%
  # addTiles() %>%
  addPolygons(weight = 0, fillOpacity = 0.8, smoothFactor = 1,
              fillColor = ~pal2(StdAreaKm2))
b
