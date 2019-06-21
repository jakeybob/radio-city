library(tidyverse)
library(leaflet)
# library(sf)
# library(sp)
# library(rgdal)
# library(rmapshaper)

kilbirnie <- c(55.755622, -4.684996)
glengarnock <- c(55.739150, -4.676749)

markers <- tibble(lat = c(kilbirnie[1], glengarnock[1]),
                  long = c(kilbirnie[2], glengarnock[2]),
                  name = c("Kilbirnie", "Glengarnock"))

dat <- read_rds("data/data_indicators.rds") 

# latest year info for one specific indicator at IZ level
df_income_deprived <- dat %>% 
  filter(indicator == "Young people living in the most income deprived quintile",
         area_type == "Intermediate zone",
         # year == max(year), 
         year == 2016) %>%
  mutate_if(sapply(., is.character), as.factor)


# intermediate zone choropleth
# https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search;jsessionid=7BFD03DFFDA5CD66CC065C75FBAD2172#/metadata/389787c0-697d-4824-9ca9-9ce8cb79d6f5
mapdata_income_deprived <- st_read("data/SG_IntermediateZoneBdry_2011") %>%
  ms_simplify(., drop_null_geometries = TRUE) %>%
  ms_filter_islands(., min_area = 1e8) %>%
  st_transform(crs="+proj=longlat +datum=WGS84") %>% 
  left_join(select(df_income_deprived, area_code, measure), by=c("InterZone" = "area_code"))

pal2 <- colorNumeric("Blues", domain = mapdata_income_deprived$measure)

p <- mapdata_income_deprived %>% 
  leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  setView(-3, 57, 5.5) %>%
  addPolygons(weight = 0.2, fillOpacity = 0.6, smoothFactor = 1,
              fillColor = ~pal2(measure)) %>%
  addTiles() %>%
  # addMarkers(markers$long, markers$lat, popup = markers$name, label = markers$name) %>%
  addLabelOnlyMarkers(markers$long, markers$lat, label = markers$name)

p
