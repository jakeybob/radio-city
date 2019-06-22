# create maps (interactive, and png) for each indicator and write out
library(tidyverse)
library(leaflet)
library(sf)
library(mapview)
library(rmapshaper)
library(htmltools)

#### DATAFRAME AND MAPDATA GENERATION ####
# create dataframes for each indicator at lowest geography level for latest available year
dat <- read_rds("data/data_indicators.rds")


##### Young people living in the most income deprived quintile ####
# Intermediate zone, 2017
df_income_deprived <- dat %>% 
  filter(indicator == "Young people living in the most income deprived quintile",
         area_type == "Intermediate zone") %>%
  filter(year == max(year)) %>%
  mutate_if(sapply(., is.character), as.factor)

# IZ 2011 shapefile
# https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search;jsessionid=7BFD03DFFDA5CD66CC065C75FBAD2172#/metadata/389787c0-697d-4824-9ca9-9ce8cb79d6f5
mapdata_income_deprived <- st_read("data/SG_IntermediateZoneBdry_2011") %>%
  ms_simplify(., drop_null_geometries = TRUE) %>%
  ms_filter_islands(., min_area = 1e8) %>%
  st_transform(crs="+proj=longlat +datum=WGS84") %>% 
  left_join(select(df_income_deprived, area_code, area_name, measure), by=c("InterZone" = "area_code")) %>%
  mutate(labels = sprintf("<strong>%s</strong><br/>%g&#37;", area_name, measure) %>% 
           lapply(HTML))


#### Alcohol-related hospital stays, aged 11-25 years ####
# Council area, 2015
df_alcohol_hospital <- dat %>%
  filter(indicator == "Alcohol-related hospital stays, aged 11-25 years",
         area_type == "Council area") %>%
  filter(year == max(year)) %>%
  mutate(area_code = recode(area_code, S12000046 = "S12000049", S12000044 = "S12000050")) %>%
  mutate_if(sapply(., is.character), as.factor)

# Local authority shapefiles
# https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search#/metadata/1cd57ea6-8d6e-412b-a9dd-d1c89a80ad62
mapdata_alcohol_hospital <- st_read("data/pub_las") %>%
  ms_simplify(., drop_null_geometries = TRUE) %>%
  ms_filter_islands(., min_area = 1e8) %>%
  st_transform(crs="+proj=longlat +datum=WGS84") %>% 
  left_join(select(df_alcohol_hospital, area_code, area_name, measure), by=c("code" = "area_code")) %>%
  mutate(labels = sprintf("<strong>%s</strong><br/>%g per 100,000", area_name, measure) %>% 
           lapply(HTML))


#### Drug-related hospital stays, aged 11-25 years ####
# Council area, 2015
df_drug_hospital <- dat %>%
  filter(indicator == "Drug-related hospital stays, aged 11-25 years",
         area_type == "Council area") %>%
  filter(year == max(year)) %>%
  mutate(area_code = recode(area_code, S12000046 = "S12000049", S12000044 = "S12000050")) %>%
  mutate_if(sapply(., is.character), as.factor)

# Local authority shapefiles
# https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search#/metadata/1cd57ea6-8d6e-412b-a9dd-d1c89a80ad62
mapdata_drug_hospital <- st_read("data/pub_las") %>%
  ms_simplify(., drop_null_geometries = TRUE) %>%
  ms_filter_islands(., min_area = 1e8) %>%
  st_transform(crs="+proj=longlat +datum=WGS84") %>% 
  left_join(select(df_drug_hospital, area_code, area_name, measure), by=c("code" = "area_code")) %>%
  mutate(labels = sprintf("<strong>%s</strong><br/>%g per 100,000", area_name, measure) %>% 
           lapply(HTML))


#### Employment rate for 16-24 year olds ####
# Council area, 2016
df_employment <- dat %>%
  filter(indicator == "Employment rate for 16-24 year olds",
         area_type == "Council area") %>%
  filter(year == max(year)) %>%
  mutate(area_code = recode(area_code, S12000046 = "S12000049", S12000044 = "S12000050")) %>%
  mutate_if(sapply(., is.character), as.factor)

# Local authority shapefiles
# https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search#/metadata/1cd57ea6-8d6e-412b-a9dd-d1c89a80ad62
mapdata_employment <- st_read("data/pub_las") %>%
  ms_simplify(., drop_null_geometries = TRUE) %>%
  ms_filter_islands(., min_area = 1e8) %>%
  st_transform(crs="+proj=longlat +datum=WGS84") %>% 
  left_join(select(df_employment, area_code, area_name, measure), by=c("code" = "area_code")) %>%
  mutate(labels = sprintf("<strong>%s</strong><br/>%g&#37;", area_name, measure) %>% 
           lapply(HTML))


#### Population within 500 metres of a derelict site ####
# Intermediate zone, 
df_derelict <- dat %>%
  filter(indicator == "Population within 500 metres of a derelict site",
         area_type == "Intermediate zone") %>%
  filter(year == max(year)) %>%
  mutate_if(sapply(., is.character), as.factor)

# IZ 2011 shapefile
# https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search;jsessionid=7BFD03DFFDA5CD66CC065C75FBAD2172#/metadata/389787c0-697d-4824-9ca9-9ce8cb79d6f5
mapdata_derelict <- st_read("data/SG_IntermediateZoneBdry_2011") %>%
  ms_simplify(., drop_null_geometries = TRUE) %>%
  ms_filter_islands(., min_area = 1e8) %>%
  st_transform(crs="+proj=longlat +datum=WGS84") %>% 
  left_join(select(df_derelict, area_code, area_name, measure), by=c("InterZone" = "area_code")) %>%
  mutate(labels = sprintf("<strong>%s</strong><br/>%g&#37;", area_name, measure) %>% 
           lapply(HTML))



#### PLOTS ####
height = 1150
width = 800

# marker points
kilbirnie <- c(55.755622, -4.684996)
glengarnock <- c(55.739150, -4.676749)
markers <- tibble(lat = c(kilbirnie[1], glengarnock[1]),
                  long = c(kilbirnie[2], glengarnock[2]),
                  name = c("Kilbirnie", "Glengarnock"))

# Scotland view
zoom <- 7
view_long <- -4.15
view_lat <- 57.7

# Kilbirnie / North Ayrshire view for IZ
zoom <- 10
view_long <- -4.684395
view_lat <- 55.755429

# Kilbirnie / North Ayrshire view for council area
zoom <- 9
view_long <- -4.684395
view_lat <- 55.755429


##### Young people living in the most income deprived quintile ####
# Scotland view
zoom <- 7
view_long <- -4.15
view_lat <- 57.7

pal <- colorNumeric("Blues", domain = mapdata_income_deprived$measure)
p_income_deprived <- mapdata_income_deprived %>% 
  leaflet(height=height, width=width) %>%
  addAwesomeMarkers(data = markers[1,], ~long, ~lat, popup = ~name, label =~name,
                    labelOptions = list(textsize = "20px",
                                        permanent = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = ~measure, opacity = .95, 
            title = "", 
            labFormat = labelFormat(suffix = "%")) %>%
  setView(view_long, view_lat, zoom) %>%
  addPolygons(weight = 0.2, fillOpacity = .95, smoothFactor = 1,
              fillColor = ~pal(measure),
              highlight = highlightOptions(weight = 1, fillOpacity = 1, bringToFront = TRUE),
              label = ~labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
  )

mapshot(p_income_deprived, file = file.path(getwd(), "pics", "map_income_deprived_scot.png"),
        vheight = height, vwidth = width, zoom = 1)

# Kilbirnie / North Ayrshire view for IZ
zoom <- 10
view_long <- -4.684395
view_lat <- 55.755429

p_income_deprived <- p_income_deprived %>%
  setView(view_long, view_lat, zoom)

mapshot(p_income_deprived, file = file.path(getwd(), "pics", "map_income_deprived_zoom.png"),
        vheight = height, vwidth = width, zoom = 1)

p_income_deprived <- p_income_deprived %>%
  addMiniMap("bottomleft", minimized = FALSE, toggleDisplay = TRUE)

write_rds(p_income_deprived, "pics/map_income_deprived_zoom.rds")


#### Alcohol-related hospital stays, aged 11-25 years ####
pal <- colorNumeric("Blues", domain = mapdata_alcohol_hospital$measure)

# Scotland view
zoom <- 7
view_long <- -4.15
view_lat <- 57.7

p_alcohol_hospital <- mapdata_alcohol_hospital %>% 
  leaflet(height=height, width=width) %>%
  addAwesomeMarkers(data = markers[1,], ~long, ~lat, popup = ~name, label =~name,
                    labelOptions = list(textsize = "20px",
                                        permanent = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = ~measure, opacity = .95, 
            title = "", 
            labFormat = labelFormat(suffix = " per 100,000")) %>%
  setView(view_long, view_lat, zoom) %>%
  addPolygons(weight = 0.2, fillOpacity = .95, smoothFactor = 1,
              fillColor = ~pal(measure),
              highlight = highlightOptions(weight = 1, fillOpacity = 1, bringToFront = TRUE),
              label = ~labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
  )

mapshot(p_alcohol_hospital, file = file.path(getwd(), "pics", "map_alcohol_hospital_scot.png"),
        vheight = height, vwidth = width, zoom = 1)

# Kilbirnie / North Ayrshire view for council area
zoom <- 9
view_long <- -4.684395
view_lat <- 55.755429

p_alcohol_hospital <- p_alcohol_hospital %>%
  setView(view_long, view_lat, zoom)

mapshot(p_alcohol_hospital, file = file.path(getwd(), "pics", "map_alcohol_hospital_zoom.png"),
        vheight = height, vwidth = width, zoom = 1)

p_alcohol_hospital <- p_alcohol_hospital %>%
  addMiniMap("bottomleft", minimized = FALSE, toggleDisplay = TRUE)

write_rds(p_alcohol_hospital, "pics/map_alcohol_hospital_zoom.rds")


#### Drug-related hospital stays, aged 11-25 years ####
pal <- colorNumeric("Blues", domain = mapdata_drug_hospital$measure)

# Scotland view
zoom <- 7
view_long <- -4.15
view_lat <- 57.7

p_drug_hospital <- mapdata_drug_hospital %>% 
  leaflet(height=height, width=width) %>%
  addAwesomeMarkers(data = markers[1,], ~long, ~lat, popup = ~name, label =~name,
                    labelOptions = list(textsize = "20px",
                                        permanent = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = ~measure, opacity = .95, 
            title = "", 
            labFormat = labelFormat(suffix = " per 100,000")) %>%
  setView(view_long, view_lat, zoom) %>%
  addPolygons(weight = 0.2, fillOpacity = .95, smoothFactor = 1,
              fillColor = ~pal(measure),
              highlight = highlightOptions(weight = 1, fillOpacity = 1, bringToFront = TRUE),
              label = ~labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
  )

mapshot(p_drug_hospital, file = file.path(getwd(), "pics", "map_drug_hospital_scot.png"),
        vheight = height, vwidth = width, zoom = 1)

# Kilbirnie / North Ayrshire view for council area
zoom <- 9
view_long <- -4.684395
view_lat <- 55.755429

p_drug_hospital <- p_drug_hospital %>%
  setView(view_long, view_lat, zoom)

mapshot(p_drug_hospital, file = file.path(getwd(), "pics", "map_drug_hospital_zoom.png"),
        vheight = height, vwidth = width, zoom = 1)

p_drug_hospital <- p_drug_hospital %>%
  addMiniMap("bottomleft", minimized = FALSE, toggleDisplay = TRUE)

write_rds(p_drug_hospital, "pics/map_drug_hospital_zoom.rds")


#### Employment rate for 16-24 year olds ####
pal <- colorNumeric("Blues", domain = mapdata_employment$measure)

# Scotland view
zoom <- 7
view_long <- -4.15
view_lat <- 57.7

p_employment <- mapdata_employment %>% 
  leaflet(height=height, width=width) %>%
  addAwesomeMarkers(data = markers[1,], ~long, ~lat, popup = ~name, label =~name,
                    labelOptions = list(textsize = "20px",
                                        permanent = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = ~measure, opacity = .95, 
            title = "", 
            labFormat = labelFormat(suffix = "%")) %>%
  setView(view_long, view_lat, zoom) %>%
  addPolygons(weight = 0.2, fillOpacity = .95, smoothFactor = 1,
              fillColor = ~pal(measure),
              highlight = highlightOptions(weight = 1, fillOpacity = 1, bringToFront = TRUE),
              label = ~labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
  )

mapshot(p_employment, file = file.path(getwd(), "pics", "map_employment_scot.png"),
        vheight = height, vwidth = width, zoom = 1)

# Kilbirnie / North Ayrshire view for council area
zoom <- 9
view_long <- -4.684395
view_lat <- 55.755429

p_employment <- p_employment %>%
  setView(view_long, view_lat, zoom)

mapshot(p_employment, file = file.path(getwd(), "pics", "map_employment_zoom.png"),
        vheight = height, vwidth = width, zoom = 1)

p_employment <- p_employment %>%
  addMiniMap("bottomleft", minimized = FALSE, toggleDisplay = TRUE)

write_rds(p_employment, "pics/map_employment_zoom.rds")


#### Population within 500 metres of a derelict site ####
# Scotland view
zoom <- 7
view_long <- -4.15
view_lat <- 57.7

pal <- colorNumeric("Blues", domain = mapdata_derelict$measure)
p_derelict <- mapdata_derelict %>% 
  leaflet(height=height, width=width) %>%
  addAwesomeMarkers(data = markers[1,], ~long, ~lat, popup = ~name, label =~name,
                    labelOptions = list(textsize = "20px",
                                        permanent = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = ~measure, opacity = .95, 
            title = "", 
            labFormat = labelFormat(suffix = "%")) %>%
  setView(view_long, view_lat, zoom) %>%
  addPolygons(weight = 0.2, fillOpacity = .95, smoothFactor = 1,
              fillColor = ~pal(measure),
              highlight = highlightOptions(weight = 1, fillOpacity = 1, bringToFront = TRUE),
              label = ~labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
  )

mapshot(p_derelict, file = file.path(getwd(), "pics", "map_derelict_scot.png"),
        vheight = height, vwidth = width, zoom = 1)

# Kilbirnie / North Ayrshire view for IZ
zoom <- 10
view_long <- -4.684395
view_lat <- 55.755429

p_derelict <- p_derelict %>%
  setView(view_long, view_lat, zoom)

mapshot(p_derelict, file = file.path(getwd(), "pics", "map_derelict_zoom.png"),
        vheight = height, vwidth = width, zoom = 1)

p_derelict <- p_derelict %>%
  addMiniMap("bottomleft", minimized = FALSE, toggleDisplay = TRUE)

write_rds(p_derelict, "pics/map_derelict_zoom.rds")
