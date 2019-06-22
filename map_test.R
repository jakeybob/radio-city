library(tidyverse)
library(leaflet)
library(sf)
library(mapview)
library(rmapshaper)
library(htmltools)

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
         year == max(year)) %>%
  mutate_if(sapply(., is.character), as.factor)


# intermediate zone choropleth
# https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search;jsessionid=7BFD03DFFDA5CD66CC065C75FBAD2172#/metadata/389787c0-697d-4824-9ca9-9ce8cb79d6f5

# note that 40 IZs don't have full names. Will use names from scotPHO data as they are more descriptive for this 40.
mapdata_income_deprived <- st_read("data/SG_IntermediateZoneBdry_2011") %>%
  ms_simplify(., drop_null_geometries = TRUE) %>%
  ms_filter_islands(., min_area = 1e8) %>%
  st_transform(crs="+proj=longlat +datum=WGS84") %>% 
  left_join(select(df_income_deprived, area_code, area_name, measure), by=c("InterZone" = "area_code")) %>%
  mutate(labels = sprintf("<strong>%s</strong><br/>%g&#37;", area_name, measure) %>% 
           lapply(HTML))

pal <- colorNumeric("Blues", domain = mapdata_income_deprived$measure)
height = 1150
width = 800

# Scotland view
zoom <- 7
view_long <- -4.15
view_lat <- 57.7

# Kilbirnie / North Ayrshire view
zoom <- 10
view_long <- -4.684395
view_lat <- 55.755429


p <- mapdata_income_deprived %>% 
  leaflet(height=height, width=width) %>%
  # addTiles() %>%
  # addMiniMap("bottomleft", minimized = TRUE, toggleDisplay = TRUE) %>%
  addAwesomeMarkers(data = markers[1,], ~long, ~lat, popup = ~name, label =~name,
             labelOptions = list(textsize = "20px",
                                 permanent = TRUE)) %>%
  addLegend("bottomright", pal = pal, values = ~measure, opacity = .95, 
            title = "", 
            labFormat = labelFormat(suffix = "%")) %>%
  setView(view_long, view_lat, zoom) %>%
  # addRectangles(-7.6, 54.5, -0.7, 60.9) %>%
  addPolygons(weight = 0.2, fillOpacity = .95, smoothFactor = 1,
              fillColor = ~pal(measure),
              highlight = highlightOptions(weight = 1, fillOpacity = 1, bringToFront = TRUE),
              label = ~labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
              )
p
mapshot(p, file = file.path(getwd(), "pics", "test3.png"),
        vheight = height, vwidth = width, zoom = 1)
write_rds(p, "pics/testmap.rds")
