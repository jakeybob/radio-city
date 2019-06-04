library(tidyverse)

df <- read_csv("data/scotpho_data_extract.csv") %>% filter(area_type != "NA")

area_names <- list()
for(type in unique(df$area_type)){
  names <- df %>% filter(area_type == type) %>%
    select(area_name) %>%
    distinct()
  area_names[[type]] <- names
}
View(area_names)

# Following are relevant area names:
# "Alcohol & drug partnership": "North Ayrshire"
# "Health board": "Ayrshire & Arran"
# "Council area": "North Ayrshire"
# "Intermediate zone": "Kilbirnie North", "Kilbirnie South & Longbar"
# "HSC partnership": "North Ayrshire"
# "HSC locality": "Garnock Valley"

# reduce dataset to relevant Garnock/Kilbirnie/Ayshire, and Scotland for comparisons
df_comparisons <- df %>%
  filter(area_name %in% c("Scotland", "North Ayrshire", "Ayrshire & Arran", "Kilbirnie North", "Kilbirnie South & Longbar",
                          "Garnock Valley")) %>%
  select(-area_code)

write_rds(df_comparisons, "data/data.rds", "bz")
