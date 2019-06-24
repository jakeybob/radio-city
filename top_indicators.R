library(tidyverse)

#### Top 20 lists
# most extreme indicators (i.e. 20 "best" or "worst" indicators & areas for all time periods)
df <- read_rds("data/data_national_comparison.rds") %>%
  group_by(indicator, area_name, area_type)

df_scot <- df %>% filter(area_name == "Scotland")
df_rc <- df %>% filter(area_name != "Scotland") 
  
top_20_by_ind_area_all <- df_rc %>% left_join(df_scot, by=c("indicator", "year", "period", "definition")) %>%
  mutate(ratio = measure.x / measure.y) %>%
  group_by(indicator, area_name.x) %>%
  summarise(mean_ratio = mean(ratio)) %>%
  filter(mean_ratio != Inf) %>%
  mutate(abs_diff_ratio = abs(1 - mean_ratio)) %>%
  arrange(-abs_diff_ratio) %>%
  head(20)

# as above but for most recent data only
df <- read_rds("data/data_national_comparison.rds") %>%
  group_by(indicator, area_name, area_type) %>%
  filter(if_else(as.numeric(year) == max(as.numeric(year)), TRUE, FALSE) == TRUE) # most recent

df_scot <- df %>% filter(area_name == "Scotland")
df_rc <- df %>% filter(area_name != "Scotland") 

top_20_by_ind_area_rec <- df_rc %>% left_join(df_scot, by=c("indicator", "year", "period", "definition")) %>%
  mutate(ratio = measure.x / measure.y) %>%
  group_by(indicator, area_name.x) %>%
  summarise(mean_ratio = mean(ratio)) %>%
  filter(mean_ratio != Inf) %>%
  mutate(abs_diff_ratio = abs(1 - mean_ratio)) %>%
  arrange(-abs_diff_ratio) %>%
  head(20)

# output
print(top_20_by_ind_area_rec$indicator) # top 20 most recent
print(top_20_by_ind_area_all$indicator) # top 20 all years

print(unique(top_20_by_ind_area_all %>% bind_rows(top_20_by_ind_area_rec) %>% select(indicator))) # uniques in both lists
