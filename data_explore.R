library(tidyverse)
library(ggplot2)
library(plotly)

# specific indicators to look into
indicators <- c("Young people living in the most income deprived quintile",
                "Population within 500 metres of a derelict site",
                "Drug-related hospital stays, aged 11-25 years",
                "Alcohol-related hospital stays, aged 11-25 years",
                "Employment rate for 16-24 year olds")

dat <- read_rds("data/data.rds")
df_scot <- dat %>% filter(area_name == "Scotland")
df_rc <- dat %>% filter(area_name != "Scotland") 

# dataframe of all relevant local info with Scot info for comparisons
# df <- df_rc %>% left_join(df_scot, by = c("indicator", "year", "period", "definition"), suffix = c(".rc", ".scot")) %>%
#   filter(indicator %in% indicators)
df <- df_rc %>% bind_rows(df_scot)

p <- df %>% filter(indicator == indicators[2]) %>%
  # filter(area_name == "Scotland") %>%
  ggplot(aes(x = year, y = measure, color=area_name)) +
  geom_point() +
  geom_line()
ggplotly(p)
