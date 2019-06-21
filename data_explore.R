library(tidyverse)
library(ggplot2)
library(plotly)
library(RColorBrewer)

# specific indicators to look into
indicators <- c("Young people living in the most income deprived quintile",
                "Population within 500 metres of a derelict site",
                "Drug-related hospital stays, aged 11-25 years",
                "Alcohol-related hospital stays, aged 11-25 years",
                "Employment rate for 16-24 year olds")

dat <- read_rds("data/data_national_comparison.rds")
df_scot <- dat %>% filter(area_name == "Scotland")
df_rc <- dat %>% filter(area_name != "Scotland") 

# dataframe of all relevant local info with Scot info for comparisons
# df <- df_rc %>% left_join(df_scot, by = c("indicator", "year", "period", "definition"), suffix = c(".rc", ".scot")) %>%
#   filter(indicator %in% indicators)
df <- df_rc %>% bind_rows(df_scot)

df <- df %>% filter(indicator %in% indicators)

normal_size <- 1
scot_size <- 3
line_sizes <- rep(normal_size, length(unique(paste0(df$area_name, df$area_type))))
line_sizes[which(str_detect(unique(paste0(df$area_name, df$area_type)), "Scotland"))] <- scot_size

p <- df %>% 
  mutate(area_type = recode(area_type, Scotland = "National")) %>%
  mutate(area_name = paste0(area_name, " (", area_type, ")")) %>%
  mutate(indicator = paste0(indicator, ": ", definition)) %>%
  ggplot(aes(x = year, y = measure, color=area_name)) +
  geom_point(size = max(line_sizes) + 2) +
  geom_line(aes(size = area_name)) +
  scale_size_manual(values = line_sizes) +
  # scale_color_brewer(palette = "Set1") +
  # scale_color_manual(values=c("#7FC97F", "#AE9EC4", "#FDC086", "#DFDF79", "#F0027F", "#BF5B17", "#386CB0")) +
  scale_color_manual(values=c("#E41A1C", "#A65628", "#4DAF4A", "#984EA3", "#FF7F00", "#DFDF13", "#276EA8")) +
  facet_wrap(vars(indicator), scales = "free_y", labeller=label_wrap_gen(width = 50, multi_line = TRUE), nrow = 2) +
  theme_bw() + 
  xlab("") + ylab("") + 
  theme(legend.title = element_blank(),
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"))
  
p
ggsave(filename = "pics/plot.pdf", device="pdf", width = 297, height = 210, units = "mm")
write_rds(p, "pics/all_five_scot.rds")
write_rds(ggplotly(p), "pics/all_five_scot_plotly.rds")

# ggplotly(p)
