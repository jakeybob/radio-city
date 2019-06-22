library(tidyverse)
library(ggplot2)
library(plotly)
library(RColorBrewer)

#### DATA PREP ####
# specific indicators to look into
indicators <- c("Young people living in the most income deprived quintile",
                "Population within 500 metres of a derelict site",
                "Drug-related hospital stays, aged 11-25 years",
                "Alcohol-related hospital stays, aged 11-25 years",
                "Employment rate for 16-24 year olds")

dat <- read_rds("data/data_national_comparison.rds")
df_scot <- dat %>% filter(area_name == "Scotland")
df_rc <- dat %>% filter(area_name != "Scotland") 

df <- df_rc %>% bind_rows(df_scot)

df <- df %>% filter(indicator %in% indicators)

normal_size <- 1
scot_size <- 3
line_sizes <- rep(normal_size, length(unique(paste0(df$area_name, df$area_type))))
line_sizes[which(str_detect(unique(paste0(df$area_name, df$area_type)), "Scotland"))] <- scot_size

#### all trends ####
p <- df %>% 
  mutate(area_type = recode(area_type, Scotland = "National")) %>%
  mutate(area_name = paste0(area_name, " (", area_type, ")")) %>%
  mutate(indicator = paste0(indicator, ": ", definition)) %>%
  ggplot(aes(x = year, y = measure, color=area_name)) +
  geom_point(size = max(line_sizes) + 2) +
  geom_line(aes(size = area_name)) +
  scale_size_manual(values = line_sizes) +
  scale_color_manual(values=c("#E41A1C", "#A65628", "#4DAF4A", "#984EA3", "#FF7F00", "#DFDF13", "#276EA8")) +
  facet_wrap(vars(indicator), scales = "free_y", labeller=label_wrap_gen(width = 50, multi_line = TRUE), nrow = 3) +
  theme_bw() + 
  xlab("") + ylab("") + 
  theme(legend.title = element_blank(),
        legend.position = c(0.75, 0.13),
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"))

ggsave(filename = "pics/all_trends.png", device = "png", dpi = 300, width = 8, units = "in")

#### Alcohol-related hospital stays, aged 11-25 years ####
pal <- c("#E41A1C", "#FF7F00", "#276EA8")
df %>% filter(indicator == "Alcohol-related hospital stays, aged 11-25 years") %>%
  mutate(area_type = recode(area_type, Scotland = "National")) %>%
  mutate(area_name = paste0(area_name, " (", area_type, ")")) %>%
  mutate(indicator = paste0(indicator, ": ", definition)) %>%
  ggplot(aes(x = year, y = measure, color=area_name)) +
  geom_point(size = max(line_sizes) + 2) +
  geom_line(aes(size = area_name)) +
  scale_size_manual(values = line_sizes) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = min(df$year):max(df$year)) +
  theme_bw() + xlab("") + 
  ylab("Age-sex standardised rate per 100,000") + ggtitle("Alcohol-related hospital stays, aged 11-25 years") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"))

ggsave(filename = "pics/trend_alcohol_hospital.png", device = "png", dpi = 300, width = 8, units = "in")

p <- df %>% filter(indicator == "Alcohol-related hospital stays, aged 11-25 years") %>%
  mutate(area_type = recode(area_type, Scotland = "National")) %>%
  mutate(area_name = paste0(area_name, " (", area_type, ")")) %>%
  mutate(indicator = paste0(indicator, ": ", definition)) %>%
  plot_ly(x = ~year,
        y = ~measure,
        legendgroup = ~area_name,
        width = 800,
        height = 700,
        colors = pal) %>%
  add_lines(legendgroup=~area_name, color = ~area_name, name = ~area_name, showlegend = TRUE, 
            line = list(width = 5)) %>%
  add_markers(legendgroup=~area_name, color = ~area_name, name = ~area_name, showlegend = FALSE,
              marker = list(size = 18)) %>%
  layout(xaxis = list(title=""),
         legend = list(x = 100, y = 0.5),
         title = "Alcohol-related hospital stays, aged 11-25 years",
         yaxis = list(title = "Age-sex standardised rate per 100,000"))

write_rds(p, "pics/trend_alcohol_hospital.rds")



#### Drug-related hospital stays, aged 11-25 years ####
pal <- c("#E41A1C", "#FF7F00", "#276EA8")
df %>% filter(indicator == "Drug-related hospital stays, aged 11-25 years") %>%
  mutate(area_type = recode(area_type, Scotland = "National")) %>%
  mutate(area_name = paste0(area_name, " (", area_type, ")")) %>%
  mutate(indicator = paste0(indicator, ": ", definition)) %>%
  ggplot(aes(x = year, y = measure, color=area_name)) +
  geom_point(size = max(line_sizes) + 2) +
  geom_line(aes(size = area_name)) +
  scale_size_manual(values = line_sizes) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = min(df$year):max(df$year)) +
  theme_bw() + xlab("") + 
  ylab("Age-sex standardised rate per 100,000") + ggtitle("Drug-related hospital stays, aged 11-25 years") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"))

ggsave(filename = "pics/trend_drug_hospital.png", device = "png", dpi = 300, width = 8, units = "in")

p <- df %>% filter(indicator == "Drug-related hospital stays, aged 11-25 years") %>%
  mutate(area_type = recode(area_type, Scotland = "National")) %>%
  mutate(area_name = paste0(area_name, " (", area_type, ")")) %>%
  mutate(indicator = paste0(indicator, ": ", definition)) %>%
  plot_ly(x = ~year,
          y = ~measure,
          legendgroup = ~area_name,
          width = 800,
          height = 700,
          colors = pal) %>%
  add_lines(legendgroup=~area_name, color = ~area_name, name = ~area_name, showlegend = TRUE, 
            line = list(width = 5)) %>%
  add_markers(legendgroup=~area_name, color = ~area_name, name = ~area_name, showlegend = FALSE,
              marker = list(size = 18)) %>%
  layout(xaxis = list(title=""),
         legend = list(x = 100, y = 0.5),
         yaxis = list(title = "Age-sex standardised rate per 100,000"),
         title = "Drug-related hospital stays, aged 11-25 years")

write_rds(p, "pics/trend_drug_hospital.rds")



#### Employment rate for 16-24 year olds ####
pal <- c("#E41A1C", "#FF7F00", "#276EA8")
df %>% filter(indicator == "Employment rate for 16-24 year olds") %>%
  mutate(area_type = recode(area_type, Scotland = "National")) %>%
  mutate(area_name = paste0(area_name, " (", area_type, ")")) %>%
  mutate(indicator = paste0(indicator, ": ", definition)) %>%
  ggplot(aes(x = year, y = measure, color=area_name)) +
  geom_point(size = max(line_sizes) + 2) +
  geom_line(aes(size = area_name)) +
  scale_size_manual(values = line_sizes) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = min(df$year):max(df$year)) +
  theme_bw() + xlab("") + 
  ylab("percent") + ggtitle("Employment rate for 16-24 year olds") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"))

ggsave(filename = "pics/trend_employment.png", device = "png", dpi = 300, width = 8, units = "in")

p <- df %>% filter(indicator == "Employment rate for 16-24 year olds") %>%
  mutate(area_type = recode(area_type, Scotland = "National")) %>%
  mutate(area_name = paste0(area_name, " (", area_type, ")")) %>%
  mutate(indicator = paste0(indicator, ": ", definition)) %>%
  plot_ly(x = ~year,
          y = ~measure,
          legendgroup = ~area_name,
          width = 800,
          height = 700,
          colors = pal) %>%
  add_lines(legendgroup=~area_name, color = ~area_name, name = ~area_name, showlegend = TRUE, 
            line = list(width = 5)) %>%
  add_markers(legendgroup=~area_name, color = ~area_name, name = ~area_name, showlegend = FALSE,
              marker = list(size = 18)) %>%
  layout(xaxis = list(title=""),
         legend = list(x = 100, y = 0.5),
         yaxis = list(title = "percent"),
         title = "Employment rate for 16-24 year olds")

write_rds(p, "pics/trend_employment.rds")


#### Population within 500 metres of a derelict site ####
pal <- c("#E41A1C", "#A65628", "#4DAF4A", "#984EA3", "#FF7F00", "#DFDF13", "#276EA8")
df %>% filter(indicator == "Population within 500 metres of a derelict site") %>%
  mutate(area_type = recode(area_type, Scotland = "National")) %>%
  mutate(area_name = paste0(area_name, " (", area_type, ")")) %>%
  mutate(indicator = paste0(indicator, ": ", definition)) %>%
  ggplot(aes(x = year, y = measure, color=area_name)) +
  geom_point(size = max(line_sizes) + 2) +
  geom_line(aes(size = area_name)) +
  scale_size_manual(values = line_sizes) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = min(df$year):max(df$year)) +
  theme_bw() + xlab("") + 
  ylab("percent") + ggtitle("Population within 500 metres of a derelict site") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"))

ggsave(filename = "pics/trend_derelict.png", device = "png", dpi = 300, width = 8, units = "in")

p <- df %>% filter(indicator == "Population within 500 metres of a derelict site") %>%
  mutate(area_type = recode(area_type, Scotland = "National")) %>%
  mutate(area_name = paste0(area_name, " (", area_type, ")")) %>%
  mutate(indicator = paste0(indicator, ": ", definition)) %>%
  plot_ly(x = ~year,
          y = ~measure,
          legendgroup = ~area_name,
          width = 800,
          height = 700,
          colors = pal) %>%
  add_lines(legendgroup=~area_name, color = ~area_name, name = ~area_name, showlegend = TRUE, 
            line = list(width = 5)) %>%
  add_markers(legendgroup=~area_name, color = ~area_name, name = ~area_name, showlegend = FALSE,
              marker = list(size = 18)) %>%
  layout(xaxis = list(title=""),
         legend = list(x = 100, y = 0.5),
         yaxis = list(title = "percent"),
         title = "Population within 500 metres of a derelict site")

write_rds(p, "pics/trend_derelict.rds")


##### Young people living in the most income deprived quintile ####
pal <- c("#E41A1C", "#A65628", "#4DAF4A", "#984EA3", "#FF7F00", "#DFDF13", "#276EA8")
df %>% filter(indicator == "Young people living in the most income deprived quintile") %>%
  mutate(area_type = recode(area_type, Scotland = "National")) %>%
  mutate(area_name = paste0(area_name, " (", area_type, ")")) %>%
  mutate(indicator = paste0(indicator, ": ", definition)) %>%
  ggplot(aes(x = year, y = measure, color=area_name)) +
  geom_point(size = max(line_sizes) + 2) +
  geom_line(aes(size = area_name)) +
  scale_size_manual(values = line_sizes) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = min(df$year):max(df$year)) +
  theme_bw() + xlab("") + 
  ylab("percent") + ggtitle("Young people living in the most income deprived quintile") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"))

ggsave(filename = "pics/trend_income_deprived.png", device = "png", dpi = 300, width = 8, units = "in")

p <- df %>% filter(indicator == "Young people living in the most income deprived quintile") %>%
  mutate(area_type = recode(area_type, Scotland = "National")) %>%
  mutate(area_name = paste0(area_name, " (", area_type, ")")) %>%
  mutate(indicator = paste0(indicator, ": ", definition)) %>%
  plot_ly(x = ~year,
          y = ~measure,
          legendgroup = ~area_name,
          width = 800,
          height = 700,
          colors = pal) %>%
  add_lines(legendgroup=~area_name, color = ~area_name, name = ~area_name, showlegend = TRUE, 
            line = list(width = 5)) %>%
  add_markers(legendgroup=~area_name, color = ~area_name, name = ~area_name, showlegend = FALSE,
              marker = list(size = 18)) %>%
  layout(xaxis = list(title=""),
         legend = list(x = 100, y = 0.5),
         yaxis = list(title = "percent"),
         title = "Young people living in the most income deprived quintile")

write_rds(p, "pics/trend_income_deprived.rds")
