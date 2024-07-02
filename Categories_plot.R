#Making plots with different country groups#

library(ggplot2)
library(dplyr)
library(tidyverse)
library(colorspace)
library(readxl)

library(showtext)
font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

#Load data#
df <- read_excel("C:/Users/frme/OneDrive - Folkehelseinstituttet/Dokumenter/SoMiCo/2024_1_Categorizations/Actual numbers.xlsx")

# Create new column with only the numbers
df$Landkode <- as.numeric(sub("\\..*", "", df$Country))
landnavn <- read_excel("C:/Users/frme/OneDrive - Folkehelseinstituttet/Dokumenter/SoMiCo/2024_1_Categorizations/Landnavn.xlsx")
df <- left_join(df, landnavn, by = "Landkode")

#Keep only countries with more than 100 children#
df <- df %>%
  filter(Antall_barn_i_datasett > 100 & !is.na(Antall_barn_i_datasett))


#Create quartiles#
df_project <- 
  df %>% 
  group_by(Gruppe) %>% 
  mutate(
    median = median(Estimat),
    q25 = quantile(Estimat, probs = .25),
    q75 = quantile(Estimat, probs = .75),
    n = n()
  ) %>% 
  ungroup() %>% 
  mutate(gruppe_num = as.numeric(fct_rev(Gruppe))*3) 


#Create dataframe with countries instead of individuals#
countries <- data.frame(
  Coefficient = rnorm(80, mean = 2, sd = 0.2),
  Group = sample(c("EU/EEA/Oceania/USA/Canada", 
                   "Europe except EU/EEA", 
                   "Asia",
                   "Africa", 
                   "Asia",
                   "Latin America", 
                   "Europe and Central Asia",
                   "Middle East and North Africa", 
                   "North America",
                   "South Asia", 
                   "Sub Saharan Africa",
                   "HDI Very high", 
                   "HDI High",
                   "HDI Middle", 
                   "HDI Low",
                   "High income countries", 
                   "Upper middle income countries",
                   "Lower middle income countries"), 80, replace = TRUE)
)

#Create ranks#
mean_estimates <- df_project %>%
  group_by(Gruppe) %>%
  summarise(mean_estimat = median(Estimat, na.rm = TRUE))

# Rank the means
mean_estimates <- mean_estimates %>%
  arrange(desc(mean_estimat)) %>%
  mutate(rank = rank(mean_estimat, ties.method = "random"))

df_project <- df_project %>%
  left_join(mean_estimates, by = "Gruppe")

df_project$rank <- df_project$rank*2  

df_project$farge <- as.factor(df_project$rank)



ggplot(df_project, aes(Estimat, rank)) +
  geom_boxplot(
    aes(
      color = Gruppe,
      color = after_scale(darken(color, .1, space = "HLS"))
    ),
    width = 0,
    size = .9
  ) +
  geom_rect(
    aes(
      xmin = q25,
      xmax = median,
      ymin = rank - .10,
      ymax = rank - .55,
      color = Gruppe,
      fill = after_scale(lighten(color, .7, space = "HLS")))
  ) +
  geom_rect(
    aes(
      xmin = q75,
      xmax = median,
      ymin = rank - .10,
      ymax = rank - .55,
      color = Gruppe,
      fill = after_scale(lighten(color, .5, space = "HLS")))
  ) +
  geom_segment(
    aes(
      x = q25, 
      xend = q25,
      y = rank - .10,
      yend = rank - .55,
      color = Gruppe,
      color = after_scale(darken(color, .05, space = "HLS"))
    ),
    size = .25
  ) +
  geom_segment(
    aes(
      x = q75, 
      xend = q75,
      y = rank - .10,
      yend = rank - .55,
      color = Gruppe,
      color = after_scale(darken(color, .05, space = "HLS"))
    ),
    size = .25
  ) +
  ggdist::stat_halfeye(
    aes(
      y = rank,
      color = Gruppe,
      fill = after_scale(lighten(color, .5))
    ),
    shape = 18,
    point_size = 3,
    adjust = .5,
  ) +
  geom_text(
    data = df_project %>% 
      group_by(Kategori, Gruppe, rank) %>% 
      summarize(m = unique(median)),
    aes(
      x = m, 
      y = rank - .80,
      label = format(round(m, 2), nsmall = 0),
      color = Gruppe
    ),
    inherit.aes = F,
    family = "Montserrat",
    fontface = "bold",
    size = 4
  ) +
  geom_text(
    data = df_project %>% 
      group_by(Kategori, Gruppe, rank) %>% 
      summarize(n = unique(n), max = max(Estimat, na.rm = T)),
    aes(
      x = max + .005, 
      y = rank - .02,
      label = glue::glue("n = {n}"),
      color = Gruppe
    ),
    inherit.aes = F,
    family = "Montserrat",
    size = 3.5,
    hjust = 0
  ) +
  geom_text(
    data = df_project %>% 
      group_by(Kategori, Gruppe, rank) %>% 
      summarize(m = unique(median), label = unique(Gruppe)),
    aes(
      x = m - 0.022, 
      y = rank + .8,
      label = label,
      color = Gruppe
    ),
    inherit.aes = F,
    family = "Montserrat",
    size = 3.5,
    hjust = 1
  ) +
  geom_point(data = df_project,
             aes(
               y = rank - 0.2,
               color = Gruppe,
               color = after_scale(darken(color, .05, space = "HLS"))
             ),
             size = 1.5,
             alpha = 0.2,
             position = position_jitter(
               seed = 0.01, width = .01, height = 0
             )
  ) +
  coord_cartesian(clip = "off") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(family = "Montserrat", size = 20, hjust = 0.5),
    axis.text.y = element_blank(),
    axis.ticks.length = unit(0, "lines"),
    plot.subtitle = element_text(margin = margin(0, 0, -10, 0)),
    legend.position = "none",
  ) +
  labs(x = "", y = "", title = "Health care use groups") +
  scale_x_continuous(limits = c(0.5, 2)) +
  scale_colour_manual(values = c("#009E73", "#009E73", "#009E73", "#56B4E9", "#56B4E9", "#009E73",
                                 "#CC79A7", "#CC79A7", "#CC79A7", "#CC79A7", "#E69F00", "#1ABC9C",
                                 "#56B4E9", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9", "#56B4E9",
                                 "#56B4E9", "#E69F00")) +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "grey") +
  annotate("text", x = 1.10, y = -1, label = "More health care usage\n than Norwegians", family = "Montserrat", size = 3, color = "grey20") +
  annotate("text", x = 0.90, y = -1, label = "Less health care usage\n than Norwegians", family = "Montserrat", size = 3, color = "grey20") +
  annotate("segment",  x = 0.95, y = -2.5, xend = 0.86, yend = -2.5, arrow = arrow(type = "closed", length = unit(0.005, "npc"))) +
  annotate("segment",  x = 1.05, y = -2.5, xend = 1.14, yend = -2.5, arrow = arrow(type = "closed", length = unit(0.005, "npc")))
