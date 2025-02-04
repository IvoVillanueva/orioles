library(tidyverse)
library(rvest)
library(mlbstatsR)
library(ggtext)
library(ggimage)
library(here)

theme_ivo <- function () {
  theme_minimal(base_size=10.5, base_family="Chivo") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = '#f4f4f4', color = "#f4f4f4"),
      plot.caption = element_markdown(size = 5, hjust = 1)
    )
}
asp_ratio <- 1.618033

url <- "https://www.baseball-reference.com/teams/BAL/"


oriol <- url %>%
  read_html() %>%
  html_element("table") %>%
  html_table() %>%
  janitor::clean_names()

df <- oriol %>%
  select(year, r, ra) %>%
  arrange(year)


oriolcolor <- get_png_logos() %>%
  select(tm = full_name, primary = team_color, secundary = alternate_color, logo = logologodefault)

tmcolor <- oriolcolor %>% filter(tm == "Baltimore Orioles") %>%
  mutate( primary = case_when(
    tm == "Baltimore Orioles" ~ "#DF4601",
    TRUE ~ primary))

orioles <- df %>% filter(year >= 1984) %>%
  ggplot() +
  geom_line(aes(x = year, y = r, group = 1), color = "#DF4601", linetype = 'dashed', size = .4) +
  geom_point(color = 'black', fill = "#DF4601", shape = 21, size = 3.5, alpha = .75, aes(x = year, y = r)) +
  geom_line(aes(x = year, y = ra, group = 1), color = "black", linetype = 'dashed', size = .4) +
  geom_point(color = '#DF4601', fill = "black", shape = 21, size = 3.5, alpha = .75, aes(x = year, y = ra)) +

  theme_ivo() +
  scale_y_continuous(limits = c(250, 1010),breaks = scales::pretty_breaks(n = 20)) +
  scale_x_continuous(breaks = seq(1984, 2021, 1)) +
  labs(x = "Year\n",
       y = "Carreras") +
  labs(title = "Carreras <span style='color:#DF4601'>*Anotadas*</span> vs Carreras *Permitidas* de los <span style='color:#DF4601'>*Baltimore Orioles*</span>",
       subtitle = "Desde 1984 a 2021",
       caption = "**Datos**: *@baseball_ref* **Gráfico**: *Ivo Villanueva  @elcheff*") +
  theme(plot.title = element_markdown(face = 'bold', size = 15),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(size = 5),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
  annotate(geom = 'label', x = 2021, y = 517, size = 2, lineheight = 2.5, vjust = -0.7, label = "Anotadas", family = "Chivo", fill = "#DF4601", fontface = 'bold',  alpha = .75) +
  annotate(geom = 'label', x = 2021, y = 740 , size = 2,  lineheight = 2.5, vjust = -0.9, label = "Permitidas", family = "Chivo",  fill = "black", color = "#DF4601", fontface = 'bold', alpha = .75) +
  geom_curve(aes(x = 1990, y = 450, xend = 1984, yend = 655), curvature = -0.2, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(x = 1992, y = 450, xend = 1997, yend = 665), curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))) +
   annotate(geom = 'label', x = 1991, y = 460, hjust = .5, label = "Época de Altibajos", family = "Chivo", size = 2) +
  geom_curve(aes(x = 2003.5, y = 450, xend = 1998, yend = 770), curvature = -0.2, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(x = 2006.5, y = 450, xend = 2011, yend = 690), curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = 'label', x = 2005, y = 460,  label = "Periodo de Recesión", family = "Chivo", size = 2) +
  geom_curve(aes(x = 2013, y = 550, xend = 2012, yend = 690), curvature = -0.4, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(x = 2015, y = 550, xend = 2016, yend = 700), curvature = 0.4, arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = 'label', x = 2014, y = 545, label = "Vuelta al Éxito", family = "Chivo", size = 2) +
  geom_curve(aes(x = 2019, y = 1000, xend = 2017, yend = 850), curvature = 0.4, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(x = 2019, y = 1000, xend = 2021, yend = 790), curvature = -0.4, arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = 'label', x = 2019, y = 1005, label = "Reconstrucción", family = "Chivo", size = 2)


ggsave("Oriol.png", orioles,

       height = 6.5, width = 6.5*asp_ratio, dpi = "retina", type ="cairo"
)
