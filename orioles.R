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

# logos <- orioles +
#   geom_curve(aes(x = 1984, y = 350, xend = 1988, yend = 250), curvature = -0.2, arrow = arrow(length = unit(0.01, "npc"))) +
#   geom_image(
#   aes(
#     x = 1985, y = 350,
#     image = "https://content.sportslogos.net/logos/53/52/full/4bmrvfygivt6dgyw9hntp3aqc.png"
#   ) , hjust=1,
#   # Set size, and aspect ratio
#   size = 0.05, by = "width", asp = asp_ratio
# ) +
#   geom_curve(aes(x = 1990, y = 350, xend = 1989, yend = 250), curvature = 0.2, arrow = arrow(length = unit(0.01, "npc"))) +
#   geom_curve(aes(x = 1990, y = 350, xend = 1991, yend = 250), curvature = -0.2, arrow = arrow(length = unit(0.01, "npc"))) +
# geom_image(
#   aes(
#     x = 1991, y = 350,
#     image = "https://content.sportslogos.net/logos/53/52/full/b7sogwikzpvtf2gotztgvtpyf.png"
#   ) , hjust=1,
#   # Set size, and aspect ratio
#   size = 0.05, by = "width", asp = asp_ratio
# )  +
#   geom_curve(aes(x = 1993, y = 350, xend = 1992, yend = 250), curvature = 0.2, arrow = arrow(length = unit(0.01, "npc"))) +
#   geom_curve(aes(x = 1993, y = 350, xend = 1994, yend = 250), curvature = -0.2, arrow = arrow(length = unit(0.01, "npc"))) +
#   geom_image(
#     aes(
#       x = 1994, y = 350,
#       image = "https://i1.wp.com/www.retroseasons.com/retroimages//0-logo-BBALO-1992.gif"
#     ) , hjust=1,
#     # Set size, and aspect ratio
#     size = 0.07, by = "width", asp = asp_ratio
#   )  +
#   geom_curve(aes(x = 1996, y = 350, xend = 1995, yend = 250), curvature = 0.2, arrow = arrow(length = unit(0.01, "npc"))) +
#   geom_curve(aes(x = 1996, y = 350, xend = 1997, yend = 250), curvature = -0.2, arrow = arrow(length = unit(0.01, "npc"))) +
#   geom_image(
#     aes(
#       x = 1998.5, y = 350,
#       image = "https://logos-world.net/wp-content/uploads/2020/05/Baltimore-Orioles-Logo-1995-1997.png"
#     ) , hjust=1,
#     # Set size, and aspect ratio
#     size = 0.12, by = "width", asp = asp_ratio
#   )  +
#   geom_segment(aes(x = 1998, y = 410, xend = 1998, yend = 250), arrow = arrow(length = unit(0.01, "npc"))) +
#   geom_image(
#     aes(
#       x = 2000.4, y = 410,
#       image = "https://logos-world.net/wp-content/uploads/2020/05/Baltimore-Orioles-Logo-1998-700x394.png"
#     ) , hjust=1,
#     # Set size, and aspect ratio
#     size = 0.12, by = "width", asp = asp_ratio
#   ) +
#   geom_curve(aes(x = 2004, y = 350, xend = 1999, yend = 250), curvature = 0.2, arrow = arrow(length = unit(0.01, "npc"))) +
#   geom_curve(aes(x = 2004, y = 350, xend = 2008, yend = 250), curvature = -0.2, arrow = arrow(length = unit(0.01, "npc"))) +
#   geom_image(
#     aes(
#       x = 2006.4, y = 350,
#       image = "https://logos-world.net/wp-content/uploads/2020/05/Baltimore-Orioles-Logo-1999-2008-700x394.png"
#     ) , hjust=1,
#     # Set size, and aspect ratio
#     size = 0.12, by = "width", asp = asp_ratio
#   ) +
#   geom_curve(aes(x = 2013.5, y = 350, xend = 2009, yend = 250), curvature = 0.2, arrow = arrow(length = unit(0.01, "npc"))) +
#   geom_curve(aes(x = 2013.5, y = 350, xend = 2018, yend = 250), curvature = -0.2, arrow = arrow(length = unit(0.01, "npc"))) +
#   geom_image(
#     aes(
#       x = 2016, y = 350,
#       image = "https://logos-world.net/wp-content/uploads/2020/05/Baltimore-Orioles-Logo-2009-2018-700x394.png"
#     ) , hjust=1,
#     # Set size, and aspect ratio
#     size = 0.12, by = "width", asp = asp_ratio
#   ) +
#   geom_segment(aes(x = 2019, y = 350, xend = 2019, yend = 250), arrow = arrow(length = unit(0.01, "npc"))) +
#   geom_curve(aes(x = 2019, y = 350, xend = 2021, yend = 250), curvature = -0.2, arrow = arrow(length = unit(0.01, "npc"))) +
# geom_image(
#   aes(
#     x = 2020.3, y = 350,
#     image = tmcolor$logo
#   ) , hjust=1,
#   # Set size, and aspect ratio
#   size = 0.05, by = "width", asp = asp_ratio
# )
# # https://content.sportslogos.net/logos/53/52/full/4bmrvfygivt6dgyw9hntp3aqc.png
# # https://content.sportslogos.net/logos/53/52/full/bqer2xhao4mm1blj4ngbnx1tj.png
# # https://content.sportslogos.net/logos/53/52/full/b7sogwikzpvtf2gotztgvtpyf.png
# # https://logos-marcas.com/wp-content/uploads/2020/06/Baltimore-Orioles-Logotipo-1995-1997.png
# # https://content.sportslogos.net/logos/53/52/full/6y68fyd4c7c6sgfhykhe.png
# # https://content.sportslogos.net/logos/53/52/full/p51ytld8grrh11nhiolx.png
# # https://content.sportslogos.net/logos/53/52/full/lty880yrmrra64y6tqfqmdnbf.png
# # https://i1.wp.com/www.retroseasons.com/retroimages//0-logo-BBALO-1992.gif
# # https://www.vhv.rs/dpng/d/532-5322860_baltimore-orioles-logo-font-baltimore-orioles-logo-svg.png

ggsave("Oriol.png", orioles,

       height = 6.5, width = 6.5*asp_ratio, dpi = "retina", type ="cairo"
)
