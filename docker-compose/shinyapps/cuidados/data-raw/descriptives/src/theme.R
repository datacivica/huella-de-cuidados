#
# Author: OE
# Maintainer(s): OE, AF, GR
# License: (c) Data Cívica 2022, GPL v2 or newer
# --------------------------------------------------
# premio-gabo-cuidados/data/descriptives/src/theme.R
#

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, extrafont)
extrafont::loadfonts(quiet=T)

Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)

tema <-  theme_minimal() +
  theme(text = element_text(family = "Barlow Condensed", color = "grey35"),
        plot.title = element_text(size = 20, face = "bold", color = "black", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "bold", color = "#666666", hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
        panel.grid = element_line(linetype = 2), 
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold")
  )

pal_5 <-  c("#E69A73","#b32c1c","#0C9DAE","#286099","#838c5c" )

caption <- "Fuente: Elaboración propia a partir de la Encuesta Nacional de Uso de Tiempo"

# done.
