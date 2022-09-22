#
# Author: Alicia Franco
# Maintainer(s): OE, AF, GR
# License: (c) Data Cívica 2022, GPL v2 or newer
# -----------------------------------------------------------
# premio-gabo-cuidados/data/descriptives/src/saldo-hrs.R
#

#### Paquetería ####
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, data.table, ggplot2, sf, janitor, scales)
X11(type="cairo")
#### Tema y funciones ####
source(here("data-raw/descriptives/src/theme.R"))

save <- function(name){
  ggsave(paste0(here("data/descriptives/out/"), name, ".png"), width = 10, height = 6)
  ggsave(paste0(here("data/descriptives/out/"), name, ".png"), width = 10, height = 6)
}
#### Abrir ####
enut_clean <- readRDS(here("data-raw/import-clean/out/enut.rds"))

# Reduce
reduced <- enut_clean

#### Edad ####
reduced %>%
  group_by(edad, tiene_hij) %>%
  summarise(saldo = sum(saldo*per_group)/sum(per_group)) %>%
  ungroup() %>%
  distinct() %>%
  filter(edad<=75) %>%
  ggplot(., aes(x = edad, y = saldo,
                  group = tiene_hij, color = tiene_hij,
                  fill = tiene_hij)) +
  geom_hline(yintercept = 0, color = pal_5[5], size = 1, linetype = "dotted") +
  geom_line(alpha = 0.9, size = 1.5) +
  geom_point(size = 2) +
  annotate('text',
           x = 78,
           y = 0,
           label = "Distribución\nequitativa",
           color = pal_5[5],
           size = 10,
           family = "Barlow Condensed") +
  scale_color_manual(values = pal_5) +
  scale_fill_manual(values = pal_5) +
  labs(title = "Saldo de cuidados",
       subtitle = "Según hijes",
       caption = caption,
       y = "Horas semanales más allá de una distribución equitativa",
       x = "Edad") +
  tema +
  theme(legend.title = element_blank(),
        legend.position = "right")

save("edad-mat-pat-line")

reduced %>%
  group_by(edad, tiene_hij) %>%
  summarise(saldo = sum(saldo*per_group)/sum(per_group)) %>%
  ungroup() %>%
  distinct() %>%
  filter(edad<=75) %>%
  ggplot(., aes(x = edad, y = saldo,
                group = tiene_hij, color = tiene_hij,
                fill = tiene_hij)) +
  geom_hline(yintercept = 0, color = pal_5[5], size = 1, linetype = "dotted") +
  geom_area(alpha = 0.5) +
  annotate('text',
           x = 78,
           y = 0,
           label = "Distribución\nequitativa",
           color = pal_5[5],
           size = 8,
           family = "Barlow Condensed") +
  scale_color_manual(values = pal_5) +
  scale_fill_manual(values = pal_5) +
  labs(title = "Saldo de cuidados",
       subtitle = "Según hijes",
       caption = caption,
       y = "Horas semanales más allá de una distribución equitativa",
       x = "Edad") +
  tema +
  theme(legend.title = element_blank(),
        legend.position = "right")

save("edad-mat-pat-area")

#### Escolaridad ####
ggplot() +
  geom_hline(yintercept = 0, color = pal_5[5], size = 1, linetype = "dotted") +
  geom_boxplot(data = reduced %>% filter(edad %in% 20:50),
               aes(x = niv,
                   y = saldo,
                   fill = tiene_hij,
                   color = tiene_hij,
                   weight = per_group),
               alpha = 0.3, coef = 1, fatten = 2, outlier.shape = NA,
               outlier.size = NA) +
  geom_point(data =
               reduced %>%
               filter(edad %in% 20:50) %>%
               group_by(niv,tiene_hij) %>%
               summarise(saldo = sum(saldo*per_group)/sum(per_group)) %>%
               ungroup(),
             aes(x = niv, y = saldo, shape = tiene_hij, group = tiene_hij, color = tiene_hij),
             size = 4, position = position_dodge(0.75)) +
  ylim(-50,60)+
  scale_x_discrete(
    limits = c("Preescolar o menos", "Primaria", "Secundaria", "Preparatoria",
               "Licenciatura o más", "")
  ) +
  annotate('text',
           x = 5.8,
           y = 0,
           label = "División\nequitativa",
           color = pal_5[5],
           size = 8, fontface = 2,
           family = "Barlow Condensed") +
  scale_color_manual(values = pal_5) +
  scale_fill_manual(values = pal_5) +
  labs(title = "Horas semanales más allá de una distribución equitativa",
       subtitle = "Por escolaridad para personas entre 20 y 50 años",
       y = "Saldo en horas semanales",
       x = "",
       caption = caption) +
  tema +
  theme(legend.title = element_blank()) +
  geom_text(label = "Distribución\nequitativa",
            x = 6, y = 0)

save("escolaridad-mat-pat")

#### Trab_dom ####
#### Boxplot
ggplot() +
  geom_hline(yintercept = 0, color = pal_5[5], size = 1, linetype = "dotted") +
  geom_boxplot(data = reduced,
               aes(x = trab_dom,
                   y = saldo,
                   fill = tiene_hij,
                   color = tiene_hij,
                   weight = per_group),
               alpha = 0.3, coef = 1, fatten = 2, outlier.shape = NA,
               outlier.size = NA) +
  geom_point(data =
               reduced %>%
               filter(edad %in% 20:50) %>%
               group_by(trab_dom,tiene_hij) %>%
               summarise(saldo = sum(saldo*per_group)/sum(per_group)) %>%
               ungroup(),
             aes(x = trab_dom, y = saldo, shape = tiene_hij, group = tiene_hij, color = tiene_hij),
             size = 4, position = position_dodge(0.75)) +
  annotate('text',
           x = 2.48,
           y = 0,
           label = "División\nequitativa",
           color = pal_5[5],
           size = 8, fontface = 2,
           family = "Barlow Condensed") +
  scale_color_manual(values = pal_5) +
  scale_fill_manual(values = pal_5) +
  labs(title = "Horas semanales más allá de una distribución equitativa",
       subtitle = "¿Contrata servicio doméstico o de enfermería?",
       y = "Saldo en horas semanales",
       x = "",
       caption = caption) +
  tema +
  theme(legend.title = element_blank()) +
  geom_text(label = "Distribución\nequitativa",
            x = 6, y = 0)
save("trab_dom-mat-pat-box")

#### Histograma
#### A
ggplot(reduced %>%
         filter(str_detect(tiene_hij, "sin")) %>%
         pivot_wider(names_from = tiene_hij, values_from = saldo) %>%
         select(c("Mujer sin hijes", "Hombre sin hijes","per_group", "trab_dom")),
       aes(weight = per_group, group = trab_dom)) +
  geom_vline(xintercept = 0, color = pal_5[5], size = 1, linetype = "dotted") +
  geom_histogram(aes(x = .data[["Mujer sin hijes"]], y = ..density.., color = "Mujer sin hijes", fill = "Mujer sin hijes"), alpha = 0.5) +
  geom_histogram(aes(x =  .data[["Hombre sin hijes"]], y = -..density.., color = "Hombre sin hijes", fill = "Hombre sin hijes"), alpha = 0.5) +
  scale_color_manual(values = c("Mujer sin hijes" = pal_5[1], "Hombre sin hijes" = pal_5[3]), guide = F) +
  scale_fill_manual(values = c("Mujer sin hijes" = pal_5[1], "Hombre sin hijes" = pal_5[3])) +
  coord_flip() +
  geom_text(data = data.frame(x = 1.5, y = 0.015, trab_dom = "Sí contrata", per_group = 1, lab = "División\nequitativa"),
            aes(label=lab, x = x, y = y),
            color = pal_5[5],
            family = "Barlow Condensed",
            size = 8, fontface = 2) +
  facet_wrap(~trab_dom, strip.position = "bottom") +
  labs(title = "Horas semanales más allá de una distribución equitativa",
       subtitle = "¿Contrata servicio doméstico o de enfermería?",
       x = "Saldo en horas semanales",
       y = "",
       caption = caption) +
  tema +
  theme( legend.title = element_blank())

save("trab_dom-hsh-msh-hist")
#### B
ggplot(reduced %>%
         filter(!(str_detect(tiene_hij, "sin"))) %>%
         pivot_wider(names_from = tiene_hij, values_from = saldo) %>%
         select(c("Madre", "Padre","per_group", "trab_dom")),
       aes(weight = per_group, group = trab_dom)) +
  geom_vline(xintercept = 0, color = pal_5[5], size = 1, linetype = "dotted") +
  geom_histogram(aes(x = .data[["Madre"]], y = ..density.., color = "Madre", fill = "Madre"), alpha = 0.5) +
  geom_histogram(aes(x =  .data[["Padre"]], y = -..density.., color = "Padre", fill = "Padre"), alpha = 0.5) +
  scale_color_manual(values = c("Madre" = pal_5[2], "Padre" = pal_5[4]), guide = F) +
  scale_fill_manual(values = c("Madre" = pal_5[2], "Padre" = pal_5[4])) +
  coord_flip() +
  geom_text(data = data.frame(x = 3, y = -.025, trab_dom = "Sí contrata", per_group = 1, lab = "División\nequitativa"),
            aes(label=lab, x = x, y = y),
            color = pal_5[5],
            family = "Barlow Condensed", fontface = 2,
            size = 8) +
  facet_wrap(~trab_dom, strip.position = "bottom") +
  labs(title = "Horas semanales más allá de una distribución equitativa",
       subtitle = "¿Contrata servicio doméstico o de enfermería?",
       x = "Saldo en horas semanales",
       y = "",
       caption = caption) +
  tema +
  theme(axis.text.x = element_blank(), legend.title = element_blank())

save("trab_dom-mat-pat-hist")
#### Estancia ####
#### Box-plot
ggplot() +
  geom_hline(yintercept = 0, color = pal_5[5], size = 1, linetype = "dotted") +
  geom_boxplot(data = reduced,
               aes(x = estancia,
                   y = saldo,
                   fill = tiene_hij,
                   color = tiene_hij,
                   weight = per_group),
               alpha = 0.3, coef = 1, fatten = 2, outlier.shape = NA,
               outlier.size = NA) +
  geom_point(data =
               reduced %>%
               group_by(estancia,tiene_hij) %>%
               summarise(saldo = sum(saldo*per_group)/sum(per_group)) %>%
               ungroup(),
             aes(x = estancia, y = saldo, shape = tiene_hij, group = tiene_hij, color = tiene_hij),
             size = 4, position = position_dodge(0.75)) +
  annotate('text',
           x = 2.48,
           y = 0,
           label = "División\nequitativa",
           color = pal_5[5],
           size = 8,
           fontface = 2,
           family = "Barlow Condensed") +
  scale_color_manual(values = pal_5) +
  scale_fill_manual(values = pal_5) +
  labs(title = "Horas semanales más allá de una distribución equitativa",
       subtitle = "¿Su trabajo le ofrece estancia o guardería infantil?",
       y = "Saldo en horas semanales",
       x = "",
       caption = caption) +
  tema +
  theme(legend.title = element_blank())

save("estancia-mat-pat-box")

#### Histograma
###### A
ggplot(reduced %>%
         filter(str_detect(tiene_hij, "sin")) %>%
         pivot_wider(names_from = tiene_hij, values_from = saldo) %>%
         select(c("Mujer sin hijes", "Hombre sin hijes","per_group", "estancia")),
       aes(weight = per_group, group = estancia)) +
  geom_vline(xintercept = 0, color = pal_5[5], size = 1, linetype = "dotted") +
  geom_histogram(aes(x = .data[["Mujer sin hijes"]], y = ..density.., color = "Mujer sin hijes", fill = "Mujer sin hijes"), alpha = 0.5) +
  geom_histogram(aes(x =  .data[["Hombre sin hijes"]], y = -..density.., color = "Hombre sin hijes", fill = "Hombre sin hijes"), alpha = 0.5) +
  scale_color_manual(values = c("Mujer sin hijes" = pal_5[1], "Hombre sin hijes" = pal_5[3]), guide = F) +
  scale_fill_manual(values = c("Mujer sin hijes" = pal_5[1], "Hombre sin hijes" = pal_5[3])) +
  coord_flip() +
  geom_text(data = data.frame(x = 0, y = -0.018, estancia = "Sí tiene", per_group = 1, lab = "División\nequitativa"),
            aes(label=lab, x = x, y = y),
            color = pal_5[5], fontface = 2,
            family = "Barlow Condensed",
            size = 8) +
  facet_wrap(~estancia, strip.position = "bottom") +
  labs(title = "Horas semanales más allá de una distribución equitativa",
       subtitle = "¿Su trabajo le ofrece estancia o guardería infantil?",
       x = "Saldo en horas semanales",
       y = "",
       caption = caption) +
  tema +
  theme(axis.text.x = element_blank(), legend.title = element_blank())

save("estancia-hsh-msh-hist")
######B
ggplot(reduced %>%
         filter(!(str_detect(tiene_hij, "sin"))) %>%
         pivot_wider(names_from = tiene_hij, values_from = saldo) %>%
         select(c("Madre", "Padre","per_group", "estancia")),
       aes(weight = per_group, group = estancia)) +
  geom_vline(xintercept = 0, color = pal_5[5], size = 1, linetype = "dotted") +
  geom_histogram(aes(x = .data[["Madre"]], y = ..density.., color = "Madre", fill = "Madre"), alpha = 0.5) +
  geom_histogram(aes(x =  .data[["Padre"]], y = -..density.., color = "Padre", fill = "Padre"), alpha = 0.5) +
  scale_color_manual(values = c("Madre" = pal_5[2], "Padre" = pal_5[4]), guide = F) +
  scale_fill_manual(values = c("Madre" = pal_5[2], "Padre" = pal_5[4])) +
  coord_flip() +
  geom_text(data = data.frame(x = 0, y = -.02, estancia = "Sí tiene", per_group = 1, lab = "División\nequitativa"),
            aes(label=lab, x = x, y = y),
            color = pal_5[5], fontface=2,
            family = "Barlow Condensed",
            size = 8) +
  facet_wrap(~estancia, strip.position = "bottom") +
  labs(title = "Horas semanales más allá de una distribución equitativa",
       subtitle = "¿Su trabajo le ofrece estancia o guardería infantil?",
       x = "Saldo en horas semanales",
       y = "",
       caption = caption) +
  tema +
  theme(axis.text.x = element_blank(), legend.title = element_blank())
save("estancia-mat-pat-hist")

#### Entidad Federativa ####
reduced %>%
  group_by(ent) %>%
  summarise(saldo = sum(saldo*per_group)/sum(per_group)) %>%
  ungroup() %>%
  left_join(.,
            st_read(here("data/import-clean/inp/estados/ESTADOS.shp"), options = "ENCODING=latin1") %>%
              clean_names() %>%
              select(ent = cve_ent, nom_ent, geometry)) %>%
ggplot(.) +
  geom_sf(aes(geometry= geometry, fill = saldo), color = "black")+
  scale_fill_gradientn(colors = c(pal_5[2], "white", pal_5[4]),
                       values = rescale(c(-10,0,11)), limits=c(-10,11)) +
  labs(title = "Horas semanales más allá de una distribución equitativa regional",
       caption = caption) +
  tema+
  theme(axis.text= element_blank())
save("saldo-map")

reduced %>%
  group_by(ent, tiene_hij) %>%
  summarise(saldo = sum(saldo*per_group)/sum(per_group)) %>%
  ungroup() %>%
  left_join(.,
            st_read(here("data/import-clean/inp/estados/ESTADOS.shp"), options = "ENCODING=latin1") %>%
              clean_names() %>%
              select(ent = cve_ent, nom_ent, geometry)) %>%
  ggplot(.) +
  geom_sf(aes(geometry= geometry, fill = saldo), color = "black")+
  facet_wrap(~tiene_hij)+
  scale_fill_gradientn(colors = c(pal_5[2], "white", pal_5[4]),
                       values = rescale(c(-20,0,30)), limits=c(-20,30)) +
  labs(title = "Horas semanales más allá de una distribución equitativa regional",
       caption = caption) +
  tema  +
  theme(axis.text= element_blank())
save("saldo-tiene-hij-map")

#### Pareja ####

#### Histograma
###### A
ggplot(reduced %>%
         filter(str_detect(tiene_hij, "sin")) %>%
         pivot_wider(names_from = tiene_hij, values_from = saldo) %>%
         select(c("Mujer sin hijes", "Hombre sin hijes","per_group", "pareja")),
       aes(weight = per_group, group = pareja)) +
  geom_vline(xintercept = 0, color = pal_5[5], size = 1, linetype = "dotted") +
  geom_histogram(aes(x = .data[["Mujer sin hijes"]], y = ..density.., color = "Mujer sin hijes", fill = "Mujer sin hijes"), alpha = 0.5) +
  geom_histogram(aes(x =  .data[["Hombre sin hijes"]], y = -..density.., color = "Hombre sin hijes", fill = "Hombre sin hijes"), alpha = 0.5) +
  scale_color_manual(values = c("Mujer sin hijes" = pal_5[1], "Hombre sin hijes" = pal_5[3]), guide = F) +
  scale_fill_manual(values = c("Mujer sin hijes" = pal_5[1], "Hombre sin hijes" = pal_5[3])) +
  coord_flip() +
  geom_text(data = data.frame(x = 0, y = -0.018, pareja = "Sí tiene", per_group = 1, lab = "División\nequitativa"),
            aes(label=lab, x = x, y = y),
            color = pal_5[5], fontface = 2,
            family = "Barlow Condensed",
            size = 8) +
  facet_wrap(~ pareja, strip.position = "bottom") +
  labs(title = "Horas semanales más allá de una distribución equitativa",
       subtitle = "¿Tienes pareja?",
       x = "Saldo en horas semanales",
       y = "",
       caption = caption) +
  tema +
  theme(axis.text.x = element_blank(), legend.title = element_blank())
save("pareja-hsh-msh-hist")

###### B
ggplot(reduced %>%
         filter(!(str_detect(tiene_hij, "sin"))) %>%
         pivot_wider(names_from = tiene_hij, values_from = saldo) %>%
         select(c("Madre", "Padre","per_group", "pareja")),
       aes(weight = per_group, group = pareja)) +
  geom_vline(xintercept = 0, color = pal_5[5], size = 1, linetype = "dotted") +
  geom_histogram(aes(x = .data[["Madre"]], y = ..density.., color = "Madre", fill = "Madre"), alpha = 0.5) +
  geom_histogram(aes(x =  .data[["Padre"]], y = -..density.., color = "Padre", fill = "Padre"), alpha = 0.5) +
  scale_color_manual(values = c("Madre" = pal_5[2], "Padre" = pal_5[4]), guide = F) +
  scale_fill_manual(values = c("Madre" = pal_5[2], "Padre" = pal_5[4])) +
  coord_flip() +
  geom_text(data = data.frame(x = 0, y = -.02, pareja = "Sí tiene", per_group = 1, lab = "División\nequitativa"),
            aes(label=lab, x = x, y = y),
            color = pal_5[5], fontface=2,
            family = "Barlow Condensed",
            size = 8) +
  facet_wrap(~pareja, strip.position = "bottom") +
  labs(title = "Horas semanales más allá de una distribución equitativa",
       subtitle = "¿Tienes pareja?",
       x = "Saldo en horas semanales",
       y = "",
       caption = caption) +
  tema +
  theme(axis.text.x = element_blank(), legend.title = element_blank())
save("pareja-mat-pat-hist")

# done
rm(list=ls())
