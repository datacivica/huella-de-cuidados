#
# Author: Alicia Franco
# Maintainer(s): OE, AF, GR
# License: (c) Data Cívica 2022, GPL v2 or newer
# -----------------------------------------------------------
# premio-gabo-cuidados/data/descriptives/src/funcoines.R
#

#### Paquetería ####
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, data.table, ggplot2, janitor)
X11(type = "cairo")
#### Tema y funciones ####
source(here("data-raw/descriptives/src/theme.R"))

save <- function(name){
  ggsave(paste0(here("data/descriptives/out/"), name, ".png"), width = 10, height = 6)
  ggsave(paste0(here("data/descriptives/out/"), name, ".svg"), width = 10, height = 6)
}
#### Abrir ####
enut_clean <- readRDS(here("data-raw/import-clean/out/enut.rds"))

# Reduce
reduced <- enut_clean %>%
  filter(hrs_cuidado <= 168, !is.na(estancia), !is.na(trab_dom), !is.na(tiene_hij)) %>%
  group_by(ent, niv, sexo, edad, estancia, trab_dom, tiene_hij, pareja, hrs_cuidado)  %>%
  summarise(hrs_cuidado_reduced = sum(hrs_cuidado*fac_per, na.rm = T),
            prom_cuidado_reduced = hrs_cuidado_reduced/sum(fac_per),
            per_group = sum(fac_per)) %>%
  ungroup() %>%
  mutate(tot_cuidado_nac_percap = sum(hrs_cuidado_reduced)/sum(per_group),
         saldo = prom_cuidado_reduced - tot_cuidado_nac_percap,
         trab_dom = factor(ifelse(trab_dom == 1, "Sí contrata", "No contrata")),
         estancia = factor(ifelse(estancia == 1, "Sí tiene", "No tiene")),
         tiene_hij = factor(ifelse(str_detect(tiene_hij,"sin"), "No tiene", "Sí tiene"),
                            levels = c("No tiene",
                                       "Sí tiene")),
         pareja = factor(ifelse(pareja == 1, "Sí tiene", "No tiene"))
  )

#### Histograma ####

tempo <- reduced %>%
  filter(tiene_hij == "Sí tiene", trab_dom == "Sí contrata", niv == "Licenciatura o más") %>%
  group_by(sexo) %>%
  summarise(prom = sum(hrs_cuidado*tot_cuidado_nac_percap)/sum(tot_cuidado_nac_percap),
            carac = "sexo",
            presence = ifelse(sexo == "mujer", prom, NA)) %>%
  ungroup() %>%
  distinct()
ggplot(reduced,
       aes(weight = per_group)) +
  geom_rect(xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "lightblue", alpha = 0.15) +
  geom_rect(xmin = 0, xmax = -Inf, ymin = -Inf, ymax = Inf, fill = "lightpink", alpha = 0.15) +
  geom_text(aes(x =  0 + 10, y = 0.015), label = "Mundo ideal:\nsi todos\ntrabajaramos\nlo mismo", color = pal_5[3], family = "Barlow Condensed") +
  geom_text(aes(x =  tempo$prom[2] + 7, y = 0.0025), label = "Aqui estás tú", color = pal_5[5], family = "Barlow Condensed") +
  geom_density(aes(x = saldo)) +
  geom_vline(xintercept = 0, color = pal_5[3], size = 1, linetype = "dashed") +
  geom_vline(xintercept = tempo$prom[2], color = pal_5[5], size = 1, linetype = "dotted") +
  geom_text(aes(x = 40, y = 0.01), label = "A quienes\nse les debe", color = "blue", family = "Barlow Condensed") +
  geom_text(aes(x =  -45, y = 0.01), label = "Quienes\ndeben", color = "red", family = "Barlow Condensed") +
  labs(x = "Sobrecarga de cuidado",
       y = "",
       title = "Distribución de saldos de cuidado en méxico") +
  tema +
  theme(axis.text.y = element_blank())

save("final1")


#### Contrafactuales ####
# Sexo
# Sí tienen hijes o no
# Escolaridad
# Su contratan a una trabajadora del hogar


# Caso de ejemplo, respondió
# Mujer
# Con hijes
# Sin trabajadora doméstica
# Con licenciatura
tempo <- reduced %>%
  filter(tiene_hij == "Sí tiene", trab_dom == "Sí contrata", niv == "Licenciatura o más") %>%
  group_by(sexo) %>%
  summarise(prom = sum(hrs_cuidado*tot_cuidado_nac_percap)/sum(tot_cuidado_nac_percap),
            carac = "sexo") %>%
  ungroup() %>%
  distinct() %>%
  rename(valor = sexo) %>%
  rbind(
    reduced %>%
      filter(sexo == "mujer", trab_dom == "Sí contrata", niv == "Licenciatura o más") %>%
      group_by(tiene_hij) %>%
      summarise(prom = sum(hrs_cuidado*tot_cuidado_nac_percap)/sum(tot_cuidado_nac_percap),
                carac = "tiene_hij") %>%
      ungroup() %>%
      distinct() %>%
      rename(valor = tiene_hij)
  ) %>%
  rbind(
    reduced %>%
      filter(sexo == "mujer", tiene_hij == "Sí tiene", trab_dom == "Sí contrata") %>%
      group_by(niv) %>%
      summarise(prom = sum(hrs_cuidado*tot_cuidado_nac_percap)/sum(tot_cuidado_nac_percap),
                carac = "niv") %>%
      ungroup() %>%
      distinct() %>%
      rename(valor = niv) %>%
      filter(valor %in% c("Secundaria"))
  ) %>%
  rbind(
    reduced %>%
      filter(sexo == "mujer", tiene_hij == "Sí tiene", niv == "Licenciatura o más") %>%
      group_by(trab_dom) %>%
      summarise(
        prom = sum(hrs_cuidado * tot_cuidado_nac_percap) / sum(tot_cuidado_nac_percap),
        carac = "trab_dom"
      ) %>%
      ungroup() %>%
      distinct() %>%
      rename(valor = trab_dom)
  )

per <- tempo %>%
  filter(valor %in% c("mujer", "Sí contrata","Licenciatura o más", "Sí tiene")) %>%
  distinct(prom) %>%
  mutate(valor = "Tu realidad", carac = "usuario")

final <- rbind(per, tempo %>%
                 filter(!(valor %in% c("mujer", "Sí contrata","Licenciatura o más", "Sí tiene"))))

ggplot(final %>% mutate(tot = 1), aes(x = prom, y = tot, fill = valor, color = valor)) +
  geom_point(size = 9, alpha = 0.7, shape = 21, stroke = 2, position = position_dodge(width = 0.6)) +
  scale_color_manual(values = pal_5)+
  scale_fill_manual(values = pal_5)+
  labs(x = "Horas semanales de trabajo del hogar y de cuidado",
       title = "Compara tu carga de trabajo del hogar y de cuidado\ncon un mundo en donde en vez de....",
       caption = caption,
       y = "") +
  tema +
  theme(legend.title = element_blank(), axis.text.y = element_blank())

ggsave(paste0(here("data/descriptives/out/"),"final2.svg"), height = 12, width = 8)
save("final2")





