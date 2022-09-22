#
# Author: Georgina Jimenez
# Maintainer(s): OE, AF, GR
# License: (c) Data Cívica 2022, GPL v2 or newer
# -----------------------------------------------------------
# premio-gabo-cuidados/data/descriptives/src/oferta-demanda.R

#### Paquetería ####
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, ggplot2, ggalt)
options(scipen=999)

#### Tema y funciones ####
source(here("data-raw/descriptives/src/theme.R"))

save <- function(name){
  ggsave(paste0(here("data/descriptives/out/"), name, ".png"), width = 12, height = 8)
  ggsave(paste0(here("data/descriptives/out/"), name, ".svg"), width = 12, height = 8)
}

#### Abrir ####
gente <- readRDS(here("data-raw/import-clean/out/poblacion-necesita-cuidados.rds"))
lugares <- readRDS(here("data-raw/import-clean/out/establecimientos-cuidados.rds"))

#Promedio y mediana personas por guardería
lugares%>%
filter(guarderia==1)%>%
group_by(guarderia)%>%
summarize(cuidar_toodlers1=mean(cuidar_toodlers, na.rm=T),
          cuidar_toodlers2=median(cuidar_toodlers, na.rm=T))
#Median 84 mean 130

lugares%>%
filter(discapacidad==1)%>%
group_by(discapacidad)%>%
summarize(cuidar_discapacidad1=mean(cuidar_discapacidad, na.rm=T),
          cuidar_discapacidad2=median(cuidar_discapacidad, na.rm=T))

#Median 44, mean 100

lugares%>%
filter(asilo==1)%>%
group_by(asilo)%>%
summarize(cuidar_viejitos1=mean(cuidar_viejitos, na.rm=T),
          cuidar_viejitos2=median(cuidar_viejitos, na.rm=T))

#Median 168, mean 324

#Procesar
lugares_temp <- group_by(lugares, inegi, cve_ent, entidad, cve_mun, municipio)%>%
           summarize(est_guarderia=sum(guarderia, na.rm=T),
                     est_asilo=sum(asilo, na.rm=T),
                     est_discapacidad=sum(discapacidad, na.rm=T),
                     cuidar_toodlers=sum(cuidar_toodlers, na.rm=T),
                     cuidar_viejitos=sum(cuidar_viejitos, na.rm=T),
                     cuidar_discapacidad=sum(cuidar_discapacidad, na.rm=T)) %>%
  ungroup()


data <- left_join(lugares_temp, gente)
data$est_guarderia[is.na(data$est_guarderia)] <-0
data$est_asilo[is.na(data$est_asilo)] <-0
data$est_discapacidad[is.na(data$est_discapacidad)] <-0
data$cuidar_toodlers[is.na(data$cuidar_toodlers)] <-0
data$cuidar_viejitos[is.na(data$cuidar_viejitos)] <-0
data$cuidar_discapacidad[is.na(data$cuidar_discapacidad)] <-0

to_save <- data %>%
  ungroup() %>%
  select(inegi,starts_with("est"), toodler, adulte_mayor, discapacidad) %>%
  rename(cuidar_toodler = toodler, cuidar_viejitos = adulte_mayor, cuidar_discapacidad = discapacidad) %>%
  right_join(
    read_csv("data-raw/geo-catalog.csv") %>%
      ungroup() %>%
      select(-ends_with("short")) %>%
      mutate(inegi = paste0(str_pad(id_ent, width = 2, side = "left", pad = "0"),
                            str_pad(id_mun, width = 3, side = "left", pad = "0")),
             name_ent = case_when(name_ent == "México" ~ "Estado de México",
                                  str_detect(name_ent, "Coahuila") ~ "Coahuila",
                                  str_detect(name_ent, "Veracruz") ~ "Veracruz",
                                  str_detect(name_ent, "Michoacán") ~ "Michoacán",
                                  T ~ name_ent)) %>%
      select(inegi, starts_with("name"))
    ) %>%
  mutate_at(vars(starts_with("est"),
                 starts_with("cuidar")),
            ~replace_na(., 0))

data.frame(x = seq(min(to_save$cuidar_toodler), max(to_save$cuidar_toodler), 10000)) %>%
  mutate(y =  log((1 / 130) * x + 1),
         z = log(x),
         var = "guarderias") %>%
  rbind(.,
        data.frame(x = seq(min(to_save$cuidar_viejitos), max(to_save$cuidar_viejitos), 10000)) %>%
          mutate(y =  log((1 / 324) * x + 1),
                 z = log(x+1),
                 var = "asilos")) %>%
  rbind(.,
        data.frame(x = seq(min(to_save$cuidar_discapacidad), max(log(to_save$cuidar_discapacidad))+2000000, 100000)) %>%
          mutate(y =  log((1 / 100) * x + 1),
                 z = log(x+1),
                 var = "estancias")) %>%
  write.csv(., "data-raw/descriptives/out/ideal_prop.csv")

to_save_ent <-  pivot_longer(to_save, est_guarderia:cuidar_discapacidad, names_to = "concepto", values_to = "valor") %>%
  mutate(prefix_concepto = str_extract(concepto, "[^_]+"),
         concepto = case_when(str_detect(concepto, "toodler|guarderia") ~ "guarderias",
                              str_detect(concepto, "asilo|viejitos") ~ "asilos",
                              str_detect(concepto, "discapacidad") ~ "estancias")) %>%
  pivot_wider(names_from = prefix_concepto, values_from = valor) %>%
  mutate(
    razon = case_when(concepto == "guarderias" ~ 130,
                      concepto == "asilos" ~ 324,
                      concepto == "estancias" ~ 100),
    faltan_est = round((cuidar/razon) - est, 0),
    ent = str_sub(inegi, 1, 2)
  )

estancias <- c("guarderias", "asilos", "estancias")

for(i in 1:length(estancias)){
  filter(ungroup(to_save_ent), concepto == estancias[i]) %>% slice_sample(., prop = 0.25) %>%
    write.csv(paste0("data-raw/descriptives/out/samples_estancias/", estancias[i],".csv"))
}


ent_code <- str_pad(1:32, width = 2, side = "left", pad = "0")

for(i in 1:length(ent_code)){
    ungroup(to_save_ent) %>%
      filter(ent == ent_code[i]) %>%
    write.csv(paste0("data-raw/descriptives/out/ent_o_d_graph/",ent_code[i],".rds"))
  }



walk2(ent_code, estancias, ~x_ent(entidad = .x, est = .y))

walk2(names_endireh, id_crime, ~filter_append(df = endireh, file_names = .x,crime_ids = .y, year_sql = year_sql))
readRDS("data-raw/descriptives/out/samples_estancias/guarderias.rds")



save <- to_save_ent %>% filter(name_ent == unique(to_save_ent$name_ent)[1])


#### Scatters ####
tempo <-
  data.frame(x = seq(min(data$toodler), max(data$toodler), 1)) %>%
  mutate(y =  log((1 / 130) * x + 1),
         z = log(x))

ggplot() +
  geom_ribbon(data = tempo,
              aes(x = z,
                  ymin = y,
                  ymax = Inf),
              alpha = 0.3, fill = pal_5[5])+
  geom_text(data = data.frame(label = "Municipios\ncon suficientes\nguarderías", x = 3, y = 2),
            aes(x = x, y = y, label = label),
            color = pal_5[5], fontface = "bold", family = "Barlow Condensed") +
  geom_ribbon(data = tempo,
              aes(x = z,
                  ymin = -Inf,
                  ymax = y),
              alpha = 0.3, fill = pal_5[2]) +
  geom_text(data = data.frame(label = "Municipios\nsin suficientes\nguarderías",
                              x = 11, y = 2),
            aes(x = x, y = y, label = label),
            color = pal_5[2], fontface = "bold", family = "Barlow Condensed") +
  geom_point(data = data, aes(x = log(toodlers), y =log(est_guarderia+1)),
             size = 3, alpha = 0.6, color=pal_5[1])+
  tema+
  geom_line(data = tempo, aes(x = z, y =y), linetype = "dashed") +
  labs(title="¿Hay suficientes guarderías en tu municipios?*",
       subtitle="Menores de seis años vs guarderías a nivel municipal",
       caption="Fuente: DENUE y CENSO 2020 \n *El cálculo se realizó tomando en cuenta les menores promedio que puede cuidar una guardería según DENUE, las Reglas de operación de Estancias Infantiles \n y el Marco Analítico y metodología para diagnosticar las brechas de cuidado en los municipios de México",
       x="log(menores de seis años + 1)",
       y="log(guarderias + 1)")
save("scatter-toodlers")

tempo <-
  data.frame(x = seq(min(data$adulte_mayor), max(data$adulte_mayor), 1)) %>%
  mutate(y =  log((1 / 324) * x + 1),
         z = log(x+1))

ggplot() +
  geom_ribbon(data = tempo,
              aes(x = z,
                  ymin = y,
                  ymax = Inf),
              alpha = 0.3, fill = pal_5[5])+
  geom_text(data = data.frame(label = "Municipios\ncon suficientes\nasilos", x = 3, y = 2),
            aes(x = x, y = y, label = label),
            color = pal_5[5], fontface = "bold", family = "Barlow Condensed") +
  geom_ribbon(data = tempo,
              aes(x = z,
                  ymin = -Inf,
                  ymax = y),
              alpha = 0.3, fill = pal_5[2]) +
  geom_text(data = data.frame(label = "Municipios\nsin suficientes\nasilos",
                              x = 10, y = 3.5),
            aes(x = x, y = y, label = label),
            color = pal_5[2], fontface = "bold", family = "Barlow Condensed") +
  geom_point(data = data, aes(x =log(adulte_mayor+1), y = log(asilo + 1)), size = 3, alpha = 0.6, color=pal_5[2])+
  geom_line(data = tempo, aes(x = z, y =y), linetype = "dashed") +
  tema+
  labs(title="¿Hay suficientes asilos en tu municipio?*",
       subtitle="Mayores de 65 años  vs asilos a nivel municipal",
       caption="Fuente: DENUE y CENSO 2020 \n *El cálculo se realizó tomando en cuenta les menores promedio que puede cuidar una guardería según DENUE, las Reglas de operación de Estancias Infantiles \n y el Marco Analítico y metodología para diagnosticar las brechas de cuidado en los municipios de México",
       x="log(mayores de 65 años + 1)",
       y="log(asilos + 1)")
save("scatter-viejitos")

tempo <-
  data.frame(x = seq(min(data$discapacidad), max(log(data$est_discapacidad)+50000, 1))) %>%
  mutate(y =  log((1 / 100) * x + 1),
         z = log(x+1))

ggplot() +
  geom_ribbon(data = tempo,
              aes(x = z,
                  ymin = y,
                  ymax = Inf),
              alpha = 0.3, fill = pal_5[5])+
  geom_text(data = data.frame(label = "Municipios\ncon suficientes\nestancias", x = 3, y = 2),
            aes(x = x, y = y, label = label),
             color = pal_5[5], fontface = "bold", family = "Barlow Condensed") +
  geom_ribbon(data = tempo,
              aes(x = z,
                  ymin = -Inf,
                  ymax = y),
              alpha = 0.3, fill = pal_5[2]) +
  geom_text(data = data.frame(label = "Municipios\nsin suficientes\nestancias",
                              x = 9.5, y = 3.5),
            aes(x = x, y = y, label = label),
              color = pal_5[2], fontface = "bold", family = "Barlow Condensed") +
  geom_point(data = data, aes(x =log(discapacidad+1), y =log(est_discapacidad+1)),
             size = 3, alpha = 0.6, color=pal_5[3])+
  geom_line(data = tempo, aes(x = z, y =y), linetype = "dashed") +
  tema+
  labs(title="¿Hay suficientes estancias para personas con discapacidad en tu municipio?*",
       subtitle="Personas con discapacidad** vs estancias para personas con discapacidad",
       caption="Fuente: DENUE y CENSO 2020 \n *El cálculo se realizó tomando en cuenta les menores promedio que puede cuidar una guardería según DENUE, las Reglas de operación de Estancias Infantiles \n y el Marco Analítico y metodología para diagnosticar las brechas de cuidado en los municipios de México \n **Se consideran sólo personas que no puedan hablar, caminar, bañarse por sí mismas o que tienen alguna discapacidad mental",
       x="log(personas con discapacidad + 1)",
       y="log(estancias + 1)")
save("scatter-discapacidad")

#### Barras ####
data <- mutate(data, dif_toodlers=toodler-cuidar_toodlers,
                     dif_viejitos=adulte_mayor-cuidar_viejitos,
                     per_toodlers=cuidar_toodlers/toodler*100,
                     per_viejitos=cuidar_viejitos/adulte_mayor*100)

data%>%
group_by(entidad)%>%
summarize(per_toodlers=mean(per_toodlers, na.rm=T))%>%
ungroup()%>%
filter(!is.na(entidad))%>%
ggplot(aes(x =reorder(entidad, per_toodlers), y=per_toodlers)) +
geom_bar(stat="identity", position="dodge",
           color=pal_5[1], fill=pal_5[1], size=1)+
tema+
labs(title="¿Para cuántos menores de 6 años alcanzan las guarderías existentes?",
     subtitle="Porcentaje de los menores en el estado que están cubiertos",
     y="", x="")+
theme(axis.text = element_text(angle=90,
                         hjust=1))
save("bar-toodlers")


data%>%
  group_by(entidad)%>%
  summarize(per_viejitos=mean(per_viejitos, na.rm=T))%>%
  ungroup()%>%
  filter(!is.na(entidad))%>%
  ggplot(aes(x =reorder(entidad, per_viejitos), y=per_viejitos)) +
  geom_bar(stat="identity", position="dodge",
           color=pal_5[1], fill=pal_5[1], size=1)+
  tema+
  labs(title="¿Para cuántos adultos mayores alcanzan los asilos existentes?",
       subtitle="Porcentaje de los adultos mayores en el estado que están cubiertos",
       y="", x="")+
  theme(axis.text = element_text(angle=90,
                                 hjust=1))
save("bar-adultes-mayores")


#### Líneas de diferencia ####
data%>%
group_by(cve_ent, entidad)%>%
summarize(toodler=sum(toodler, na.rm=T),
          cuidar_toodlers=sum(cuidar_toodlers, na.rm=T),
          guarderias=sum(guarderia, na.rm=T))%>%
ungroup()%>%
filter(!is.na(entidad))%>%
mutate(dif_toodlers=toodler-cuidar_toodlers,
       guarderias_faltantes=dif_toodlers/84,
       guarderias_ideales=guarderias+guarderias_faltantes)%>%
select(cve_ent, entidad, guarderias, guarderias_ideales)%>%
ggplot(aes(x=guarderias, xend=guarderias_ideales, y=reorder(entidad, guarderias))) +
geom_dumbbell(color=pal_5[1], size=2, colour_x = pal_5[2],
                colour_xend = pal_5[3]) +
tema+
labs(title="¿Cuántas guarderías le faltan a cada estado?",
     subtitle="",
     y="", x="")+
geom_text(color="black", size=4, hjust=1.5,
          aes(x=guarderias, y=reorder(entidad, guarderias),
              label=round(guarderias, 0), family="Barlow Condensed"))+
geom_text(color="black", size=4, hjust=-1,
          aes(x=guarderias_ideales, y=reorder(entidad, guarderias),
                label=round(guarderias_ideales, 0), family="Barlow Condensed"))
save("dumbell-guarderias")


data%>%
  group_by(cve_ent, entidad)%>%
  summarize(adulte_mayor=sum(adulte_mayor, na.rm=T),
            cuidar_viejitos=sum(cuidar_viejitos, na.rm=T),
            asilos=sum(asilo, na.rm=T))%>%
  ungroup()%>%
  filter(!is.na(entidad))%>%
  mutate(dif_viejitos=adulte_mayor-cuidar_viejitos,
         asilos_faltantes=dif_viejitos/276,
         asilos_ideales=asilos+asilos_faltantes)%>%
  select(cve_ent, entidad, asilos, asilos_ideales)%>%
  ggplot(aes(x=asilos, xend=asilos_ideales, y=reorder(entidad, asilos))) +
  geom_dumbbell(color=pal_5[1], size=2, colour_x = pal_5[2],
                colour_xend = pal_5[3]) +
  tema+
  labs(title="¿Cuántas asilos le faltan a cada estado?",
       subtitle="",
       y="", x="")+
  geom_text(color="black", size=4, hjust=1.5,
            aes(x=asilos, y=reorder(entidad, asilos),
                label=round(asilos, 0), family="Barlow Condensed"))+
  geom_text(color="black", size=4, hjust=-1,
            aes(x=asilos_ideales, y=reorder(entidad, asilos),
                label=round(asilos_ideales, 0), family="Barlow Condensed"))
save("dumbell-asilos")


