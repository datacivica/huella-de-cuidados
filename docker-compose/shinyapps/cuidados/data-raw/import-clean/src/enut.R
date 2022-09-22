require(spatstat)
#
# Author: Alicia Franco
# Maintainer(s): OE, AF, GR
# License: (c) Data Cívica 2022, GPL v2 or newer
# -----------------------------------------------------------
# premio-gabo-cuidados/data/import-clean/src/enut.R
#
#### Paquetería ####
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, data.table, janitor)

#### Acceso Google Drive ####
options(gargle_oob_default = TRUE)

#### Archivero ####
paths <- list(
  inp = drive_ls(as_id("1iy8MBYHK8BJPvQa-h7-AF5PVV9qF4Ua2")),
  out = here("data-raw/import-clean/out/")
)

#### Functions ####
read_clean <- function(dirs){
  df <- fread(dirs) %>%
    clean_names()

  if(str_detect(dirs,"tmodulo")){
    df <- df %>%
      select(
        any_of(var_to_select),
        #módulo
        "ent", "edad_v", "niv","p4_4", #pareja
        ## beneficios
        "p5_6_1","p5_6_4","p5_6_5",
        ##cuidados
        starts_with("p6_11"), # cuidados básicos que le dedicó a otras personas
        ### a integrantes de 0a5 ¡
        starts_with("p6_12"),
        ### a integrantes de 0a14
        starts_with("p6_13"),
        ### a integrantes de 15a59
        starts_with("p6_14"),
        ### integrantes 60+
        starts_with("p6_15"),
        ##labores domésticas para consumo del hogar
        starts_with("p6_3"), #cuidar animales
        starts_with("p6_4"), #desgranar
        starts_with("p6_5"), # barrer
        starts_with("p6_6"), # arreglar ropa, manteles cortinas o sábanas
        starts_with("p6_7"), #reparar medios de transporte del hogar
        starts_with("p6_8"), #comprar refacciones
        starts_with("p6_9"), #pagar sercicios
        starts_with("p6_10")) #llevar a arreglar ropa

  }
  if(str_detect(dirs,"thogar")){
    df <- df %>%
      select(any_of(var_to_select),
             "p2_4_8","p2_6_1", "p2_6_2", "p2_6_3",
             "p3_11", "p2_8_1", "p2_8_2", "p2_8_3") %>%
      mutate(id_viv = paste(str_pad(upm, width = 7, pad = "0", side = "left"),
                            str_pad(viv_sel, width = 2, pad = "0", side = "left"),
                            hogar,
                            sep = "."))
  }
  if(str_detect(dirs,"sdem")){
    df <- df %>%
      select(any_of(var_to_select),
             "paren", "sexo", "edad",
             "p3_7", "p3_8", "p3_9", "p3_10", "p3_11b")
  }
  if(str_detect(dirs,"sdem|modulo")){
    df <- df %>%
      mutate(id_per = paste(str_pad(upm, width = 7, pad = "0", side = "left"),
                            str_pad(viv_sel, width = 2, pad = "0", side = "left"),
                            hogar,
                            str_pad(n_ren, width = 2, pad = "0", side = "left"),
                            sep = "."
      ))
  }
  df<- df

}

#### Import ####
tables <- paste0("conjunto_de_datos_t", c("hogar", "sdem", "modulo"), "_enut_tradicional_2019.csv")
var_to_select <- c("fac_per", "upm", "viv_sel", "hogar", "n_ren", "ent")

#### Downloading ####
to_imp <- which(str_detect(paths$inp$name, tables))
tmp <- paste(tempdir(), paths$inp$name[to_imp], sep = "/")

walk2(paths$inp$id[to_imp], tmp, ~drive_download(as_id(.x), path = .y, overwrite = TRUE))

#### Build ####
enut <- map(tmp, ~read_clean(dirs = .x)) %>%
  reduce(left_join)

#### Categorize ####
clean <- enut %>%
  mutate(
    sexo = ifelse(sexo == 2, "mujer", "hombre"),
    niv = case_when(niv %in% 0:1 ~ "Primaria o menos",
                    niv  == 2 ~ "Primaria o menos",
                    niv == 3 ~ "Secundaria",
                    niv %in% 4:7 ~ "Preparatoria",
                    niv > 7 ~ "Licenciatura o más"),
    niv = factor(niv, levels = c("Primaria o menos",
                                 "Secundaria",
                                 "Preparatoria",
                                 "Licenciatura o más")),
    ent = str_pad(ent, width = 2, side = "left", pad = "0"),
    edad = ifelse(edad != edad_v, as.integer(edad_v),as.integer(edad)),
    edad = ifelse(edad %in% 0:97, edad, NA),
    licencia = as.integer(p5_6_1 == 1),
    estancia = as.integer(p5_6_4 == 1),
    pres_cuidado = as.integer(p5_6_5 == 1),
    trab_dom = as.integer(p2_6_1 == 1 | p2_6_2 == 1 | p2_6_3 == 1),
    per_c_disc = as.integer(p3_11 == 1)
    ) %>%
  group_by(id_viv) %>%
  mutate(
    miem = paste(paren, collapse = "."),
    tiene_hij = as.integer(paren == 1 & str_detect(miem, ".3|.4")|
                             paren == 2 & str_detect(miem, ".3|.4")|
                             paren == 6),
    tiene_hij = case_when(tiene_hij == 1 & sexo == "mujer" ~ "madre",
                          tiene_hij == 1 & sexo == "hombre" ~ "padre",
                          tiene_hij == 0 & sexo == "mujer" ~ "mujer sin hijes",
                          tiene_hij == 0 & sexo == "hombre" ~ "hombre sin hijes",
                          T ~ NA_character_),
    pareja = as.integer(str_detect(miem,".2") & paren %in% as.character(1:2))
  ) %>%
  ungroup() %>%
  mutate(across(
    c(p2_8_1, p2_8_2, p6_11_01:p6_10a_7_4),
    ~ replace_na(.x, 0)
  ))

### Horas de trabajo de lxs encuestadxs
hrs_enc <- clean %>%
  select(starts_with("id"),starts_with("p6")) %>%
  select(starts_with("id"),contains("a")) %>%
  mutate(across(ends_with("_2"), ~.x/60),
         across(ends_with("_4"), ~.x/60))

#### Build ####
clean_hr <- clean %>%
  select(starts_with("id"), niv,ent,sexo:pareja, fac_per, -starts_with("p3_")) %>%
  left_join(., ### Horas de trabajo de trabajadoras dom y enfermeras
              clean %>%
                select(starts_with("id"), starts_with("p2_8")) %>%
                mutate(hrs_trabdom_enf = p2_8_1 + p2_8_2 + p2_8_3)  %>%
                select(starts_with("id"), starts_with("hrs"))) %>%
  left_join(., hrs_enc %>%
                mutate(across(starts_with("p6_"), as.numeric),
                       hrs_cuidado = rowSums(select(.,starts_with("p6_")))) %>%
                select(starts_with("id"), hrs_cuidado)) %>%
  filter(hrs_cuidado <= 168, !is.na(trab_dom), !is.na(tiene_hij)) %>%
  # REGIONES
  mutate(region = case_when(ent %in% c("01", "02", "03", "08", "10", "25", "26", "32") ~ "Aridoamérica Occidental",
                            ent %in% c("05", "19", "24", "28") ~ "Aridoamérica Oriental",
                            ent %in% c("12", "20", "21", "29", "30") ~ "Mesoamérica",
                            ent %in% c("09", "13", "15", "17") ~ "Mesoamérica Central",
                            ent %in% c("06", "11", "14", "16", "18", "22") ~ "Mesoamérica Occidental",
                            ent %in% c("04", "07", "23", "27", "31") ~ "Zona Maya"),
         trab_dom = factor(ifelse(trab_dom == 1, "Sí contrata", "No contrata")),
         tiene_hij = factor(ifelse(str_detect(tiene_hij,"sin"), "No tiene", "Sí tiene"),
                            levels = c("No tiene",
                                       "Sí tiene")),
         pareja = factor(ifelse(pareja == 1, "Sí tiene", "No tiene")),
         estancia = factor(ifelse(estancia == 1, "Sí tiene", "No tiene")),
         tot_cuidado_nac_percap = weighted.median(hrs_cuidado,fac_per,na.rm = T),
         saldo = hrs_cuidado-tot_cuidado_nac_percap) %>%
  select(region, sexo, niv, trab_dom, tiene_hij, pareja, fac_per, hrs_cuidado, tot_cuidado_nac_percap, saldo)

#### Combinations ####

all <- expand.grid(
  region = unique(clean_hr$region),
  niv = unique(clean_hr$niv),
  sexo = unique(clean_hr$sexo),
  trab_dom = unique(clean_hr$trab_dom),
  tiene_hij = unique(clean_hr$tiene_hij),
  pareja = unique(clean_hr$pareja)
) %>%
  left_join(.,
            clean_hr %>%
              group_by(region, niv, sexo, trab_dom, tiene_hij, pareja) %>%
              summarise(prom = mean(hrs_cuidado, na.rm = T)) %>%
              ungroup()) %>%
  filter(is.na(prom)) %>%
  select(-prom) %>%
  # Para las que no están, la nacional
  left_join(., clean_hr %>%
              group_by(niv, sexo, trab_dom, tiene_hij, pareja, tot_cuidado_nac_percap) %>%
              summarise(
                        hrs_cuidado = weighted.median(hrs_cuidado,fac_per, na.rm = T),
                        fac_per = sum(fac_per),
                        saldo = hrs_cuidado-tot_cuidado_nac_percap) %>%
              ungroup() %>%
              distinct()) %>%
  mutate(region = "Nacional")

#### save ####
saveRDS(rbind(clean_hr, all), paste0(paths$out, "enut.rds"))

# done

