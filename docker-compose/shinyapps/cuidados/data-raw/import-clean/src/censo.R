#
# Author: Georgina Jimenez
# Maintainer(s): OE, AF, GR
# License: (c) Data Cívica 2022, GPL v2 or newer
# -----------------------------------------------------------
# premio-gabo-cuidados/data/import-clean/src/censo.R

####Import censo ####
if(!require(pacman))install.packages("pacman")
p_load(tidyverse, here, janitor, googledrive)

drive_dirs <- drive_ls(as_id("1HfBJl47CU3WDAKS7J3FLV3V2LCec9pel"))
tables <- paste0(paste0("Personas", c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12",
                                      "13", "14", "15", "16", "17", "18", "19", "20", "21", "22",
                                      "23", "24", "25", "26", "27", "28", "29", "30", "31", "32")), ".csv") 
tmp <- paste(tempdir(), tables, sep = "/")

walk2(drive_dirs$id[order(match(drive_dirs$name, tables))],
      tmp,
      ~ drive_download(as_id(.x), path = .y, overwrite = TRUE))

censo_all <- data.frame()
for (i in 1:length(tmp)){
  tempo <- read.csv(tmp[i])%>% 
    clean_names() %>% 
    mutate(ent = formatC(as.integer(ent), width = 2, flag = 0, format = "d"),
           mun = formatC(as.integer(mun), width = 3, flag = 0, format = "d"),
           inegi = paste0(ent, mun))%>%
    select(inegi, ent:factor, numper:edad, sersalud, dhsersal1, dhsersal2,
           dis_ver:dis_mental, cau_ver:cau_mental, nivacad,
           escolari)
  censo_all <- bind_rows(censo_all, tempo)
  rm(tempo)
}


#### Limpiar censo ####
disc <- function(x){
  x <- ifelse(x==4 | x==5, 1, 0)
}

censo_all <- mutate(censo_all, toodler=ifelse(edad<6, 1, 0),
                               nine=ifelse(edad>5 & edad<16, 1, 0),
                               adulte_mayor=ifelse(edad>65, 1, 0))%>%
             mutate_at(vars(dis_caminar, dis_banarse, dis_mental, dis_hablar), disc)%>%
             mutate(discapacidad=dis_caminar+dis_banarse+dis_hablar+dis_mental,
                    discapacidad=ifelse(discapacidad>0, 1, 0))%>%
             group_by(inegi)%>%
             summarize(toodler=sum(toodler*factor, na.rm=T),
                       nine=sum(nine*factor, na.rm=T),
                       adulte_mayor=sum(adulte_mayor*factor, na.rm=T),
                       discapacidad=sum(discapacidad*factor, na.rm=T),
                       poblacion=sum(factor, na.rm=T))%>%
             ungroup()%>%
             mutate(tasa_toodler=toodler/poblacion*100000,
                    tasa_nine=nine/poblacion*100000,
                    tasa_adulte_mayor=adulte_mayor/poblacion*100000,
                    tasa_discapacidad=discapacidad/poblacion*100000)

saveRDS(censo_all, here("data/import-clean/out/poblacion-necesita-cuidados.rds"))




         
                   