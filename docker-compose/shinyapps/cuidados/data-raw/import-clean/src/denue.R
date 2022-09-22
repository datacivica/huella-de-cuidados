#
# Author: Gina Jiménez
# Maintainer(s): Gina Jiménez
#Data Cívica
# ------------------------------------------------------------------------------------
# 
#Import servicios de cuidados

####Import denue ####
if(!require(pacman))install.packages("pacman")
require(pacman)
p_load(tidyverse, here, janitor, googledrive)

temp <- tempfile(fileext = ".csv")
dl <- drive_download(as_id("1lg0VD7_KV6TkhTOwrPDFqpcbXoqHxUdY"), 
                          path = temp, overwrite = TRUE)
#### Funciones ####
limpiar <- function(x){
  x = gsub("\xfa","ú", x)
  x = gsub("\xed","í", x)
  x = gsub("\xe1","á", x)
  x = gsub("\xe9","é", x)
  x = gsub("\xf3","ó", x)
  x = gsub("\\xf1","ñ", x)
  x = gsub("\xfc\xbe\x99\x86\x94\xbc","í", x)
}

limpiar_edos <- function(x){
  x = gsub("Coahuila de Zaragoza","Coahuila", x)
  x = gsub("Veracruz de Ignacio de la Llave","Veracruz", x)
  x = gsub("Michoacán de Ocampo","Michoacán", x)
}


#### Bajar y limpiar DENUE ####
data <- read.csv(temp)%>%
        clean_names()%>%
        mutate(codigo=substr(codigo_act, 0, 4),
               guarderia=ifelse(codigo==6244, 1,0),
               asilo=ifelse(codigo==6233, 1,0),
               discapacidad=ifelse(codigo==6232, 1,0),
               nombre_act=limpiar(nombre_act),
               cve_ent=formatC(as.integer(cve_ent), width = 2, flag = 0, format = "d"),
               cve_mun=formatC(as.integer(cve_mun), width = 3, flag = 0, format = "d"),
               inegi=paste0(cve_ent, cve_mun))%>%
        filter(codigo==6244 | codigo==6233 | codigo==6232)%>%
        filter(codigo_act!=623222 | codigo_act!=623221)%>%
        select(id, inegi, codigo_act:per_ocu, cve_ent:municipio,
               ageb, manzana, latitud, longitud, guarderia, asilo,
               discapacidad)%>%
        mutate_at(vars(nombre_act, entidad, 
                       municipio, per_ocu), funs(limpiar))%>%
        mutate(entidad=limpiar_edos(entidad), 
               tipo=as.numeric(str_detect(nombre_act, "sector privado")),
               tipo=factor(tipo, levels=c(0,1), labels=c("Público", "Privado")),
               cuidar_toodlers=case_when(guarderia==1 & per_ocu=="0 a 5 personas" ~  44,
                                         guarderia==1 & per_ocu=="6 a 10 personas" ~ 84,
                                         guarderia==1 & per_ocu=="11 a 30 personas" ~ 244,
                                         guarderia==1 & per_ocu=="31 a 50 personas" ~ 404,
                                         guarderia==1 & per_ocu=="51 a 100 personas" ~ 804,
                                         guarderia==1 & per_ocu=="101 a 250 personas" ~ 2004,
                                         guarderia==1 & per_ocu=="251 y más personas" ~ 2004,
                                         T ~ 0),
               cuidar_viejitos=case_when(asilo==1 & per_ocu=="0 a 5 personas" ~  88,
                                        asilo==1 & per_ocu=="6 a 10 personas" ~ 168,
                                        asilo==1 & per_ocu=="11 a 30 personas" ~ 488,
                                        asilo==1 & per_ocu=="31 a 50 personas" ~ 808,
                                        asilo==1 & per_ocu=="51 a 100 personas" ~ 1608,
                                        asilo==1 & per_ocu=="101 a 250 personas" ~ 4008,
                                        asilo==1 & per_ocu=="251 y más personas" ~ 4008, 
                                        T ~ 0),
               cuidar_discapacidad=case_when(discapacidad==1 & per_ocu=="0 a 5 personas" ~  44,
                                             discapacidad==1 & per_ocu=="6 a 10 personas" ~ 84,
                                             discapacidad==1 & per_ocu=="11 a 30 personas" ~ 244,
                                             discapacidad==1 & per_ocu=="31 a 50 personas" ~ 404,
                                             discapacidad==1 & per_ocu=="51 a 100 personas" ~ 804,
                                             discapacidad==1 & per_ocu=="101 a 250 personas" ~ 2004,
                                             discapacidad==1 & per_ocu=="251 y más personas" ~ 2004,
                                             T ~ 0))
saveRDS(data, here("data/import-clean/out/establecimientos-cuidados.rds"))

        

