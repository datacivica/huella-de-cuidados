

get_representative_emails <- function(mun_info) {
  all_senators_emails <- fread(
    "data-raw/senadores.csv",
    select = c("cve_ent", "cve_mun", "nombre_completo", "mail")
  )

  filtered_senators_emails <- all_senators_emails %>%
    filter(
      cve_ent == as.integer(mun_info$id_ent[1]) &
      cve_mun == as.integer(mun_info$id_mun[1]) &
      mail != "NA"
    )

  all_deputies_emails <- fread(
    "data-raw/diputados_federales.csv",
    select = c("cve_ent", "cve_mun", "nombre_completo", "mail")
  )

  filtered_deputies_emails <- all_deputies_emails %>%
    filter(
      cve_ent == as.integer(mun_info$id_ent[1]) &
      cve_mun == as.integer(mun_info$id_mun[1]) &
      mail != "NA"
    )
  # Join all emails and remove duplicates
  all_emails_by_mun <- bind_rows(filtered_senators_emails, filtered_deputies_emails) %>%
    select("mail") %>%
    distinct()

  all_emails_by_mun$mail <- as.character(all_emails_by_mun$mail)
  # Return collapsed text of all_emails_by_mun dataframe
  return(paste(all_emails_by_mun$mail, collapse = ","))
}

mod_email_details_server <- function(id, user_values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$demand_email <- renderUI({
      tags$a(
          href = paste0(
            "mailto:",
            # Email recievers
            get_representative_emails(user_values$mun_info()),
            # Email Subject
            "?subject=¡Exigimos%20Sistema%20Nacional%20de%20Cuidados!:%20Tu%20huella%20de%20cuidados&",
            # Email Body
            "body=EstimadX%20Representante.%0D%0A%0D%0A",
            "Mi%20nombre%20es%20",
            user_values$name(),
            "%20y%20usted%20es%20mi%20representante%20legislativo.%0D%0A",
            "Le%20escribo%20este%20correo%20con%20el%20objetivo%20de%20expresarle%20mi%20preocupación%20por%20la%20falta%20de%20servicios%20de%20cuidado%20en%20México.%0D%0A",
            "En%20nuestro%20pa%C3%ADs%2C%20existen%2012%20millones%20de%20ni%C3%B1os%20menores%20de%20cinco%20a%C3%B1os%2C%209%20millones%20de%20personas%20adultas%20mayores%20y%20m%C3%A1s%20de%20un%20mill%C3%B3n%20de%20personas%20con%20discapacidad.%20",
            "Todas%20estas%20personas%20requieren%20trabajo%20de%20cuidados%20por%20varias%20horas%20a%20la%20semana.%20Estas%20horas%20de%20cuidado%20han%20reca%C3%ADdo%20tradicionalmente%20en%20un%20solo%20grupo%20de%20la%20poblaci%C3%B3n%3B%20las%20mujeres.%20",
            "En%20M%C3%A9xico%2C%20las%20mujeres%20realizan%20este%20trabajo%20sin%20ninguna%20remuneraci%C3%B3n%20econ%C3%B3mica%20ni%20prestaci%C3%B3n%20laboral%2C%20lo%20que%20contribuye%20a%20su%20situaci%C3%B3n%20de%20vulnerabilidad.%20",
            "Adem%C3%A1s%2C%20estas%20no%20son%20todas%20las%20personas%20requieren%20cuidados%2C%20todos%20hemos%20requerido%20y%20seguiremos%20requiriendo%20de%20cuidados%20en%20varios%20momentos%20de%20nuestras%20vidas.%20",
            "Nadie%20puede%20llegar%20a%20ejercer%20una%20diputaci%C3%B3n%20o%20senadur%C3%ADa%20si%20no%20le%20cuidaron%20durante%20la%20infancia%20primero.%20",
            "Es%20por%20eso%2C%20que%20en%20nombre%20de%20la%20injusticia%20hist%C3%B3rica%20que%20ha%20representado%20esta%20situaci%C3%B3n%20para%20las%20mujeres%2C%20y%20especialmente%20para%20las%20madres%2C%20quiero%20solicitarle%20que%2C%20como%20mi%20representante%2C%20impulse%20los%20cambios%20legislativos%20necesarios%20para%20la%20constituci%C3%B3n%20de%20un%20Sistema%20Nacional%20de%20Cuidados.%0D%0A",
            "Los%20movimientos%20%23YoCuidoM%C3%A9xico%20y%20la%20%23RedNacionalDeCuidados%20han%20identificado%20algunos%20de%20estos%20cambios%3A%20%20%0A",
            # Listed email points start here
            "%20%20%20%201.%20Aprobar%20la%20iniciativa%20legislativa%20que%20busca%20reformar%20el%20cuarto%20art%C3%ADculo%20de%20la%20Constituci%C3%B3n%20para%20reconocer%20el%20derecho%20al%20cuidado%20digno%20y%20el%20derecho%20de%20las%20mujeres%20al%20tiempo%20propio.%0D%0A",
            "%20%20%20%202.%20Ratificar%20el%20Convenio%20156%20sobre%20las%20y%20los%20trabajadores%20con%20responsabilidades%20familiares%20de%20la%20Organizaci%C3%B3n%20Internacional%20del%20Trabajo%20(OIT)%20para%20que%20hombres%20y%20mujeres%20con%20responsabilidades%20de%20cuidado%20puedan%20integrarse%20al%20mercado%20laboral.%0A",
            "%20%20%20%203.%20Legislar%20licencias%20de%20cuidado%20iguales%2C%20intransferibles%20y%20remuneradas%20para%20todas%20las%20personas%20trabajadoras.%0A",
            "%20%20%20%204.%20Impulsar%20una%20reforma%20fiscal%20progresiva%20que%20permita%20sustentar%20el%20Sistema%20Nacional%20de%20Cuidados.%0A",
            "%0D%0ATodas%20las%20personas%20tenemos%20huella%20de%20cuidados%2C%20para%20conocer%20la%20suya%20visite%20http%3A%2F%2Fanimalpolitico.com%2Ftuhuelladecuidados%0A%0A"
          ),
          "Escríbele a tu representante",
          class = "email-button",
          rel = "noopener noreferrer"
        )
    })
  })
}

mod_demand_email <- function(id) {
  ns <- NS(id)
    uiOutput(ns("demand_email"))
}

