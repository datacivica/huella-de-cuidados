# require(shinycustomloader)
require(data.table)
require(stringr)
#' nombreSelect UI Function
#'
#' @description This module selects and stores user name
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'


mod_nombreSelect_ui <- function(id){
  ns <- NS(id)
  textInput(ns("nom"), "¿Cuál es tu nombre?", "")

}

#' sexoSelect UI Function
#'
#' @description This module selects and stores user name
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

mod_sexoSelect_ui <- function(id){
  ns <- NS(id)
  radioButtons(
    ns("sex"),
    "¿Qué sexo* tienes?",
    choiceNames = c("Mujer", "Hombre"),
    choiceValues = c("mujer","hombre")
  )
}


#' parejaSelect UI Function
#'
#' @description This module selects and stores user name
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

mod_parejaSelect_ui <- function(id){
  ns <- NS(id)
  radioButtons(
    ns("parej"),
    "Si tienes pareja, ¿vives con ella?",
    choiceNames = c("Sí", "No", "No tengo pareja"),
    choiceValues = c("Sí tiene","No tiene", "No tiene")
  )
}

#' lugarSelect UI Function
#'
#' @description This module selects and stores user name
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

mod_lugarSelect_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    selectInput(inputId =  ns("ents"),
                label = "¿Dónde vives?",
                choices = str_sort(c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
                            "Coahuila",
                            "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
                            "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de México",
                            "Michoacán",
                            "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                            "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco",
                            "Tamaulipas", "Tlaxcala",
                            "Veracruz",
                            "Yucatán", "Zacatecas")), selectize = FALSE),
    withLoader(uiOutput(ns("muns")), type = "html", loader = "loader4")
  )
}

#' escolaridadSelect UI Function
#'
#' @description This module selects and stores user name
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

mod_escolaridadSelect_ui <- function(id){
  ns <- NS(id)
  selectInput(ns("esc"),
              "¿Hasta qué grado estudiaste?",
              c("Primaria o menos","Secundaria","Preparatoria","Licenciatura o más"),
              selectize = F)
}
#' hijesSelect UI Function
#'
#' @description This module selects and stores user name
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

mod_hijesSelect_ui <- function(id){
  ns <- NS(id)
  radioButtons(
    ns("hij"),
    "Si tienes hijes, ¿vives con elles?",
    choiceNames = c("Sí", "No", "No tengo hijes"),
    choiceValues = c("Sí tiene","No tiene", "No tiene")
  )
}
#' trabajadoraSelect UI Function
#'
#' @description This module selects and stores user name
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

mod_trabajadoraSelect_ui <- function(id){
  ns <- NS(id)
  radioButtons(
    ns("trab"),
    "¿En tu hogar contratan a trabajadoras del hogar y/o enfermeras?",
    choiceNames = c("Sí", "No"),
    choiceValues = c("Sí contrata","No contrata")
  )
}


#' values Select Server Functions
#'
#' @noRd

mod_valuesSelect_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    temp_mun <- eventReactive(input$ents,{
      fread("data-raw/geo-catalog.csv") %>%
        filter(name_ent == input$ents) %>%
        select(-ends_with("short")) %>%
        mutate(id_ent = str_pad(id_ent, width = 2, side = "left", pad = "0"),
               id_mun = str_pad(id_mun, width = 3, side = "left", pad = "0"))
    })

    output$muns <- renderUI({
      selectInput(ns('muni'), NULL, str_sort(unique(temp_mun()$name_mun)), selectize = FALSE)
    })

    user_values <- list(
      name = reactive({ input$nom }),
      sex = reactive({ input$sex }),
      partner = reactive({ input$parej }),
      region = reactive({
        case_when(
          input$ents %in% c(
            "Aguascalientes", "Baja California", "Baja California Sur", "Chihuahua","Durango", "Sinaloa", "Sonora", "Zacatecas"
          ) ~ "Aridoamérica Occidental",
          input$ents %in% c("Coahuila", "Nuevo León", "San Luis Potosí", "Tamaulipas") ~ "Aridoamérica Oriental",
          input$ents %in% c("Guerrero", "Oaxaca", "Puebla", "Tlaxcala", "Veracruz") ~ "Mesoamérica",
          input$ents %in% c("Ciudad de México", "Hidalgo", "Estado de México", "Morelos") ~ "Mesoamérica Central",
          input$ents %in% c(
            "Colima", "Guanajuato", "Jalisco", "Michoacán", "Nayarit", "Querétaro"
          ) ~ "Mesoamérica Occidental",
          input$ents %in% c("Campeche", "Chiapas", "Quintana Roo", "Tabasco", "Yucatán") ~ "Zona Maya"
        )
      }),
      state = reactive({
        input$ents
                  }),
      municipal = reactive({
        input$muni
        }),
      mun_info = reactive({
        temp_mun() %>% filter(name_mun ==  input$muni)
      }),
      school = reactive({ input$esc }),
      child = reactive({ input$hij }),
      worker = reactive({ input$trab })
    )

    return(user_values)
  })
}

mod_prueba_ui <- function(id){
  ns <- NS(id)
  fluidRow(verbatimTextOutput(ns("val")))

}

mod_prueba_server <- function(id, input_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$val <- renderPrint(input_data$semanas_imss)


  })
}

ui <- fluidPage(
  mod_prueba_ui("hol")
)

server <- function(input, output, session) {
  mod_prueba_server("hol", enut_reduced)
}

shinyApp(ui, server)



ui <- fluidPage(
  mod_lugarSelect_ui("hol")
)

server <- function(input, output, session) {
  mod_lugarSelect_server("hol")
}

#shinyApp(ui, server)



