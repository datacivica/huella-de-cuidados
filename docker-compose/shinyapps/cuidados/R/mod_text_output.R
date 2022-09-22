
#' text_output UI Function
#'
#' @description This module selects and stores user name
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

mod_text_output_1_ui <- function(id){
  ns <- NS(id)
  textOutput(ns("text1"))
}

mod_text_output_2_ui <- function(id){
  ns <- NS(id)
  fluidRow(textOutput(ns("text2")))
}

mod_text_output_3_ui <- function(id){
  ns <- NS(id)
  fluidRow(textOutput(ns("text3")))
}

mod_text_output_3_mobile_1 <- function(id){
  ns <- NS(id)
  fluidRow(textOutput(ns("text3_mob_1")))
}

mod_text_output_3_mobile_2 <- function(id){
  ns <- NS(id)
  fluidRow(textOutput(ns("text3_mob_2")))
}

mod_text_output_3_mobile_3 <- function(id){
  ns <- NS(id)
  fluidRow(textOutput(ns("text3_mob_3")))
}

mod_text_output_4_ui <- function(id){
  ns <- NS(id)
  fluidRow(textOutput(ns("text4")))
}

mod_text_output_1_server <- function(id, user_values, hrs_output_values, comp_values){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    saldo <- reactive({ifelse(hrs_output_values()$saldo>0, "acreedor", "deudor")})
    output$text1 <- renderText(
        paste0(
          "Bueno, ",
          user_values$name(),
          ", si las horas de cuidado estuvieran distribuidas equitativamente en México, tú realizarías ",
          round((hrs_output_values()$tot_cuidado_nac_percap / 7), 1),
          " horas de trabajo doméstico y de cuidados diarios, sin embargo, según tu perfil sabemos que realizas ",
          round((hrs_output_values()$prom_cuidado_reduced / 7), 1),
          ":"
        )
      )

    output$text2 <- renderText(
      if(saldo() == "acreedor"){
        paste0("Si multiplicamos esto por el último año, tienes un saldo positivo, es decir la sociedad te debe ",
               round(abs(hrs_output_values()$saldo)*52,0),
               " horas de cuidado y de trabajo doméstico.")
      } else{
        paste0("Si multiplicamos esto por el último año, tienes un saldo negativo, es decir le debes a alguien ",
               round(abs(hrs_output_values()$saldo)*52,0),
               " horas de cuidado y de trabajo doméstico.")
      }
    )

    output$text3 <- output$text3_mob_1 <- output$text3_mob_2 <- output$text3_mob_3<- renderText(
      if(saldo() == "acreedor"){
        "Las horas que te deben del último año implican que dejaste de:"
      } else{
        "Las horas que debes del último año implican que alguien:"
      }
    )

    output$text4 <- renderText(
        paste0(
          ifelse(user_values$sex() == "mujer", "Las mujeres ", "Los hombres "),
          "como tú hacen aproximadamente ",
          round(hrs_output_values()$prom_cuidado_reduced,0),
          " horas semanales de trabajo doméstico y de cuidados. ",
         abs(round(comp_values() %>% filter(carac == "sexo") %>% pull(prom) - hrs_output_values()$prom_cuidado_reduced,0)),
          " horas ",
          ifelse(comp_values() %>% filter(carac == "sexo") %>% pull(prom) - hrs_output_values()$prom_cuidado_reduced < 0, "más", "menos"),
          " que ",
         ifelse(user_values$sex() == "mujer", "los hombres", "las mujeres"),
         " de tu misma escolaridad, pero ",
         abs(round(comp_values() %>% filter(carac == "tiene_hij") %>% pull(prom) - hrs_output_values()$prom_cuidado_reduced,0)),
         " horas ",
          ifelse(comp_values() %>% filter(carac == "tiene_hij") %>% pull(prom) - hrs_output_values()$prom_cuidado_reduced < 0, "más", "menos"),
          " que ",
          case_when(user_values$sex() == "mujer" & user_values$child() == "Sí tiene" ~ "las mujeres sin hijes",
                    user_values$sex() == "hombre" & user_values$child() == "Sí tiene" ~ "los hombres sin hijes",
                    user_values$sex() == "mujer" & user_values$child() == "No tiene" ~ "las madres",
                    user_values$sex() == "hombre" & user_values$child() == "No tiene" ~ "los padres"),
          " de tu misma escolaridad."
        )
    )
  })
}

ui <- fluidPage(
  mod_text_output_1_ui("hol")
)

server <- function(input, output, session) {
  mod_text_output_1_server("hol")
}

shinyApp(ui, server)
