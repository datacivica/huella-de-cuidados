
#' distribucion UI Function
#'
#' @description This module selects and stores user name
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#'
#'
#HTML("I want a line break here <br/> since the label is too long"))),

mod_contra_select_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(2, actionButton(inputId = ns("graf_sexo"), label = "Ser mujer...")),
    column(4, actionButton(inputId = ns("graf_esc"), label = "Estudiar hasta la<br/>primaria...")),
    column(2, actionButton(inputId = ns("graf_hij"), label = "Ser madre...")),
    column(4, actionButton(inputId = ns("graf_trab"), label = HTML("No contratar a una<br/>trabajadora del hogar"))))
}

mod_contra_select_server <- function(id, user_values){
  moduleServer(id, function(input, output, session){
    observeEvent(user_values$sex(),
                 updateActionButton(session,
                                    "graf_sexo", label = paste0("Ser ", user_values$sex(), "...")))

    observeEvent(user_values$school(),
                 updateActionButton(session,
                                    "graf_esc",
                                    label = HTML(paste0("Estudiar hasta la<br/>", str_to_lower(user_values$school()), "..."))))

    observeEvent(user_values$worker(),
                 updateActionButton(session, "graf_trab", label = HTML(paste0(user_values$worker(),"r a una<br/>trabajadora del hogar"))))

    observeEvent({
      user_values$child()
      user_values$sex()
      },
                 updateActionButton(session, "graf_hij", label = case_when(
                   user_values$child() == "No tiene" ~ "No tener hijes...",
                   user_values$sex() == "mujer" & user_values$child() == "Sí tiene"  ~ "Ser madre...",
                   user_values$sex() == "hombre" & user_values$child() == "Sí tiene" ~ "Ser padre..."
                 )))

    filter_var <- reactiveValues(selected = "sexo")

    observeEvent(input$graf_esc, {
      filter_var$selected <- "niv"
    })
    observeEvent(input$graf_hij, {
      filter_var$selected <- "tiene_hij"
    })
    observeEvent(input$graf_sexo, {
      filter_var$selected <- "sexo"
    })
    observeEvent(input$graf_trab, {
      filter_var$selected <- "trab_dom"
    })

    return(filter_var)
  })
}


