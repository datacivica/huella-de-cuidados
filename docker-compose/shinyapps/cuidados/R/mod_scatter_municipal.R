require(shinycustomloader) # no se por qué pero lo necesita
require(ggrepel)
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
mod_selectscatter_ui <- function(id){
  ns <- NS(id)
  selectInput(ns("select_scatter"),
              "",
              choices = c("guarderias", "asilos", "estancias"),width = "30%", selected = "estancias", selectize = FALSE)
}

mod_selectscatter2_ui <- function(id){
  ns <- NS(id)
  selectInput(ns("select_scatter_2"),
              "",
              choices = c("guarderias", "asilos", "estancias"),width = "30%", selected = "guarderias", selectize = FALSE)
}

mod_municipalscatter_ui <- function(id){
  ns <- NS(id)
  fluidRow(withLoader(plotOutput(outputId = ns("muni_graf")), type = "html", loader = "loader4"))
}

mod_municipalscatter2_ui <- function(id){
  ns <- NS(id)
  fluidRow(withLoader(plotOutput(outputId = ns("muni_graf_2")), type = "html", loader = "loader4"))
}

mod_scatter_text_per_ui <- function(id){
  ns <- NS(id)
  withLoader(textOutput(ns("text_per")), type = "html", loader = "loader4")
}

mod_scatter_text_est_ui <- function(id){
  ns <- NS(id)
  withLoader(textOutput(ns("text_est")), type = "html", loader = "loader4")
}

mod_scatter_text_faltan_ui <- function(id){
  ns <- NS(id)
  withLoader(textOutput(ns("text_faltan")), type = "html", loader = "loader4")
}

mod_municipalscatter_server <- function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Responsive sizing
    # Width
    plotWidth <- reactive({session$clientData[["output_user-muni_graf_width"]]})

    # Height
    plotHeight <- function(){
      width <- plotWidth()
     h <- ifelse(width > 425, width*0.54, width*0.75)
      return(h)}

    # Font
    fontbase <- 8

    textFunction <- function(){
      width <- plotWidth()
      textSize <- ifelse(width > 425, fontbase, 0.5*fontbase)
      return(textSize)}


    ent_code <- reactive({
      left_join(
        tibble(name_ent = user_data$state()),
        fread("data-raw/geo-catalog.csv") %>%
          mutate(id_ent = str_pad(id_ent,width = 2,side = "left",pad = "0")) %>%
          distinct(id_ent, name_ent)) %>%
        pull(id_ent)
    })

    # State data
    ent_data <- reactive({
      fread(paste0("data-raw/descriptives/out/ent_o_d_graph/", ent_code(),".csv"))
    })


    # Textos #

    info_text <- reactive({
      dplyr::filter(ent_data(), name_mun == user_data$municipal())
    })

    output$text_per <- renderText(
      paste0("En tu municipio existen ",
             pull(info_text() %>% filter(concepto == "guarderias"), cuidar),
             " personas menores de 5 años, ",
             pull(info_text() %>% filter(concepto == "asilos"), cuidar),
             " personas adultas mayores y ",
             pull(info_text() %>% filter(concepto == "estancias"), cuidar),
             " personas con discapacidad.")
    )

    output$text_est <- renderText(
      paste0("En tu municipio existen ",
             ifelse(nrow(info_text() %>% filter(concepto == "guarderias")) == 0,"no tiene",
                    pull(info_text() %>% filter(concepto == "guarderias"), est)),
             " guarderías, ",
             ifelse(nrow(info_text() %>% filter(concepto == "asilos")) == 0,"no tiene",
                    pull(info_text() %>% filter(concepto == "asilos"), est)),
             " hogares para personas adultas mayores y ",
             ifelse(nrow(info_text() %>% filter(concepto == "estancias")) == 0,"no tiene",
                    pull(info_text() %>% filter(concepto == "estancias"), est)),
             " espacios para personas con discapacidad.")
    )

    output$text_faltan <- renderText(
      paste0("Es decir, para poder atender a todas las personas que lo necesitan, en tu municipio faltan ",
             pull(info_text() %>% filter(concepto == "guarderias"), faltan_est),
             " guarderías, ",
             pull(info_text() %>% filter(concepto == "asilos"), faltan_est),
             " hogares para personas adultas mayores y ",
             pull(info_text() %>% filter(concepto == "estancias"), faltan_est),
             " lugares para personas con discapacidad."))

    # User selected values (user info and scatter dropdown)
    lug <- reactive({input$select_scatter})
    lug_2 <- reactive({input$select_scatter_2})

    ideal <- fread("data-raw/descriptives/out/ideal_prop.csv")

    # Ideal line
    tempo_line <- reactive({
      dplyr::filter(ideal, var == lug())
    })
    tempo_line_2 <- reactive({
      dplyr::filter(ideal, var == lug_2())
    })
    # State data selecter
    ent_lug_data <- reactive({
      ent_data() %>%
        filter(concepto == lug())
    })
    ent_lug_data_2 <- reactive({
      ent_data() %>%
        filter(concepto == lug_2())
    })

    # Municipal data selected
    ent_lug_mun_data <- reactive({
      ent_lug_data() %>%
        filter(name_mun == user_data$municipal())
    })
    ent_lug_mun_data_2 <- reactive({
      ent_lug_data_2() %>%
        filter(name_mun == user_data$municipal())
    })

    output$muni_graf <- renderPlot({
      ggplot() +
        geom_ribbon(
          data = tempo_line(),
          aes(x = z,
              ymin = y,
              ymax = Inf),
          alpha = 0.3,
          fill = "#71939f"
        ) +
        geom_ribbon(
          data = tempo_line(),
          aes(x = z,
              ymin = -Inf,
              ymax = y),
          alpha = 0.6,
          fill = "#fadbd9"
        ) +
        ylim(0, case_when(lug() == "estancias" ~ 10,
                          lug() == "asilos" ~ 7,
                          lug () == "guarderias" ~ 9)) +
        xlim(0, case_when(lug() == "estancias" ~ 13,
                          lug() == "asilos" ~ 13,
                          lug () == "guarderias" ~ 13))+
        geom_line(data = tempo_line(),
                  aes(x = z, y = y),
                  color = "#363f52",
                  linetype = "dashed") +
        geom_point(
          data = fread(paste0("data-raw/descriptives/out/samples_estancias/", lug(),".csv")),
          aes(x = log(cuidar),
              y = log(est + 1)),
          size = 3,
          color = "#363f52",
          shape = 21,
          fill = "#71939f"
        ) +
        geom_label_repel(
          data = data.frame(
            label = paste0("Municipios con\n", lug(), "\nsuficientes"),
            x = 4,
            y = 4
          ),
          aes(x = x, y = y, label = label),
          color = "white",
          size = 0.65*textFunction(),
          fill = "#71939f",
          family = "Fredoka SemiBold",
          force = 0.5,
          box.padding =0,
          point.padding = NA,
          segment.colour = NA,
          xlim = c(0, 11),
          ylim = c(0, 7),
        ) +
        geom_label(
          data = data.frame(
            label = paste0("Municipios sin\n", lug(), " suficientes"),
            x = 11,
            y = case_when(lug() == "estancias" ~ 9,
                      lug() == "asilos" ~ 6,
                      lug () == "guarderias" ~ 8)
          ),
          aes(x = x, y = y, label = label),
          color = "black",
          size = 0.65*textFunction(),
          fill = "#fadbd9",
          family = "Fredoka SemiBold",
          force = 0.5,
          box.padding =0,
          point.padding = NA,
          segment.colour = NA,
          xlim = c(0, 11),
          ylim = c(0, 7),
        ) +
        geom_point(
          data = ent_lug_data(),
          aes(x = log(cuidar),
              y = log(est + 1)),
          size = 3,
          color = "#363f52",
          shape = 21,
          fill = "#e27f63"
        ) +
        geom_point(
          data = ent_lug_mun_data(),
          aes(x = log(cuidar),
              y = log(est + 1)),
          size = 6,
          color = "#363f52",
          shape = 21,
          fill = "#e27f63"
        ) +
        geom_label(data = ent_lug_mun_data(),
                      aes(x = log(cuidar),
                          y = log(est + 1) + 1),
                      label = paste0("En tu municipio faltan\n", pull(ent_lug_mun_data(), faltan_est),
                                     " ",
                                     lug()),
                      fill = "#363f52",
                      color = "white",
                      family = "Fredoka SemiBold",
                      size = 0.65*textFunction(),
                      xlim = c(2, 10),
                      ylim = c(2, 5.5),
                      # force = 0.5,
                      box.padding =0,
                      # point.padding = NA,
                      # segment.colour = NA
        ) +
        labs(caption = "Fuente: DENUE y CENSO 2020. *El cálculo se realizó tomando en cuenta les menores promedio que\npuede cuidar una guardería según DENUE,las Reglas de operación de Estancias Infantiles y\nel Marco Analítico y metodología para diagnosticar las brechas de cuidado en los municipios de México",
             x = case_when(lug() == "guarderias" ~ "Menores de seis años en el municipio",
                           lug() == "asilos" ~ "Mayores de 65 años",
                           lug() == "estancias" ~ "Personas con discapacidad"),
             y = paste0(str_to_title(lug())," en el municipio")) +
        theme(
          plot.title = element_blank(),
          axis.text.x = element_text(
            color = "#363f52",
            size = 2.25*textFunction(),
            family = "Fredoka"
          ),
          axis.text.y = element_text(
            color = "#363f52",
            size = 2.25*textFunction(),
            family = "Fredoka"
          ),
          axis.title = element_text(
            color = "#363f52",
            size = 2.5*textFunction(),
            family = "Fredoka SemiBold"
          ),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          panel.background = element_rect(fill = "#fcedd3"),
          plot.background = element_rect(fill = "#fcedd3", color = NA),
          plot.caption = element_text(
            color = "#363f52",
            family = "Fredoka",
            hjust = 0,
            size = 1.2*textFunction()
          ),
          panel.grid.major = element_line(color = "#363f52", linetype = "dashed"),
          panel.grid.minor = element_line(color = "#363f52", linetype = "dashed"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          legend.position = "top")
    }, height = plotHeight)



    output$muni_graf_2 <- renderPlot({
      ggplot() +
        geom_ribbon(
          data = tempo_line_2(),
          aes(x = z,
              ymin = y,
              ymax = Inf),
          alpha = 0.3,
          fill = "#71939f"
        ) +
        geom_ribbon(
          data = tempo_line_2(),
          aes(x = z,
              ymin = -Inf,
              ymax = y),
          alpha = 0.6,
          fill = "#fadbd9"
        ) +
        ylim(0, case_when(lug_2() == "estancias" ~ 10,
                          lug_2() == "asilos" ~ 7,
                          lug_2() == "guarderias" ~ 9)) +
        xlim(0, case_when(lug_2() == "estancias" ~ 13,
                          lug_2() == "asilos" ~ 13,
                          lug_2() == "guarderias" ~ 13))+
        geom_line(data = tempo_line_2(),
                  aes(x = z, y = y),
                  color = "#363f52",
                  linetype = "dashed") +
        geom_point(
          data =  fread(paste0("data-raw/descriptives/out/samples_estancias/", lug_2(),".csv")),
          aes(x = log(cuidar),
              y = log(est + 1)),
          size = 3,
          color = "#363f52",
          shape = 21,
          fill = "#71939f"
        ) +
        geom_label_repel(
          data = data.frame(
            label = paste0("Municipios con\n", lug(), "\nsuficientes"),
            x = 4,
            y = 4
          ),
          aes(x = x, y = y, label = label),
          color = "white",
          size = 0.65*textFunction(),
          fill = "#71939f",
          family = "Fredoka SemiBold",
          force = 0.5,
          box.padding =0,
          point.padding = NA,
          segment.colour = NA,
          xlim = c(0, 11),
          ylim = c(0, 7),
        ) +
        geom_label(
          data = data.frame(
            label = paste0("Municipios sin\n", lug_2(), " suficientes"),
            x = 11,
            y = case_when(lug_2() == "estancias" ~ 9,
                          lug_2() == "asilos" ~ 6,
                          lug_2() == "guarderias" ~ 8)
          ),
          aes(x = x, y = y, label = label),
          color = "black",
          size = 0.65*textFunction(),
          fill = "#fadbd9",
          family = "Fredoka SemiBold",
          force = 0.5,
          box.padding =0,
          point.padding = NA,
          segment.colour = NA,
          xlim = c(0, 11),
          ylim = c(0, 7),
        ) +
        geom_point(
          data = ent_lug_data_2(),
          aes(x = log(cuidar),
              y = log(est + 1)),
          size = 3,
          color = "#363f52",
          shape = 21,
          fill = "#e27f63"
        ) +
        geom_point(
          data = ent_lug_mun_data_2(),
          aes(x = log(cuidar),
              y = log(est + 1)),
          size = 6,
          color = "#363f52",
          shape = 21,
          fill = "#e27f63"
        ) +
        geom_label(data = ent_lug_mun_data_2(),
                   aes(x = log(cuidar),
                       y = log(est + 1) + 1),
                   label = paste0("En tu municipio faltan\n", pull(ent_lug_mun_data_2(), faltan_est),
                                  " ",
                                  lug_2()),
                   fill = "#363f52",
                   color = "white",
                   family = "Fredoka SemiBold",
                   size = 0.65*textFunction(),
                   xlim = c(2, 10),
                   ylim = c(2, 5.5),
                   # force = 0.5,
                   box.padding =0,
                   # point.padding = NA,
                   # segment.colour = NA
        ) +
        labs(caption = "Fuente: DENUE y CENSO 2020. *El cálculo se realizó tomando en cuenta les menores promedio que\npuede cuidar una guardería según DENUE,las Reglas de operación de Estancias Infantiles y\nel Marco Analítico y metodología para diagnosticar las brechas de cuidado en los municipios de México",
             x = case_when(lug_2() == "guarderias" ~ "Menores de seis años en el municipio",
                           lug_2() == "asilos" ~ "Mayores de 65 años",
                           lug_2() == "estancias" ~ "Personas con discapacidad"),
             y = paste0(str_to_title(lug_2())," en el municipio")) +
        theme(
          plot.title = element_blank(),
          axis.text.x = element_text(
            color = "#363f52",
            size = 2.25*textFunction(),
            family = "Fredoka"
          ),
          axis.text.y = element_text(
            color = "#363f52",
            size = 2.25*textFunction(),
            family = "Fredoka"
          ),
          axis.title = element_text(
            color = "#363f52",
            size = 2.75*textFunction(),
            family = "Fredoka SemiBold"
          ),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          panel.background = element_rect(fill = "#fcedd3"),
          plot.background = element_rect(fill = "#fcedd3", color = NA),
          plot.caption = element_text(
            color = "#363f52",
            family = "Fredoka",
            hjust = 0,
            size = 1.12*textFunction()
          ),
          panel.grid.major = element_line(color = "#363f52", linetype = "dashed"),
          panel.grid.minor = element_line(color = "#363f52", linetype = "dashed"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          legend.position = "none")
    }, height = plotHeight)
  })
}


