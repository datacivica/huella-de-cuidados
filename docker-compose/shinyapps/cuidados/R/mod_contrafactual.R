require(spatstat)
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
#'

mod_dumbbells_ui <- function(id){
  ns <- NS(id)
  plotOutput(outputId = ns("contra_graf"))
}


mod_dumbbells_server <- function(id, enut, input_data, hrs_output_values, vari) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    plotWidth <- reactive({session$clientData[["output_user-contra_graf_width"]]})

    plotHeight <- function(){
      width <- plotWidth()
      h <- ifelse(width > 425, width*0.40, width*0.70)
      return(h)}


    fontbase <- 8

    textFunction <- function(){
      width <- plotWidth()
      j <- ifelse(width > 425, fontbase, 0.45*fontbase)
      return(j)}


    contra <- reactive({
      bind_rows(
        # Sexo
        enut %>%
          dplyr::filter(
            tiene_hij == input_data$child(),
            trab_dom == input_data$worker(),
            niv == input_data$school()
          ) %>%
          group_by(sexo) %>%
          summarise(
            prom = weighted.median(hrs_cuidado, fac_per, na.rm = T),
            carac = "sexo",
            presence = ifelse(sexo == input_data$sex(), prom, NA)
          ) %>%
          ungroup() %>%
          distinct() %>%
          fill(presence, .direction = "down") %>%
          fill(presence, .direction = "up") %>%
          rename(valor = sexo),
        # Hijes
        enut %>%
          dplyr::filter(
            sexo == input_data$sex(),
            trab_dom == input_data$worker(),
            niv == input_data$school()
          ) %>%
          group_by(tiene_hij) %>%
          summarise(
            prom = weighted.median(hrs_cuidado, fac_per, na.rm = T),
            carac = "tiene_hij",
            presence = ifelse(tiene_hij == input_data$child(), prom, NA)
          ) %>%
          ungroup() %>%
          distinct() %>%
          fill(presence, .direction = "down") %>%
          fill(presence, .direction = "up") %>%
          rename(valor = tiene_hij),
        # Escolaridad
        enut %>%
          dplyr::filter(
            sexo == input_data$sex(),
            trab_dom == input_data$worker(),
            tiene_hij == input_data$child()
          ) %>%
          group_by(niv) %>%
          summarise(
            prom = weighted.median(hrs_cuidado, fac_per, na.rm = T),
            carac = "niv",
            presence = ifelse(niv == input_data$school(), prom, NA)
          ) %>%
          ungroup() %>%
          distinct() %>%
          fill(presence, .direction = "down") %>%
          fill(presence, .direction = "up") %>%
          rename(valor = niv),
        # Trabajadoras del hogar
        enut %>%
          dplyr::filter(
            sexo == input_data$sex(),
            tiene_hij == input_data$child(),
            niv == input_data$school()
          ) %>%
          group_by(trab_dom) %>%
          summarise(
            prom = weighted.median(hrs_cuidado, fac_per, na.rm = T),
            carac = "trab_dom",
            presence = ifelse(trab_dom == input_data$worker(), prom, NA)
          ) %>%
          ungroup() %>%
          distinct() %>%
          fill(presence, .direction = "down") %>%
          fill(presence, .direction = "up") %>%
          ungroup() %>%
          rename(valor = trab_dom)
      ) %>%
        mutate(dif = presence - prom) %>%
        group_by(carac) %>%
        dplyr::filter(abs(dif) == max(abs(dif))) %>%
        ungroup() %>%
        mutate(hover = case_when(valor == "hombre" ~ "fueras hombre", # ser mujer fueras hombre
                                 valor == "mujer" ~ "fueras mujer", # ser hombre hombre
                                 valor == "No tiene" ~ "no tuvieras hijos", # ser mamá no tuvieras hijos
                                 valor == "Sí tiene" & input_data$sex() == "mujer" ~ "fueras madre", #no tuvieras hijos, si tuviera
                                 valor == "Sí tiene" & input_data$sex() == "hombre" ~ "fueras padre", # no tener hijos, si tuviera
                                 valor == "No contrata" ~ "no lo haces",
                                 valor == "Sí contrata" ~ "sí lo haces",
                                 carac == "niv" ~ paste0("hubieras estudiado hasta\n", str_to_lower(valor))))
    })


    final <- reactive({contra() %>%
        dplyr::select(valor, carac, prom, hover) %>%
        rbind(data.frame(valor = "Tu realidad", prom = hrs_output_values()$prom_cuidado_reduced, carac = "Tu realidad", hover=""))})

    pal_base <- c("#ffcf7c", "#f4785c", "#ffd9d8", "#6794a1", "black")

    rsvg_svg("inst/app/www/img/Manita_Marcador_2.svg", "inst/app/www/img/Manita_Marcador_2-cairo.svg")
    SVGlogo <- readPicture("inst/app/www/img/Manita_Marcador_2-cairo.svg")
    grid.picture(SVGlogo)


    sym.grob <- reactive({
      symbolsGrob(
        SVGlogo,
        x = rescale(hrs_output_values()$prom_cuidado_reduced, from = c(min(final()$prom)-5, max(final()$prom)+5)),
        y = rescale(1, from = c(0.75, 1.25)),
        size = 0.175
      )
    })

    output$contra_graf <- renderPlot({
      ggplot() +
        geom_line(data  = final() %>%
                    dplyr::filter(carac %in% c("Tu realidad", vari$selected)),
                  aes(x = prom, y = 1),
                  color = "#363f52", size = 2) +
        geom_point(
          data = final() %>%
            dplyr::filter(carac != "Tu realidad") %>%
            dplyr::mutate(trans = ifelse(carac == vari$selected,1, 0.5)),
          aes(x = as.numeric(prom), y = 1, fill = carac, alpha = trans),
          size = 1.875*textFunction(),
          shape = 21,
          stroke = 2,
          position = position_dodge(width = 0.6),
          color = "#363f52"
        ) +
        geom_text(
          data = final() %>% dplyr::filter(valor == "Tu realidad"),
          aes(x = prom-0.1, y = 1 + 0.07),
          label = "Tu realidad",
          family = "Fredoka SemiBold",
          color = "#363f52",
          size = 0.75*textFunction()
        ) +
        geom_label(
          data = final() %>% dplyr::filter(valor == "Tu realidad"),
          aes(x = prom-0.1, y = 1 - 0.075, label = paste(round(prom, 0), "horas", sep = " ")),
          family = "Fredoka SemiBold",
          color = "white",
          size = 0.875*textFunction(),
          fill = "#363f52",
          label.padding = unit(0.6, "lines"),
          label.size = 0,
          label.r = unit(0.35, "lines")
        ) +
        geom_label(
          data = final() %>% dplyr::filter(carac == vari$selected),
          aes(x = prom-0.1, y = 1 + 0.125, label = hover),
          family = "Fredoka SemiBold",
          color = "#363f52",
          fill = "white", alpha = 0.75,
          label.padding = unit(0.6, "lines"),
          size = 0.75*textFunction()
        ) +
        geom_label(
          data = final() %>% dplyr::filter(carac == vari$selected),
          aes(x = prom-0.1, y = 1 - 0.125, label = paste(round(prom, 0), "horas", sep = " ")),
          family = "Fredoka SemiBold",
          color = "#363f52",
          size = 0.875*textFunction(),
          fill = "white",
          label.size = 0,
          label.padding = unit(0.6, "lines")
          , alpha = 0.75,
          label.r = unit(0.35, "lines")
        ) +
        annotation_custom(sym.grob())  +
        scale_y_continuous(limits = c(0.75, 1.25), minor_breaks = c(1, 1.00001)) +
        scale_x_continuous(limits = c(min(final()$prom)-5,max(final()$prom)+5))  +
        scale_fill_manual(values = pal_base) +
        scale_alpha_identity() +
        labs(x = "Horas semanales de trabajo del hogar y de cuidado",
             caption = "Fuente: Elaboración propia a partir de la Encuesta Nacional de Uso de Tiempo") +
        theme(plot.title = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(color = "#363f52", size = 2.75*textFunction(), family = "Fredoka"),
              axis.ticks = element_blank(),
              axis.ticks.y = element_blank(),
              strip.background = element_blank(),
              panel.background = element_rect(fill = "#ffd9d8"),
              panel.grid.minor.y = element_line(color = "#363f52",
                                                size = 0.5,
                                                linetype = 2),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = "#363f52",
                                                size = 0.5,
                                                linetype = 2),
              plot.background = element_rect(fill = "#ffd9d8", color = NA),
              panel.border = element_blank(),
              axis.line.x = element_blank(),
              axis.title.y = element_blank(),
              axis.title = element_text(size = 2.75*textFunction(), color = "#363f52", family = "Fredoka SemiBold"),
              plot.caption = element_text(color = "#363f52", family = "Fredoka", size = 1.75*textFunction(), hjust = 0.5),
              legend.position = "none")}, height =plotHeight)

    # ----- report graf
    sym.grob_rep <- reactive({
    symbolsGrob(
      SVGlogo,
      x = rescale(final() %>%
                    dplyr::filter(carac== "Tu realidad") %>%
                    pull(prom),
                  from = c(
                    min(final() %>%
                          dplyr::filter(carac %in% c("Tu realidad", "tiene_hij", "sexo")) %>%
                          pull(prom))-5,
                    max(final() %>%
                          dplyr::filter(carac %in% c("Tu realidad", "tiene_hij", "sexo")) %>%
                          pull(prom))+5)),
      y = rescale(1, from = c(0.85, 1.15)),
      size = 0.175
    )})
    contra_report <- reactive(

      ggplot() +
        geom_line(data  = final() %>%
                    dplyr::filter(carac %in% c("Tu realidad", "tiene_hij", "sexo")),
                  aes(x = prom, y = 1),
                  color = "#363f52", size = 2)+
        geom_point(
          data = final() %>%
            dplyr::filter(carac %in% c("sexo", "tiene_hij")),
          aes(x = as.numeric(prom), y = 1, fill = carac),
          size = 28,
          shape = 21,
          stroke = 2,
          position = position_dodge(width = 0.6),
          color = "#363f52"
        ) +
        geom_text(
          data = final() %>% dplyr::filter(valor == "Tu realidad"),
          aes(x = prom, y = 1 + 0.07),
          label = "Tu realidad",
          family = "Fredoka SemiBold",
          color = "#363f52",
          size = 12
        ) +
        geom_label(
          data = final() %>% dplyr::filter(valor == "Tu realidad"),
          aes(x = prom, y = 1 - 0.07, label = paste(round(prom, 0), "horas", sep = " ")),
          family = "Fredoka SemiBold",
          color = "white",
          size = 12,
          fill = "#363f52",
          label.padding = unit(0.6, "lines"),
          label.size = 0,
          label.r = unit(0.35, "lines")
        ) +
        geom_label(
          data = final() %>% dplyr::filter(carac %in% c("sexo", "tiene_hij")) %>%
            dplyr::mutate(hover2 = case_when(valor == "hombre" ~ "ser mujer,\nfueras hombre", # ser mujer fueras hombre
                                             valor == "mujer" ~ "ser hombre,\nfueras mujer",
                                             valor  == "No tiene" & input_data$sex() == "mujer"  ~ "ser mamá, no\ntuvieras hijos",
                                             valor == "No tiene"& input_data$sex() == "hombre"  ~ "ser papá, no\ntuvieras hijos", # ser mamá no tuvieras hijos
                                             valor == "Sí tiene" & input_data$sex() == "mujer" ~ "no tener hijes,\nsí tuviera", #no tuvieras hijos, si tuviera
                                             valor == "Sí tiene" & input_data$sex() == "hombre" ~ "no tener hijes,\nsí tuviera",
                                             )),
          aes(x = prom, y = 1 + 0.095, label = hover2),
          family = "Fredoka SemiBold",
          color = "#363f52",
          fill = "white",
          label.padding = unit(0.6, "lines"),
          size = 8
        ) +
        geom_label(
          data = final() %>% dplyr::filter(carac %in% c("sexo", "tiene_hij")) ,
          aes(x = prom, y = 1 - 0.095, label = paste(round(prom, 0), "horas", sep = " ")),
          family = "Fredoka SemiBold",
          color = "#363f52",
          size = 8,
          fill = "white",
          label.size = 0,
          label.padding = unit(0.6, "lines")
          ,
          label.r = unit(0.35, "lines")
        ) +
        annotation_custom(sym.grob_rep()) # es manita
      +
        scale_y_continuous(limits = c(0.85, 1.15), minor_breaks = c(1, 1.00001)) +
        scale_x_continuous(limits = c(min(final() %>%
                                            dplyr::filter(carac %in% c("Tu realidad", "tiene_hij", "sexo")) %>%
                                            pull(prom))-5,
                                      max(final() %>%
                                            dplyr::filter(carac %in% c("Tu realidad", "tiene_hij", "sexo")) %>%
                                            pull(prom))+5))
      +
        scale_fill_manual(values = pal_base) +
        scale_alpha_identity() +
        labs(x = "Horas semanales de trabajo\ndel hogar y de cuidado",
             caption = "Fuente: Elaboración propia a partir de\nla Encuesta Nacional de Uso de Tiempo") +
        theme(plot.title = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(color = "#363f52", size = 22, family = "Fredoka"),
              axis.ticks = element_blank(),
              axis.ticks.y = element_blank(),
              strip.background = element_blank(),
              panel.background = element_rect(fill = "transparent"),
              panel.grid.minor.y = element_line(color = "#363f52",
                                                size = 0.5,
                                                linetype = 2),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = "#363f52",
                                                size = 0.5,
                                                linetype = 2),
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.border = element_blank(),
              axis.line.x = element_blank(),
              axis.title.y = element_blank(),
              axis.title = element_text(size = 32, color = "#363f52", family = "Fredoka SemiBold"),
              plot.caption = element_text(color = "#363f52", family = "Fredoka", size = 24, hjust = 0.5),
              legend.position = "none")

    )


    output$contra_report <- renderPlot({
      contra_report()
    })


    return(list(final = final,
                contra_report = contra_report))

  })
}




