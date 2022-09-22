
# Custom download button --------------------------------------------------

### Hacemos estos para poder moficiar en css con nuestra propia clase
myDownloadButton <- function(outputId, label = "DESCARGA Y COMPARTE TUS RESULTADOS"){
  tags$a(id = outputId, class = "download-results-button shiny-download-link", href = "",
         target = "_blank", download = NA, NULL, label)
}


# Reports matrix --------------------------------------------------------------------
x <- rep(1:35, 20)
dummy <- as.data.frame(matrix(x, ncol = 20))


# UI ----------------------------------------------------------------------
mod_report_ui <- function(id){
  ns <- NS(id)

  tagList(
    myDownloadButton(ns("report_plot"))
  )
}


# Server  -----------------------------------------------------------------
mod_report_server <- function(id,
                              density_plot,
                              contra_plot,
                              hrs_output_values,
                              user_values){

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # report 1 ----
    my_report_one <- reactive({

      temp <<- tempfile()
      ggsave(temp, density_plot$dist_report(), width = 18, height = 15, units = "in", dpi = 300, device = "png")

      if (hrs_output_values()$saldo > 0) {
        p1 <- dummy %>%
          ggplot(aes(V1, V2)) +
          theme_void() +
          geom_richtext(aes(x = 18, y = 31,
                            label = paste("<span style = 'color:#363f52; text-align:center;'>Según la herramienta desarrollada por Data Cívica, la gente como<br> yo dedica", round((hrs_output_values()$prom_cuidado_reduced / 7), 1), "horas diarias al trabajo doméstico o de<br> cuidados;", abs(round((hrs_output_values()$prom_cuidado_reduced / 7 - hrs_output_values()$tot_cuidado_nac_percap / 7), 1)),
                                          "horas más de las que se dedicaría <br> si el trabajo estuviera distribuido equitativamente</span>")
          ), fill = NA, label.color = NA,  family = "Fredoka") +
          geom_label(aes(x = 18,
                         y = 27),
                     label = paste("DISTRIBUCIÓN DE SALDOS DE CUIDADO EN MÉXICO"),
                     fill = "#363f52", colour = "#ffcf7c", size = 5, fontface = "bold", family = "Fredoka"
          ) +
          xlim(c(1,35)) +
          ylim(c(1,35)) +
          theme(panel.background = element_rect(fill = '#fcedd3', colour = '#fcedd3'))

        p1 <- ggdraw(p1) +
          draw_image(here("data-raw", "assets-graphs", "PlecaLogos_Resultados.jpg"), scale = 1,  x = 0, y =  -0.46) +
          draw_image(temp, scale = 0.6, y =  -0.1)

        return(p1)

      } else {
        p1 <- dummy %>%
          ggplot(aes(V1, V2)) +
          theme_void() +
          geom_richtext(aes(x = 18, y = 31,
                            label = paste("<span style = 'color:#363f52;text-align:center;'>Según la herramienta desarrollada por Data Cívica, la gente como<br> yo dedica", abs(round((hrs_output_values()$prom_cuidado_reduced / 7), 1)), "horas diarias al trabajo doméstico o de<br> cuidados;", abs(round((hrs_output_values()$prom_cuidado_reduced / 7 - hrs_output_values()$tot_cuidado_nac_percap / 7), 1)),
                                          "horas menos de las que se debería  <br>si el trabajo estuviera distribuido equitativamente.</span>")
          ), fill = NA, label.color = NA,  family = "Fredoka") +
          geom_label(aes(x = 18,
                         y = 27),
                     label = paste("DISTRIBUCIÓN DE SALDOS DE CUIDADOS EN MÉXICO"),
                     fill = "#363f52", colour = "#ffd9d8", size = 5, fontface = "bold", family = "Fredoka",
                     label.size = 0.2
          ) +
          xlim(c(1,35)) +
          ylim(c(1,35)) +
          theme(panel.background = element_rect(fill = '#ffd9d8', colour = '#ffd9d8'))

        p1 <- ggdraw(p1) +
          draw_image(here("data-raw", "assets-graphs", "PlecaLogos_Resultados.jpg"), scale = 1,  x = 0, y =  -0.46) +
          draw_image(temp, scale = 0.6, y =  -0.1)

        return(p1)

      }
    })

    output$report <- renderPlot({
      my_report_one()
    })


    # report 2 ----


    labs <- reactive({
      dummy %>%
        ggplot(aes(V1, V2)) +
        theme_void() +
        geom_richtext(aes(x = 7,
                          y = 26),
                      label = ifelse(hrs_output_values()$saldo<0,
                                     paste("<b><span style ='color:#373f50'>Cotizar</span><span style = 'color:#e27f63'>", round(((abs(hrs_output_values()$saldo)*52)/45)), "semanas</span><br><span style ='color:#373f50'>en el IMSS o</span></b>"),
                                     paste("<b><span style ='color:#373f50'>Cotizado</span><span style = 'color:#e27f63'>", round(((abs(hrs_output_values()$saldo)*52)/45)), "semanas</span><br><span style ='color:#373f50'>en el IMSS o</span></b>")

                      ),
                      fill = "white", label.color = "#363f52", family = "Fredoka", size = 4, label.padding = unit(c(1, 2, 1, 1), "lines")) +
        geom_richtext(aes(x = 18,
                          y = 26),
                      label = ifelse(hrs_output_values()$saldo<0,
                                     paste("<b><span style ='color:#373f50'>Ver</span><span style = 'color:#e27f63'>", round(((abs(hrs_output_values()$saldo*52))/2)), "<br>películas  </span><span style = 'color:#373f50'>          o</span></b>"),
                                     paste("<b><span style ='color:#373f50'>Visto</span><span style = 'color:#e27f63'>", round(((abs(hrs_output_values()$saldo*52))/2)), "<br>películas  </span><span style = 'color:#373f50'>          o</span></b>")
                      ),
                      fill = "white", label.color = "#363f52", family = "Fredoka", size = 4, label.padding = unit(c(1, 1.5, 1, 1), "lines")) +
        geom_richtext(aes(x = 29,
                          y = 26),
                      label = ifelse(hrs_output_values()$saldo<0,
                                     paste("<b><span style ='color:#373f50'>Pasar </span><span style = 'color:#e27f63'>", round(abs(hrs_output_values()$saldo*52)), "horas </span><br><span style = 'color:#373f50'>con sus amistades</span></b>"),
                                     paste("<b><span style ='color:#373f50'>Pasado </span><span style = 'color:#e27f63'>", round(abs(hrs_output_values()$saldo*52)), "horas</span><br><span style = 'color:#373f50'>con sus amistades</span></b>")
                      ),
                      fill = "white", label.color = "#363f52",  family = "Fredoka", size = 4, label.padding = unit(c(1, 1.5, 1, 1), "lines")) +
        geom_richtext(aes(x = 18,
                          y = 33),
                      label = ifelse(hrs_output_values()$saldo<0,
                                     paste("<span style = 'color:#363f52'>Solo por el último año, le debo a la sociedad<br>", abs(round(hrs_output_values()$saldo*52)), "horas de trabajo doméstico y de cuidados<br><br>Es decir, alguién dejó de:</span>"),
                                     paste("<span style = 'color:#363f52'>Solo por el último año, la sociedad me debe<br>", abs(round(hrs_output_values()$saldo*52)), "<span style = 'color:#363f52'>horas de trabajo doméstico y de cuidados<br><br>con este tiempo pude haber:</span>")
                      ),
                      fill = NA, label.color = NA, family = "Fredoka") +
        xlim(c(1,35)) +
        ylim(c(1,35)) +
        theme(panel.background = element_rect(fill = '#ffd9d8', colour = '#ffd9d8'))
    })


    backgrounds <-  reactive(
      if (hrs_output_values()$saldo > 0) {
        p2 <- labs() +
          theme(panel.background = element_rect(fill = '#fcedd3', colour = '#fcedd3'))
      } else {
        p2 <- labs() +
          theme(panel.background = element_rect(fill = '#ffd9d8', colour = '#ffd9d8'))
      }
    )



    my_report_two <- reactive(
      if(hrs_output_values()$place == 1){
        ggdraw(backgrounds()) +
          draw_image(here("data-raw", "assets-graphs",
                          "dinero-pila-chica_Sombra.png"),
                     x = -0.30, y = -0.095, scale = 0.5) +
          draw_image(here("data-raw", "assets-graphs", "peliculas-tira-corta_Sombra.png"),
                     x = 0, y =  -0.095, scale = 0.5) +
          draw_image(here("data-raw", "assets-graphs", "tazas-pila-chica_Sombra.png"),
                     x = 0.35, y =  -0.095, scale = 0.5) +
          draw_image(here("data-raw", "assets-graphs", "PlecaLogos_Resultados.jpg"), scale = 1,  x = 0, y =  -0.46)
      } else(
        if(hrs_output_values()$place == 2){
          ggdraw(backgrounds()) +
            draw_image(here("data-raw", "assets-graphs",
                            "dinero-pila-mediana_Sombra.png"),
                       x = -0.30, y = -0.095, scale = 0.5) +
            draw_image(here("data-raw", "assets-graphs", "peliculas-tira-mediana_Sombra.png"),
                       x = 0, y =  -0.095, scale = 0.5) +
            draw_image(here("data-raw", "assets-graphs", "tazas-pila-mediana_Sombra.png"),
                       x = 0.35, y =  -0.095, scale = 0.5) +
            draw_image(here("data-raw", "assets-graphs", "PlecaLogos_Resultados.jpg"), scale = 1,  x = 0, y =  -0.46)

        } else(
          if(hrs_output_values()$place == 3){
            ggdraw(backgrounds()) +
              draw_image(here("data-raw", "assets-graphs",
                              "dinero-pila-alta_Sombra.png"),
                         x = -0.35, y = -0.095, scale = 0.5) +
              draw_image(here("data-raw", "assets-graphs", "peliculas-tira-larga_Sombra.png"),
                         x = 0, y =  -0.095, scale = 0.5) +
              draw_image(here("data-raw", "assets-graphs", "tazas-pila-alta_Sombra.png"),
                         x = 0.35, y =  -0.095, scale = 0.5) +
              draw_image(here("data-raw", "assets-graphs", "PlecaLogos_Resultados.jpg"), scale = 1,  x = 0, y =  -0.46)

          }
        )
      )
    )



    output$report_two <- renderPlot({
      my_report_two()
    })



    # report 3 ----
    my_report_three <- reactive({

      temp <<- tempfile()
      ggsave(temp, contra_plot$contra_report(), width = 9, height = 11, units = "in", dpi = 300, device = "png")

      if (hrs_output_values()$saldo > 0) {
        p3 <-dummy %>%
          ggplot(aes(V1, V2)) +
          theme_void() +
          geom_richtext(aes(x = 18,
                            y = 31),
                        label = HTML(paste0(
                          "<span style = 'color:#363f52'>Si en vez de ser ",
                          user_values$sex(),
                          " yo fuera",
                          ifelse(user_values$sex() == "hombre", "mujer<br>", "hombre<br>"),
                          " haría ",
                          round(contra_plot$final() %>% filter(carac == "sexo") %>% pull(prom)),
                          " horas de trabajo diario. <br><br> Si en vez de ",
                          case_when(user_values$sex() == "mujer" & user_values$child() == "Sí tiene" ~ "ser mamá<br>",
                                    user_values$sex() == "hombre" & user_values$child() == "Sí tiene"~ "ser papá<br>",
                                    user_values$child() == "No tiene"~ "no tener hijes<br>"),
                          case_when(user_values$child() == "Sí tiene" ~ "no tuviera hijes",
                                    user_values$child() == "No tiene" & user_values$sex() == "mujer" ~ "fuera mamá",
                                    user_values$child() == "No tiene" & user_values$sex() == "hombre" ~ "fuera papá"),
                          " haría ",
                          round(contra_plot$final() %>% filter(carac == "tiene_hij") %>% pull(prom))),
                          " horas de trabajo.</span>"),
                        fill = NA, label.color = NA, family = "Fredoka") +
          geom_label(aes(x = 18,
                         y = 25),
                     label = paste("COMPARA TU CARGA DE TRABAJO DEL HOGAR Y \n DE CUIDADO CON UN MUNDO EN DONDE EN VEZ DE..."),
                     fill = "#363f52", colour = "#ffcf7c", size = 5, fontface = "bold", family = "Fredoka"
          ) +
          xlim(c(1,35)) +
          ylim(c(1,35)) +
          theme(panel.background = element_rect(fill = '#fcedd3', colour = '#fcedd3'))

        p3 <- ggdraw(p3) +
          draw_image(here("data-raw", "assets-graphs", "PlecaLogos_Resultados.jpg"), scale = 1,  x = 0, y =  -0.46) +
          draw_image(temp, scale = 0.5, y =  -0.1)

        return(p3)

      } else {

        p3 <-dummy %>%
          ggplot(aes(V1, V2)) +
          theme_void() +
          geom_richtext(aes(x = 18,
                            y = 31),
                        label = HTML(paste0(
                          "<span style = 'color:#363f52'>Si en vez de ser ",
                          user_values$sex(),
                          " yo fuera",
                          ifelse(user_values$sex() == "hombre", "mujer<br>", "hombre<br>"),
                          " haría ",
                          round(contra_plot$final() %>% filter(carac == "sexo") %>% pull(prom)),
                          " horas de trabajo diario. <br><br> Si en vez de ",
                          case_when(user_values$sex() == "mujer" & user_values$child() == "Sí tiene" ~ "ser mamá<br>",
                                    user_values$sex() == "hombre" & user_values$child() == "Sí tiene"~ "ser papá<br>",
                                    user_values$child() == "No tiene"~ "no tener hijes<br>"),
                          case_when(user_values$child() == "Sí tiene" ~ "no tuviera hijes",
                                    user_values$child() == "No tiene" & user_values$sex() == "mujer" ~ "fuera mamá",
                                    user_values$child() == "No tiene" & user_values$sex() == "hombre" ~ "fuera papá"),
                          " haría ",
                          round(contra_plot$final() %>% filter(carac == "tiene_hij") %>% pull(prom))),
                          " horas de trabajo.</span>"),
                        fill = NA, label.color = NA, family = "Fredoka") +
          geom_label(aes(x = 18,
                         y = 26),
                     label = paste("COMPARA TU CARGA DE TRABAJO DEL HOGAR Y \n DE CUIDADO CON UN MUNDO EN DONDE EN VEZ DE..."),
                     fill = "#363f52", colour = "#ffd9d8", size = 5, fontface = "bold", family = "Fredoka",
                     label.size = 0.2
          ) +
          xlim(c(1,35)) +
          ylim(c(1,35)) +
          theme(panel.background = element_rect(fill = '#ffd9d8', colour = '#ffd9d8'))

        p3 <- ggdraw(p3) +
          draw_image(here("data-raw", "assets-graphs", "PlecaLogos_Resultados.jpg"), scale = 1,  x = 0, y =  -0.46) +
          draw_image(temp, scale = 0.5, y =  -0.1)

        return(p3)

      }
    })

    output$report_three <- renderPlot({
      my_report_three()
    })

    # report 4 ----
    my_report_four <- reactive({
      p4 <- ggdraw() +
        draw_image(here("data-raw", "assets-graphs", "Resultados-THCD_4.jpeg"))

      return(p4)
    })

    output$report_four <- renderPlot({
      my_report_four()
    })

    # download reports----
    output$report_plot <- downloadHandler(
      filename = 'mi-reporte.zip',
      content = function(file){
        withProgress(message = 'Descargando tus resultados, por favor espera', {
          # Set temporary working directory
          owd <- setwd(tempdir())
          on.exit(setwd(owd))

          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(5/10)


          # Save the reports
          ggsave("mi-reporte-1.png", plot = my_report_one(), device = "png")
          ggsave("mi-reporte-2.png", plot = my_report_two(), device = "png")
          ggsave("mi-reporte-3.png", plot = my_report_three(), device = "png")
          ggsave("tu-huella.png", plot = my_report_four(), device = "png")

          # Zip them up
          zip(file, c("mi-reporte-1.png", "mi-reporte-2.png", "mi-reporte-3.png", "tu-huella.png"))

        })
      })
  })

}



# DONE.
