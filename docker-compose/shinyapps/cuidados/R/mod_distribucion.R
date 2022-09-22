# require(shinycustomloader)
require(rsvg)
require(grImport2)
require(scales)

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

mod_distribucion_ui <- function(id){
  ns <- NS(id)
  plotOutput(outputId = ns("distribucion"))
}


mod_distribucion2_ui <- function(id){
  ns <- NS(id)
 plotOutput(outputId = ns("distribucion2"))
}

#' distribucion Server Functions
#'
#' @noRd

mod_distribucion_server <- function(id, input_data, hrs_output_values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ylim <- 0.03

    rsvg_svg("inst/app/www/img/Manita_Marcador_2.svg", "inst/app/www/img/Manita_Marcador_2-cairo.svg")
    SVGlogo <- readPicture("inst/app/www/img/Manita_Marcador_2-cairo.svg")


plotWidth <- reactive({session$clientData[["output_user-distribucion_width"]]})


plotHeight <- function() {
      w <- plotWidth()
      h <- case_when(w >= 683 ~ w*0.36,
                     w < 683 ~ w*0.80,
                     T ~ 0.36*w)
      return(h)
    }

  fontbase <- 8

textFunction <- function() {
      width <- plotWidth()
      textsize <- ifelse(width > 768, fontbase, 0.4*fontbase)
      return(textsize)
    }


dist_cuidados  <- reactive({
      sym.grob <- symbolsGrob(SVGlogo,
                              x=rescale(hrs_output_values()$saldo, from = c(-40,105)),
                              y=rescale((ylim-0.0015)*0.66666, from = c(0,ylim)),
                              size=0.17)

      ggplot() +
        geom_rect(aes(xmin = 0, xmax = Inf, ymin = 0, ymax = ylim-0.0035), fill = "#6794a1", alpha = 0.3) +
        geom_rect(aes(xmin = 0, xmax = -Inf, ymin = 0, ymax = ylim-0.0035), fill = "#f4785c",alpha = 0.3) +
        geom_density(
          data = input_data %>% filter(saldo <100),
          aes(x = saldo, weight = fac_per), color = "#f4785c",
          size = 2,
          fill = alpha("#f4785c", 0.58)
        ) +
        geom_segment(aes(x = 0 , y = 0, xend = 0, yend = ylim-0.0035), color = "#363f52", size = 1) +
        scale_y_continuous(limits = c(0, ylim), expand = c(0, 0)) +
        scale_x_continuous(limits = c(-40,105), expand = c(0, 0)) +
        geom_text(aes(x = 80, y = ylim - 0.0010), label = "A quienes le deben", color = "#363f52", family = "Fredoka",  fontface = "bold", size = 0.9*textFunction()) +
        geom_text(aes(x =  -20, y = ylim - 0.0010), label = "Quienes deben", color = "#f4785c", family = "Fredoka", fontface = "bold", size = 0.9*textFunction()) +
        labs(x = "Sobrecarga semanal en horas",
             y = "") +
        theme(plot.title = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(color = "#363f52", size = 3.12*textFunction(), family = "Fredoka" ),
              axis.ticks = element_blank(),
              axis.ticks.y = element_blank(),
              strip.background = element_blank(),
              panel.background = element_rect(fill = "#ffd9d8"),
              plot.background = element_rect(fill = "#ffd9d8", color = NA),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              axis.line.x = element_line(color = "#363f52", size = 1),
              axis.title = element_text(size = 3.5*textFunction(), color = "#363f52", family = "Fredoka SemiBold")) +
        geom_segment(aes(
          x = hrs_output_values()$saldo,
          xend = hrs_output_values()$saldo,
          y = 0,
          yend = ylim * 0.66666
        ),
        color = "#363f52",
        linetype = "dotted") +
        annotation_custom(sym.grob) +
        geom_text(
          aes(x = hrs_output_values()$saldo, y =  (ylim + 0.0075) * 0.66666),
          label = "Aquí estás tú",
          family = "Fredoka",
          color = "#363f52",
          size = 1*textFunction(),
          hjust="inward"
        ) +
        geom_text(
          aes(x = hrs_output_values()$saldo, y =  (ylim+0.0035) * 0.66666),
          label = paste0(round(hrs_output_values()$saldo,0), " horas semanales"),
          family = "Fredoka Bold",
          color = "#363f52",
          size = 0.85*textFunction(),
          hjust="inward"
        )


    })

output$distribucion2 <- output$distribucion <- renderPlot({dist_cuidados() }, height = plotHeight)



    dist_report <- reactive({
      sym.grob <- symbolsGrob(SVGlogo,
                              x=rescale(hrs_output_values()$saldo, from = c(-55,105)),
                              y=rescale((ylim-0.0015)*0.66666, from = c(0,ylim)),
                              size=0.17)

      ggplot() +
        geom_rect(aes(xmin = 0, xmax = Inf, ymin = 0, ymax = ylim-0.0035), fill = "#6794a1", alpha = 0.3) +
        geom_rect(aes(xmin = 0, xmax = -Inf, ymin = 0, ymax = ylim-0.0035), fill = "#f4785c",alpha = 0.3) +
        geom_density(data = input_data %>% filter(saldo <100), aes(x = saldo, weight = fac_per), color = "#f4785c", size = 2, fill = alpha("#f4785c", 0.58)) +
        geom_segment(aes(x = 0 , y = 0, xend = 0, yend = ylim-0.0035), color = "#363f52", size = 1) +
        scale_y_continuous(limits = c(0, ylim), expand = c(0, 0)) +
        scale_x_continuous(limits = c(-55,105), expand = c(0, 0)) +
        geom_text(aes(x = 80, y = ylim - 0.002), label = "A quienes le deben", color = "#363f52", family = "Fredoka",  fontface = "bold", size = 14) +
        geom_text(aes(x =  -35, y = ylim - 0.002), label = "Quienes deben", color = "#f4785c", family = "Fredoka", fontface = "bold", size = 14) +
        labs(x = "Sobrecarga semanal en horas",
             y = "") +
        geom_segment(aes(
          x = hrs_output_values()$saldo,
          xend = hrs_output_values()$saldo,
          y = 0,
          yend = ylim * 0.66666
        ),
        color = "#363f52",
        linetype = "dotted") +
        annotation_custom(sym.grob) +
        geom_text(
          aes(x = hrs_output_values()$saldo, y =  (ylim + 0.005) * 0.66666),
          label = "Aquí estás tú",
          family = "Fredoka",
          color = "#363f52",
          size = 14
        ) +
        geom_text(
          aes(x = hrs_output_values()$saldo, y =  (ylim+0.0035) * 0.66666),
          label = paste0(round(hrs_output_values()$saldo,0), " horas semanales"),
          family = "Fredoka Bold",
          color = "#363f52",
          size = 12
        ) +
        theme(plot.title = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(color = "#363f52", size = 20, family = "Fredoka" ),
              axis.ticks = element_blank(),
              axis.ticks.y = element_blank(),
              strip.background = element_blank(),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              axis.line.x = element_line(color = "#363f52", size = 1),
              axis.title = element_text(size = 20, color = "#363f52", family = "Fredoka SemiBold"))


    })


    output$distribucion_report <- renderPlot({
      dist_report()
    })

    return(list(dist_cuidados = dist_cuidados,
                dist_report = dist_report))
  })
}
