require(magick)
require(shiny)
require(ggplot2)
require(dplyr)
require(tidyr)
require(cowplot)
require(here)
require(png)
require(ggtext)
require(stringr)

fbold <- here("inst", "app", "www", "Fredoka", "Fredoka-Bold.ttf")
flight <- here("inst", "app", "www", "Fredoka", "Fredoka-Light.ttf")
fmedium <- here("inst", "app", "www", "Fredoka", "Fredoka-Medium.ttf")
fregular <- here("inst", "app", "www", "Fredoka", "Fredoka-Regular.ttf")
fsemibold <- here("inst", "app", "www", "Fredoka", "Fredoka-SemiBold.ttf")

dir.create('~/.fonts')
file.copy(fbold, "~/.fonts")
file.copy(flight, "~/.fonts")
file.copy(fmedium, "~/.fonts")
file.copy(fregular, "~/.fonts")
file.copy(fsemibold, "~/.fonts")
system('fc-cache -f ~/.fonts')

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

mod_bar_cost_ui <- function(id){
  ns <- NS(id)
  plotOutput(outputId = ns("bar_ilust"))
}


mod_bar_cost_ui_mob_1 <- function(id){
  ns <- NS(id)
  plotOutput(outputId = ns("bar_ilust_mob_1"))
}

mod_bar_cost_ui_mob_2 <- function(id){
  ns <- NS(id)
  plotOutput(outputId = ns("bar_ilust_mob_2"))
}

mod_bar_cost_ui_mob_3 <- function(id){
  ns <- NS(id)
  plotOutput(outputId = ns("bar_ilust_mob_3"))
}

mod_bar_cost_server <- function(id, hrs_output_values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # Store in a convenience variable
    cdata <- session$clientData
    
    # Values from cdata returned as text
    output$clientdataText <- renderText({
      cnames <- names(cdata)
      
      allvalues <- lapply(cnames, function(name) {
        paste(name, cdata[[name]], sep = " = ")
      })
      paste(allvalues, collapse = "\n")
    })
    
    
    plotWidth <- reactive({
      session$clientData[["output_user-bar_ilust_width"]]
    })
    
    plotHeight <- function() {
      width <- plotWidth()
      imgHeight <- ifelse(width <= 768, 0.65, 0.80)
      return(imgHeight)
    }
    
    labSize <- function() {
      width <- plotWidth()
      imgHeight <- ifelse(width <= 1020, 3, 6.5)
      return(imgHeight)
    }
    
    
    x <- rep(1:35, 20)
    dummy <- as.data.frame(matrix(x, ncol = 20)) %>%
      mutate(V2 = 35)
    
    
    labs <- reactive({
      dummy %>%
        ggplot(aes(V1, V2)) +
        theme_void() +
        geom_richtext(aes(x = 5.5,
                          y = 31),
                      label = ifelse(hrs_output_values()$saldo<0,
                                     paste("<b><span style ='color:#373f50'>No cotizó</span><span style = 'color:#e27f63'>", round(((abs(hrs_output_values()$saldo)*52)/45)), "SEMANAS</span><br><span style ='color:#373f50'>en el IMSS o </span></b>"),
                                     paste("<b><span style ='color:#373f50'>Cotizar</span><span style = 'color:#e27f63'>", round(((abs(hrs_output_values()$saldo)*52)/45)), "SEMANAS</span><br><span style ='color:#373f50'>en el IMSS o </span></b>")
                      ),
                      fill = "white", label.color = "#363f52", family = "Fredoka", size = 5.8, label.padding = unit(c(1, 2, 1, 1), "lines")) +
        geom_richtext(aes(x = 18,
                          y = 31),
                      label = ifelse(hrs_output_values()$saldo<0,
                                     paste("<b><span style ='color:#373f50'>No vió</span><span style = 'color:#e27f63'>", round(((abs(hrs_output_values()$saldo*52))/2)), "<br>PELÍCULAS </span><span style = 'color:#373f50'>          o</span></b>"),
                                     paste("<b><span style ='color:#373f50'>Ver</span><span style = 'color:#e27f63'>", round(((abs(hrs_output_values()$saldo*52))/2)), "<br>PELÍCULAS </span><span style = 'color:#373f50'>          o</span></b>")
                      )
                      ,
                      fill = "white", label.color = "#363f52", family = "Fredoka", size = 5.8, label.padding = unit(c(1, 1.5, 1, 1), "lines")) +
        geom_richtext(aes(x = 30.5,
                          y = 31),
                      label = ifelse(hrs_output_values()$saldo<0,
                                     paste("<b><span style ='color:#373f50'>No pasó</span><span style = 'color:#e27f63'>", round(abs(hrs_output_values()$saldo*52)), "HORAS</span><br><span style = 'color:#373f50'>con sus amistades</span></b>"),
                                     paste("<b><span style ='color:#373f50'>Pasar</span><span style = 'color:#e27f63'>", round(abs(hrs_output_values()$saldo*52)), "HORAS</span><br><span style = 'color:#373f50'>con tus amistades</span></b>")
                      )
                      ,
                      fill = "white", label.color = "#363f52",  family = "Fredoka", size = 5.8, label.padding = unit(c(1, 1.5, 1, 1), "lines")) +
        xlim(c(1,35)) +
        ylim(c(1,33)) +
        theme(panel.background = element_rect(fill = '#ffd9d8', colour = '#ffd9d8')) })
    
    
    output$bar_ilust <- renderPlot(
      
      if(hrs_output_values()$place == 1){
        ggdraw(labs()) +
          draw_image(here("data-raw", "assets-graphs",
                          "dinero-pila-chica_Sombra.png"),
                     x = -0.35, y = -0.095, scale = 0.62) +
          draw_image(here("data-raw", "assets-graphs", "peliculas-tira-corta_Sombra.png"),
                     x = 0, y =  -0.095, scale = 0.62)+
          draw_image(here("data-raw", "assets-graphs", "tazas-pila-chica_Sombra.png"),
                     x = 0.35, y = -0.095, scale = 0.62)
      } else(
        if(hrs_output_values()$place == 2){
          ggdraw(labs()) +
            draw_image(here("data-raw", "assets-graphs",
                            "dinero-pila-mediana_Sombra.png"),
                       x = -0.35, y = -0.095, scale = 0.62) +
            draw_image(here("data-raw", "assets-graphs", "peliculas-tira-mediana_Sombra.png"),
                       x = 0, y =  -0.095, scale = 0.62)+
            draw_image(here("data-raw", "assets-graphs", "tazas-pila-mediana_Sombra.png"),
                       x = 0.35, y = -0.095, scale = 0.62)
          
        } else(
          if(hrs_output_values()$place == 3){
            ggdraw(labs()) +
              draw_image(here("data-raw", "assets-graphs",
                              "dinero-pila-alta_Sombra.png"),
                         x = -0.35, y = -0.095, scale = 0.62) +
              draw_image(here("data-raw", "assets-graphs", "peliculas-tira-larga_Sombra.png"),
                         x = 0, y =  -0.095, scale = 0.62)+
              draw_image(here("data-raw", "assets-graphs", "tazas-pila-alta_Sombra.png"),
                         x = 0.35, y =  -0.095, scale = 0.62)
            
          }
        )
      )
    )
    
  })
}

dummy_mob <- as.data.frame(matrix(rep(1:12,2), ncol = 2)) %>%
  mutate(V2 = 35)




mod_bar_cost_mob_server_1 <- function(id, hrs_output_values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    labs_mobile <- reactive({
      dummy_mob %>%
        ggplot(aes(V1, V2)) +
        theme_void() +
        xlim(c(1,12)) +
        ylim(c(1,35)) +
        theme(panel.background = element_rect(fill = '#ffd9d8', colour = '#ffd9d8')) })
    
    rich_lab_1 <- reactive({
      labs_mobile() + geom_richtext(aes(x = 6.5,
                                        y = 32.5),
                                    label = ifelse(hrs_output_values()$saldo<0,
                                                   paste("<b><span style ='color:#373f50'>No cotizó</span><span style = 'color:#e27f63'>", round(((abs(hrs_output_values()$saldo)*52)/45)), "SEMANAS</span><br><span style ='color:#373f50'>en el IMSS o</span></b>"),
                                                   paste("<b><span style ='color:#373f50'>Cotizar</span><span style = 'color:#e27f63'>", round(((abs(hrs_output_values()$saldo)*52)/45)), "SEMANAS</span><br><span style ='color:#373f50'>en el IMSS o</span></b>")
                                    ),
                                    fill = "white", label.color = "#363f52", family = "Fredoka", size = 6, label.padding = unit(c(1, 2, 1, 1), "lines"))
      
    })
    
    rich_lab_2 <-
      reactive({
        labs_mobile() + geom_richtext(aes(x = 6.5,
                                          y = 32.5),
                                      label = ifelse(hrs_output_values()$saldo<0,
                                                     paste("<b><span style ='color:#373f50'>No vió</span><span style = 'color:#e27f63'>", round(((abs(hrs_output_values()$saldo*52))/2)), "<br>PELÍCULAS     </span><span style = 'color:#373f50'>          o</span></b>"),
                                                     paste("<b><span style ='color:#373f50'>Ver </span><span style = 'color:#e27f63'>", round(((abs(hrs_output_values()$saldo*52))/2)), "<br> PELÍCULAS     </span><span style = 'color:#373f50'>          o</span></b>")
                                      ) ,
                                      fill = "white", label.color = "#363f52", family = "Fredoka", size = 6, label.padding = unit(c(1, 2, 1, 1), "lines"))
        
      })
    
    rich_lab_3 <-
      reactive({
        labs_mobile() + geom_richtext(aes(x = 6.5,
                                          y = 32.5),
                                      label = ifelse(hrs_output_values()$saldo<0,
                                                     paste("<b><span style ='color:#373f50'>No pasó</span><span style = 'color:#e27f63'>", round(abs(hrs_output_values()$saldo*52)), "HORAS</span><br><span style = 'color:#373f50'>con sus amistades</span></b>"),
                                                     paste("<b><span style ='color:#373f50'>Pasar</span><span style = 'color:#e27f63'>", round(abs(hrs_output_values()$saldo*52)), "HORAS</span><br><span style = 'color:#373f50'>con tus amistades</span></b>")
                                      ),
                                      fill = "white", label.color = "#363f52", family = "Fredoka", size = 6, label.padding = unit(c(1, 2, 1, 1), "lines"))
        
      })
    
    output$bar_ilust_mob_1 <- renderPlot(
      if(hrs_output_values()$place == 1){
        ggdraw(rich_lab_1()) +
          draw_image(here("data-raw", "assets-graphs",
                          "dinero-pila-chica_Sombra.png"),
                     x = 0, y = -0.095, scale = 0.8)
      } else(
        if(hrs_output_values()$place == 2){
          ggdraw(rich_lab_1()) +
            draw_image(here("data-raw", "assets-graphs",
                            "dinero-pila-mediana_Sombra.png"),
                       x = 0, y = -0.095, scale = 0.8)
          
        } else(
          if(hrs_output_values()$place == 3){
            ggdraw(rich_lab_1()) +
              draw_image(here("data-raw", "assets-graphs",
                              "dinero-pila-alta_Sombra.png"),
                         x = 0, y = -0.095, scale = 0.8)
            
          }
        )
      )
    )
    
    output$bar_ilust_mob_2 <- renderPlot(
      if(hrs_output_values()$place == 1){
        ggdraw(rich_lab_2()) +
          draw_image(here("data-raw", "assets-graphs",
                          "peliculas-tira-corta_Sombra.png"),
                     x = 0, y = -0.095, scale = 0.8)
      } else(
        if(hrs_output_values()$place == 2){
          ggdraw(rich_lab_2()) +
            draw_image(here("data-raw", "assets-graphs",
                            "peliculas-tira-mediana_Sombra.png"),
                       x = 0, y = -0.095, scale = 0.8)
          
        } else(
          if(hrs_output_values()$place == 3){
            ggdraw(rich_lab_2()) +
              draw_image(here("data-raw", "assets-graphs",
                              "peliculas-tira-larga_Sombra.png"),
                         x = 0, y = -0.095, scale = 0.8)
            
          }
        )
      )
    )
    
    output$bar_ilust_mob_3 <- renderPlot(
      if(hrs_output_values()$place == 1){
        ggdraw(rich_lab_3()) +
          draw_image(here("data-raw", "assets-graphs",
                          "tazas-pila-chica_Sombra.png"),
                     x = 0, y = -0.095, scale = 0.8)
      } else(
        if(hrs_output_values()$place == 2){
          ggdraw(rich_lab_3()) +
            draw_image(here("data-raw", "assets-graphs",
                            "tazas-pila-mediana_Sombra.png"),
                       x = 0, y = -0.095, scale = 0.8)
          
        } else(
          if(hrs_output_values()$place == 3){
            ggdraw(rich_lab_3()) +
              draw_image(here("data-raw", "assets-graphs",
                              "tazas-pila-alta_Sombra.png"),
                         x = 0, y = -0.095, scale = 0.8)
            
          }
        )
      )
    )
  })
}


ui <- fluidPage(
  mod_bar_cost_ui("hol")
)

server <- function(input, output, session) {
  mod_bar_cost_server("hol")
}

shinyApp(ui, server)


