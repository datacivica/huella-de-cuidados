library(shinyjs)
library(promises)
library(future)
require(dplyr)
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  autoInvalidate <- reactiveTimer(1000)
  observe({
    autoInvalidate()
    cat(".")
  })
  # Your application server logic
  enut_data <- mod_read_enut_data_server("user")
  input_values <- mod_valuesSelect_server("user")
  enut_output_values <- mod_hrs_output_server(
    "user",
    user_values = input_values,
    input_data = enut_data
  )
  #mod_distribucion_server("user", input_data = enut_data, hrs_output_values = enut_output_values)
  mod_email_details_server("user", user_values = input_values)
  enut_output_values <- mod_hrs_output_server("user",
                                              user_values = input_values,
                                              input_data = enut_data)
  plot_dist <- mod_distribucion_server("user", input_data = enut_data, hrs_output_values = enut_output_values)
  var <- mod_contra_select_server("user", user_values = input_values)
  contra_values <- mod_dumbbells_server("user",
                                        enut = enut_data,
                                        input_data = input_values,
                                        hrs_output_values  = enut_output_values,
                                        vari = var)
  mod_text_output_1_server("user",
                           user_values = input_values,
                           hrs_output_values = enut_output_values,
                           comp_values = contra_values$final)
  mod_bar_cost_server("user",
                      hrs_output_values = enut_output_values)
  mod_bar_cost_mob_server_1("user",
                            hrs_output_values = enut_output_values)
  mod_municipalscatter_server("user",
                              user_data = input_values)
  downloads <- mod_report_server("user",
                    density_plot = plot_dist,
                    contra_plot =  contra_values,
                    hrs_output_values  = enut_output_values,
                    user_values = input_values)
}
