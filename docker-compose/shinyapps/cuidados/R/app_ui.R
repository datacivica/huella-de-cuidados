require(dplyr)
library(shinyjs)

jsCode <- "shinyjs.downloadFn = async function(){
  let element = document.createElement('a');
  element.setAttribute('href', 'www/tmp/mi-reporte.zip');
  element.setAttribute('download', 'mi-reporte.zip');

  element.style.display = 'none';
  document.body.appendChild(element);

  element.click();

  document.body.removeChild(element);
}"
#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
  tagList(
      # Leave this function for adding external resources
      golem_add_external_resources(),
      useShinyjs(),
      extendShinyjs(text = jsCode, functions = c("downloadFn")),
      # Your application UI logic
      bootstrapPage(
      htmlTemplate(filename = "inst/app/www/template.html", # template
                   # User selects personal info
                   nombre = mod_nombreSelect_ui("user"),
                   sexo = mod_sexoSelect_ui("user"),
                   pareja = mod_parejaSelect_ui("user"),
                   lugar = mod_lugarSelect_ui("user"),
                   escolaridad = mod_escolaridadSelect_ui("user"),
                   hijes = mod_hijesSelect_ui("user"),
                   trabajadora = mod_trabajadoraSelect_ui("user"),
                   # Output slides
                   # 1. Distribucion 1
                   text_mi_huella_1 = mod_text_output_1_ui("user"),
                   distribucion = mod_distribucion_ui("user"),
                   # 2. Distribucion 2
                   text_mi_huella_2 = mod_text_output_2_ui("user"),
                   distribucion_2 = mod_distribucion2_ui("user"),
                   # 3. Costo de oportunidad
                   text_bar_cost_op = mod_text_output_3_ui("user"),
                   text_bar_cost_op_mobile_1 = mod_text_output_3_mobile_1("user"),
                   text_bar_cost_op_mobile_2 = mod_text_output_3_mobile_2("user"),
                   text_bar_cost_op_mobile_3 = mod_text_output_3_mobile_3("user"),
                   cost_op = mod_bar_cost_ui("user"),
                   cost_op_mob_1 = mod_bar_cost_ui_mob_1("user"),
                   cost_op_mob_2 = mod_bar_cost_ui_mob_2("user"),
                   cost_op_mob_3 = mod_bar_cost_ui_mob_3("user"),
                   # 4. Contrafactual
                   text_contra_1 = mod_text_output_4_ui("user"),
                   contra_bells = mod_dumbbells_ui("user"),
                   contra_select = mod_contra_select_ui("user"),
                   # 5. Municipal scatter
                   scatter_select = mod_selectscatter_ui("user"),
                   scatter_select_2 = mod_selectscatter2_ui("user"),
                   scatter_muni = mod_municipalscatter_ui("user"),
                   scatter_muni_2 = mod_municipalscatter2_ui("user"),
                   scatter_text_per = mod_scatter_text_per_ui("user"),
                   scatter_text_est = mod_scatter_text_est_ui("user"),
                   scatter_text_faltan = mod_scatter_text_faltan_ui("user"),
                   # Output email
                   demand_email = mod_demand_email("user"),
                   scatter_muni = mod_municipalscatter_ui("user"),
                   # 6. Descarga tus resultados
                   reporte_ui = mod_report_ui("user"),
                   # Keep session active
                   timer = mod_timer("user")
                   ),
      theme = bs_theme(version = 5)
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )


  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Tu Huella de Cuidados"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
