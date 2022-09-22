
#' mod_read_enut_data_server Server Function
#'
#' @noRd

mod_read_enut_data_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    enut_reduced <- readRDS("data-raw/import-clean/out/enut.rds")

    return(enut_reduced)

  })
}
