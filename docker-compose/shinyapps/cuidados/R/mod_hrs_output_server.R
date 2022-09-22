require(spatstat)
#' mod_hrs_output_server Select Server Functions
#'
#' @noRd

mod_hrs_output_server <- function(id, user_values, input_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    perfiles <- input_data %>%
      group_by(region, niv, sexo, trab_dom, tiene_hij, pareja, tot_cuidado_nac_percap,) %>%
      summarise(prom_cuidado_reduced = weighted.median(hrs_cuidado, fac_per, na.rm = T),
                per_group = sum(fac_per)) %>%
      ungroup() %>%
      distinct() %>%
      mutate(saldo = prom_cuidado_reduced - tot_cuidado_nac_percap,
             place = hutils::weighted_ntile(abs(saldo), per_group, n = 3)) # Para cost_op


    hrs_user <- reactive({
     perfiles %>%
        filter(
          region == user_values$region(),
          niv == user_values$school(),
          sexo == user_values$sex(),
          trab_dom == user_values$worker(),
          tiene_hij == user_values$child(),
          pareja == user_values$partner()
        )
    })


    final <- reactive({
      if(nrow(hrs_user()) == 1){
          hrs_user()
      } else{
          if(nrow(hrs_user())<1) {
            perfiles %>%
              filter(
                niv == user_values$school(),
                sexo == user_values$sex(),
                trab_dom == user_values$worker(),
                tiene_hij == user_values$child(),
                pareja == user_values$partner(),
                region == "Nacional"
              )

          }
      }
    })

    return(final)
  })
}

