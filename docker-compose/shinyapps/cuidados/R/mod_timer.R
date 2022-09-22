

mod_timer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$currentTime <- renderText({
      # invalidateLater causes this output to automatically
      # become invalidated when input$interval milliseconds
      # have elapsed
      invalidateLater(1000, session)

      format(Sys.time())
    })
  })
}

mod_timer <- function(id) {
  ns <- NS(id)
  fluidRow(
    h4(textOutput(ns("currentTime"), container = span))
  )
}

