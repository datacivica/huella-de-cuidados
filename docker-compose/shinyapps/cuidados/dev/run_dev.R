# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(
  shiny.port = httpuv::randomPort(),
  shiny.autoreload = TRUE
)

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Paquetes
#
library(shiny)
library(ggplot2)
library(magrittr)
library(data.table)
library(tidyr)
library(stringr)
library(dplyr)
require(shinycustomloader)
library(cowplot)
library(here)
library(png)
library(ggtext)
library(grid)
library(grImport2)
library(rsvg)
library(scales)
library(hutils)
library(lubridate)
library(ggrepel)

# Run the application
run_app()



