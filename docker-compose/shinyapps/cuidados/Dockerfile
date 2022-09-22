FROM rocker/r-ver:4.1.2
RUN apt-get update
RUN apt-get update
RUN apt-get update
RUN apt-get install -y sudo git-core libcurl4-openssl-dev libnlopt-dev libgit2-dev libicu-dev libssl-dev libxml2-dev librsvg2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN apt-get update
RUN apt-get update
RUN apt-get update
RUN apt-get install -y r-base-dev libssl-dev libmagick++-6.q16-dev && rm -rf /var/lib/apt/lists/*

RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.2.1")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.30")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.0.2")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.2")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.6")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_github("wilkelab/gridtext")'
RUN Rscript -e "install.packages(c('shinyjs', 'remotes'), repos='https://cloud.r-project.org/')"

# App packages
RUN Rscript -e 'install.packages("future")'
RUN Rscript -e 'install.packages("promises")'
RUN Rscript -e 'install.packages("dplyr")'
RUN Rscript -e 'install.packages("rsvg")'
RUN Rscript -e 'install.packages("grImport2")'
RUN Rscript -e 'install.packages("scales")'
RUN Rscript -e 'install.packages("grid")'
RUN Rscript -e 'install.packages("gridExtra")'
RUN Rscript -e 'install.packages("data.table")'
RUN Rscript -e 'install.packages("ggplot2")'
RUN Rscript -e 'install.packages("tidyverse")'
RUN Rscript -e 'install.packages("hutils")'
RUN Rscript -e 'install.packages("cowplot")'
RUN Rscript -e 'install.packages("ggtext")'
RUN Rscript -e 'install.packages("here")'
RUN Rscript -e 'install.packages("png")'
RUN Rscript -e 'install.packages("magick")'
RUN Rscript -e 'install.packages("shinycustomloader")'
RUN Rscript -e 'install.packages("ggrepel")'
RUN Rscript -e 'install.packages("spatstat")'

ARG CACHEBUST=1

RUN mkdir $HOME/build_zone
RUN chmod -R 755 $HOME/build_zone
ADD . /root/build_zone/app
# make all app files readable (solves issue when dev in Windows, but building in Ubuntu)
RUN chmod -R 755 $HOME/build_zone/app

# setting locale encoding and time
RUN sed -i '/es_MX.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen

ENV LANG="es_MX.UTF-8"
ENV LANGUAGE=
ENV LC_CTYPE="es_MX.UTF-8"
ENV LC_NUMERIC="es_MX.UTF-8"
ENV LC_TIME="es_MX.UTF-8"
ENV LC_COLLATE="es_MX.UTF-8"
ENV LC_MONETARY="es_MX.UTF-8"
ENV LC_MESSAGES="es_MX.UTF-8"
ENV LC_PAPER="es_MX.UTF-8"
ENV LC_NAME="es_MX.UTF-8"
ENV LC_ADDRESS="es_MX.UTF-8"
ENV LC_TELEPHONE="es_MX.UTF-8"
ENV LC_MEASUREMENT="es_MX.UTF-8"
ENV LC_IDENTIFICATION="es_MX.UTF-8"

# Use port 3838 inside container and run shiny
EXPOSE 3838
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');shiny::runApp('$HOME/build_zone/app/')"
