FROM rocker/shiny:latest

# TODO: be smarter (read this list from somewhere, or install on package install?)
RUN R -e "install.packages(c('tidyverse', 'DT'))"

COPY . /opt/glicko

RUN R CMD INSTALL --no-multiarch --with-keep.source /opt/glicko

COPY ./app/ /srv/shiny-server/pingpong/
