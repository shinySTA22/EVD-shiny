FROM rocker/shiny:4
RUN R -e 'install.packages(c(\
                "shiny", \
                "ggplot2", \
                "plotly", \
                "rmarkdown", \
                "bslib", \
                "MASS", \
                "broom"))'

WORKDIR /home/shinyusr
COPY app.R app.R
COPY www www
COPY helper.R helper.R
CMD Rscript app.R