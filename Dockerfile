FROM rocker/r-ver:4.3.1

MAINTAINER Anders Sundelin "epkanol@gmail.com"

# libglpk-dev is for igraph - do we really need it?

RUN apt-get update && apt-get install -y \
    cmake       \
    libssl-dev  \
    pandoc      \ 
    libglpk-dev \
    libv8-dev \
    libcurl4-openssl-dev \
    libxt6 \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -m -u 2000 app
USER app

RUN mkdir -p /home/app/R/library && mkdir /home/app/ownership

WORKDIR /home/app

RUN echo ".libPaths('/home/app/R')" >> .Rprofile && R -e "install.packages('renv')"

USER root
COPY renv.lock /home/app/renv.lock
RUN chown -R app:app /home/app/renv.lock
USER app
RUN R -e "renv::restore()"
RUN R -e 'install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")), version="2.33.1")'
RUN R -e 'cmdstanr::install_cmdstan()'

# source code changes here, after building the R image incl. renv packages and Stan
USER root
COPY study /home/app/study
RUN chown -R app:app /home/app/study

USER app
RUN mkdir -p /home/app/study/.cache

EXPOSE 3435
CMD ["R", "-e", "rmarkdown::run('study/index.Rmd', shiny_args = list(port=3435,host='0.0.0.0'))" ]
