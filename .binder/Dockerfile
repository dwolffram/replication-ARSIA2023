FROM rocker/binder
LABEL maintainer='Daniel Wolffram'
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/dwolffram/replication-ARSIA2023/raw/main/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-05-06/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
