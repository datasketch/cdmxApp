FROM rocker/r-ver:4.1.1
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libssl-dev libxml2-dev libudunits2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.9")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.7.0")'
RUN Rscript -e 'remotes::install_version("shinydisconnect",upgrade="never", version = "0.1.0")'
RUN Rscript -e 'remotes::install_version("shinybusy",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("renv",upgrade="never", version = "0.15.4")'
RUN Rscript -e 'remotes::install_version("plyr",upgrade="never", version = "1.8.7")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.8.0")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.23")'
RUN Rscript -e 'remotes::install_github("datasketch/shinypanels@8be05c0ff074000f82f5903d597ce9e0eb0fc6b2")'
RUN Rscript -e 'remotes::install_github("datasketch/shinyinvoer@dd8178db99cac78f0abbd236e83e07bf1f22ba18")'
RUN Rscript -e 'remotes::install_github("datasketch/shi18ny@37f276f1081f1f0a4c99f42f4f8038dbe999ba9a")'
RUN Rscript -e 'remotes::install_github("datasketch/parmesan@e1ae0769b2663725fc2fa29a48b89e6248be224c")'
RUN Rscript -e 'remotes::install_github("datasketch/lfltmagic@7677b096a1440ba105c67c883d1144830204923e")'
RUN Rscript -e 'remotes::install_github("datasketch/hgchmagic@1b7105c88979e44b1c45be9dee74cc63e20cd6cb")'
RUN Rscript -e 'remotes::install_github("datasketch/dsmodules@a495c845e842e3dfe1c56a6931a61b037bad8454")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN rm -rf /build_zone
RUN R -e 'renv::restore();renv::install("remotes");remotes::install_local(upgrade="never");renv::snapshot()'
EXPOSE 80
EXPOSE 8787
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');cdmxApp::run_app()"
