FROM rocker/r-ver:4.1.0

MAINTAINER Alex Yahiaoui Martinez <yahiaoui-martinez.alex@outlook.com>

# Install dependencies
RUN apt-get update && \
  apt-get install -y -qq --no-install-recommends \
  default-jre \
  libcurl4-openssl-dev \
  libgdal-dev \
  libicu-dev \
  libssl-dev \
  make \
  unixodbc-dev \
  zlib1g-dev \
  && rm -rf /var/lib/apt/lists/*
  

# Install MySQL 8.0 Unicode Driver
ADD https://dev.mysql.com/get/Downloads/Connector-ODBC/8.0/mysql-connector-odbc-8.0.26-linux-glibc2.12-x86-64bit.tar.gz .
RUN tar -C . -xzvf mysql-connector-odbc-8.0.26-linux-glibc2.12-x86-64bit.tar.gz
RUN cp -r mysql-connector-odbc-8.0.26-linux-glibc2.12-x86-64bit/bin/* /usr/local/bin
RUN cp -r mysql-connector-odbc-8.0.26-linux-glibc2.12-x86-64bit/lib/* /usr/local/lib
RUN myodbc-installer -a -d -n "MySQL ODBC 8.0 Unicode Driver" -t "Driver=/usr/local/lib/libmyodbc8w.so"
RUN myodbc-installer -a -d -n "MySQL ODBC 8.0" -t "Driver=/usr/local/lib/libmyodbc8a.so"
  

# Install Google chrome
RUN apt-get update -qq && \
  apt-get install -y -qq --no-install-recommends \
  curl \
  libz-dev \
  libpoppler-cpp-dev \
  pandoc \
  pandoc-citeproc
        
RUN curl -L http://bit.ly/google-chrome-stable -o google-chrome-stable.deb && \
    apt-get -y install ./google-chrome-stable.deb && \
    rm google-chrome-stable.deb
    
RUN install2.r --error --deps TRUE pagedown


# Set the environment variables
COPY .Renviron .Renviron

# Install R packages
RUN R -q -e 'install.packages("remotes")'
RUN R -q -e 'remotes::install_github("geneseng/genesengApp", upgrade = "never")'

# Install Python modules
RUN R -q -e 'remotes::install_github("rstudio/reticulate@v1.27", upgrade = "never")'
RUN R -q -e 'reticulate::install_miniconda()'
RUN R -q -e 'reticulate::conda_install("r-reticulate",  c("numpy<1.24", "autogluon==0.6.2"), pip = TRUE)'
RUN R -q -e 'reticulate::conda_install("r-reticulate", "flaml==1.1.2", pip = TRUE)'
RUN R -q -e 'reticulate::conda_install("r-reticulate", "auto-sklearn", pip = TRUE)'
RUN R -q -e 'reticulate::conda_install("r-reticulate", "scikit-learn>=1.0.0,<1.2", pip = TRUE)'

EXPOSE 3838

CMD R -e 'genesengApp::geneseng_run_App()'

