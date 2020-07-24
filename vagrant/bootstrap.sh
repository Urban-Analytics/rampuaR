
DEBIAN_FRONTEND=noninteractive add-apt-repository -y ppa:cran/travis
DEBIAN_FRONTEND=noninteractive apt-get update -qq
DEBIAN_FRONTEND=noninteractive apt-get install -y gdebi-core qpdf devscripts

curl -O https://cdn.rstudio.com/r/ubuntu-1604/pkgs/r-3.5.3_1_amd64.deb
gdebi --non-interactive r-3.5.3_1_amd64.deb 

ln -s /opt/R/3.5.3/bin/R /usr/local/bin/R
ln -s /opt/R/3.5.3/bin/Rscript /usr/local/bin/Rscript

Rscript -e "install.packages('remotes', repos='https://cran.ma.imperial.ac.uk/')"
Rscript -e "remotes::install_github('r-hub/sysreqs')"
cp -r /vagrant/* .
Rscript -e "remotes::install_deps(dependencies = TRUE)"
