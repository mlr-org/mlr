# from https://github.com/eddelbuettel/r-travis/blob/master/run.sh

# Set up our CRAN mirror.
sudo add-apt-repository "deb ${CRAN}/bin/linux/ubuntu $(lsb_release -cs)/"
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

# Add marutter's c2d4u repository.
#sudo add-apt-repository -y "ppa:marutter/rrutter"
#sudo add-apt-repository -y "ppa:marutter/c2d4u"

# Update after adding all repositories.
#sudo apt-get update -qq

#sudo apt-get install -y r-cran-roxygen2 r-cran-pander r-cran-purrr r-cran-mlbench r-cran-knitr r-cran-dplyr r-cran-ggplot2 r-cran-ranger r-cran-randomForest r-cran-kernlab r-cran-Rfast r-cran-igraph r-cran-rjson r-cran-rmarkdown r-cran-shiny r-cran-svglite r-cran-xgboost r-cran-xml2

WARMUPPKGS_without_binaries="mlrMBO mldr RWeka RWekajars"
Rscript -e 'pkgs = trimws(strsplit(Sys.getenv("WARMUPPKGS_without_binaries"), " ")[[1]]); pkgs = pkgs[!pkgs %in% installed.packages()]; if (length(pkgs) > 0) install.packages(pkgs)'
