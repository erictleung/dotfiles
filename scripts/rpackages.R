# Download useful packages
# Last updated: 2018-01-12

# install useful packages
installPkges <- c(
  "tidyverse",      # packages related to tidy data
  "knitr",          # create reproducible documents
  "rmarkdown",      # convert R Markdown documents
  "tables",         # help create publication tables
  "MASS",           # modern applied statistics functions
  "devtools",       # help create R packages
  "caret"           # powerful metapackage for predictive modeling
)

install.packages(installPkges)

# install Bioconductor
# source: https://www.bioconductor.org/install/
source("https://bioconductor.org/biocLite.R")
biocLite()
