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
  "tableone",       # table one of basic demographics or sample statistics
  "summarytools",   # quick data exploration
  "skimr",          # quick data exploration
  "Hmisc"           # miscellaneous useful functions
)

install.packages(installPkges)

# install Bioconductor
# source: https://www.bioconductor.org/install/
#
# For R < 3.5.0
# > source("https://bioconductor.org/biocLite.R")
# > biocLite()
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install()
