# Download useful packages
# Last updated: 2018-01-12

# install useful packages
installPkges <- c(
  # General tidy packages
  "tidyverse",      # packages related to tidy data
  "knitr",          # create reproducible documents
  "rmarkdown",      # convert R Markdown documents

  # Tables, summary tools, and exploration
  "tables",         # help create publication tables
  "tableone",       # table one of basic demographics or sample statistics
  "summarytools",   # quick data exploration
  "skimr",          # quick data exploration
  "Hmisc",          # miscellaneous useful functions

  # Miscellaneous
  "devtools",       # help create R packages
  "MASS",           # modern applied statistics functions
  "caret"           # powerful metapackage for predictive modeling
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
