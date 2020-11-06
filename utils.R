cat("\014") 

# clear environment
clear_env = readline("Clar the R environment? (y/n) > ")

if (clear_env=="y"){
  rm(list = ls()) 
}

cat("\014")  

# preliminars
installations <- function(paquete) {
  if (!require(paquete,character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages(as.character(paquete), dependecies = TRUE, repos = "http://cran.us.r-project.org")
    library(paquete, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }
}

packages <- c('corrr', 'data.table', 'ddpcr', 'devtools', 'dplyr',  'plotly', 
             'foreign', 'GISTools', 'ggraph', 'glmnet', 'haven', 'htmlwidgets', 
             'igraph', 'linkcomm', 'maptools', 'mapview', 'network', 'RANN', 
             'raster', 'readr', 'RColorBrewer', 'rgdal', 'rgeos', 'rlist',
             'scales', 'sjlabelled', 'sf', 'sp', 'tidyverse', 'tidygraph')

lapply(packages, installations)

