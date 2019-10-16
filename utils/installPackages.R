packages <- c("AnnotationDbi", "AnnotationFilter", "bsplus", "BiocManager", "data.table", "DBI", "dbplyr", "DT", "EnsDb.Hsapiens.v86", "ensembldb", "flexdashboard", "formattable", "GenomicFeatures", "GenomicRanges", "ggiraph", "ggplot2", "ggpubr", "htmltools", "htmlwidgets", "httr", "jsonlite", "knitr", "magrittr", "plotly", "readr", "Repitools", "reshape2", "RPostgres", "RSQLite", "rtracklayer", "scales", "shiny", "TnT")

install.packages(packages, dependencies = TRUE, quiet = TRUE)
BiocManager::install(packages, ask = FALSE, dependencies = TRUE)

