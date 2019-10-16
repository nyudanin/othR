source('.Rprofile')
readRenviron(".Renviron")

#' Pipe operator
#'
#'
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

source("R/00_functions.R")
envisagenics_palettes <- makeEnvisagenicsPalettes()
theme_envisagenics <- createEnvisagenicsTheme()
source("R/A1_loadSQL.R")





