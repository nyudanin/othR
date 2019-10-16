#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot guides
#' @importFrom ggiraph geom_bar_interactive
#' @importFrom ggpubr compare_means
#' @importFrom formattable formattable formatter style as.datatable

library(formattable)
library(DT)

exonTrioSeq <- t(cbind(selected_results$exon1, selected_results$exon2, selected_results$exon3))
exonTrioSeq <- as.data.frame(exonTrioSeq, stringsAsFactors = FALSE)
colnames(exonTrioSeq) <- "Sequence"
exonTrioSeq$Exon <- c("1", "2", "3")
exonTrioSeq <- exonTrioSeq[, c(2:1)]
exonTrioSeq$Exon <- as.integer(exonTrioSeq$Exon)
exonTrioSeq$Sequence <- toupper(exonTrioSeq$Sequence)

exonTable <- formattable::formattable(exonTrioSeq, list(Sequence = formatter("span",
  style = ~ formattable::style(color = ifelse(Exon == 2, "white", "#8d5393"))
)))
exonDataTable <- formattable::as.datatable(exonTable,
  extensions = "Buttons",
  rownames = FALSE,
  colnames = "",
  options = list(
    dom = "tB",
    buttons = c("csv", "copy"),
    autoWidth = TRUE,
    scrollX = TRUE,
    searching = FALSE
  ),
  class = "display"
) %>%
  DT::formatStyle("Sequence", "Exon", backgroundColor = styleEqual(c(1, 2, 3), c("none", "#8d5393", "none"))) %>%
  DT::formatStyle("Exon", color = "lightgray", backgroundColor = styleEqual(c(1, 2, 3), c("none", "#8d5393", "none")))


rm(exonTrioSeq)
