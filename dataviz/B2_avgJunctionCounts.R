#' @importFrom ggplot2 ggplot aes geom_bar labs theme_set theme element_blank coord_flip
library(ggplot2)

levels(avg_junction_reads$Group) <- c("Case", "Control")
levels(avg_junction_reads$Isoform) <- c("Long", "Short")

AvgJunctionCounts_plot <- ggplot2::ggplot(data = avg_junction_reads) +
  ggplot2::aes(x = Group, fill = Isoform, weight = Avg_Junction_Reads) +
  ggplot2::geom_bar(alpha = 0.75) +
  scale_fill_envisagenics(palette = "purples") +
  ggplot2::labs(
    x = " ",
    y = "Reads"
  ) +
  theme_set(theme_envisagenics) +
  ggplot2::theme(legend.title = element_blank()) +
  ggplot2::coord_flip()

rm(avg_junction_reads)

