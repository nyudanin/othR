library(ggplot2)
library(plotly)

avg_psi$Group <- gsub("case", "Case", avg_psi$Group)
avg_psi$Group <- gsub("ctrl", "Control", avg_psi$Group)

psi <- ggplot(avg_psi, aes(x = Group, y = PSI, fill = Group, color = Group)) +
  ggplot2::geom_violin(trim = FALSE, alpha = 0.5, scale = "area", na.rm = TRUE, show.legend = FALSE) +
  ggplot2::geom_jitter(height = 0, width = 0.1) +
  ggplot2::geom_boxplot(width = 0.1, alpha = 0.75) +
  scale_fill_envisagenics(reverse = FALSE) +
  scale_color_envisagenics(reverse = FALSE) +
  ggplot2::labs(
    x = "",
    y = "PSI"
  ) +
  ggplot2::theme_set(theme_envisagenics) +
  ggplot2::theme(legend.position = "None", panel.grid.major.y = element_line(linetype = "dotted", color = "gray29", size = 0.1)) +
  ggplot2::coord_flip()
psi_plot <- plotly::ggplotly(psi)

rm(avg_psi, psi)
