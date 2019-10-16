#' @importFrom ggplot2 ggplot aes geom_tile theme_set coord_flip theme element_blank element_line labs position_nudge
#' @importFrom ggpubr stat_compare_means
library(ggplot2)

### BRCA Tumor Type
brca_psi$tumor_type <- gsub("LUMINAL", "Luminal", brca_psi$tumor_type)
brca_psi$tumor_type <- gsub("BASAL", "Basal", brca_psi$tumor_type)

tumor_type <- ggplot(data = brca_psi[-grep("N/A", brca_psi$tumor_type), ]) +
  aes(y = psi, x = tumor_type, fill = tumor_type) +
  geom_tile(alpha = 0.35, na.rm = TRUE, show.legend = FALSE, width = 0.6, height = 0.01, stat = "identity", position = "identity") +
  scale_fill_envisagenics(palette = "purples_grays_blues", reverse = TRUE) +
  scale_color_envisagenics(palette = "purples_grays_blues", reverse = TRUE) +
  theme_set(theme_envisagenics) +
  coord_flip() +
  theme(
    legend.position = "none",
    axis.line = element_blank(), axis.ticks = element_blank(), panel.grid.major.y = element_line(linetype = "dotted", color = "gray29", size = 0.1)
  ) +
  labs(x = NULL, y = "PSI") +
  stat_compare_means(method = "kruskal.test", label = "p.format", label.x.npc = "right", label.y.npc = "top", position = position_nudge(x = 0.45, y = -0.8), color = "gray29", size = 3.5)
tumor_type


### Tumor status
brca_psi$tumor_status <- gsub("TUMOR FREE", "Tumor-free", brca_psi$tumor_status)
brca_psi$tumor_status <- gsub("WITH TUMOR", "With tumor", brca_psi$tumor_status)

tumor_status <- ggplot(data = brca_psi[-grep("N/A", brca_psi$tumor_status), ]) +
  aes(y = psi, fill = tumor_status, x = tumor_status) +
  geom_tile(alpha = 0.35, na.rm = TRUE, show.legend = FALSE, width = 0.6, height = 0.01, stat = "identity", position = "identity") +
  scale_fill_envisagenics(palette = "purples_grays", reverse = TRUE) +
  scale_color_envisagenics(palette = "purples_grays", reverse = TRUE) +
  theme_set(theme_envisagenics) +
  coord_flip() +
  theme(legend.position = "none", panel.grid.major.y = element_line(linetype = "dotted", color = "gray29", size = 0.1), axis.ticks = element_blank()) +
  labs(x = NULL, y = "PSI") +
  stat_compare_means(method = "wilcox.test", label = "p.format", label.x.npc = "right", label.y.npc = "top", position = position_nudge(x = 0.4, y = -0.8), color = "gray29", size = 3.5)
tumor_status

### Vital status
vital_status <- ggplot(data = brca_psi) +
  aes(y = psi, fill = vital_status, x = vital_status) +
  geom_tile(alpha = 0.35, na.rm = TRUE, show.legend = FALSE, width = 0.6, height = 0.01, stat = "identity", position = "identity") +
  scale_fill_envisagenics(palette = "purples_grays", reverse = TRUE) +
  scale_color_envisagenics(palette = "purples_grays", reverse = TRUE) +
  theme_set(theme_envisagenics) +
  coord_flip() +
  theme(legend.position = "none", axis.line = element_blank(), axis.ticks = element_blank(), panel.grid.major.y = element_line(linetype = "dotted", color = "gray29", size = 0.1)) +
  labs(x = NULL, y = "PSI") +
  stat_compare_means(method = "wilcox.test", label = "p.format", label.x.npc = "right", label.y.npc = "top", position = position_nudge(x = 0.4, y = -0.8), color = "gray29", size = 3.5)
vital_status

rm(brca_psi)

