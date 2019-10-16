#' @importFrom ggplot2 position_dodge2 scale_fill_manual labs theme element_blank element_line element_text theme_set coord_flip
#' @importFrom plotly ggplotly layout
#'
library(ggpubr)
library(data.table)
gtex <- t(selected_gtex[, 5:35])
gtex <- data.table(gtex, keep.rownames = TRUE)
colnames(gtex) <- c("Tissue", "Reproducibility")
gtex$Tissue <- gsub("_reproducibility", "", gtex$Tissue)
gtex$Tissue <- gsub("_", " ", gtex$Tissue)
gtex$Group <- as.factor(gtex$Tissue)
gtex <- gtex[order(gtex$Reproducibility, decreasing = TRUE), ]

gtexTissueColors <- read.csv("data/gtexTissueColors.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(gtexTissueColors) <- c("Tissue", "HEX")
gtex <- subset(gtex, gtex$Tissue %in% gtexTissueColors$Tissue)
gtex_pal <- as.list(paste0("#", gtexTissueColors$HEX))
names(gtex_pal) <- (gtexTissueColors$Tissue)
gtex_pal <- unlist(gtex_pal)
gtex_pal %>% unique() %>% length()

gtex_plot <- ggplot(gtex, aes(fill = Group)) +
  geom_col(aes(order(Tissue, Reproducibility), y = Reproducibility, fill = Group),
    position = position_dodge2(reverse = TRUE), show.legend = FALSE
  ) +
  scale_fill_manual(values = gtex_pal[gtex$Group]) +
  labs(x = as.character(gtex$Tissue), y = "% Reproducibility") +
  theme(legend.position = "None", legend.title = element_blank(), axis.title.y = element_blank()) +
  theme(
    axis.text = element_text(family = "Helvetica"),
    axis.text.x = element_text(family = "Helvetica")
  ) +
  theme_set(theme_envisagenics) +
  coord_flip()

gtex_plot <- ggplotly(gtex_plot)
gtex_plot$x$layout$font$size <- 0.5 * gtex_plot$x$layout$font$size
gtex_plot <- layout(gtex_plot, showlegend = FALSE)
gtex_plot$x$layout$yaxis$ticktext <-  as.character(gtex$Tissue)
gtex_plot$x$layout$yaxis$ticks <- gtex$Tissue
gtex_plot$x$layout$yaxis$categoryarray <- gtex$Tissue
gtex_plot$x$layout$yaxis$nticks <- length(gtex$Tissue)
gtex_plot$x$layout$yaxis$tickvals <- seq_along(gtex$Tissue)
gtex_plot$x$layout$margin$l <- 25
gtex_plot$x$layout$yaxis$showgrid <- FALSE
gtex_plot$x$layout$yaxis$showline <- TRUE
gtex_plot$x$layout$yaxis$autorange <- TRUE

gtex_plot
rm(gtexTissueColors, gtex_pal, gtex)
