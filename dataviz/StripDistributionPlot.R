#############
library(magrittr)
library(ggplot2)
library(scales)
library(ggpubr)

purples <- c(
  "Purple" = "#8d5393",
  "Lavender" = "#b37ab5",
  "Gray" = "#BDBDBD"
)
reds <- c(
  "Peach" = "#f69788",
  "Guava" = "#c16177",
  "Pinot" = "#91425d"
)

blues <- c(
  "Teal" = "#17a2b8",
  "Blue" = "#337ab7",
  "Gray" = "#BDBDBD"
)

grays <-
  c(
    "Dark Gray" = "#58595b",
    "Gray" = "#BDBDBD",
    "Light Gray" = "#9f9f9f"
  )

allcolors <- c(
  "Purple" = "#8d5393",
  "Lavender" = "#b37ab5",
  "Gray" = "#BDBDBD",
  "Peach" = "#f69788",
  "Guava" = "#c16177",
  "Pinot" = "#91425d",
  "Teal" = "#17a2b8",
  "Blue" = "#337ab7",
  "Dark Gray" = "#58595b",
  "Light Gray" = "#9f9f9f"
)

purples_grays_blues <- c(
  "Purple" = "#8d5393",
  "Lavender" = "#b37ab5",
  "Gray" = "#BDBDBD",
  "Teal" = "#17a2b8",
  "Blue" = "#337ab7"
)

purples_blues <- c(
  "Purple" = "#8d5393",
  "Lavender" = "#b37ab5",
  "Teal" = "#17a2b8",
  "Blue" = "#337ab7"
)
purples_grays <- c(
  "Purple" = "#8d5393",
  "Lavender" = "#b37ab5",
  "Gray" = "#BDBDBD",
  "Light Gray" = "#9f9f9f"
)


envisagenics_palettes <-
  list(
    purples,
    reds,
    blues,
    grays,
    purples_grays_blues,
    purples_blues,
    purples_grays,
    allcolors
  )

names(envisagenics_palettes) <-
  c(
    "purples",
    "reds",
    "blues",
    "grays",
    "purples_grays_blues",
    "purples_blues",
    "purples_grays",
    "allcolors"
  )

rm(
  purples,
  reds,
  blues,
  grays,
  purples_grays_blues,
  purples_blues,
  purples_grays,
  allcolors
)


envisagenics_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (envisagenics_palettes)

  envisagenics_palettes[cols]
}

envisagenics_pal <-
  function(palette = "purples",
           reverse = FALSE,
           ...) {
    pal <- envisagenics_palettes[[palette]]

    if (reverse)
      pal <- rev(pal)

    colorRampPalette(pal, ...)
  }

#' Color scale constructor for envisagenics colors
scale_color_envisagenics <-
  function(palette = "purples",
           discrete = TRUE,
           reverse = FALSE,
           ...) {
    pal <- envisagenics_pal(palette = palette, reverse = reverse)

    if (discrete) {
      discrete_scale("color", paste0("envisagenics_", palette), palette = pal, ...)
    } else {
      scale_color_gradientn(colours = pal(256), ...)
    }
  }

#' Fill scale constructor for envisagenics colors
scale_fill_envisagenics <-
  function(palette = "purples",
           discrete = TRUE,
           reverse = FALSE,
           ...) {
    pal <- envisagenics_pal(palette = palette, reverse = reverse)

    if (discrete) {
      discrete_scale("fill", paste0("envisagenics_", palette), palette = pal, ...)
    } else {
      scale_fill_gradient(colours = pal(256), ...)
    }
  }

##%######################################################%##
#                                                          #
####                    Custom theme                    ####
#                                                          #
##%######################################################%##


theme_envisagenics <- theme_get()
theme_envisagenics <- theme_replace() + theme_pubclean()
theme_envisagenics <- theme_replace() +
  theme(
    plot.subtitle = element_text(colour = NA, family = "Helvetica"),
    plot.caption = element_text(colour = NA, family = "Helvetica",
                                hjust = 0),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", size = 0.1, color = "gray29"),
    panel.grid.minor = element_line(colour = NA,
                                    size = 0),
    axis.title = element_text(family = "Helvetica",
                              size = 12,
                              colour = "gray29",
                              hjust = 0.55
    ),
    axis.text = element_text(size = 10, family = "Helvetica"),
    axis.text.x = element_text(size = 10,
                               vjust = 0, family = "Helvetica"),
    axis.text.y = element_text(size = 10,
                               vjust = 0, family = "Helvetica"),
    plot.title = element_text(
      size = 12,
      colour = "gray29",
      vjust = 0, family = "Helvetica"
    ),
    legend.text = element_text(colour = "gray29", family = "Helvetica"),
    legend.title = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.key = element_rect(colour = NA),
    legend.background = element_rect(fill = NA),
    strip.background = element_blank(),
    strip.text.x = element_text(colour = "gray29", family = "Helvetica")
  )
theme_set(theme_envisagenics)
##########################

values <- seq(1:10)
groups <- c(rep("A", 5), rep("B", 5)) %>% as.factor()
df <- cbind.data.frame(groups, values)

stripdist_plot <- ggplot(data = df) +
  aes(y = values, x = groups, fill = groups) +
  geom_tile(alpha = 0.35, na.rm = TRUE, show.legend = FALSE, width = 0.6, height = 0.01, stat = "identity", position = "identity") +
  scale_fill_envisagenics(palette = "purples_grays_blues", reverse = TRUE) +
  scale_color_envisagenics(palette = "purples_grays_blues", reverse = TRUE) +
  theme_set(theme_envisagenics) +
  coord_flip() +
  theme(
    legend.position = "none",
    axis.line = element_blank(), axis.ticks = element_blank(), panel.grid.major.y = element_line(linetype = "dotted", color = "gray29", size = 0.1)
  ) +
  labs(x = "Groups", y = "Values")
  # stat_compare_means(method = "kruskal.test", label = "p.format", label.x.npc = "right", label.y.npc = "top", position = position_nudge(x = 0.45, y = -0.8), color = "gray29", size = 3.5)

stripdist_plot

