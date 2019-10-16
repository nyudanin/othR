#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot guides
#' @importFrom ggiraph geom_bar_interactive
#' @importFrom ggpubr compare_means
#' @importFrom formattable formattable formatter style as.datatable
#'
# library(ggplot2)
# library(formattable)
# library(DT)

junction_counts$Sample <- as.factor(junction_counts$Sample)
junction_counts$Type <- gsub("jin", "Spliced in", junction_counts$Type)
junction_counts$Type <- gsub("jot", "Spliced out", junction_counts$Type)
junction_counts$Type <- as.factor(junction_counts$Type)
junction_counts$Group <- as.factor(junction_counts$Group)
junction_counts <- junction_counts[order(junction_counts$Group), ]
junction_counts <- junction_counts %>% dplyr::mutate(Value = ifelse(Type == "Spliced in", Value, Value * -1))
junction_counts$GroupType <- as.factor(paste0(junction_counts$Group, "-", junction_counts$Type))

junctionCounts_plot <- ggplot2::ggplot(data = junction_counts) +
  aes(x = Sample, fill = GroupType, weight = Value, tooltip = paste(GroupType, Sample, sep = " ")) +
  geom_bar(position = "identity", na.rm = TRUE, stat = "count") +
  ggplot2::scale_y_continuous(breaks = pretty(junction_counts$Value), labels = abs(pretty(junction_counts$Value))) +
  ggplot2::facet_grid(rows = dplyr::vars(junction_counts$Group), scales = "free", as.table = TRUE, space = "free", shrink = FALSE) +
  scale_fill_envisagenics(palette = "purples_grays", reverse = FALSE) +
  theme_set(theme_envisagenics) +
  ggplot2::theme(legend.title = element_blank()) +
  coord_flip() +
  labs(x = NULL, y = "Count") +
  ggplot2::theme(
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(colour = NA),
    legend.position = "top",
    strip.text.y = element_blank(),
    axis.text.y = element_text(size = 6, vjust = 0, family = "Helvetica", color = "gray29")
  )

junctionCounts_plot <- junctionCounts_plot +
  ggiraph::geom_bar_interactive() +
  ggplot2::guides(fill = "none")

junctionCountsGroup_table <- ggpubr::compare_means(Value ~ GroupType, junction_counts, group.by = "Group", paired = TRUE)
junctionCountsType_table <- ggpubr::compare_means(Value ~ GroupType, junction_counts, group.by = "Type", paired = FALSE)


junctionCountsGroup_table <- formattable::formattable(junctionCountsGroup_table[, c(1, 3:9)], list(
  Group = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393"))),
  group1 = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393"))),
  group2 = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393"))),
  p = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393"))),
  p.adj = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393"))),
  p.format = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393"))),
  p.signif = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393"))),
  method = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393")))
))

junctionCountsGroup_table <- formattable::as.datatable(junctionCountsGroup_table,
  extensions = "Buttons",
  rownames = FALSE,
  options = list(
    dom = "tB",
    buttons = c("csv", "copy"),
    searching = FALSE
  ),
  class = "display"
)

junctionCountsType_table <- formattable::formattable(junctionCountsType_table[, c(1, 3:9)], list(
  Type = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393"))),
  group1 = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393"))),
  group2 = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393"))),
  p = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393"))),
  p.adj = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393"))),
  p.format = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393"))),
  p.signif = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393"))),
  method = formattable::formatter("span", style = ~ formattable::style(color = ifelse(p.signif == "ns", "", "#8d5393")))
))

junctionCountsType_table <- formattable::as.datatable(junctionCountsType_table,
  extensions = "Buttons",
  rownames = FALSE,
  options = list(
    dom = "tB",
    buttons = c("csv", "copy"),
    searching = FALSE
  ),
  class = "display"
)

