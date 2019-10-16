
source('/mnt/nfs1/SpliceViz/diseaseEnrichment/R/plotTheme.R')
if(!interactive()) {
  input <- as.character(readline("Enter the gene list file path: "))
  assertthat::assert_that(assertthat::has_extension(input, "txt"), msg = "Gene list must be provided as a text file ending in .txt \n")
  deGeneList <-
    utils::read.table(
      input,
      quote = "\"",
      comment.char = "",
      stringsAsFactors = FALSE
    )
}


gene <- deGeneList$V1
entrez <- org.Hs.eg.db::org.Hs.eg.db
columns <- AnnotationDbi::columns(entrez)[grep("ENTREZ|ENSEMBL", AnnotationDbi::columns(entrez))]
entrez_geneId <- AnnotationDbi::select(entrez,
                                       keys = gene,
                                       keytype = "SYMBOL",
                                       columns = columns)
entrez_geneId <- unique(entrez_geneId$ENTREZID)

entrez_de <- DOSE::enrichDO(
  gene  = entrez_geneId,
  ont           = "DO",
  pvalueCutoff  = 0.05,
  pAdjustMethod = "BH",
  minGSSize     = 5,
  maxGSSize     = 500,
  qvalueCutoff  = 0.2,
  readable      = TRUE
)

entrez_ncg <- DOSE::enrichNCG(
  entrez_geneId,
  pvalueCutoff  = 1,
  pAdjustMethod = "BH",
  minGSSize     = 0,
  maxGSSize     = 500,
  qvalueCutoff  = 1,
  readable      = TRUE
)

entrez_dgn <- DOSE::enrichDGN(
  entrez_geneId,
  pvalueCutoff  = 0.05,
  pAdjustMethod = "BH",
  minGSSize     = 5,
  maxGSSize     = 500,
  qvalueCutoff  = 0.2,
  readable      = TRUE
)

##DGN
pdf(paste0(gsub(".txt", "_dgn_cnetplot.pdf", geneListFile)), w = 6, h = 6)
clusterProfiler::cnetplot(entrez_dgn, layout = "kk") +
  theme_void(base_size = 8, base_family = "Helvetica") +
  scale_color_envisagenics(discrete = TRUE) +
  theme(legend.position = "none")
dev.off()

pdf(paste0(gsub(".txt", "_dgn_dotplot.pdf", geneListFile)))
clusterProfiler::dotplot(entrez_dgn,
                         x = "Count",
                         showCategory = 30,
                         color = "p.adjust") +
  theme_set(theme_envisagenics) +
  scale_color_envisagenics(discrete = FALSE) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    axis.title.x = element_text(size = 10)
  )
dev.off()

pdf(paste0(gsub(".txt", "_dgn_heatplot.pdf", geneListFile)))
clusterProfiler::heatplot(entrez_dgn, showCategory = 30) +
  geom_tile(
    aes(fill = "p.adjust"),
    show.legend = FALSE,
    color = envisagenics_cols("Purple"),
    fill = envisagenics_cols("Lavender")
  ) +
  theme_envisagenics(base_size = 8, base_family = "Helvetica") +
  theme(axis.text.x = element_text(
    angle = -90,
    size = 5,
    hjust = 0
  ))
dev.off()

pdf(paste0(gsub(".txt", "_dgn_emapplot.pdf", geneListFile)))
clusterProfiler::emapplot(entrez_dgn, showCategory = 30, color = "p.adjust") +
  theme_void(base_size = 8, base_family = "Helvetica") +
  scale_color_envisagenics(discrete = FALSE) +
  theme(legend.position = "right")
dev.off()

pdf(paste0(gsub(".txt", "_dgn_pubmed.pdf", geneListFile)))
enrichplot::pmcplot(entrez_dgn@result$Description[1:10], 2014:2019, proportion = FALSE) +
  theme_set(theme_envisagenics) +
  scale_color_envisagenics(palette = "allcolors", discrete = TRUE)
dev.off()

## DE
pdf(paste0(gsub(".txt", "_de_cnetplot.pdf", geneListFile)), w = 6, h = 6)
clusterProfiler::cnetplot(entrez_de, layout = "kk") +
  theme_void(base_size = 8, base_family = "Helvetica") +
  scale_color_envisagenics(discrete = TRUE) +
  theme(legend.position = "none")
dev.off()

pdf(paste0(gsub(".txt", "_de_dotplot.pdf", geneListFile)))
clusterProfiler::dotplot(entrez_de,
                         x = "Count",
                         showCategory = 30,
                         color = "p.adjust") +
  theme_set(theme_envisagenics) +
  scale_color_envisagenics(discrete = FALSE) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    axis.title.x = element_text(size = 10)
  )
dev.off()

pdf(paste0(gsub(".txt", "_de_heatplot.pdf", geneListFile)))
clusterProfiler::heatplot(entrez_de, showCategory = 30) +
  geom_tile(
    aes(fill = "p.adjust"),
    show.legend = FALSE,
    color = envisagenics_cols("Purple"),
    fill = envisagenics_cols("Lavender")
  ) +
  theme_envisagenics(base_size = 8, base_family = "Helvetica") +
  theme(axis.text.x = element_text(
    angle = -90,
    size = 5,
    hjust = 0
  ))
dev.off()

pdf(paste0(gsub(".txt", "_de_emapplot.pdf", geneListFile)))
clusterProfiler::emapplot(entrez_de, showCategory = 30, color = "p.adjust") +
  theme_void(base_size = 8, base_family = "Helvetica") +
  scale_color_envisagenics(discrete = FALSE) +
  theme(legend.position = "right")
dev.off()

pdf(paste0(gsub(".txt", "_de_pubmed.pdf", geneListFile)))
enrichplot::pmcplot(entrez_de@result$Description[1:10], 2014:2019, proportion = FALSE) +
  theme_set(theme_envisagenics) +
  scale_color_envisagenics(palette = "allcolors", discrete = TRUE)
dev.off()

## NCG
pdf(paste0(gsub(".txt", "_ncg_cnetplot.pdf", geneListFile)), w = 6, h = 6)
clusterProfiler::cnetplot(entrez_ncg, layout = "kk") +
  theme_void(base_size = 8, base_family = "Helvetica") +
  scale_color_envisagenics(discrete = TRUE) +
  theme(legend.position = "none")
dev.off()

pdf(paste0(gsub(".txt", "_ncg_dotplot.pdf", geneListFile)))
clusterProfiler::dotplot(entrez_ncg,
                         x = "Count",
                         showCategory = 30,
                         color = "p.adjust") +
  theme_set(theme_envisagenics) +
  scale_color_envisagenics(discrete = FALSE) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    axis.title.x = element_text(size = 10)
  )
dev.off()

pdf(paste0(gsub(".txt", "_ncg_heatplot.pdf", geneListFile)))
clusterProfiler::heatplot(entrez_ncg, showCategory = 30) +
  geom_tile(
    aes(fill = "p.adjust"),
    show.legend = FALSE,
    color = envisagenics_cols("Purple"),
    fill = envisagenics_cols("Lavender")
  ) +
  theme_envisagenics(base_size = 8, base_family = "Helvetica") +
  theme(axis.text.x = element_text(
    angle = -90,
    size = 5,
    hjust = 0
  ))
dev.off()

pdf(paste0(gsub(".txt", "_ncg_emapplot.pdf", geneListFile)))
clusterProfiler::emapplot(entrez_ncg, showCategory = 30, color = "p.adjust") +
  theme_void(base_size = 8, base_family = "Helvetica") +
  scale_color_envisagenics(discrete = FALSE) +
  theme(legend.position = "right")
dev.off()

pdf(paste0(gsub(".txt", "_ncg_pubmed.pdf", geneListFile)))
enrichplot::pmcplot(entrez_ncg@result$Description[1:10], 2014:2019, proportion = FALSE) +
  theme_set(theme_envisagenics) +
  scale_color_envisagenics(palette = "allcolors", discrete = TRUE)
dev.off()

write.csv2(ggplot2::fortify(entrez_dgn, order = TRUE, showCategory = 100),
           file = paste0(gsub(".txt", "_dgn.txt", geneListFile)))
write.csv2(ggplot2::fortify(entrez_de, order = TRUE, showCategory = 100),
           file = paste0(gsub(".txt", "_dgn.txt", geneListFile)))
write.csv2(ggplot2::fortify(entrez_ncg, order = TRUE, showCategory = 100),
           file = paste0(gsub(".txt", "_dgn.txt", geneListFile)))
