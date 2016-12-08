library("RColorBrewer")
library("gplots")

knownGenes <- as.matrix(Known.Genes)
my_palette <- colorRampPalette( rev(brewer.pal(11, "RdBu")) )(255)
cairo_pdf("PPG Known Genes.pdf", w=4, h=3)
heatmap.2( knownGenes, 
           scale="column", 
           trace="none",
           cexRow=0.5,
           cexCol=0.5,
           offsetRow = 0.01,
           offsetCol = 0.01,
           dendrogram="both", 
           col = my_palette,
           margins = c(5,3),
           key=FALSE,
           breaks = c(seq(-2, 2, length.out = 256)),
)
dev.off()


heatmap.2(as.matrix(counts), 
          key=T, 
          trace="none",
          dendrogram="column", 
          col=colorpanel(100, "red", "blue"),
          margin=c(5, 5), 
          main="Sample Distance Matrix")


