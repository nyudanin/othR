library(ggplot2)
library(gridExtra)
library(reshape2)

ILC1genes <- as.character(degenelist[,1], na.rm=TRUE)
ILC2genes <- as.character(degenelist[,2], na.rm=TRUE)
ILC3genes <- as.character(degenelist[,3], na.rm=TRUE)

ILC1genes <- ILC1genes[which(ILC1genes %in% rownames(assay(rld)))]
ILC2genes <- ILC2genes[which(ILC2genes %in% rownames(assay(rld)))]
ILC3genes <- ILC3genes[which(ILC3genes %in% rownames(assay(rld)))]

ILC1counts <- t(counts(dds, normalized=TRUE)[ILC1genes,])
ILC1counts <- cbind2(coldata, ILC1counts)

ILC1counts <- t(counts(dds, normalized=TRUE)[ILC1genes,])
ILC2counts <- t(counts(dds, normalized=TRUE)[ILC2genes,])
ILC3counts <- t(counts(dds, normalized=TRUE)[ILC3genes,])

ILC1counts <- cbind2(coldata, ILC1counts)
ILC2counts <- cbind2(coldata, ILC2counts)
ILC3counts <- cbind2(coldata, ILC3counts)

meltedILC1 <- melt(ILC1counts, variable.name = "gene", value.name = "count" )
meltedILC2 <- melt(ILC2counts, variable.name = "gene", value.name = "count" )
meltedILC3 <- melt(ILC3counts, variable.name = "gene", value.name = "count" )

cairo_pdf(filename="ILC1 Gene Plots.pdf", w=15, h=15)
ggplot(data = meltedILC1,  aes(x = subset, y = log(count+1), fill = subset)) +
  geom_violin() +
  facet_wrap(~gene, nrow=4 ) +
  scale_fill_manual(values = c( ILC1="#C7302A", ILC2="#4266F6", ILC3="#269040", NK="#E7A626" )) +
  ggtitle("ILC1 Genes") +
  theme(axis.title.x = element_blank(),
        legend.position='none') +
  ylab("Log Normalized Counts")
dev.off()

cairo_pdf(filename="ILC2 Gene Plots.pdf", w=20, h=15)
ggplot(data = meltedILC2[meltedILC2$gene != "CAMP" & meltedILC2$gene != "PTGDR2", ],  aes(x = subset, y = log(count+1), fill = subset)) +
  geom_violin() +
  facet_wrap(~gene, nrow=5 ) +
  scale_fill_manual(values = c( ILC1="#C7302A", ILC2="#4266F6", ILC3="#269040", NK="#E7A626" )) +
  ggtitle("ILC2 Genes") +
  theme(axis.title.x = element_blank(),
        legend.position='none') +
  ylab("Log Normalized Counts")
dev.off()

cairo_pdf(filename="ILC3 Gene Plots.pdf", w=20, h=20)
ggplot(data = meltedILC3,  aes(x = subset, y = log(count+1), fill = subset)) +
  geom_violin() +
  facet_wrap(~gene, nrow=7 ) +
  scale_fill_manual(values = c( ILC1="#C7302A", ILC2="#4266F6", ILC3="#269040", NK="#E7A626" )) +
  ggtitle("ILC3 Genes") +
  theme(axis.title.x = element_blank(),
        legend.position='none') +
  ylab("Log Normalized Counts")
dev.off()





