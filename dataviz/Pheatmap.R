library(pheatmap)
library(RColorBrewer)



load("~/OneDrive - Envisagenics/Code/Ninemers/pepdata.RData")
pepcols <- pepdata[,1:4]
pepmat <- pepdata[,5:23]
mat <- pepmat
rm(pepdata)



pepcols$group<- gsub("1", "antigenic", pepcols$group)
pepcols$group<- gsub("0", "nonantigenic",pepcols$group)

rowlabels <- as.character(pepcols$group)
rowlabels <- gsub("0", "nonantigenic",pepcols$group)
rowlabels <- gsub("1", "antigenic", pepcols$group)

collabels <- colnames(pepmat)

annotationrows <- data.frame(pepcols)
annotationcols <- data.frame(collabels)

rownames(mat) <- paste(rowlabels, " ",seq_len(100))
rownames(annotationrows) <- rownames(mat)

## Heatmap Functions and colors ----------------------------------------------------
normheatmap <- function(mat, cluster_cols=TRUE, title=title, cex=1, h=2, w=1, ...){
  pheatmap(mat,
           cex = cex,
           cluster_rows=TRUE,
           scale="column",
           breaks = c(seq(-3, 3, length.out = 256)),
           border_color = NA,
           drop_levels=TRUE,
           color = my_palette,
           show_rownames=TRUE,
           cluster_cols=cluster_cols,
           annotation_row = annotationrows,
           #clustering_distance_cols = dcols,
           #clustering_distance_rows = drows,
           labels_row = rowlabels,
           annotation_colors = ann_colors,
           annotation_legend=FALSE,
           main= paste0(title),
           legend=FALSE,
           fontsize= 6,
           treeheight_col = 20,
           treeheight_row = 20,
           height=h*6,
           width=w*6,
           filename= paste0(title,".pdf"))
}


my_palette <- colorRampPalette(brewer.pal(11, "RdBu")) (255)
my_palette <- rev(my_palette)

ann_colors = list(
  group = c( nonantigenic="#C7302A", antigenic="#4266F6")[rowlabels])


dcols = dist(t(mat))
drows = dist((mat))

normheatmap(mat,
            title="Peptide Feature Heatmap")

write.csv(c(pepmat, pepcols), file = "PeptideFeatures.csv")
