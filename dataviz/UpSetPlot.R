library(devtools)
library(annotatr)
library(tidyverse)
library(reshape2)
library(ComplexHeatmap)
library(circlize)
library(UpSetR)
library(tidyverse)

combinedPSI$E1 <- as.integer(combinedPSI$E1)
combinedPSI$E2 <- as.integer(combinedPSI$E2)
combinedPSI$E3 <- as.integer(combinedPSI$E3)

ASbyGene <- as.data.frame(cbind(combinedPSI$AS, as.character(combinedPSI$GeneID)), row.names = TRUE)
ASbyGene$V1 <- as.character(ASbyGene$V1)
ASbyGene$V2 <- as.character(ASbyGene$V2)

mat <- as.matrix(ASbyGene[,-1])
row.names(mat) <- combinedPSI$GeneID

listInput <-
  list(
    AA = ASbyGene$V2[grep("A3SS", ASbyGene$V1)],
    AD = ASbyGene$V2[grep("A5SS", ASbyGene$V1)],
    CA = ASbyGene$V2[grep("AFE", ASbyGene$V1)],
    CS = ASbyGene$V2[grep("ALE", ASbyGene$V1)]
  )
upset(fromList(listInput), group.by = "sets")
upsetGraph <- UpSetR::fromList(listInput)
upsetGraph <- as.data.frame(upsetGraph)
data.table::as.data.table(upsetGraph)

upsetGraph$GeneID <- levels(combinedPSI$GeneID)
table(upsetGraph)

length(unique(ASbyGene$V2))
length(unique(ASbyGene$V1))
