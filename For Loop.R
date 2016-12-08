


resultsNames(ddsGroup)
deresults <- function (dds, fcThresh=2, baseMeanThresh=100, padjThresh=0.1){
  reslist <- list()
  resnames <- resultsNames(dds)[-1]
  for (num in 2:length(resnames)){
    for (den in 1:(num-1)){
      print(paste0(resnames[num],".", resnames[den]))
      res <- results(dds, contrast=list(resnames[num], resnames[den]))
      res <- subset(res, abs(log2FoldChange) > fcThresh  & baseMean > baseMeanThresh & padj < padjThresh)
      reslist[[paste0(resnames[num],".", resnames[den])]] <- res
    }
  }
  return(reslist)
}

results <- deresults(ddsGroup)

allres <- cbind.data.frame(res.EWAT_Control_CD4.MWAT_Control_CD4,
                           res.EWAT_Control_ILC2.MWAT_Control_ILC2)

