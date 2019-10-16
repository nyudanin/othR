selected_gene <- Sys.getenv('GENE')
selected_id <- Sys.getenv('ID')
selected_experiment <- Sys.getenv('EXPERIMENT')

selected_results <- selectByExpID(tenant21.experiment_results)

selected_controlfile <- selectByExpID(tenant21.experiment_controlfile)
selected_casefile <- selectByExpID(tenant21.experiment_casefile)
selected_gtex <- selectByID(txdb.gtex)
brca_psi <- selectByID(public.v_tcga_brca_psi_tumortype)
selected_spliceimpact <- selectByID(txdb.spliceimpact)
avg_junction_reads <- getAvgJunctionReads(selected_results)
avg_psi <- getAvgPsi(selected_results)
junction_counts <- getJunctionCounts(selected_controlfile, selected_casefile)
junction_psi <- getJunctionPsi(junction_counts)
junction_counts <- junction_counts[-grep("nrd|psi", junction_counts$Type), ]
selected_spliceimpact_seq <- getSpliceimpactSeq(selected_spliceimpact)
selected_sequence <- selected_spliceimpact_seq$inc

