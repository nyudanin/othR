# load("data/cosmic_gr.rda")

selected_gene <- Sys.getenv('GENE')
selected_id <- Sys.getenv('ID')
selected_experiment <- Sys.getenv('EXPERIMENT')

selected_results <- selectByExpID(tenant21.experiment_results)
results_stringtie <- selectByGeneID(txdb.stringtie)
results_txdbannotated <- selectByGeneID(txdb.txdbannotated)
results_splicelearn <- selectByID(txdb.splicelearn)

selected_tracklist <- buildCombinedTracklist()
cosmic_tracklist <- buildCosmicTracklist(selected_txdb_tracklist)
list2env(selected_tracklist, env = .GlobalEnv)

edb_widget <- viewBrowser(
  selected_track = edb_tracklist$edb_track,
  zoom = 2,
  tracklist = edb_tracklist
)

stringtie_widget <- viewBrowser(
  selected_track = stringtie_tracklist$stringtie_track,
  zoom = 10,
  tracklist = stringtie_tracklist
)

cosmic_widget <-  viewBrowser(
selected_track = cosmic_tracklist$selected_txdb_track,
zoom = 10,
tracklist = cosmic_tracklist
)
