############# ENVISAGENICS THEME #########

#' @title Envisagenics Palettes
#' @description Creates primary and secondary color palettes from Envisagenics branding colors
#' @return Envisagenics color palettes
#' @rdname makeEnvisagenicsPalettes
#' @export
makeEnvisagenicsPalettes <- function() {
  purples <- c("Purple" = "#8d5393",
               "Lavender" = "#b37ab5",
               "Gray" = "#BDBDBD")
  reds <- c("Peach" = "#f69788",
            "Guava" = "#c16177",
            "Pinot" = "#91425d")

  blues <- c("Teal" = "#17a2b8",
             "Blue" = "#337ab7",
             "Gray" = "#BDBDBD")

  grays <-
    c("Dark Gray" = "#58595b",
      "Gray" = "#BDBDBD",
      "Light Gray" = "#9f9f9f")

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
  return(envisagenics_palettes)
}


#' @title Envisagenics colors
#' @return Envisagenics palettes
#' @rdname envisagenics_cols
#' @export
#' @noRd
envisagenics_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (envisagenics_palettes)
  envisagenics_palettes[cols]
}

#' @title Interpolate Envisagenics palettes
#' @param palette Selected palette, Default: 'purples'
#' @param reverse Color order, Default: FALSE
#' @seealso
#'  [S4Vectors::Vector-class()]
#'  [grDevices::colorRamp()]
#' @rdname envisagenics_pal
#' @export
#' @noRd
#' @importFrom S4Vectors rev
#' @importFrom grDevices colorRampPalette
envisagenics_pal <- function(palette = "purples",
                             reverse = FALSE,
                             ...) {
  pal <- envisagenics_palettes[[palette]]
  if (reverse)
    pal <- S4Vectors::rev(pal)

  grDevices::colorRampPalette(pal, ...)
}

#' @title Scale color
#' @description Scale color envisagenics
#' @noRd
#' @param palette Palette name to use, Default: 'purples'
#' @param discrete Scale type, Default: TRUE
#' @param reverse Color order, Default: FALSE
#' @seealso
#'  [ggplot2::discrete_scale()],[ggplot2::scale_colour_gradient()]
#' @rdname scale_color_envisagenics
#' @export
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
scale_color_envisagenics <- function(palette = "purples",
                                     discrete = TRUE,
                                     reverse = FALSE,
                                     ...) {
  pal <- envisagenics_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("color", paste0("envisagenics_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @description Scale fill envisagenics
#' @title Scale fill
#' @param palette Palette name to use, Default: 'purples'
#' @param discrete Scale type, Default: TRUE
#' @param reverse Color order, Default: FALSE
#' @seealso
#'  [ggplot2::discrete_scale()],[ggplot2::scale_colour_gradient()]
#' @rdname scale_fill_envisagenics
#' @export
#' @noRd
#' @importFrom ggplot2 discrete_scale scale_fill_gradient
scale_fill_envisagenics <-
  function(palette = "purples",
           discrete = TRUE,
           reverse = FALSE,
           ...) {
    pal <- envisagenics_pal(palette = palette, reverse = reverse)
    if (discrete) {
      ggplot2::discrete_scale("fill", paste0("envisagenics_", palette), palette = pal, ...)
    } else {
      ggplot2::scale_fill_gradient(colours = pal(256), ...)
    }
  }

#' @title createEnvisagenicsTheme
#' @description Create ggplot2 Envisagenics theme
#' @seealso
#'  [ggplot2::theme_get()],[ggplot2::theme()],[ggplot2::margin()]
#'  [ggpubr::theme_pubr()]
#' @rdname create_envisagenics_theme
#' @export
#' @importFrom ggplot2 theme_get theme_replace theme element_text element_blank element_line element_rect theme_set
#' @importFrom ggpubr theme_pubclean
createEnvisagenicsTheme <- function(...) {
  theme_envisagenics <- ggplot2::theme_get()
  theme_envisagenics <-
    ggplot2::theme_replace() + ggpubr::theme_pubclean()
  theme_envisagenics <- ggplot2::theme_replace() +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(colour = NA, family = "Helvetica"),
      plot.caption = ggplot2::element_text(
        colour = NA,
        family = "Helvetica",
        hjust = 0
      ),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        linetype = "dashed",
        size = 0.1,
        color = "gray29"
      ),
      panel.grid.minor = ggplot2::element_line(colour = NA,
                                               size = 0),
      axis.title = ggplot2::element_text(
        family = "Helvetica",
        size = 12,
        colour = "gray29",
        hjust = 0.55
      ),
      axis.text = ggplot2::element_text(size = 10, family = "Helvetica"),
      axis.text.x = ggplot2::element_text(
        size = 10,
        vjust = 0,
        family = "Helvetica"
      ),
      axis.text.y = ggplot2::element_text(
        size = 10,
        vjust = 0,
        family = "Helvetica"
      ),
      plot.title = ggplot2::element_text(
        size = 12,
        colour = "gray29",
        vjust = 0,
        family = "Helvetica"
      ),
      legend.text = ggplot2::element_text(colour = "gray29", family = "Helvetica"),
      legend.title = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(colour = NA),
      legend.background = ggplot2::element_rect(fill = NA),
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(colour = "gray29", family = "Helvetica")
    )
  ggplot2::theme_set(theme_envisagenics)
  return(theme_envisagenics)
}

################## SQL QUERIES #####
#' @title Select by experiment & ID
#' @description Filter a table by selected experiment
#' @param db_table Table to filter
#' @export
#' @rdname selectByExpID
#' @importFrom magrittr %>%
#' @import dplyr
selectByExpID <- function(db_table) {
  selected_id <- Sys.getenv("ID")
  selected_experiment <- Sys.getenv("EXPERIMENT")
  db_table %>% dplyr::filter(txdb_id == selected_id &
                               experiment_id == selected_experiment)  %>% data.table::as.data.table()
}

#' @title selectByID
#' @description Filter a table by selected txdb ID
#' @param db_table Table to filter
#' @export
#' @rdname selectByID
#' @importFrom magrittr %>%
#' @import dplyr
selectByID <- function(db_table) {
  selected_id <- Sys.getenv("ID")
  db_table %>% dplyr::filter(txdb_id == selected_id)  %>% data.table::as.data.table()
}

#' @title selectByGeneID
#' @description Filter a browser table by txdb trio id
#' @param db_table Table to filter
#' @export
#' @rdname selectByGeneID
#' @importFrom magrittr %>%
#' @import dplyr
selectByGeneID <- function(db_table) {
  selected_id <- Sys.getenv("ID")
  db_table %>% dplyr::filter(gene_id == selected_id)  %>% collect()
}


#' @title getAvgJunctionReads
#' @description Get average junction read counts
#' @param selected_results Selected results
#' @seealso
#'  [tidyr::separate()]
#'  [data.table::data.table-package()]
#' @rdname getAvgJunctionReads
#' @export
#' @importFrom tidyr separate
#' @importFrom data.table data.table
getAvgJunctionReads <- function(selected_results) {
  case_long <-
    cbind(selected_results$avg_jinc_case,
          rep("case_long", length(selected_results$avg_jinc_case)))
  case_short <-
    cbind(selected_results$avg_jskip_case,
          rep("case_short", length(selected_results$avg_jskip_case)))
  control_long <-
    cbind(selected_results$avg_jinc_ctrl,
          rep("control_long", length(selected_results$avg_jinc_ctrl)))
  control_short <-
    cbind(selected_results$avg_jskip_ctrl,
          rep("control_short", length(selected_results$avg_jskip_ctrl)))
  avg_junction_reads <-
    as.data.frame(rbind(case_long, case_short, control_long, control_short),
                  stringsAsFactors = FALSE)
  colnames(avg_junction_reads) <-
    c("Avg_Junction_Reads", "Subgroup")
  avg_junction_reads <-
    tidyr::separate(avg_junction_reads, Subgroup, c("Group", "Isoform"), sep = "_")
  avg_junction_reads$Avg_Junction_Reads <-
    as.numeric(avg_junction_reads$Avg_Junction_Reads)
  avg_junction_reads <-
    data.table::data.table(avg_junction_reads, stringsAsFactors = TRUE)
}

#' @title getAvgPSI
#' @description Get average PSI
#' @param selected_results Selected results
#' @seealso
#'  [data.table::data.table-package()]
#'  [tidyr::separate()]
#' @rdname getAvgPsi
#' @export
#' @importFrom data.table data.table
#' @importFrom tidyr separate
getAvgPsi <- function(selected_results) {
  assert_data_table(selected_results,
                    any.missing = FALSE, all.missing = FALSE, min.rows = 1,
                    null.ok = FALSE)
  case_psi_max <-
    selected_results$avg_psi_case + selected_results$avg_psi_case_err
  case_psi_min <-
    selected_results$avg_psi_case - selected_results$avg_psi_case_err
  ctrl_psi_max <-
    selected_results$avg_psi_ctrl + selected_results$avg_psi_ctrl_err
  ctrl_psi_min <-
    selected_results$avg_psi_ctrl - selected_results$avg_psi_ctrl_err
  case_psi_avg <- selected_results$avg_psi_case
  ctrl_psi_avg <- selected_results$avg_psi_ctrl
  avg_psi <-
    rbind(
      case_psi_avg,
      case_psi_max,
      case_psi_min,
      ctrl_psi_avg,
      ctrl_psi_max,
      ctrl_psi_min
    )
  avg_psi <- data.table::data.table(avg_psi, keep.rownames = TRUE)
  colnames(avg_psi) <- c("Subgroup", "PSI")
  avg_psi <-
    tidyr::separate(avg_psi, Subgroup, c("Group", "Measure"), sep = "_psi_")
  avg_psi <- data.table::data.table(avg_psi)
}

#' @title getJunctionCounts
#' @description Get raw junction read counts
#' @param selected_controlfile Selected control file
#' @param selected_casefile Selected case file
#' @seealso
#'  [jsonlite::toJSON, fromJSON()]
#'  [data.table::data.table-package()]
#'  [tidyr::separate()]
#' @rdname getJunctionCounts
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom data.table data.table
#' @importFrom tidyr separate
getJunctionCounts <-
  function(selected_controlfile, selected_casefile) {
    assert_data_table(selected_controlfile,
                      any.missing = FALSE, all.missing = FALSE, min.rows = 1,
                      null.ok = FALSE)
    assert_data_table(selected_casefile,
                      any.missing = FALSE, all.missing = FALSE, min.rows = 1,
                      null.ok = FALSE)
    control_data <-
      jsonlite::fromJSON(as.character(selected_controlfile$control_data))
    case_data <-
      jsonlite::fromJSON(as.character(selected_casefile$case_data))
    control_data <-
      data.table::data.table(t(control_data), keep.rownames = TRUE)
    case_data <-
      data.table::data.table(t(case_data), keep.rownames = TRUE)
    colnames(control_data) <- c("Sample", "Value")
    colnames(case_data) <- c("Sample", "Value")
    control_data$Group <-
      rep("Control", length(control_data$Sample))
    case_data$Group <- rep("Case", length(case_data$Sample))
    junction_counts <- rbind(control_data, case_data)
    junction_counts <-
      tidyr::separate(junction_counts, Sample, c("Sample", "Type"), sep = "[.]")
    junction_counts <-
      data.table::data.table(junction_counts, stringsAsFactors = FALSE)
    junction_counts$Value <- as.numeric(junction_counts$Value)
    junction_counts <- na.omit(junction_counts)
  }

#' @title getJunctionPSI
#' @description Get individual junction PSI
#' @param junction_counts Raw junction counts
#' @seealso
#'  [jsonlite::toJSON, fromJSON()]
#'  [data.table::data.table-package()]
#'  [tidyr::separate()]
#' @rdname getJunctionPSI
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom data.table data.table
#' @importFrom tidyr separate
getJunctionPsi <- function(junction_counts) {
  junction_counts[grep("psi", junction_counts$Type), ]
}

#' @title getSpliceimpactSeq
#' @description Get exon trio sequence
#' @param selected_spliceimpact Selected sequence from SpliceImpact
#' @rdname getSpliceimpactSeq
#' @export
getSpliceimpactSeq <- function(selected_spliceimpact) {
  selected_spliceimpact_seq <- list(
    selected_spliceimpact$alt_prot,
    selected_spliceimpact$inc_prot,
    selected_spliceimpact$skip_prot
  )
  names(selected_spliceimpact_seq) <- c("alt", "inc", "skip")
  selected_spliceimpact_seq <-
    lapply(selected_spliceimpact_seq, as.character)
}

################## BROWSER #####
#' @title Build Selected TXdb Tracklist
#' @description Creates selected TXdb tracks for GenomeBrowser
#' @param selected_id TXdb ID
#' @return List of tracks for the genome browser
#' @seealso
#'  [checkmate::checkEnvironment()]
#'  [ensembldb::seqlevelsStyle()]
#'  [rtracklayer::character(0)()]
#'  [TnT::track-constructors()],[TnT::c("trackdata", "trackdata")()],[TnT::tooltip()]
#'  [scales::col_numeric()]
#'  [GenomicRanges::GRanges-class()],[GenomicRanges::genomic-range-squeezers()]
#' @rdname buildTxdbTracklist
#' @export
#' @family SpliceBrowse
#' @importFrom checkmate assertEnvironment
#' @importFrom ensembldb seqlevelsStyle
#' @importFrom rtracklayer genome
#' @importFrom TnT TxTrackFromGRanges trackData tooltip LineTrack
#' @importFrom scales col_factor
#' @importFrom GenomicRanges GRanges
buildTxdbTracklist <-
  function(results_txdbannotated) {
    selected_txdb_gr <- results_txdbannotated %>% makeGRangesFromDataFrame(keep.extra.columns = TRUE)
    keep <- GenomeInfoDb::seqlevelsInUse(selected_txdb_gr)
    selected_txdb_gr <-
      GenomeInfoDb::keepSeqlevels(selected_txdb_gr, keep, pruning.mode = "coarse")
    ensembldb::seqlevelsStyle(selected_txdb_gr) <- "Ensembl"

    selected_txdb_track <- TnT::TxTrackFromGRanges(selected_txdb_gr)
    rtracklayer::genome(selected_txdb_track) <-
      rep("GRCh38", length(genome(selected_txdb_track)))

    tooltip <- (TnT::tooltip(selected_txdb_track))
    tooltip$as_type <-
      stringr::str_split(tooltip$tx_id, "\\-", simplify = TRUE)[, 1]
    tooltip$symbol <- selected_txdb_gr$gene_name %>% unique()
    tooltip$enst_id <-
      stringi::stri_extract_all_words(selected_txdb_gr$enst_id, simplify = TRUE)[,-1] %>% unique() %>% paste0(collapse = ",")
    tooltip$txdb_id <-
      stringr::str_split(tooltip$tx_id, "\\[", simplify = TRUE)[, 1]
    tooltip$isoform <-
      stringr::str_split(tooltip$tx_id, "\\[", simplify = TRUE)[, 2]
    tooltip$isoform <-
      stringr::str_split(tooltip$isoform, "\\]", simplify = TRUE)[, 1]
    TnT::tooltip(selected_txdb_track) <- tooltip

    txdb_cols <-
      table(TnT::tooltip(selected_txdb_track)$as_type) %>%
      as.data.frame() %>%
      dplyr::arrange(dplyr::desc(Freq))
    TnT::trackData(selected_txdb_track)$color <-
      scales::col_factor(
        envisagenics_palettes$purples,
        ordered = TRUE,
        levels = dplyr::desc(txdb_cols$Var1)
      )(TnT::tooltip(selected_txdb_track)$as_type)

    selected_txdb_marginTrack <-
      selected_txdb_pos <- selected_txdb_track@Data@ranges
    selected_txdb_pos@width <-
      rep(as.integer(1), length(selected_txdb_pos@start))
    selected_txdb_marginTrack <- TnT::LineTrack(
      GRanges(selected_txdb_track@Data@seqnames, selected_txdb_pos),
      color = "#FFFFFF00",
      height = 15,
      value = 0,
      label = ""
    )

    selected_txdb_marginTrack@Height@.Data <- 15
    selected_txdb_marginTrack@Label@.Data <- "TXdb Transcripts"
    selected_txdb_track@Label@.Data <- ""
    selected_txdb_track@Height@.Data <- 80
    selected_txdb_tracklist <-
      list(selected_txdb_marginTrack, selected_txdb_track)
    names(selected_txdb_tracklist) <-
      c("selected_txdb_marginTrack", "selected_txdb_track")

    return(selected_txdb_tracklist)
  }

#' @title Build Edb Tracklist
#' @description Make Ensembl transcript track
#' @param selected_txdb_tracklist TXdb tracklist
#' @return OUTPUT_DESCRIPTION
#' @name NAME
#' @rdname buildEdbTracklist
#' @export
#' @family SpliceBrowse
#' @seealso
#'  [EnsDb.Hsapiens.v86::EnsDb.Hsapiens.v86()]
#'  [ensembldb::character(0)()],[ensembldb::seqlevelsStyle()]
#'  [biovizBase::crunch()]
#'  [AnnotationFilter::AnnotationFilter()]
#'  [GenomicRanges::GRangesList-class()],[GenomicRanges::c("GRanges-class", "genomic-range-squeezers")()]
#'  [TnT::track-constructors()],[TnT::tooltip()],[TnT::c("trackdata", "trackdata")()]
#'  [AnnotationDbi::AnnotationDb-objects()],[AnnotationDbi::Bimap-toTable()]
#'  [dplyr::arrange()]
#'  [scales::col_numeric()]
#'  [IRanges::findOverlaps-methods()]
#' @importFrom EnsDb.Hsapiens.v86 EnsDb.Hsapiens.v86
#' @importFrom ensembldb columns seqlevelsStyle
#' @importFrom biovizBase crunch
#' @importFrom AnnotationFilter TxIdFilter
#' @importFrom GenomicRanges GRangesList GRanges
#' @importFrom TnT TxTrackFromGRanges tooltip trackData LineTrack
#' @importFrom AnnotationDbi select colnames
#' @importFrom dplyr arrange
#' @importFrom scales col_factor
#' @importFrom IRanges overlapsAny
buildEdbTracklist <-
  function(selected_txdb_tracklist) {
    edb <- EnsDb.Hsapiens.v86::EnsDb.Hsapiens.v86
    columns <- ensembldb::columns(edb)
    ensembldb::seqlevelsStyle(edb) <- "Ensembl"
    key <-
      stringi::stri_extract_all_words(selected_txdb_tracklist$selected_txdb_track@Data@elementMetadata$tooltip$enst_id, simplify = TRUE) %>% unique()
    edb_gr <-
      biovizBase::crunch(edb, AnnotationFilter::TxIdFilter(key)) %>%
      GenomicRanges::GRangesList() %>% unlist()

      edb_track <- TnT::TxTrackFromGRanges(edb_gr)
      edb_tooltip <- TnT::tooltip(edb_track)
      edb_metadata <-
        AnnotationDbi::select(edb,
                              keys = edb_tooltip$tx_id,
                              keytype = "TXID",
                              columns = columns)
      edb_metadata <- edb_metadata[, c(6:10, 17, 24:30, 32)]
      colnames(edb_metadata) <-
        AnnotationDbi::colnames(edb_metadata) %>% tolower()
      colnames(edb_metadata) <-
        gsub("id", "_id", colnames(edb_metadata))

      edb_tooltip <-
        edb_metadata[match(edb_tooltip$tx_id, edb_metadata$tx_id),]
      TnT::tooltip(edb_track) <- edb_tooltip

      edb_cols <-
        table(TnT::trackData(edb_track)$tooltip$txbiotype) %>%
        as.data.frame() %>%
        dplyr::arrange(desc(Freq))
      TnT::trackData(edb_track)$color <-
        scales::col_factor(
          envisagenics_palettes$grays,
          ordered = TRUE,
          levels = factor(edb_cols$Var1)
        )(TnT::trackData(edb_track)$tooltip$txbiotype)

      edb_pos <- edb_track@Data@ranges
      edb_pos@width <- rep(as.integer(1), length(edb_pos@start))
      edb_marginTrack <-
        TnT::LineTrack(
          GenomicRanges::GRanges(edb_track@Data@seqnames, edb_pos),
          color = "#FFFFFF00",
          height = 15,
          value = 0,
          label = ""
        )

      edb_marginTrack@Height@.Data <- 15
      edb_marginTrack@Label@.Data <- "Ensembl Transcripts"
      edb_track@Label@.Data <- ""

      edb_tracklist <- list(
        selected_txdb_tracklist$selected_txdb_marginTrack,
        selected_txdb_tracklist$selected_txdb_track,
        edb_marginTrack,
        edb_track
      )

      names(edb_tracklist) <- c(
        "selected_txdb_marginTrack",
        "selected_txdb_track",
        "edb_marginTrack",
        "edb_track"
      )

      margin_scale <-
        (50 + length(grep("marginTrack", names(edb_tracklist))) * 20)
      total_tracks <- 2
      total_scale <- 1000
      edb_scale <-
        edb_tracklist$edb_track@Data[IRanges::overlapsAny(edb_tracklist$edb_track@Data,
                                                          edb_tracklist$selected_txdb_track@Data)] %>%
        length() * length(grep("edb_track", names(edb_tracklist)))
      total_tracks <- total_tracks + edb_scale
      total_scale <- total_scale - 600
      edb_tracklist$edb_track@Height@.Data <-
        edb_scale / total_tracks * (total_scale - margin_scale)
      edb_tracklist$edb_track@Label@.Data <- ""
      return(edb_tracklist)
}


#' @title Build Selected Stringtie Tracklist
#' @description Creates Stringtie tracks for GenomeBrowser
#' @family SpliceBrowse
#' @param label Track label, Default: 'Stringtie'
#' @param selected_id TXdb ID
#' @param selected_txdb_tracklist TXdb tracklist
#' @return List of tracks for the stringtie browser
#' @seealso
#'  [checkmate::checkEnvironment()]
#'  [ensembldb::seqlevelsStyle()]
#'  [rtracklayer::character(0)()]
#'  [TnT::track-constructors()],[TnT::c("trackdata", "trackdata")()],[TnT::tooltip()]
#'  [scales::col_numeric()]
#'  [GenomicRanges::GRanges-class()],[GenomicRanges::genomic-range-squeezers()]
#' @rdname buildStringtieTracklist
#' @export
#' @importFrom checkmate assertEnvironment
#' @importFrom ensembldb seqlevelsStyle
#' @importFrom rtracklayer genome
#' @importFrom TnT TxTrackFromGRanges trackData tooltip LineTrack
#' @importFrom scales col_factor
#' @importFrom GenomicRanges GRanges
buildStringtieTracklist <-
  function(selected_txdb_tracklist) {
    selected_stringtie_gr <- results_stringtie %>% makeGRangesFromDataFrame(keep.extra.columns = TRUE)
    selected_stringtie_gr$tx_id <- selected_stringtie_gr$stringtie_tx
    ensembldb::seqlevelsStyle(selected_stringtie_gr) <- "Ensembl"
    rtracklayer::genome(selected_stringtie_gr) <-
      rep("GRCh38", length(genome(selected_stringtie_gr)))
    selected_stringtie_track <- TnT::TxTrackFromGRanges(selected_stringtie_gr)
    TnT::trackData(selected_stringtie_track)$color <-
      scales::col_factor(
        palette = rev(envisagenics_palettes$blues),
        ordered = TRUE,
        levels = factor(selected_stringtie_track$key)
      )(TnT::trackData(selected_stringtie_track)$key)

    tooltip(selected_stringtie_track)$txdb_id <-
      unique(selected_stringtie_gr$txdb_id)
    tooltip(selected_stringtie_track)$stringtie_tx <-
      unique(selected_stringtie_gr$stringtie_tx)

    stringtie_pos <- selected_stringtie_track@Data@ranges
    stringtie_pos@width <- rep(as.integer(1), length(stringtie_pos@start))
    selected_stringtie_marginTrack <-
      TnT::LineTrack(
        GenomicRanges::GRanges(selected_stringtie_track@Data@seqnames, stringtie_pos),
        color = "#FFFFFF00",
        height = 15,
        value = 0,
        label = ""
      )

    selected_stringtie_marginTrack@Height@.Data <- 20
    selected_stringtie_marginTrack@Label@.Data <- "Stringtie"
    selected_stringtie_track@Label@.Data <- ""
    selected_stringtie_track@Height@.Data <- 60

    selected_stringtie_tracklist <- list(selected_stringtie_marginTrack,
                                         selected_stringtie_track)
    names(selected_stringtie_tracklist) <- c(paste0("stringtie_marginTrack"),
                                             paste0("stringtie_track"))
    selected_stringtie_tracklist <-
      c(selected_txdb_tracklist, selected_stringtie_tracklist)

    margin_scale <-
      (50 + length(grep("marginTrack", names(selected_stringtie_tracklist))) * 20)
    total_tracks <- 2
    total_scale <- 1000
    selected_stringtie_scale <-
      selected_stringtie_tracklist$stringtie_track@Data[IRanges::overlapsAny(
        selected_stringtie_tracklist$stringtie_track@Data,
        selected_stringtie_tracklist$selected_txdb_track@Data
      )] %>% length() * length(grep(
        "stringtie_track",
        names(selected_stringtie_tracklist)
      ))
    total_tracks <- total_tracks + selected_stringtie_scale
    total_scale <- total_scale - 600
    selected_stringtie_tracklist$stringtie_track@Height@.Data <-
      selected_stringtie_scale / total_tracks * (total_scale - margin_scale)
    selected_stringtie_tracklist$stringtie_track@Label@.Data <- ""
    return(selected_stringtie_tracklist)
  }

buildCosmicTracklist <- function(...) {
  selected_txdb_tracklist <- buildTxdbTracklist(results_txdbannotated)
  selected_splicelearn_gr <- results_splicelearn %>% makeGRangesFromDataFrame(keep.extra.columns = TRUE)
  selected_splicelearn_gr@ranges@width <- rep(as.integer(1), length(selected_splicelearn_gr@ranges@width))
  ensembldb::seqlevelsStyle(selected_splicelearn_gr) <- "Ensembl"
  cosmic_pintrack <- TnT::PinTrack(selected_splicelearn_gr, value = selected_splicelearn_gr$sl_score, domain = range(selected_splicelearn_gr$sl_score), color = selected_splicelearn_gr$sl_score)
    rtracklayer::genome(cosmic_pintrack) <-
      rep("GRCh38", length(rtracklayer::genome(cosmic_pintrack)))

    tooltip <- as.data.frame(TnT::tooltip(cosmic_pintrack))
    tooltip$as_type <-
      stringr::str_split(tooltip$txdb_id, "\\-", simplify = TRUE)[, 1]
    TnT::tooltip(cosmic_pintrack) <- tooltip[,-c(1:2)]

    cosmic_pos <- cosmic_pintrack@Data@ranges
    cosmic_pos@width <- rep(as.integer(1), length(cosmic_pos@start))
    cosmic_marginTrack <-
      TnT::LineTrack(
        GenomicRanges::GRanges(cosmic_pintrack@Data@seqnames, cosmic_pos),
        color = "#FFFFFF00",
        height = 15,
        value = 0,
        label = "COSMIC"
      )

    cosmic_marginTrack@Height@.Data <- 20
    cosmic_marginTrack@Label@.Data <- "COSMIC"
    cosmic_pintrack@Label@.Data <- ""
    cosmic_pintrack@Height@.Data <- 90
    TnT::trackData(cosmic_pintrack)$color <- scales::col_numeric(
      palette = colorRampPalette(envisagenics_palettes$purples_grays_blues)(255),
      domain = range(TnT::tooltip(cosmic_pintrack)$sl_score)
    )(TnT::tooltip(cosmic_pintrack)$sl_score)

    cosmic_tracklist <- list(
      cosmic_marginTrack,
      cosmic_pintrack,
      selected_txdb_tracklist$selected_txdb_marginTrack,
      selected_txdb_tracklist$selected_txdb_track
    )
    names(cosmic_tracklist) <- c(
      "cosmic_marginTrack",
      "cosmic_pintrack",
      "selected_txdb_marginTrack",
      "selected_txdb_track"
    )
  return(cosmic_tracklist)
}

buildCombinedTracklist <- function(...) {
  selected_txdb_tracklist <- buildTxdbTracklist(results_txdbannotated)
  edb_tracklist <- buildEdbTracklist(selected_txdb_tracklist)
  stringtie_tracklist <-
    buildStringtieTracklist(selected_txdb_tracklist)
  combined_tracklist <- list(edb_tracklist, stringtie_tracklist)
  names(combined_tracklist) <- c("edb_tracklist", "stringtie_tracklist")
  return(combined_tracklist)
}

# Plot tracks
#' @title Plot Tracks
#' @description Plot selected tracklists
#' @param selected_track Tracklist for selected view generation
#' @param tracklist Tracklist, Default: tracklist
#' @param zoom Allowed zoom, Default: 1
#' @seealso
#'  [IRanges::IRanges-constructor()],[IRanges::IRangesOverview()]
#'  [TnT::tntboard()]
#' @rdname viewBrowser
#' @export
#' @importFrom IRanges IRanges
#' @importFrom TnT TnTGenome trackWidget
viewBrowser <-  function (selected_track,
                          tracklist = tracklist,
                          zoom = 1) {
  selected_view <- range(selected_track) %>% as.data.frame()
  zoom <- as.numeric(zoom) * selected_view$width
  coord.range <-
    IRanges::IRanges((selected_view$start - zoom), (selected_view$end + zoom))
  TnT::TnTGenome(
    tracklist = tracklist,
    coord.range = coord.range,
    view.range = range(selected_track)
  ) %>% TnT::trackWidget()
  }

