#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @importFrom RPostgres Postgres
#' @importFrom dplyr tbl
#' @importFrom dbplyr in_schema

library(magrittr)

con_splicecore <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DEVDB"),
  host = Sys.getenv("DEVHOST"),
  port = 5432,
  user = Sys.getenv("DEVUSER"),
  password = Sys.getenv("DEVPASSWORD")
)

con_txdb <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB"),
  host = Sys.getenv("HOST"),
  port = 5432,
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD")
)

txdb.txdbannotated <-
  con_splicecore %>% dplyr::tbl(dbplyr::in_schema("txdb", "txdbannotated"))
txdb.stringtie <-
  con_splicecore %>% dplyr::tbl(dbplyr::in_schema("txdb", "stringtie"))
txdb.splicelearn <-
  con_txdb %>% dplyr::tbl("splicelearn")

public.v_tcga_brca_psi_tumortype <-
  con_splicecore %>%
  dplyr::tbl("v_tcga_brca_psi_tumor_type")
txdb.gtex <-
  con_splicecore %>%
  dplyr::tbl(dbplyr::in_schema("txdb", "gtex"))
txdb.spliceimpact <-
  con_splicecore %>%
  dplyr::tbl(dbplyr::in_schema("txdb", "spliceimpact"))

tenant21.experiment_results <-
  con_splicecore %>%
  dplyr::tbl(dbplyr::in_schema('"tenant-21"', "experiment_results"))
tenant21.experiment_result_favorites <-
  con_splicecore %>%
  dplyr::tbl(dbplyr::in_schema('"tenant-21"', "experiment_result_favorites"))
tenant21.experiment_casefile <-
  con_splicecore %>%
  dplyr::tbl(dbplyr::in_schema('"tenant-21"', "experiment_casefile"))
tenant21.experiment_controlfile <-
  con_splicecore %>%
  dplyr::tbl(dbplyr::in_schema('"tenant-21"', "experiment_controlfile"))


