#' @export
#' @title Pull DATIM code list.
#'
#' @description
#' Pulls the DATIM code list for specified datasets.
#'
#' @param dataset Dataset uid to query.
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return Code list as dataframe.
#'
pullDATIMCodeList <- function(dataset,
                              d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE)) {

  # TEST that dataset is valid
  ds <- datimutils::getMetadata(dataSets,
                                d2_session = d2_session)

  if (!dataset %in% ds$id) {
    stop("Invalid dataset uid provided!")
  }

  interactive_print(ds$name[ds$id == dataset])

  datimutils::getSqlView(sql_view_uid = "DotdxKrNZxG",
                                     variable_keys = "dataSets",
                                     variable_values = dataset,
                                     d2_session = d2_session)

}
