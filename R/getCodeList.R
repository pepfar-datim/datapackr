#' @export
#' @title Pull & combine all MER, SUBNAT, IMPATT code lists for specified COP year.
#'
#' @description
#' Pulls all code lists for MER, SUBNAT, and IMPATT for a specified COP year and
#' combines these into a unique list.
#'
#' @param datasets Character vector of dataSet IDs to pull code lists for.
#' @inheritParams datapackr_params
#' @return Combined code list as dataframe.
#'
getCodeList <- function(cop_year = NULL,
                         datastreams = c("mer_targets", "mer_results",
                                         "subnat_targets", "subnat_results",
                                         "impatt"),
                         datasets = NULL,
                         d2_session = dynGet("d2_default_session",
                                             inherits = TRUE)) {

  datasets <- datasets %missing% NULL
  cop_year <- check_cop_year(cop_year %missing% NULL)

  if (is.null(datasets)) {
    datasets <- getCOPDatasetUids(cop_year, datastreams)
  }

  datasets %<>% unique()
  # Pull list of valid datasets from DATIM
  ds <- datimutils::getMetadata("dataSets", d2_session = d2_session)
# Test that all datasets is valid
  stopifnot("Invalid dataset UID provided!" = all(datasets %in% ds$id))

  # Fetch code lists

    purrr::map_dfr(datasets, function(x) {
      datimutils::getSqlView(sql_view_uid = "DotdxKrNZxG",
                             variable_keys = "dataSets",
                             variable_values = x,
                             d2_session = d2_session)
    }) %>%
    dplyr::select(dataelement,
                  dataelementuid,
                  categoryoptioncombo,
                  categoryoptioncombouid) %>%
    dplyr::mutate(FY = cop_year + 1) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dataelement, categoryoptioncombo)

}
