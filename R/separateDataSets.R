#' @export
#' @title separateDataSets(d)
#'
#' @description After data has been extracted from all sheets in a Data Pack,
#'     this function separates datasets by either \code{MER} or
#'     \code{SUBNAT/IMPATT}.)
#'
#' @param data Dataframe or tibble to separate.
#' @inheritParams datapackr_params
#'
#' @return List object containing either MER or SUBNAT/IMPATT data.
#'
separateDataSets <- function(data, cop_year = NULL, tool = NULL) {

  # Check parameters ----
  params <- check_params(cop_year = cop_year,
                         tool = tool,
                         schema = NULL)

  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }

  rm(params, p)

  # Separate data ----
  datasets <- NULL

  datasets$data <- data

  schema %<>%
    dplyr::filter(col_type %in% c("result", "target"),
                  indicator_code != "") %>%
    dplyr::select(indicator_code, dataset)

  datasets$MER <- data %>%
    dplyr::filter(
      indicator_code %in% schema$indicator_code[schema$dataset == "mer"])

  datasets$SUBNAT_IMPATT <- data %>%
    dplyr::filter(
      indicator_code %in%
        schema$indicator_code[schema$dataset %in% c("subnat", "impatt")])

  return(datasets)
}
