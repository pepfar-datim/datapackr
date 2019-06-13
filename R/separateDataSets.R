#' @importFrom magrittr %>% %<>%
#' @title separateDataSets(d)
#'
#' @description After data has been extracted from all sheets in a Data Pack or
#'     Site Tool, this function separates datasets by either \code{MER} or
#'     \code{SUBNAT/IMPATT} and removes elements of \code{d} that are no longer
#'     necessary (\code{targets}, \code{extract}, and \code{sheet})
#'
#' @param d datapackr list object containing at least \code{d$data$targets}.
#' @return A datapackr list object, \code{d}, storing at least 2 dataframes of
#'    data extracted from submitted Data Pack or Site Tool: a \code{d$data$MER}
#'    dataframe containing all MER data to be distributed to site level, and/or
#'    \code{d$data$SUBNAT_IMPATT} containing data in the SUBNAT and IMPATT
#'    datasets from DATIM that can be imported into DATIM at the PSNU level.
separateDataSets <- function(d) {
  d$data$MER <- d$data$targets %>%
    dplyr::filter(
      indicatorCode %in% (
        datapackr::data_pack_schema %>%
          dplyr::filter(col_type == "Target",
                        dataset == "MER") %>%
          dplyr::pull(indicator_code)
      )
    )
  
  d$data$SUBNAT_IMPATT <- d$data$targets %>%
    dplyr::filter(
      indicatorCode %in% (
        datapackr::data_pack_schema %>%
          dplyr::filter(col_type == "Target",
                        dataset %in% c("SUBNAT", "IMPATT")) %>%
          dplyr::pull(indicator_code)
      )
    )
  d$data <-
    rlist::list.remove(d$data, c("targets", "extract", "sheet"))
  
  return(d)
}
