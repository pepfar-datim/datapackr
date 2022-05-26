#' @export
#' @title separateDataSets(d)
#'
#' @description After data has been extracted from all sheets in a Data Pack,
#'     this function separates datasets by either \code{MER} or
#'     \code{SUBNAT/IMPATT} and removes elements of \code{d} that are no longer
#'     necessary (\code{targets}, \code{extract}, and \code{sheet})
#'
#' @param d Datapackr object.
#'
#' @return d
#'
separateDataSets <- function(d) {
  d$data$MER <- d$data$targets %>%
    dplyr::filter(
      indicator_code %in% (
        d$info$schema %>%
          dplyr::filter(col_type == "target",
                        dataset == "mer") %>%
          dplyr::pull(indicator_code)
      )
    )

  d$data$SUBNAT_IMPATT <- d$data$targets %>%
    dplyr::filter(
      indicator_code %in% (
        d$info$schema %>%
          dplyr::filter(
            (col_type == "target" & dataset %in% c("subnat", "impatt"))
            | (col_type == "result" & dataset == "subnat")) %>%
          dplyr::pull(indicator_code)
      )
    )
  
  to_drop <- names(d$data) %in% c("targets","extract")
  d$data <- d$data[!to_drop]


  return(d)
}
