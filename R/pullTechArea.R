#' @export
#' @title getTechArea
#'
#' @description
#' Map Tech Area to dataElement id
#'
#' @param dataElements List of dataElements to filter against. (Optional)
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return Dataframe of Tech Areas mapped to dataElements
#'
getTechArea <- function(dataElements = NULL,
                        d2_session = dynGet("d2_default_session",
                                            inherits = TRUE)) {

  groupSet <- "LxhLO68FcXm"

  tech_areas <- api_call(paste0("dataElementGroupSets/",
                                groupSet),
                                d2_session = d2_session) %>%
    api_fields("dataElementGroups[name,dataElements[id]]") %>%
    api_get(d2_session = d2_session) %>%
    tidyr::unnest(cols = dataElements) %>%
    dplyr::distinct() %>%
    dplyr::select(dataElement = id,
                  tech_area = name )

  if (!is.null(dataElements)) {
    tech_areas %<>%
      dplyr::filter(dataElement %in% dataElements)
  }

  return(tech_areas)
}
