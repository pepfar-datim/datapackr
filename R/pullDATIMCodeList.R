#' @export
#' @title Pull DATIM code list.
#' 
#' @description
#' Pulls the DATIM code list for specified datasets.
#' 
#' @param dataset Dataset uid to query.
#' @param d2_session DHIS2 Session id
#' 
#' @return Code list as dataframe.
#'
pullDATIMCodeList <- function(dataset,
                              d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE)) {
  dataset.id = dataset
  
  # TEST that dataset is valid
  ds <- datimutils::getMetadata(dataSets,
                                d2_session = d2_session)
  
  if (!dataset.id %in% ds$id) {
    stop("Invalid dataset uid provided!")
  }
  
  print(ds$name[ds$id == dataset.id])
  
  # Pull Code List
  codeList <- api_sql_call(sqlView = "DotdxKrNZxG",
                           var = dataset,
                           d2_session = d2_session) %>%
    dplyr::mutate(
      dataset.id = dataset.id
    ) %>%
    dplyr::select(dataset.id, dplyr::everything())
  
  return(codeList)
  
}
