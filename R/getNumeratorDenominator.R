#' @export
#' @title getNumeratorDenominator
#' 
#' @description 
#' Map Numerator and Denominator to dataElement id
#' 
#' @param dataElements List of dataElements to filter against. (Optional)
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return Dataframe of Numerator and Denominators mapped to dataElements
#' 
getNumeratorDenominator <- function(dataElements = NULL,
                                    d2_session = dynGet("d2_default_session",
                                                        inherits = TRUE)) {
  
  groupSet = "lD2x0c8kywj"
  
  num_den <- api_call(paste0("dataElementGroupSets/",groupSet),
                      d2_session = d2_session) %>%
    api_fields("dataElementGroups[name,dataElements[id]]") %>%
    api_get(d2_session = d2_session) %>%
    tidyr::unnest(cols = dataElements) %>%
    dplyr::distinct() %>%
    dplyr::select(dataElement = id,
                  numerator_denominator = name )
  
  if (!is.null(dataElements)) {
    num_den %<>%
      dplyr::filter(dataElement %in% dataElements)
  }
  
  return(num_den)   
  
}
