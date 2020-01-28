#' @export
#' @title getNumeratorDenominator
#' 
#' @description 
#' Map Numerator and Denominator to dataElement id
#' 
#' @param dataElements List of dataElements to filter against. (Optional)
#' 
#' @return Dataframe of Numerator and Denominators mapped to dataElements
#' 
getNumeratorDenominator <- function(dataElements = NULL) {
  
  groupSet = "lD2x0c8kywj"
  
  num_den <- api_call(paste0("dataElementGroupSets/",groupSet)) %>%
    api_fields("dataElementGroups[name,dataElements[id]]") %>%
    api_get() %>%
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
