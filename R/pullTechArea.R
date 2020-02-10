#' @export
#' @title getTechArea
#' 
#' @description 
#' Map Tech Area to dataElement id
#' 
#' @param dataElements List of dataElements to filter against. (Optional)
#' 
#' @return Dataframe of Tech Areas mapped to dataElements
#' 
getTechArea <- function(dataElements = NULL) {
  
  groupSet = "LxhLO68FcXm"
  
  tech_areas <- api_call(paste0("dataElementGroupSets/",groupSet)) %>%
    api_fields("dataElementGroups[name,dataElements[id]]") %>%
    api_get() %>%
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
