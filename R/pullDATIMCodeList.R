#' @export
#' @title Pull DATIM code list.
#' 
#' @description
#' Pulls the DATIM code list for specified datasets.
#' 
#' @param dataset Dataset uid to query.
#' 
#' @return Code list as dataframe.
#'
pullDATIMCodeList <- function(dataset) {
  
  # TEST that dataset is valid
  ds <- datimutils::getMetadata(dataSets)
  
  if (!dataset %in% ds$id) {
    stop("Invalid dataset uid provided!")
  }
  
  print(ds$name[ds$id == dataset])
  
  # Pull Code List
  codeList <- api_sql_call(sqlView = "DotdxKrNZxG",
                           var = dataset)
  
  return(codeList)
  
}
