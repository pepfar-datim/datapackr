#' @export
#' @title Remove errant NAs in a Data Pack
#'
#' @description
#' Searches a Data Pack for NAs, likely introduced by opening a file with pasted
#' blank cell values, and replaces these with true blanks to prevent cascading
#' issues.
#' 
#' @param d datapackr object
#' 
#' @return Updated datapackr object with NAs removed from d$tool$wb
#'     
strip_wb_NAs <- function(d) {
  
  NA_sharedString <- grep("<si><t>NA</t></si>", d$tool$wb$sharedStrings)-1
  
  # What to do if no NAs...
  
  for (i in 1:length(d$tool$wb$worksheets)) {
    d$tool$wb$worksheets[[i]]$sheet_data$v[d$tool$wb$worksheets[[i]]$sheet_data$v == NA_sharedString] <- ""
  }
  
  return(d)
   
}
  