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
  
  d$tool$wb$sharedStrings[d$tool$wb$sharedStrings == "<si><t>NA</t></si>"] <- "<si><t></t></si>"
  
  # NA_sharedString <- grep("<si><t>NA</t></si>", d$tool$wb$sharedStrings)-1
  # 
  # if (length(NA_sharedString) > 0) {
  # 
  #   for (i in 1:length(d$tool$wb$worksheets)) {
  #     ## Strip only if sheet$data$t == 1 (String) To avoid accidentally replacing a numeric equivalent
  #     d$tool$wb$worksheets[[i]]$sheet_data$v[d$tool$wb$worksheets[[i]]$sheet_data$v == NA_sharedString] <- ""
  #     
  #     grep("1316", d$tool$wb$worksheets[[9]]$sheet_data$v)
  #     
  #     stringr::str_
  #     
  #     
  #   }
  #   
  # }
  
  return(d)
   
}
  