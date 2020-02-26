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
  
  # d$tool$wb$sharedStrings[d$tool$wb$sharedStrings == "<si><t>NA</t></si>"] <- "<si><t>0</t></si>"
  
  NA_sharedString <- grep("<si><t>NA</t></si>", d$tool$wb$sharedStrings)-1
  
  if (length(NA_sharedString) > 0) {
  
   for (i in 1:length(d$tool$wb$worksheets)) {
     ## Strip only if sheet$data$t == 1 (String) To avoid accidentally replacing a numeric equivalent
     d$tool$wb$worksheets[[i]]$sheet_data$v[d$tool$wb$worksheets[[i]]$sheet_data$v == NA_sharedString] <- ""
     
     might_be_NA <- d$tool$wb$worksheets[[i]]$sheet_data$v == NA_sharedString
     
     is_string <- d$tool$wb$worksheets[[i]]$sheet_data$t == 1
     
     is_na_string <- ifelse(is.na(might_be_NA & is_string), FALSE, might_be_NA & is_string)
     
     d$tool$wb$worksheets[[i]]$sheet_data$v[is_na_string]  <- NA_character_
     
     d$tool$wb$worksheets[[i]]$sheet_data$t[is_na_string]  <- NA
    }
   
  }
  
  return(d)
   
}
  