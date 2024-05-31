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

  # find indices of sharedStrings matching "<si><t>NA</t></si>", adjusting for zero-based indexing
  NA_sharedString <- grep("<si><t>NA</t></si>", d$tool$wb$sharedStrings) - 1

  # proceed only if such sharedStrings are found
  if (length(NA_sharedString) > 0) {

    for (i in seq_along(d$tool$wb$worksheets)) {

      # identify cells with sharedStrings that might be "NA"
      might_be_NA <- d$tool$wb$worksheets[[i]]$sheet_data$v %in% NA_sharedString

      # identify cells that are string types
      is_string <- d$tool$wb$worksheets[[i]]$sheet_data$t == 1

      # combine both conditions to find actual "NA" strings, handling NAs safely
      is_na_string <- ifelse(is.na(might_be_NA & is_string), FALSE, might_be_NA & is_string)

      # replace the values and types with NA for identified "NA" strings
      d$tool$wb$worksheets[[i]]$sheet_data$v[is_na_string] <- NA_character_
      d$tool$wb$worksheets[[i]]$sheet_data$t[is_na_string] <- NA
    }
  }

  return(d)
}
