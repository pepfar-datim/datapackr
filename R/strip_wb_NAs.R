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

  process_sheet <- function(sheet, NA_sharedString) {
    # identify cells with sharedStrings that might be "NA"
    might_be_NA <- sheet$sheet_data$v %in% NA_sharedString

    # identify cells that are string types
    is_string <- sheet$sheet_data$t == 1

    # combine both conditions to find actual "NA" strings
    is_na_string <- ifelse(is.na(might_be_NA & is_string), FALSE, might_be_NA & is_string)

    # replace the values and types with NA for identified "NA" strings
    sheet$sheet_data$v[is_na_string] <- NA_character_
    sheet$sheet_data$t[is_na_string] <- NA

    return(sheet)
  }

  # find indices of sharedStrings matching "<si><t>NA</t></si>"
  NA_sharedString <- grep("<si><t>NA</t></si>", d$tool$wb$sharedStrings) - 1

  # proceed only if such sharedStrings are found
  if (length(NA_sharedString) > 0) {
    d$tool$wb$worksheets <- lapply(d$tool$wb$worksheets, process_sheet, NA_sharedString)
  }

  return(d)
}
