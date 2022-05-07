#' @export
#' @title Unpack data from Data Pack sheets.
#'
#' @description
#' Loops through all critical sheets in a submitted Data Pack
#' and extracts data, then compiles into single flat dataframe.
#'
#' @inheritParams datapackr_params
#'
#' @return d
#'
unPackSheets <- function(d, sheets = NULL) {
  
  if (d$info$tool != "Data Pack") {
    stop("Cannot unpack sheets for that kind of tool.")
  }

  sheets_to_read <- names(d$sheets)
  sheets_to_read <- sheets_to_read[!sheets_to_read %in% c("PSNUxIM")]
  
  if (!is.null(sheets)) {
    sheets_to_read <- sheets_to_read[sheets_to_read %in% sheets]
    
    unreadable_sheets <- sheets[!sheets %in% sheets_to_read]
    
    if (length(unreadable_sheets) > 0) {
      interactive_warning(
        paste0(
          "The following sheets provided to unPackSheets were either not present",
          " or are invalid. ->  \n\t* ",
          paste(unreadable_sheets, collapse = "\n\t* "),
          "\n"))
    }
  }

  for (sheet in sheets_to_read) {
    interactive_print(sheet)

    d <- unPackDataPackSheet(d, sheet = sheet)

    if (!is.null(d$data[[as.character(sheet)]])) {
      d$data$targets <- dplyr::bind_rows(d$data$targets, d$data[[as.character(sheet)]])
    }
  }

  return(d)
}
