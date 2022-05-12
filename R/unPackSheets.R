#' @export
#' @title Unpack data from Data Pack sheets.
#'
#' @description
#' Loops through all critical sheets in a submitted Data Pack
#' and extracts data, then compiles into single flat dataframe.
#'
#' @inheritParams datapackr_params
#' 
#'
#' @return d
#'
unPackSheets <- function(d, sheets = NULL) {

  if (d$info$tool != "Data Pack") {
    stop("Cannot process that kind of tool. :(")
  }
  
  sheets <- sheets %||% grep("PSNUxIM", names(d$sheets), value = TRUE, invert = TRUE)
  
  sheets <- checkSheets(sheets = sheets,
                        cop_year = d$info$cop_year,
                        tool = d$info$tool,
                        all_sheets = FALSE,
                        psnuxim = FALSE)
  
  # Implementing this here instead of in unPackDataPack or unPackTool because 
  # while you may want to checkSheetData without running unPackSheets, you should
  # should never unPackSheets without running checkSheetData
  interactive_print("Checking sheet data...")
  d <- checkSheetData(d, sheets = sheets)

  # Unpack Sheet Data ----
  targets <- NULL

  for (sheet in sheets) {
    interactive_print(sheet)
    
    extract <- unPackDataPackSheet(d, sheet = sheet)

    if (!is.null(extract)) {
      targets <- dplyr::bind_rows(targets, extract)
    }
  }
  
  d$data$targets <- targets

  return(d)
}
