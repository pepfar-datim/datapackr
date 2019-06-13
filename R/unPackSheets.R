#' @importFrom magrittr %>% %<>%
#' @title unPackSheets(d)
#'
#' @description Loops through all critical sheets in a submitted Data Pack or
#'     Site Tool and executes \code{\link{unPackSheet}} to extract data, then
#'     compiles data into single flat dataframe. Also executes
#'     \code{\link{separateDataSets}} to separate single dataframe into at least
#'     two for \code{MER} and \code{SUBNAT/IMPATT}.
#'
#' @param d datapackr list object containing at least
#'     \code{d$keychain$submission_path}.
#' @return A datapackr list object, \code{d}, storing at least 2 dataframes of
#'    data extracted from submitted Data Pack or Site Tool: a \code{d$data$MER}
#'    dataframe containing all MER data to be distributed to site level, and/or
#'    \code{d$data$SUBNAT_IMPATT} containing data in the SUBNAT and IMPATT
#'    datasets from DATIM that can be imported into DATIM at the PSNU level.
unPackSheets <- function(d) {
  # Get sheets list
  sheets <- datapackr::data_pack_schema %>%
    dplyr::select(sheet_name) %>%
    dplyr::filter(sheet_name != "SNU x IM") %>%
    dplyr::distinct() %>%
    dplyr::pull(sheet_name)
  actual_sheets <-
    readxl::excel_sheets(d$keychain$submission_path)
  sheets_to_read <- actual_sheets[actual_sheets %in% sheets]
  
  d$data$targets <- NULL
  
  for (i in 1:length(sheets_to_read)) {
    d$data$sheet = sheets_to_read[i]
    interactive_print(d$data$sheet)
    d <- unPackSheet(d)
    d$data$targets <-
      dplyr::bind_rows(d$data$targets, d$data$extract)
  }
  
  d <- separateDataSets(d)
  
  return(d)
}

#' @title unPackSiteToolSheets(d)
#'
#' @description Loops through all critical sheets in a submitted 
#'     Site Tool and executes \code{\link{unPackSheet}} to extract data, then
#'     compiles data into single flat dataframe. Also executes
#'     \code{\link{separateDataSets}} to separate single dataframe into at least
#'     two for \code{MER} and \code{SUBNAT/IMPATT}.
#'
#' @param d datapackr list object containing at least
#'     \code{d$keychain$submission_path}.
#' @return A datapackr list object, \code{d}, storing at least 2 dataframes of
#'    data extracted from submitted Data Pack or Site Tool: a \code{d$data$MER}
#'    dataframe containing all MER data to be distributed to site level, and/or
#'    \code{d$data$SUBNAT_IMPATT} containing data in the SUBNAT and IMPATT
#'    datasets from DATIM that can be imported into DATIM at the PSNU level.
unPackSiteToolSheets <- function(d) {
  # Get sheets list
  sheets <- datapackr::site_tool_schema %>%
    dplyr::select(sheet_name) %>%
    dplyr::distinct() %>%
    dplyr::pull(sheet_name)
  
  actual_sheets <-
    readxl::excel_sheets(d$keychain$submission_path)
  sheets_to_read <- actual_sheets[actual_sheets %in% sheets]
  
  d$data$targets <- NULL
  
  for (i in 1:length(sheets_to_read)) {
    d$data$sheet = sheets_to_read[i]
    interactive_print(d$data$sheet)
    d <- unPackSiteToolSheet(d)
    
    if (!is.null(d$data$extract)) {
      d$data$targets <-
        dplyr::bind_rows(d$data$targets, d$data$extract)
    }
    
  }
  
  return(d)
}
