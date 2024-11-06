#' @export
#' @title Unpack a submitted tool
#'
#' @description
#' Processes a submitted Data Pack by identifying integrity issues, checking
#' data against DATIM validations, and extracting data.
#'
#' @inheritParams datapackr_params
#'
#' @details
#' Executes the following operations in relation to a submitted tool
#' \enumerate{
#'     \item Performs integrity checks on file structure;
#' }
#'
unPackTool <- function(submission_path = NULL,
                       tool = NULL,
                       country_uids = NULL,
                       cop_year = NULL,
                       pzns = NULL,
                       mer_data = NULL,
                       d2_session = dynGet("d2_default_session",
                                           inherits = TRUE)) {

  d <- loadDataPack(submission_path = submission_path,
                    tool = tool,
                    country_uids = country_uids,
                    cop_year = cop_year,
                    load_wb = FALSE,
                    load_sheets = FALSE,
                    d2_session = d2_session)

  # unPack file based on type
  if (d$info$tool == "Data Pack") {
    d <- unPackDataPack(d,
                        d2_session = d2_session)

    if (d$info$cop_year %in% c("2023", "2024", "2025")) {
      d <- unpackYear2Sheet(d)
    }

  } else if (d$info$tool %in% c("OPU Data Pack", "PSNUxIM")) {
    d <- unPackOPUDataPack(d,
                           pzns = pzns,
                           mer_data = mer_data,
                           d2_session = d2_session)
  } else {
    stop("Selected tool not currently supported.")
  }

  #Modify all messages with the name of the tool
  if (!is.null(d$info$messages$tool) && length(d$info$messages$tool) > 0) {
    d$info$messages$tool <- rep(d$info$tool, length(d$info$messages$tool))
  }

  return(d)
}
