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
                       d2_session = dynGet("d2_default_session",
                                           inherits = TRUE)) {
  d <- createKeychainInfo(submission_path,
                          tool,
                          country_uids,
                          cop_year,
                          d2_session)

  #TODO: Are we sure we want to make this change ? If so, the object must be cloned.

  #d$keychain$d2_session <- d2_session$clone()

  # unPack file based on type
  if (d$info$tool == "Data Pack") {
    d <- unPackDataPack(d,
                        d2_session = d2_session)
  } else if (d$info$tool == "OPU Data Pack") {
    d <- unPackOPUDataPack(d,
                           d2_session = d2_session)
  } else {
    stop("Selected tool not currently supported.")
  }

  printMessages(d$info$messages)

  return(d)
}
