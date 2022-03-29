#' @export
#' @title Detect potentially troublesome external connections.
#'
#' @description
#' Searches an Excel workbook for any external connections.
#'
#'
#' @param d datapackr object
#'
#' @return Logical whether there exists a connection in a specified Excel file
#'
checkConnections <- function(d) {

  if (is.null(d$tool$wb)) {
    wb <- openxlsx::loadWorkbook(file = d$keychain$submission_path)
    d$tool$wb <- wb
  } else {
    wb <- d$tool$wb
  }

  d$info$has_connections <- any(sapply(wb$connections, length) != 0)

  if (d$info$has_connections) {
    warning_msg <-
      paste0(
        "ERROR! Your workbook contains at least one external connection.
        This connection or external link must be removed prior to final
        to final submission.\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE
  }

  return(d)

}
