#' @export
#' @title Detect potentially troublesome external connections.
#'
#' @description
#' Searches an Excel workbook for any external connections.
#'
#'
#' @param d datapackr object
#'
#' @return datapackr d object
#'
checkConnections <- function(d) {

  if (is.null(d$info$has_extract)) {
    d <- extractWorkbook(d)
  }

  d$info$has_connections <-
    any(list.files(paste0(d$keychain$extract_path, "/xl")) == "connections.xml")

  if (d$info$has_connections) {
    warning_msg <-
      paste0(
        "WARNING! Your workbook contains at least one external connection.
        This connection or external link should be removed prior to final
        to final submission.\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
    d$info$has_error <- TRUE
  }

  return(d)

}
