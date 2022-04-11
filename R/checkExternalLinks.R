#' @export
#' @title Detect the presense of any external links in a workbook.
#'
#' @description
#' Searches an Excel workbook for any external links.
#'
#'
#' @param d datapackr object
#'
#' @return datapackr d object
#'
checkExternalLinks <- function(d) {

  if (is.null(d$info$worbook_contents)) {
    d <- listWorkbookContents(d)
  }

  d$info$has_external_links <- any(grepl("xl/externalLinks/externalLink\\d+\\.xml", d$info$worbook_contents))

  if (d$info$has_external_links) {
    warning_msg <-
      paste(
        "ERROR! Your workbook contains at least one external link.",
        "This usually results from copying and pasting from another workbook.",
        "Please find and remove the external links in your DataPack.",
        "This error may result in other validation checks failing to run properly",
        "and should be fixed immediately.")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "ERROR")
    d$info$has_error <- TRUE
  }

  d

}
