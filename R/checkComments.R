#' @export
#' @title Detect troublesome comment types
#'
#' @description
#' Searches an Excel file to detect whether there are any comments that cause
#' corruption when executing openxlsx::saveWorkbook
#'
#' @param d datapackr object
#'
#' @return Logical whether there exists a comments issue in specified Excel file
#'
checkComments <-   function(d) {

  wb <- openxlsx::loadWorkbook(d$keychain$submission_path)

  d$info$has_comments_issue <-
    purrr::map(wb$comments,
             function(y) purrr::map(y,
                                    function(x) is.null(purrr::pluck(x, "style")))) %>%
    unlist() %>%
    any()

  if (d$info$has_comments_issue) {
    warning_msg <-
      paste0(
        "ERROR! Your workbook contains at least one case of a new type of comment
        introduced in Office 365 called a 'Threaded Comment'. This type of comment,
        as opposed to the previous type of Notes used in Microsoft Excel, causes
        corruption issues when this app attempts to update your PSNUxIM tab.
        Prior to submitting for an updated PSNUxIM tab, you MUST remove all
        threaded comments. For more information about the differences between
        threaded comments and notes, see: https://support.office.com/en-us/article/the-difference-between-threaded-comments-and-notes-75a51eec-4092-42ab-abf8-7669077b7be3",
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg,"ERROR")
    d$info$has_error <- TRUE
  }

  return(d)

}
