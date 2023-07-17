#' @export
#' @title Test for file read access
#'
#' @description
#' Given a file path, will test whether the file can be read.
#'
#' @param path Filepath to test. Default is NA.
#'
#' @return Logical. \code{TRUE} if file can be read, \code{FALSE} if not.
#'
canReadFile <- function(path = NULL) {

  # Check that the file path was supplied.
  if (is.null(path)) {
    return(FALSE)
  }

  # Check for at least read permissions.
    file.access(path, 4) == 0
}



#' @export
#' @title Test an input file for read access and type. Prompt if issues.
#'
#' @description
#' When supplied a filepath, will test that the file is readable and is of the
#' correct filetype. If either readability or file type is invalid, will prompt
#' for user selection of correct filepath via computer window.
#'
#' @param path Filepath to test and use.
#' @param tool What type of tool is the submission file? Options include
#' "Data Pack", "Data Pack Template", and "OPU Data Pack Template".
#'
#' @return Character vector containing valid filepath for further use.
#'
handshakeFile <- function(path,
                          tool) {

  # Collect parameters
  tool <- tool %missing% NULL
  tool_provided <- !is.null(tool)

  if (tool %in% datapackrSupports()$tools) {
    extension <- "xlsx"
  } else {
    msg <- paste0("Please specify correct file type:",
                  paste(datapackrSupports()$tools, sep = "", collapse = ","))
    stop(msg)
  }

  tool <- stringr::str_remove(tool, " Template$")

  tool <- check_tool(tool = tool)

  # If path has issues or NA, prompt user to select file from window.
  if (!canReadFile(path)) {

    if (interactive()) {
      interactive_print("Please choose a file.")
      path <- file.choose()
    }

    if (!canReadFile(path)) {
      stop("File could not be read!")
    }

  }

  # Check the file has correct extension
  if (tools::file_ext(path) != extension) {
    stop(paste0(
      "File is not the correct format! File must have extension .",
      extension
    ))
  } else {
    return(path)
  }

}
