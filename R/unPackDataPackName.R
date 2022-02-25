#' @export
#' @title Extract the name of the datapack
#'
#' @description
#' When supplied a submission path, will return the name of the datapack.
#'
#' @return Character vector of the name of the data pack.
#'
unPackDataPackName <- function(submission_path,
                              tool) {

  submission_path <- handshakeFile(path = submission_path,
                                   tool = tool)

    readxl::read_excel(
      path = submission_path,
      sheet = "Home",
      range = dataPackName_homeCell()) %>%
    names() %>%
    unlist()

}
