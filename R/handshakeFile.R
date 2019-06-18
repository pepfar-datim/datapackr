#' @export
#' @title Test for file read access
#' 
#' @description 
#' Given a file path, will test whether the file can be read.
#' 
#' @param path Filepath to test. Default is NA.
#' @param type File type, either \code{standard} or \code{template}.
#' 
#' @return Logical. \code{TRUE} if file can be read, \code{FALSE} if not.
#'
canReadFile <- function(path, type = "standard") {
  
  #Check that the file path was supplied.
  if (is.na(path)) {
    return(FALSE)
  }
  
  #If the file to read is a template file, check for write permissions.
  else if (type == "template") {
    file.access(path, 2) == 0
  }
  
  #If the file to read is a standard file, check for at least read permissions.
  else if (type == "standard") {
    file.access(path, 4) == 0
  }
  
}



#' @export
#' @importFrom tools file_ext
#' @title Test an input file for read access and type. Prompt if issues.
#' 
#' @description 
#' Supplied a filepath, will test that this file is readable and is of the
#' correct filetype. If either readability or file type is invalid, will prompt
#' for user selection of correct filepath via computer window.
#' 
#' @param path Filepath to test and use.
#' @param extension File extension to test for. (Do not include leading period.)
#' @param type File type, whether \code{standard} or \code{template}.
#' 
#' @return Character vector containing valid filepath for further use.
#' 
handshakeFile <- function(path = NA, extension, type = "standard") {
  
  #If path has issues or NA, prompt user to select file from window.
  if (!canReadFile(path, type = type) & interactive()) {
    interactive_print("Please choose a file.")
    path <- file.choose()
  }
  
  #Check the file can be read one more time.
  interactive_print("Checking the file exists...")
  if (!canReadFile(path, type = type)) {
    stop("File could not be read!")
  }
  
  #Check the file has correct extension
  extension = stringr::str_remove(tolower(extension),"\\.")
  if (file_ext(path) != extension) {
    stop(paste0("File is not the correct format! File must have extension .",
                extension))
  } else {
    
    return(path)
  }
  
}
