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
canReadFile <- function(path) {
  
  # Check that the file path was supplied.
  if (is.na(path)) {
    return(FALSE)
  }
  
  # Check for at least read permissions.
    file.access(path, 4) == 0
    
}



#' @export
#' @importFrom tools file_ext
#' @title Test an input file for read access and type. Prompt if issues.
#' 
#' @description 
#' When supplied a filepath, will test that the file is readable and is of the
#' correct filetype. If either readability or file type is invalid, will prompt
#' for user selection of correct filepath via computer window.
#' 
#' @param path Filepath to test and use.
#' @param tool What type of tool is the submission file? Options include 
#' "Data Pack", "Data Pack Template", Site Tool", "Site Tool Template",
#' "Mechanism Map", and "Site Filter".
#' 
#' @return Character vector containing valid filepath for further use.
#' 
handshakeFile <- function(path = NA,
                          tool = NA) {
  
  if (tool %in% c("Data Pack", "Data Pack Template", "Site Tool",
                  "Site Tool Template", "Mechanism Map")) {
    extension = "xlsx"
  } else if (tool == "Site Filter") {
    extension = "csv"
  } else {
    stop(
      "Please specify correct file type: Data Pack, Data Pack Template, Site Tool,
      Site Tool Template, Mechanism Map, Data Pack Template, or Site Filter.")
    }
  
  # If path has issues or NA, prompt user to select file from window.
  if ( !canReadFile(path) ) {
    if ( interactive() ) {
      interactive_print("Please choose a file.")
      path <- file.choose()
      
      if (!canReadFile(path)) {
        stop("File could not be read!")
      }
      
    } else {
      stop("File could not be read!")
    }
    
  }
  
  # Check the file has correct extension
  if (file_ext(path) != extension) {
    stop(paste0("File is not the correct format! File must have extension .",
                extension))
  } else {return(path)}
  
}
