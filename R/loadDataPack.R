#' @export
#' @title Load a Data Pack from a supplied filepath.
#' @author Scott Jackson
#' @description loadDataPack returns a Data Pack object conserving styles and
#' formatting of the original Data Pack .xlsx file, as well as other metadata
#' necessary for processing and analyzing data in the Data Pack.
#'
#' @inheritParams datapackr_params
#'
#' @return Data Pack object
#'
loadDataPack <- function(submission_path = NULL) {
  
  d <- createKeychainInfo(submission_path)
  
  if (interactive()) {
    msg <- paste0("Congratulations. You have loaded a ",
                  "COP", stringr::str_sub(d$info$cop_year, -2,-1),
                  " ", d$info$tool,
                  " for ", d$info$datapack_name, ".")
    
    print(msg)
  }
  
  d
  
}
