#' @export
#' @importFrom magrittr %>% %<>%
#' @title writePSNUxIM(d)
#'
#' @description Checks a Data Pack for need of new or appended PSNUxIM data, then
#' writes this into the Data Pack supplied. unPackTool must be run as prerequisite.
#'
#' @param d Datapackr object
#' @param snuxim_model_data_path Filepath where SNU x IM distribution model is stored.
#' @param output_folder Local folder where you would like your Data Pack to be
#' saved upon export.
#' 
#' @return d
#' 
writePSNUxIM <- function(d,
                        snuxim_model_data_path = NULL,
                        output_folder = NULL) {
 
  d$keychain$snuxim_model_data_path = snuxim_model_data_path
  d$keychain$output_folder = output_folder
  
  # Start running log of all warning and information messages
  d$info$warning_msg <- NULL
  d$info$has_error <- FALSE
  
  if (d$info$has_comments_issue) {
    warning_msg <-
      paste0(
        "ERROR! Cannot update PSNUxIM information in a Data Pack with Threaded
        Comments. Please remove these and resubmit. For more information about
        the difference between Threaded Comments and Notes, see:
        
        https://support.office.com/en-us/article/the-difference-between-threaded-comments-and-notes-75a51eec-4092-42ab-abf8-7669077b7be3")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
    
    if (interactive()) {
      options(warning.length = 8170)
      cat(crayon::red(d$info$warning_msg))
    }
    
    return(d)
  }
  
  # Check whether to write anything into SNU x IM tab and write if needed  
  if ( !is.null(d$keychain$snuxim_model_data_path ) ) {
    if (d$info$cop_year == 2020) { 
      d <- packSNUxIM_2020(d)
    } else if (d$info$cop_year == 2021) {
      d <- packSNUxIM(d) 
    } else {
      stop(paste0("Packing SNU x IM tabs is not supported for COP ",d$info$cop_year," Data Packs."))
    }
  } else {stop("Cannot update PSNUxIM tab without model data.")}
  
  # If new information added to SNU x IM tab, reexport Data Pack for user
  if (d$info$newSNUxIM) {
    d <- strip_wb_NAs(d)
    
    if (!is.null(d$keychain$output_folder)) {
      exportPackr(
        data = d$tool$wb,
        output_path = d$keychain$output_folder,
        type = "Data Pack",
        datapack_name = d$info$datapack_name)
    }
    
  }
  
  # If warnings, show all grouped by issue
  if (!is.null(d$info$warning_msg) & interactive()) {
    options(warning.length = 8170)
    cat(crayon::red(d$info$warning_msg))
  }
  
  return(d)
  
}
