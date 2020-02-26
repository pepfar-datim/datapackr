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
  
  # Check whether to write anything into SNU x IM tab and write if needed  
  if ( !is.null(d$keychain$snuxim_model_data_path ) ) {
    d <- packSNUxIM(d)
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
    } else {stop("Must supply output_folder path in order to export Data Pack")}
    
  }
  
  # If warnings, show all grouped by sheet and issue
  if (!is.null(d$info$warning_msg) & interactive()) {
    options(warning.length = 8170)
    cat(crayon::red(d$info$warning_msg))
  }
  
  return(d)
  
}
