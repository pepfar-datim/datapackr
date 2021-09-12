#' @export
#' @importFrom magrittr %>% %<>%
#' @title loadTool
#'
#' @description 
#'
#' @inheritParams datapackr_params
#' 
#' @return d Sidecar object.
#' 
loadTool <- function(submission_path = NULL,
                     cop_year = NULL,
                     tool = NULL,
                     country_uids = NULL,
                     schema = NULL,
                     datapack_name = NULL) {
  
  d <- createKeychainInfo(submission_path = submission_path,
                          tool = tool,
                          country_uids = country_uids,
                          cop_year = cop_year,
                          schema = schema,
                          datapack_name = datapack_name)
  
  
  
  
  d
}
