#' @importFrom magrittr %>% %<>%
#' @title checkOUinfo(d)
#'
#' @description Cross-checks and updates PEPFAR Operating Unit name and id as
#'  read from Data Pack or Site Tool submission file.
#'
#' @param d datapackr list object containing at least d$keychain$submission_path.
#' @return A datapackr list object, \code{d}, storing a unique UID and Name for
#'    the PEPFAR Operating Unit related to the submitted Data Pack or Site Tool.
checkOUinfo <- function(d) {
  # Get OU name and uid
  d$info$datapack_uid <-
    names(readxl::read_excel(
      d$keychain$submission_path,
      sheet = "Home",
      range = "B25"
    ))
  
  datapack_region_name <-
    names(readxl::read_excel(
      d$keychain$submission_path,
      sheet = "Home",
      range = "B20"
    ))
  
  regional_country_name <- 
    names(readxl::read_excel(
      d$keychain$submission_path,
      sheet = "Home",
      range = "B21"
    ))
  
  is_regional_country_pack<-length(regional_country_name) != 0
  
  d$info$datapack_name<-ifelse( is_regional_country_pack,regional_country_name,datapack_region_name )
  
  regional_country <-ifelse( is_regional_country_pack, "countryName","DataPack_name") 
  
  regional_country_uid<-ifelse(is_regional_country_pack, "countryUID", "model_uid")
  
  # Check ou_name and ou_uid match
  
  verifyDataPackNameWithUID<-function(d,regional_country_uid,regional_country ) {
    
    regional_country_uid <- rlang::sym(regional_country_uid)
    regional_country <- rlang::sym(regional_country)
    
    datapack_name <- datapackr::configFile %>%
      dplyr::filter(!!regional_country_uid == d$info$datapack_uid) %>%
      dplyr::select(!!regional_country) %>%
      dplyr::pull(!!regional_country) %>% 
      unique()
    
    #If we get nothing here (like the UID does not exist, we need to bail early)
    if ( length(datapack_name) == 0  ) {
      stop("Unknown DataPack Name. Please contact the DataPack Support Team!")
    }
    
    
    
    datapack_uid <- datapackr::configFile %>%
      dplyr::filter(!!regional_country == d$info$datapack_name) %>%
      dplyr::select(!!regional_country_uid) %>%
      dplyr::pull(!!regional_country_uid) %>% 
      unique()
    
    #If we get nothing here (like the UID does not exist, we need to bail early)
    if ( length(datapack_uid) == 0 ) {
      stop("Unknown DataPack UID. Please contact the DataPack Support Team!")
    }
    
    
    # If OU name and UID do not match, force identification via user prompt in Console
    if (d$info$datapack_name != datapack_name |
        d$info$datapack_uid != datapack_uid) {
      msg <-
        "The OU UID and OU name used in this submission don't match up!"
      interactive_print(msg)
      d$info$warningMsg <- append(msg, d$info$warningMsg)
      if (interactive()) {
        d$info$datapack_name <- selectOU()
      } else {
        stop(msg)
      }
    }
    
    d
    
  }
  
  verifyDataPackNameWithUID(d,regional_country_uid,regional_country)
  
}
