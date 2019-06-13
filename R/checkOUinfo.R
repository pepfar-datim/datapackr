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
  d$info$datapack_name <-
    names(readxl::read_excel(
      d$keychain$submission_path,
      sheet = "Home",
      range = "B20"
    ))
  
  # Check ou_name and ou_uid match
  datapack_name <- datapackr::configFile %>%
    dplyr::filter(model_uid == d$info$datapack_uid) %>%
    dplyr::select(DataPack_name) %>%
    unique() %>%
    dplyr::pull(DataPack_name)
  
  datapack_uid <- datapackr::configFile %>%
    dplyr::filter(DataPack_name == d$info$datapack_name) %>%
    dplyr::select(model_uid) %>%
    unique() %>%
    dplyr::pull(model_uid)
  
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
    
    d$info$datapack_uid <- datapackr::configFile %>%
      dplyr::filter(DataPack_name == d$info$datapack_name) %>%
      dplyr::select(model_uid) %>%
      unique() %>%
      dplyr::pull(model_uid)
  }
  
  return(d)
}


#' @title checkSiteToolOUinfo(d)
#'
#' @description Cross-checks and updates PEPFAR Operating Unit name and id as
#'  read from Data Pack or Site Tool submission file.
#'
#' @param d datapackr list object containing at least d$keychain$submission_path.
#' @return A datapackr list object, \code{d}, storing a unique UID and Name for
#'    the PEPFAR Operating Unit related to the submitted Data Pack or Site Tool.
checkSiteToolOUinfo <- function(d) {
  # Get OU name and uid
  d$info$datapack_uid <-
    names(readxl::read_excel(
      d$keychain$submission_path,
      sheet = "Home",
      range = "B25"
    ))
  d$info$datapack_name <-
    names(readxl::read_excel(
      d$keychain$submission_path,
      sheet = "Home",
      range = "B20"
    ))
  
  # Check ou_name and ou_uid match
  datapack_name <- datapackr::configFile %>%
    dplyr::filter(model_uid == d$info$datapack_uid) %>%
    dplyr::select(DataPack_name) %>%
    unique() %>%
    dplyr::pull(DataPack_name)
  
  datapack_uid <- datapackr::configFile %>%
    dplyr::filter(DataPack_name == d$info$datapack_name) %>%
    dplyr::select(model_uid) %>%
    unique() %>%
    dplyr::pull(model_uid)
  
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
    
    d$info$datapack_uid <- datapackr::configFile %>%
      dplyr::filter(DataPack_name == d$info$datapack_name) %>%
      dplyr::select(model_uid) %>%
      unique() %>%
      dplyr::pull(model_uid)
  }
  
  return(d)
}
