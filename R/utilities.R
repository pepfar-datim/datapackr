#' @title Round at 0.5 toward integer with highest absolute value
#'
#' @description
#' In normal R rounding, if the first digit to be dropped is exactly 5, R uses
#' the standard programming convention of rounding to the nearest even number.
#' This can have some annoying effects.
#'
#' This function rounds numbers to the nearest integer, but always rounds to the
#' integer with the highest absolute value when the first digit to be dropped is
#' exactly 5, similar to rounding in usual mathematical contexts.
#'
#' @param x A number.
#' @return An integer.
#' @examples
#' # If the first digit to be dropped is exactly 5, round_trunc() will round to
#' # integer with the highest absolute value.
#' round_trunc(0.5)
#' round_trunc(-0.5)
#' @export
round_trunc <- function(x) {
    trunc(abs(x) + 0.5) * sign(x)
}


#' @export
#' @title Use the console to select OU
#' @importFrom magrittr %>%
#' @importFrom utils select.list
#'
#' @description
#' In some cases it may be necessary to manually identify the Operating Unit
#' associated with a submitted Data Pack or Site Tool. This function allows
#' manual selection of Operating Unit using the R console.
#'
#' One case where this is necessarily invoked is when OU name and OU id read
#' from a submitted Data Pack or Site Tool do not match one another, based on
#' cross-reference with DATIM organization hierarchies.
#'
#' @return An OU name, based on input selection.
selectOU <- function() {
  ous <- datapackr::configFile %>%
    dplyr::select(DataPack_name) %>%
    dplyr::distinct()
  promptText<-paste0("Please select the OU this file is associated with [1-",nrow(ous),"]:")
  print(promptText)
  selection <- utils::select.list(ous$DataPack_name,multiple=FALSE)
  return(selection)
}



#' @export
#' @title Pull IMPATT levels from DATIM for all PEPFAR countries
#' 
#' @description 
#' Queries DATIM to retrieve the latest version of
#' \code{/api/dataStore/dataSetAssignments/ous}
#' 
#' @return Dataframe of country metadata, including prioritization, planning,
#' country, community, and facility levels in DATIM organization hierarchy.
#'
getIMPATTLevels <- function(){
  
  datapackr::loginToDATIM(secrets)
  
  impatt_levels <-
    paste0(getOption("baseurl"),"api/",datapackr::api_version(),
           "dataStore/dataSetAssignments/ous") %>%
    httr::GET() %>%
    httr::content(., "text") %>%
    jsonlite::fromJSON(., flatten = TRUE) %>%
    do.call(rbind.data.frame, .) %>%
    dplyr::select(operating_unit = name3, country_name = name4, dplyr::everything()) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(country_name =
                    dplyr::case_when(country_name == "" ~ operating_unit,
                                     TRUE ~ country_name))
  
  return(impatt_levels)
}


#' @export
#' @title Pull all _Military nodes from DATIM for all PEPFAR countries
#' 
#' @description 
#' Queries DATIM (\code{api/organisationUnits}) to retrieve the latest list of
#' _Military nodes for each PEPFAR country.
#' 
#' @return Dataframe of _Military names and ids, with associated Operating Units
#' and Countries.
#' 
getMilitaryNodes <- function() {
  militaryNodes <- paste0(getOption("baseurl"),"api/",datapackr::api_version(),
                          "/organisationUnits.json?paging=false",
                          "&filter=organisationUnitGroups.id:eq:nwQbMeALRjL",
                          "&fields=name,id,level,ancestors[id,name]") %>%
    httr::GET() %>%
    httr::content(., "text") %>%
    jsonlite::fromJSON(., flatten = TRUE) %>%
    do.call(rbind.data.frame, .) %>%
  # Tag Operating Unit and Country (name & id) - accommodate for eventuality of
  #    _Military at level 5 in Regional OUs
    dplyr::mutate()
}