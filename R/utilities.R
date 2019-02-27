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
#' @importFrom magrittr %>% %<>%
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
  
  datapackr::loginToDATIM(getOption("secrets"))
  
  impatt_levels <-
    paste0(getOption("baseurl"),"api/",datapackr::api_version(),
           "/dataStore/dataSetAssignments/ous") %>%
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
#' @importFrom magrittr %>% %<>%
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
  datapackr::loginToDATIM(getOption("secrets"))
  
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
    dplyr::mutate(
      country_uid = dplyr::case_when(
        level == 4 ~ purrr::map_chr(ancestors,
                              function(x) magrittr::use_series(x, id) %>%
                                magrittr::extract(3)),
        level == 5 ~ purrr::map_chr(ancestors,
                                    function(x) magrittr::use_series(x, id) %>%
                                      magrittr::extract(4))),
      country_name = dplyr::case_when(
        level == 4 ~ purrr::map_chr(ancestors,
                                    function(x) magrittr::use_series(x, name) %>%
                                      magrittr::extract(3)),
        level == 5 ~ purrr::map_chr(ancestors,
                                    function(x) magrittr::use_series(x, name) %>%
                                      magrittr::extract(4))
      )
    ) %>%
    dplyr::select(-ancestors)
  
  return(militaryNodes)
}

#' @export
#' @title Swap columns between dataframes
#' 
#' @description 
#' Replaces columns in \code{to} with those with identical names in \code{from}.
#' 
#' @param to Dataframe to pull columns into
#' @param from Data frame to pull columns from
#' 
#' @return dataframe with swapped columns
#' 
swapColumns <- function(to, from) {
  cols = colnames(from)
  for (i in 1:length(cols)) {
    col = cols[i]
    if (col %in% colnames(to)) {
      dots <- setNames(list(lazyeval::interp(~ magrittr::use_series(from,x), x = as.name(col))), col)
      to <- to %>%
        dplyr::mutate_(.dots = dots)
    } else {next}
  }
  return(to)
}

#' @export
#' @title Write character vector of Excel formulas into Openxlsx Workbook object
#' horizontally, rather than vertically.
#' 
#' @description
#' Takes a character vector of Excel formulas (\code{x}) and writes these into
#' an Openxlsx Workbook object (\code{wb}) horizontally, beginning at the cell
#' position demarcated by \code{xy}. This function augments the existing
#' function in the Openxlsx package \code{\link[openxlsx]{writeFormula}} by
#' overcoming its strict ability to write formulas only rowwise.
#' 
#' @param wb A Workbook object containing a worksheet.
#' @param sheet The worksheet to write to. Can be the worksheet index or name.
#' @param x A character vector of Excel formulas.
#' @param xy A vector of the form \code{c(start column, start row)}.
#' 
writeFxColumnwise <- function(wb, sheet, x, xy) {
  fx <- x %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate_all(as.character)
  
  for (i in 1:length(fx)) {
    class(fx[[i]]) <- c(class(fx[[i]]), "formula")
  }
  
  openxlsx::writeData(wb = wb, sheet = sheet, x = fx, xy = xy, colNames = FALSE)
}