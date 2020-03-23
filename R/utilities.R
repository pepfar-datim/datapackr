#' @export
#' @title Returns `default` categoryOptionCombo uid.
#'
#' @return `Default` categoryOptionCombo uid.
#' 
default_catOptCombo <- function() { "HllvX50cXC0" }


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
  interactive_print(promptText)
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
  impatt_levels <-
    paste0(getOption("baseurl"),"api/",datapackr::api_version(),
           "/dataStore/dataSetAssignments/orgUnitLevels") %>%
    httr::GET() %>%
    httr::content(., "text") %>%
    jsonlite::fromJSON(., flatten = TRUE) %>%
    do.call(rbind.data.frame, .) %>%
    dplyr::rename(operating_unit = name3, country_name = name4) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(country_name =
                    dplyr::case_when(country == 3 ~ operating_unit,
                                     country == 4 ~ country_name))
  
  # Add country_uids ####
  countries <-
    datapackr::api_call("organisationUnits") %>%
    datapackr::api_filter(field = "organisationUnitGroups.id",
                          operation = "eq",
                          match = "cNzfcPWEGSH") %>%
    datapackr::api_fields(fields = "id,name,level,ancestors[id,name]") %>%
    datapackr::api_get()
  
  impatt_levels %<>%
    dplyr::left_join(countries, by = c("country_name" = "name")) %>%
    dplyr::rename(country_uid = id) %>%
    dplyr::select(operating_unit, country_name, country_uid,
                  dplyr::everything(), -ancestors, -level)
  
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
  #loginToDATIM(getOption("secrets"))
  
  militaryNodes <- paste0(
    getOption("baseurl"),"api/",datapackr::api_version(),
      "/organisationUnits.json?paging=false",
      "&filter=name:$ilike:_Military",
      #"&filter=organisationUnitGroups.id:eq:nwQbMeALRjL", (New _Mil nodes not here...)
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


#' @importFrom lazyeval interp
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
  # Grab column names from `from`
    cols = colnames(from)
  
  # If `from` is a null dataframe, skip and return `to`
    if (length(cols) != 0) {
  
  # Loop through `from` columns and if there's a match in `to`, copy and paste
  #   it into `to`
      for (i in 1:length(cols)) {
        col = cols[i]
        if (col %in% colnames(to)) {
          dots <-
            stats::setNames(list(lazyeval::interp(
              ~ magrittr::use_series(from, x), x = as.name(col)
            )), col)
          to <- to %>%
            dplyr::mutate_(.dots = dots)
        } else {
          next
        }
      }
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



#' @export
#' 
#' @title Prints message if session is interactive.
#' 
#' @description 
#' Supplied a message, will print it only if the session is currently interactive.
#' 
#' @param x Message to print.
#' 
#' @return Printed message, \code{x}.
#' 
interactive_print <- function(x) {
  if (interactive()) { print(x) }
}



#' @export
#' @title Pull list of Countries from DATIM.
#' 
#' @description
#' Queries DATIM to extract list of Countries for specified Data Pack UID and
#' adds additional Countries not currently in DATIM as needed.
#' 
#' @param datapack_uid A unique ID specifying the PEPFAR Operating Unit or
#' specific Data Pack country grouping. If left unspecified, will pull all
#' Country Names.
#' 
#' @return Data frame of Countries
#' 
getCountries <- function(datapack_uid = NA) {
  
  # Pull Country List
    countries <-
      datapackr::api_call("organisationUnits") %>%
      datapackr::api_filter(field = "organisationUnitGroups.id",
                            operation = "eq",
                            match = "cNzfcPWEGSH") %>%
      datapackr::api_fields(fields = "id,name,level,ancestors[id,name]") %>%
      datapackr::api_get() %>%
    
  # Remove countries no longer supported
      dplyr::filter(
        !name %in% 
          c("Antigua & Barbuda","Bahamas","Belize","China","Dominica","Grenada",
            "Saint Kitts & Nevis","Saint Lucia","Saint Vincent & the Grenadines",
            "Turkmenistan","Uzbekistan")) %>%
      dplyr::select(country_name = name, country_uid = id, dplyr::everything()) %>%
  
  # Add metadata
      dplyr::mutate(
        data_pack_name = dplyr::case_when(
          country_name %in% c("Burma","Cambodia","India","Indonesia",
                              "Kazakhstan","Kyrgyzstan","Laos",
                              "Nepal","Papua New Guinea","Tajikistan",
                              "Thailand") ~ "Asia Region",
          country_name %in% c("Barbados","Guyana","Jamaica","Suriname",
                              "Trinidad & Tobago") ~ "Caribbean Region",
          country_name %in% c("Brazil","Costa Rica","El Salvador",
                              "Guatemala","Honduras","Nicaragua",
                              "Panama") ~ "Central America Region",
          country_name %in% c("Burkina Faso","Ghana","Liberia","Mali",
                              "Senegal","Sierra Leone","Togo") 
                              ~ "West Africa Region",
          TRUE ~ country_name),
        model_uid = dplyr::case_when(
          data_pack_name == "Asia Region" ~ "ptVxnBssua6",
          data_pack_name == "Caribbean Region" ~ "nBo9Y4yZubB",
          data_pack_name == "Central America Region" ~ "vSu0nPMbq7b",
          data_pack_name == "West Africa Region" ~ "G0BT4KrJouu",
          TRUE ~ country_uid
        ),
        is_region = data_pack_name %in% c("Asia Region",
                                          "Caribbean Region",
                                          "Central America Region",
                                          "West Africa Region"),
        level3name = purrr::map_chr(ancestors, list("name", 3), .default = NA),
        level3name = dplyr::if_else(level == 3, country_name, level3name),
        uidlevel3 = purrr::map_chr(ancestors, list("id", 3), .default = NA),
        uidlevel3 = dplyr::if_else(level == 3, country_uid, uidlevel3),
        level4name = dplyr::case_when(level == 4 ~ country_name),
        uidlevel4 = dplyr::case_when(level == 4 ~ country_uid),
        country_in_datim = TRUE
      )
    
  if (!is.na(datapack_uid)) {
    countries %<>%
      dplyr::filter(model_uid == datapack_uid)
  }
    
  return(countries)
    
}


#' @export
#' @title Add list of columns as NULL columns to supplied dataframe.
#' 
#' @description
#' Supplied a character vector of column names, \code{cnames}, \code{addcols}
#' will add one new, \code{NULL} column to \code{data} for each element of 
#' \code{cnames} and name it after the corresponding element of \code{cnames}.
#' 
#' @param data Dataframe to add columns.
#' @param cnames Character vector of one or more column names to be added to
#' \code{data}.
#' 
#' @return Dataframe \code{data} with added columns listed in \code{cnames}.
#'
addcols <- function(data, cnames) {
  add <- cnames[!cnames %in% names(data)]
  
  if (length(add) != 0) {
    data[add] <- NA_character_
  }
  
  return(data)
  
}

#' @export
#' @title Return current FY based on system date.
#' 
#' @return Current FY as numeric.
#'
currentFY <- function() {
  current_year <- Sys.Date() %>%
    format("%Y") %>%
    as.numeric()
  
  current_month <- Sys.Date() %>%
    format("%m") %>%
    as.numeric()
  
  current_FY <- ifelse(current_month > 9, current_year + 1, current_year)
  
  return(current_FY)
}


#' @export
#' @title Determine whether the user is logged into DATIM or not. 
#' 
#' @return TRUE or FALSE
#'
isLoggedIn<-function() {
  baseurl<-getOption("baseurl")
  if (is.null(baseurl)) {
    return(FALSE)} else {
      httr::set_config(httr::config(http_version = 0))
      url <- URLencode(URL = paste0(baseurl, "api/me"))
      #Logging in here will give us a cookie to reuse
      r <- httr::GET(url)
      if (r$status != 200L) {
        return(FALSE)
      } else {
        
        return(TRUE)
      }
    }
}


#' @export
#' @title Pull & combine all  UIDS for specified data_stream types and FY.
#' 
#' @description
#' Pulls all uids for types argument for a specified FY.
#' 
#' @param FY Reporting FY for which to filter active code lists, numeric, if null current year is used.
#' @param types Specify MER, SUBNAT, or IMPATT, or omit to specify all.
#' 
#' @return a character vector with the requested uids respecting the selection in the type parameter and FY parameter.
#'
#' @example getCopDatasetUids(FY=2021, types = c("MER", "SUBNAT", "IMPATT"))

getCopDatasetUids <- function(FY = NULL, types = NULL) {
  
  if (is.null(FY)) {FY = currentFY()}
  if (is.null(types)) {types = c("MER", "SUBNAT", "IMPATT")}
  
  data <- api_get(api_call("dataSets"))
  data <- data[grepl("^MER Targets: (Community|Facility)|MER Target Setting: PSNU|^(Host Country Targets|Planning Attributes): COP Prioritization SNU",
        data$displayName),]
  if(FY != currentFY()+1)
  {
    data <- data[grepl(paste0("FY",FY), data$displayName),]
  }else{
    data <- data[!(grepl("FY[0-9]{4}", data$displayName)),]
  }
  
  data$fiscal_year <- ifelse(!stringr::str_detect(data$displayName, "FY"), currentFY()+1, 
                        as.numeric(stringr::str_extract(data$displayName,"(?<=FY)\\d{4}$")))
  data$data_stream <- ifelse(stringr::str_detect(data$displayName, "^MER "), "MER",
                        ifelse(stringr::str_detect(data$displayName, "^Host Country Targets"),
                               "SUBNAT","IMPATT"))
  if(!(all((types %in% data$data_stream))))
  {
    stop(paste0("UID Not Found for ", setdiff(types, data$data_stream), " for FY ", FY))
  }
  
  print(paste0("returning uids for " , FY))
  return(data$id[data$data_stream %in% types])
  
}


#' @export
#' @title grab all raw data in DATIM for a country for the COP data sets for a given fiscal year.
#' 
#' @description
#' grab all raw data in DATIM for a country for the COP data sets for a given fiscal year.
#' 
#' @param country_uid country_uid
#' @param fiscal_year_yyyy fiscal_year
#' @param base_url base to append api calls normally defined beforehand
#' 
#' @return raw data in DATIM for a country for the COP data sets for a given fiscal year.
#'
#' @example getCopDataFromDatim(country_uid = d$info$country_uids, 
#'                    fiscal_year_yyyy = d$info$cop_year)

getCopDataFromDatim <- function(country_uid, fiscal_year_yyyy, base_url = getOption("baseurl"))
{
  org_unit_uids <- country_uid
  cop_yyyy <- as.character(fiscal_year_yyyy)
  fiscal_yy <- substr(fiscal_year_yyyy+1, 3, 4)  
  
  dataset_uids <- ## this should use getCopDatasetUids
    c(datapackcommons::getDatasetUids(fiscal_yy, "targets"),
      datapackcommons::getDatasetUids(fiscal_yy, "subnat_impatt"))
  
  # package parameters for getDataValueSets function call
  parameters <- 
    dplyr::bind_rows( 
      tibble::tibble(key = "dataSet", value = dataset_uids),
      tibble::tibble(key = "orgUnit", value = org_unit_uids),
      tibble::tribble(~ key, ~ value,
                      "children", "true",
                      "categoryOptionComboIdScheme", "code",
                      "includeDeleted", "false",
                      "period", paste0(cop_yyyy, "Oct")
      )
    )
  
  # get data from datim usinfg dataValueSets
  # rename to standard names
  datim_data <-
    getDataValueSets(parameters$key,
                     parameters$value,
                     base_url = base_url) %>%
    dplyr::rename(
      datim_value = value,
      data_element_uid = data_element,
      org_unit_uid = org_unit,
      category_option_combo_uid = category_option_combo,
      attribute_option_combo_code = attribute_option_combo
    )
  
  return(datim_data)
}







