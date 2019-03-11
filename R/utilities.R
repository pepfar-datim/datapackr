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
  
  loginToDATIM(getOption("secrets"))
  
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
  loginToDATIM(getOption("secrets"))
  
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
#' @title Export output.
#' 
#' @description 
#' Exports a datapackr output to specified filepath.
#'
#' @param data Data object to export. Can be either a dataframe or an Openxlsx
#' Workbook object.
#' @param output_path A local path directing to the folder where you would like
#' outputs to be saved. If not supplied, will output to working directory.
#' @param type File prefix to be applied in output filename, as follows:
#'   \describe{
#'     \item{Site Tool}{Openxlsx Workbook object containing Site Tool.}
#'     \item{Data Pack}{Openxlsx Workbook object containing Data Pack.}
#'     \item{FAST Export}{Data frame containing FAST export data.}
#'     \item{SUBNAT IMPATT}{Data frame containing SUBNAT/IMPATT data.}
#'     \item{Results Archive}{List object containing results archive.}
#' }
#' @param datapack_name Country name or OU name to be listed in output filename.
#' 
exportPackr <- function(data, output_path, type, datapack_name) {
  packName <- function(output_path, type, datapack_name, extension) {
    paste0(
      output_path,
      if (is.na(stringr::str_extract(output_path,"/$"))) {"/"} else {},
      type,"_",
      datapack_name,"_",
      format(Sys.time(), "%Y%m%d%H%M%S"),
      extension
    )
  }

  if (type %in% c("Site Tool", "Data Pack", "Mechanism Map")) {
    if (class(data) != "Workbook") {
      stop("Output type and data do not match!")
    }
    
    output_file_name <- packName(output_path, type, datapack_name, extension = ".xlsx")
    
    openxlsx::saveWorkbook(wb = data, file = output_file_name, overwrite = TRUE)
  }
  
  if (type %in% c("FAST Export","SUBNAT IMPATT")) {
    if (class(data) != "data.frame") {
      stop("Output type and data do not match!")
    }
    
    output_file_name <- packName(output_path, type, datapack_name, extension = ".csv")
    
    readr::write_csv(data, output_file_name)
  }
  
  if (type %in% c("Results Archive")) {
    if (class(data) != "list") {
      stop("Output type and data do not match!")
    }

    output_file_name <- packName(output_path, type, datapack_name, extension = ".rds")
    
    saveRDS(data, output_file_name)
  }
    
  print(paste0("Successfully saved ",type," to ", output_file_name))
}


interactive_print <- function(x) if (interactive()) { print(x) }


#' @export
#' @title Pull list of PSNUs from DATIM and format based on datapackr structure.
#' 
#' @description
#' Queries DATIM to extract list of PSNUs for specified Data Pack UID and adds
#' additional PSNUs not currently in DATIM as needed.
#' 
#' @param datapack_uid A unique ID specifying the PEPFAR Operating Unit 
#' the Site Tool belongs to.
#' 
#' @return Data frame of PSNUs
#' 
getPSNUs <- function(datapack_uid) {
  # Pull PSNUs from DATIM SQL view
    datim_country_name_string <- datapackr::configFile %>%
      dplyr::filter(model_uid == datapack_uid,
             Currently_in_DATIM == "Y") %>%
      dplyr::select(countryName) %>%
      dplyr::mutate(
        countryName = stringr::str_replace(countryName,
                                           "d'Ivoire",
                                           "d Ivoire")) %>%
      dplyr::pull(countryName) %>%
      unique() %>%
      paste(collapse = ",") %>%
    # & is a reserved character for DATIM API. URLencode() doesn't convert `&`,
    #   and URLencode(..., reserved = false) introduces unintended consequences
      stringr::str_replace_all("&","%26") %>%
      stringr::str_replace_all(" ","%20")

    r <- paste0(getOption("baseurl"),
                    "api/",
                    datapackr::api_version(),
                    "/sqlViews/PjjAyeXUbBd/data.json?",
                    "fields=operating_unit,uid,name",
                    "&filter=name:!like:_Military",
                    "&filter=operating_unit:in:[",
                    datim_country_name_string,"]") %>%
      URLencode() %>%
      httr::GET() %>%
      httr::content(., "text") %>%
      jsonlite::fromJSON(., flatten = TRUE)
    
    columnNames <- purrr::pluck(r, "headers", "name")
    
    PSNUs <- as.data.frame(r$rows,stringsAsFactors = FALSE) %>%
      setNames(., columnNames) %>%
      dplyr::select(country = operating_unit, uid, name) %>%
      dplyr::distinct()
  
  # Add new new countries as PSNUs
    newCountries <- datapackr::configFile %>%
      dplyr::filter(
        model_uid == datapack_uid,
        Currently_in_DATIM == "N",
        isMil == "0") %>%
      dplyr::select(countryName, countryUID)
    
  if (NROW(newCountries) != 0) {
    PSNUs <- PSNUs %>%
      dplyr::bind_rows(newCountries %>%
                         dplyr::mutate(name = countryName) %>%
                         dplyr::select(country = countryName,
                                      uid = countryUID,
                                      name))
  }
  
  # No Mil PSNUs at this point. Add them from Config file
    milPSNUs <- datapackr::configFile %>%
      dplyr::filter(model_uid == datapack_uid,
             !is.na(milPSNU)) %>%
      dplyr::select(DataPack_name, milPSNU, milPSNUuid, milPSNU_in_DATIM) %>%
      dplyr::distinct()
    
    PSNUs <- PSNUs %>%
      dplyr::bind_rows(milPSNUs %>%
                       dplyr::select(country = DataPack_name,
                              uid = milPSNUuid,
                              name = milPSNU))
  
  # Patch for Suriname and Jamaica
  if(datapack_uid == "Caribbean_Data_Pack") {
    patchList <- c("Suriname","Jamaica")
    
    JamaicaSuriname = NULL
    
    for (i in 1:length(patchList)) {
      URL <- paste0(getOption("baseurl"),
                    "api/sqlViews/kEtZ2bSQCu2/data.json?fields=level4name,organisationunituid,name&filter=level4name:eq:",
                    patchList[i],
                    dplyr::case_when(patchList[i] == "Jamaica" ~ "&filter=level:eq:6",
                                     patchList[i] == "Suriname" ~ "&filter=level:eq:5"))
      Export <- URL %>%
        URLencode() %>%
        httr::GET() %>%
        httr::content(., "text") %>%
        jsonlite::fromJSON(., flatten = TRUE)
      JamaicaSuriname <- as.data.frame(Export$rows,stringsAsFactors = FALSE) %>%
        setNames(.,Export$headers$name) %>%
        dplyr::select(country = level4name, uid = organisationunituid, name) %>%
        dplyr::distinct() %>%
        dplyr::bind_rows(JamaicaSuriname,.)
    }
    
    PSNUs <- PSNUs %>%
      dplyr::filter(!country %in% c("Jamaica", "Suriname")) %>%
      dplyr::bind_rows(JamaicaSuriname)
  }
  
  # Create Data Pack Site ID & tag with country name breadcrumb where country != PSNU
  isRegion = datapackr::configFile %>%
    dplyr::filter(model_uid == datapack_uid) %>%
    dplyr::pull(isRegion) %>%
    unique()
    
  needsCountryTag = datapackr::configFile %>%
    dplyr::filter(model_uid == datapack_uid &
                  Prioritizing_at_Natl_or_SNU == "SNU" &
                  isRegion == 1) %>%
    dplyr::pull(countryName) %>%
    unique()
    
    PSNUs <- PSNUs %>%
      dplyr::mutate(DataPackSiteID = paste0(
        ## Country Name only where country != PSNU
        dplyr::case_when(isRegion == 1
                         & country %in% needsCountryTag
                         ~ paste0(country," > "),
                         TRUE ~ ""),
        ## name & uid
        name, " (", uid,")")
    )
  
  return(PSNUs)
}
