#' @export
#' @title Returns `default` categoryOptionCombo uid.
#'
#' @return `Default` categoryOptionCombo uid.
#' 
default_catOptCombo <- function() { "HllvX50cXC0" }


#' @export
#' @title Returns current COP Year
#'
#' @return Current COP Year. (e.g., for COP19, returns 2019)
#' 
cop_year <- function() { 2019 }



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
           "/dataStore/dataSetAssignments/ous") %>%
    httr::GET() %>%
    httr::content(., "text") %>%
    jsonlite::fromJSON(., flatten = TRUE) %>%
    do.call(rbind.data.frame, .) %>%
    dplyr::select(operating_unit = name3, country_name = name4, dplyr::everything()) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(country_name =
                    dplyr::case_when(country == 3 ~ operating_unit,
                                     country == 4 ~ country_name))
  
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
#' @title Pull list of PSNUs from DATIM and format based on datapackr structure.
#' 
#' @description
#' Queries DATIM to extract list of PSNUs for specified Data Pack UID and adds
#' additional PSNUs not currently in DATIM as needed.
#' 
#' @param country_uids Character vector of DATIM country IDs. This can only
#' include countries. Regional Operating Unit uids will not be accepted
#' @param include_mil Logical. If \code{TRUE}, will also include _Military nodes
#' related to \code{country_uids}. Default is \code{FALSE}.
#' 
#' @return Data frame of PSNUs
#' 
getPSNUs <- function(country_uids = NA,
                     include_mil = FALSE) {
  
  countries <- datapackr::api_call("organisationUnits") %>%
    datapackr::api_filter("organisationUnitGroups.id:eq:cNzfcPWEGSH") %>%
    datapackr::api_fields("id,name,level") %>%
    datapackr::api_get()
  
  PSNUs <- datapackr::api_call("organisationUnits") %>%
    datapackr::api_filter(paste0("organisationUnitGroups.id:in:[AVy8gJXym2D",
                                 dplyr::if_else(include_mil, ",nwQbMeALRjL", ""),
                                 "]")) %>%
    datapackr::api_fields("id,name,level,ancestors[id,name],organisationUnitGroups[id,name]") %>%
    datapackr::api_get() %>%
    dplyr::mutate(
      operating_unit = purrr::map_chr(ancestors,list("name",3)),
      operating_unit_id = purrr::map_chr(ancestors,list("id",3)),
      psnu_type =
        dplyr::case_when(
          stringr::str_detect(as.character(organisationUnitGroups), "nwQbMeALRjL") ~ "Military",
          stringr::str_detect(as.character(organisationUnitGroups), "AVy8gJXym2D") ~ "PSNU"),
      in_region = operating_unit_id %in% 
        (datapackr::dataPackMap %>%
           dplyr::filter(is_region) %>%
           dplyr::pull(uidlevel3) %>%
           unique()),
      country_uid = dplyr::case_when(
        operating_unit_id %in% countries$id ~ operating_unit_id,
        id %in% countries$id ~ id,
        TRUE ~ purrr::map_chr(ancestors, list("id",4), .default = NA)),
      country_name = dplyr::case_when(
        operating_unit_id %in% countries$id ~ operating_unit,
        id %in% countries$id ~ name,
        TRUE ~ purrr::map_chr(ancestors, list("name",4), .default = NA))
    ) %>%
    dplyr::select(operating_unit, operating_unit_id, country_name, country_uid,
                  psnu = name, psnu_uid = id, psnu_type, in_region)
  
  if (!all(is.na(country_uids))) {
    PSNUs %<>%
      dplyr::filter(country_uid %in% country_uids)
  }
    
  # Patch for Suriname and Jamaica
    # REMOVE THIS dependent upon https://github.com/pepfar-datim/Global/issues/4655
  # if(datapack_uid == "Caribbean_Data_Pack") {
  #   patchList <- c("Suriname","Jamaica")
  #   
  #   JamaicaSuriname = NULL
  #   
  #   for (i in 1:length(patchList)) {
  #     URL <- paste0(getOption("baseurl"),
  #                   "api/sqlViews/kEtZ2bSQCu2/data.json?fields=level4name,organisationunituid,name&filter=level4name:eq:",
  #                   patchList[i],
  #                   dplyr::case_when(patchList[i] == "Jamaica" ~ "&filter=level:eq:6",
  #                                    patchList[i] == "Suriname" ~ "&filter=level:eq:5"))
  #     Export <- URL %>%
  #       URLencode() %>%
  #       httr::GET() %>%
  #       httr::content(., "text") %>%
  #       jsonlite::fromJSON(., flatten = TRUE)
  #     JamaicaSuriname <- as.data.frame(Export$rows,stringsAsFactors = FALSE) %>%
  #       setNames(.,Export$headers$name) %>%
  #       dplyr::select(country = level4name, uid = organisationunituid, name) %>%
  #       dplyr::distinct() %>%
  #       dplyr::bind_rows(JamaicaSuriname,.)
  #   }
  #   
  #   PSNUs <- PSNUs %>%
  #     dplyr::filter(!country %in% c("Jamaica", "Suriname")) %>%
  #     dplyr::bind_rows(JamaicaSuriname)
  # }
  
  # Create Data Pack PSNU ID & tag with country name breadcrumb where country != PSNU
    PSNUs %<>%
      dplyr::mutate(
        dp_psnu = paste0(
          dplyr::case_when(
            in_region & is.na(country_uid) ~ paste0(operating_unit, " > "),
            in_region & psnu_uid != country_uid ~ paste0(country_name, " > "),
            TRUE ~ ""),
          psnu, " (", psnu_uid,")")
      )
  
  return(PSNUs)
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
#' @title Extract and save schema from Data Pack template.
#' 
#' @description
#' Supplied a filepath to a Data Pack template (XLSX), will extract and save a
#' schema based on the template as a global object.
#' 
#' @param path Local filepath for a Data Pack template (XLSX).
#' 
#' @return Data Pack schema saved as a global object.
#'
unPackSchema <- function(path) {
  sheets <- tidyxl::xlsx_sheet_names(path)
  sheets_to_loop <- sheets[which(!stringr::str_detect(sheets, "Home|Quotes|Summary|Spectrum|Visualizations|Validations"))]
  
  schema <- tidyxl::xlsx_cells(path = filepath, include_blank_cells = FALSE) %>%
    dplyr::select(sheet_name = sheet, col, row, character, formula, numeric)
  
  data.table::setDT(schema)[,sheet_num:=.GRP, by = c("sheet_name")]
  
  schema %<>%
    dplyr::filter(sheet_name %in% sheets_to_loop,
                  row %in% c(3,5,6)) %>%
    tidyr::gather(key,value,-sheet_num,-sheet_name,-col,-row) %>%
    tidyr::unite(new.col, c(key,row)) %>%
    tidyr::spread(new.col,value) %>%
    dplyr::select(sheet_num,sheet_name,col,
                  label = character_3,
                  indicator_code = character_5,
                  formula = formula_6,
                  value = numeric_6) %>%
    dplyr::mutate(
      formula = dplyr::case_when(is.na(formula) ~ value,
                                 TRUE ~ formula),
      col_type = dplyr::case_when(
        stringr::str_detect(indicator_code, "20T")
        & !(sheet_name == "Prioritization" & indicator_code != "IMPATT.PRIORITY_SNU.20T")
        & !(sheet_name == "HTS" & (!stringr::str_detect(indicator_code,"^HTS_") | stringr::str_detect(indicator_code,"HTS_TST_PMTCT")))
        & !(sheet_name == "VMMC" & stringr::str_detect(indicator_code, "POP_EST|coverage"))
        ~ "Target",
        indicator_code %in% c("PSNU","Age","Sex","ID","AgeCoarse","IDAgeCoarse",
                              "PSNUCheck","KeyPop","sheet_name","indicatorCode",
                              "CoarseAge","sheet_num")
        ~ "Row Header"),
      dataset = dplyr::case_when(
        col_type == "Target" & stringr::str_detect(indicator_code,"SUBNAT|VL_SUPPRESSED") ~ "SUBNAT",
        col_type == "Target" & stringr::str_detect(indicator_code,"PLHIV|POP_EST|HIV_PREV|PRIORITY|KP_ESTIMATES") ~ "IMPATT",
        col_type == "Target" ~ "MER")) %>%
    dplyr::select(-value) %>%
    dplyr::arrange(sheet_num, col)
  
  return(schema)
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
