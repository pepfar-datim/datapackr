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
getIMPATTLevels <- function(d2_session = dynGet("d2_default_session",
                                                inherits = TRUE)){
  impatt_levels <-
    paste0(d2_session$base_url,"api/",datapackr::api_version(),
           "/dataStore/dataSetAssignments/orgUnitLevels") %>%
    httr::GET(httr::timeout(180), handle = d2_session$handle) %>%
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
    datapackr::api_call("organisationUnits", d2_session = d2_session) %>%
    datapackr::api_filter(field = "organisationUnitGroups.id",
                          operation = "eq",
                          match = "cNzfcPWEGSH") %>%
    datapackr::api_fields(fields = "id,name,level,ancestors[id,name]") %>%
    datapackr::api_get(d2_session = d2_session)

  impatt_levels %<>%
    dplyr::left_join(countries, by = c("country_name" = "name")) %>%
    dplyr::rename(country_uid = id) %>%
    dplyr::select(operating_unit, country_name, country_uid,
                  dplyr::everything(), -ancestors, -level)

  return(impatt_levels)

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
      datapackr::api_call("organisationUnits", d2_session = d2_session) %>%
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
#' @param type \code{character}, \code{numeric}, \code{logical}
#'
#' @return Dataframe \code{data} with added columns listed in \code{cnames}.
#'
addcols <- function(data, cnames, type = "character") {
  add <- cnames[!cnames %in% names(data)]

  if (length(add) != 0) {
    if (type == "character") {data[add] <- NA_character_
    } else if (type == "numeric") {data[add] <- NA_real_
    } else if (type == "logical") {data[add] <- NA}
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
isLoggedIn <- function(d2_session = dynGet("d2_default_session",
                                           inherits = TRUE)) {
  baseurl <- d2_session$base_url

  if (is.null(baseurl)) {
    return(FALSE)} else {
      httr::set_config(httr::config(http_version = 0))
      url <- URLencode(URL = paste0(baseurl, "api/me"))
      #Logging in here will give us a cookie to reuse
      r <- httr::GET(url,
                     httr::timeout(180),
                     handle = d2_session$handle)
      if (r$status != 200L) {
        return(FALSE)
      } else {

        return(TRUE)
      }
    }
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

getCopDataFromDatim <- function(country_uid,
                                fiscal_year_yyyy,
                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE))
{

  dataset_uids <- getDatasetUids(fiscal_year_yyyy,
                                 c("targets", "subnat", "impatt"))

  # package parameters for getDataValueSets function call
  parameters <-
    dplyr::bind_rows(
      tibble::tibble(key = "dataSet", value = dataset_uids),
      tibble::tibble(key = "orgUnit", value = country_uid),
      tibble::tribble(~ key, ~ value,
                      "children", "true",
                      "categoryOptionComboIdScheme", "code",
                      "includeDeleted", "false",
                      "period", paste0(fiscal_year_yyyy - 1, "Oct")
      )
    )

  # get data from datim usinfg dataValueSets
  # rename to standard names
  datim_data <-
    getDataValueSets(parameters$key,
                     parameters$value,
                     d2_session = d2_session) %>%
    dplyr::rename(
      datim_value = value,
      data_element_uid = data_element,
      org_unit_uid = org_unit,
      category_option_combo_uid = category_option_combo,
      attribute_option_combo_code = attribute_option_combo
    )

  return(datim_data)
}

#' @export
#' @title getDatasetUids
#'
#' @description returns character vector of dataset uids for a given FY {"2019", "2020", "2021"}
#' and type {"targets", "results","subnat_impatt"}
#' @param fiscal_year character - one of {"2019", "2020", "2021"}
#' @param type character vector - one or more of {"targets", "results", "subnat_impatt"}
#' @return returns a character vector of the related dataset uids
#'
getDatasetUids <-  function(fiscal_year,
                            type = c("mer_targets", "mer_results",
                                     "subnat_targets", "subnat_results",
                                     "impatt")){

  datasets = character(0)
  if (fiscal_year == "2022") {
    if ("mer_targets" %in% type) {
      datasets <- c(datasets,
                    "YfZot37BbTm", # MER Target Setting: PSNU (Facility and Community Combined)
                    "cihuwjoY5xP", # MER Target Setting: PSNU (Facility and Community Combined) - DoD ONLY)
                    "wvnouBMuLuE") # Host Country Targets: DREAMS (USG)
    }
    if ("mer_results" %in% type) {
      stop("FY22 results input not supported by getDatasetUids")
    }
    if ("subnat_targets" %in% type) {
      datasets <- c(datasets,
                    "Va7TYyHraRn") #Host Country Targets: COP Prioritization SNU (USG)
    }
    if ("subnat_results" %in% type) {
      stop("FY22 results input not supported by getDatasetUids")
    }
    if ("impatt" %in% type) {
      datasets <- c(datasets,
                    "Zn27xns9Fmx") #Planning Attributes: COP Prioritization SNU
    }
  } else if(fiscal_year == "2021") {
    if("mer_targets" %in% type) {
      datasets <- c(datasets,
                    "Pmc0yYAIi1t", # MER Target Setting: PSNU (Facility and Community Combined) (TARGETS) FY2021
                    "s1sxJuqXsvV")  # MER Target Setting: PSNU (Facility and Community Combined) - DoD ONLY) FY2021
    }
    if("mer_results" %in% type) {
      datasets <- c(datasets,
                    "zL8TlPVzEBZ", # MER Results: Facility Based
                    #"",  # MER Results: Facility Based - DoD ONLY
                    #"", # MER Results: Community Based - DoD ONLY FY2020Q4
                    "TBcmmtoaCBC", # MER Results: Community Based
                    "qHyrHc4zwx4") # Host Country Results: DREAMS (USG)
    }
    if("subnat_targets" %in% type) {
      datasets <- c(datasets,
                    "j7jzezIhgPj") #Host Country Targets: COP Prioritization SNU (USG) FY2021

    }
    if("subnat_results" %in% type) {
      datasets <- c(datasets,
                    "xiTCzZJ2GPP") #Host Country Results: COP Prioritization SNU (USG)

    }
    if("impatt" %in% type) {
      datasets <- c(datasets,
                    "jxnjnBAb1VD") # Planning Attributes: COP Prioritization SNU FY2021

    }
  } else if (fiscal_year == "2020") {
    if("mer_targets" %in% type) {
      datasets <- c(datasets,
                    "sBv1dj90IX6", # MER Targets: Facility Based FY2020
                    "nIHNMxuPUOR", # MER Targets: Community Based FY2020
                    "C2G7IyPPrvD", # MER Targets: Community Based - DoD ONLY FY2020
                    "HiJieecLXxN") # MER Targets: Facility Based - DoD ONLY FY2020
    }
    if ("mer_results" %in% type){
      datasets <- c(datasets,
                    "qzVASYuaIey", # MER Results: Community Based FY2020Q4
                    "BPEyzcDb8fT", # MER Results: Community Based - DoD ONLY
                    "jKdHXpBfWop", # MER Results: Facility Based FY2020Q4
                    "em1U5x9hhXh", # MER Results: Facility Based - DoD ONLY
                    "mbdbMiLZ4AA") # Host Country Results: DREAMS (USG) FY2020Q4
    }
    if("subnat_targets" %in% type) {
      datasets <- c(datasets,
                    "N4X89PgW01w") # Host Country Targets: COP Prioritization SNU (USG) FY2020
    }
    if("subnat_results" %in% type) {
      datasets <- c(datasets,
                    "ctKXzmv2CVu") # Host Country Results: COP Prioritization SNU (USG) FY2020Q4
    }
    if("impatt" %in% type) {
      datasets <- c(datasets,
                    "pTuDWXzkAkJ") # Planning Attributes: COP Prioritization SNU FY2020
    }
  } else if (fiscal_year == "2019") {
    if("mer_targets" %in% type) {
      datasets <- c(datasets,
                    "BWBS39fydnX", # MER Targets: Community Based - DoD ONLY FY2019
                    "l796jk9SW7q", # MER Targets: Community Based FY2019
                    "X8sn5HE5inC", # MER Targets: Facility Based - DoD ONLY FY2019
                    "eyI0UOWJnDk") # MER Targets: Facility Based FY2019)
    }
    if("mer_results" %in% type) {
      datasets <- c(datasets,
                    "KWRj80vEfHU", # MER Results: Facility Based FY2019Q4
                    "fi9yMqWLWVy", # MER Results: Facility Based - DoD ONLY FY2019Q4
                    "zUoy5hk8r0q", # MER Results: Community Based FY2019Q4
                    "PyD4x9oFwxJ", # MER Results: Community Based - DoD ONLY FY2019Q4
                    "EbZrNIkuPtc") # Host Country Results: DREAMS (USG) FY2019Q4
    }
    if("subnat_targets" %in% type) {
      datasets <- c(datasets,
                    "Ncq22MRC6gd") # Host Country Targets: COP Prioritization SNU (USG) FY2019
    }
    if("subnat_results" %in% type) {
      datasets <- c(datasets,
                    "iJ4d5HdGiqG") # Host Country Results: COP Prioritization SNU (USG) FY2019Q4
    }
    if("impatt" %in% type) {
      datasets <- c(datasets,
                    "pTuDWXzkAkJ") # Planning Attributes: COP Prioritization SNU FY2020 - last used FY2020 also valid for FY2019

    }
  } else {
    stop(paste("FY", fiscal_year, "input not supported by getDatasetUids"))
  }
  return(datasets)
  }


#' @export
#' @title Define prioritization values.
#'
#' @return dict
#'
prioritization_dict <- function() {
  dict <-
    tibble::tribble(
      ~value, ~name,
      0, "No Prioritization",
      1, "Scale-up: Saturation",
      2, "Scale-up: Aggressive",
      4, "Sustained",
      5, "Centrally Supported",
      6, "Sustained: Commodities",
      7, "Attained",
      8, "Not PEPFAR Supported"
    ) %>%
    dplyr::mutate(Prioritization = paste0(value, " - ", name))

  return(dict)
}

#' @export
#' @title Take Max along row among columns matching regex
#' 
#' @param df Dataframe
#' @param cn Name (character string) of Max column to create
#' @param regex String of regex to use in identifying columns.
#'
#' @return df
#'
rowMax <- function(df, cn, regex) {
  df_filtered <- df %>%
    dplyr::select(tidyselect::matches(match = regex))
  
  if (NCOL(df_filtered) == 0) {
    df[[cn]] <- NA_integer_
    return(df)
  }
  
  df[[cn]] <- df_filtered %>%
    purrr::pmap(pmax, na.rm = T) %>%
    as.numeric
  
  return(df)
}
