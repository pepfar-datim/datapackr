#' @export
#' @title Returns `default` categoryOptionCombo uid.
#'
#' @return `Default` categoryOptionCombo uid.
#'
default_catOptCombo <- function() {
  "HllvX50cXC0"
}

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
#' @param digits Number of digits to round to. Default is 0
#'
#' @return An integer.
#' @examples
#' # If the first digit to be dropped is exactly 5, round_trunc() will round to
#' # integer with the highest absolute value.
#' round_trunc(0.5)
#' round_trunc(-0.5)
#' @export
round_trunc <- function(x, digits = 0) {
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z * sign(x)

}


#' @export
#' @importFrom magrittr %>% %<>%
#' @title Pull IMPATT levels from DATIM for all PEPFAR countries
#'
#' @description
#' Queries DATIM to retrieve the latest version of
#' \code{/api/dataStore/dataSetAssignments/ous}
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return Dataframe of country metadata, including prioritization, planning,
#' country, community, and facility levels in DATIM organization hierarchy.
#'
getIMPATTLevels <- function(d2_session = dynGet("d2_default_session",
                                                inherits = TRUE)) {
  impatt_levels <-
    paste0(d2_session$base_url, "api/", datapackr::api_version(),
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
    datapackr::api_fields(fields = "id,name,level,ancestors[id,name]") %>% # nolint
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
    cols <- colnames(from)

  # If `from` is a null dataframe, skip and return `to`
    if (length(cols) != 0) {

  # Loop through `from` columns and if there's a match in `to`, copy and paste
  #   it into `to`
      for (i in seq_along(cols)) {
        col <- cols[i]
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
  if (rlang::is_interactive()) {
    print(x)
  }
}

#' @export
#'
#' @title Uses r message instead of print if session is interactive.
#'
#' @description
#' Supplied a message, will share as r message() only if the session is currently interactive.
#'
#' @param x Message to print.
#'
#' @return Printed message, \code{x}.
#'
interactive_message <- function(x) {
  if (rlang::is_interactive()) {
    message(x)
  }
}

#' @export
#'
#' @title Issue a warning if the session is interactive
#'
#' @description
#' Supplied a string, will issue a warning as r warning() only if the session is currently interactive.
#'
#' @param x Warning string.
#'
#' @return Warning message, \code{x}.
#'
interactive_warning <- function(x) {
  if (rlang::is_interactive()) {
    warning(x,call. = FALSE)
  }
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
      datapackr::api_fields(fields = "id, name, level, ancestors[id, name]") %>%
      datapackr::api_get() %>%

  # Remove countries no longer supported
      dplyr::filter(
        !name %in%
          c("Antigua & Barbuda", "Bahamas", "Belize", "China", "Dominica", "Grenada",
            "Saint Kitts & Nevis", "Saint Lucia", "Saint Vincent & the Grenadines",
            "Turkmenistan", "Uzbekistan")) %>%
      dplyr::select(country_name = name, country_uid = id, dplyr::everything()) %>%

  # Add metadata
      dplyr::mutate(
        data_pack_name = dplyr::case_when(
          country_name %in% c("Burma", "Cambodia", "India", "Indonesia",
                              "Kazakhstan", "Kyrgyzstan", "Laos",
                              "Nepal", "Papua New Guinea", "Tajikistan",
                              "Thailand") ~ "Asia Region",
          country_name %in% c("Barbados", "Guyana", "Jamaica", "Suriname",
                              "Trinidad & Tobago") ~ "Caribbean Region",
          country_name %in% c("Brazil", "Costa Rica", "El Salvador",
                              "Guatemala", "Honduras", "Nicaragua",
                              "Panama") ~ "Central America Region",
          country_name %in% c("Burkina Faso", "Ghana", "Liberia", "Mali",
                              "Senegal", "Sierra Leone", "Togo")
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
    if (type == "character") {
      data[add] <- NA_character_
    } else if (type == "numeric") {
      data[add] <- NA_real_
    } else if (type == "logical") {
      data[add] <- NA
    }
  }

  return(data)

}


#' @export
#' @title getDatasetUids
#'
#' @description returns character vector of dataset uids for a given FY:
#' {"2019", "2020", ... , "2023"}
#' and type {"mer_targets", "mer_results", "subnat_targets", "subnat_results",
#' "impatt"}
#' @param fiscal_year character - one of: {"2019", "2020", "2021", "2022",
#' "2023"}
#' @param type character vector - one or more of:
#' {"mer_targets", "mer_results", "subnat_targets", "subnat_results", "impatt"}
#' @return returns a character vector of the related dataset uids
#'
getDatasetUids <-  function(fiscal_year,
                            type = c("mer_targets", "mer_results",
                                     "subnat_targets", "subnat_results",
                                     "impatt")) {
  datasets <- character(0)
  if  (fiscal_year == "2023") {
    if ("mer_targets" %in% type) {
      datasets <- c(datasets,
                    "iADcaCD5YXh", # MER Target Setting: PSNU (Facility and Community Combined)
                    "cihuwjoY5xP", # MER Target Setting: PSNU (Facility and Community Combined) - DoD ONLY)
                    "vzhO50taykm") # Host Country Targets: DREAMS (USG)
    }
    if ("mer_results" %in% type) {
      stop("FY23 results input not supported by getDatasetUids")
    }
    if ("subnat_targets" %in% type) {
      datasets <- c(datasets,
                    "J4tdiDEi08O") #Host Country Targets: COP Prioritization SNU (USG)
    }
    if ("subnat_results" %in% type) {
      stop("FY23 results input not supported by getDatasetUids")
    }
    if ("impatt" %in% type) {
      datasets <- c(datasets,
                    "CxMsvlKepvE") #Planning Attributes: COP Prioritization SNU
    }
  } else if  (fiscal_year == "2022") {
    if ("mer_targets" %in% type) {
      datasets <- c(datasets,
                    "YfZot37BbTm", # MER Target Setting: PSNU (Facility and Community Combined) FY2022
                    "cihuwjoY5xP", # MER Target Setting: PSNU (Facility and Community Combined) - DoD ONLY)
                    "wvnouBMuLuE") # Host Country Targets: DREAMS (USG) FY2022
    }
    if ("mer_results" %in% type) {
      datasets <- c(datasets,
                    "BHlhyPmRTUY", # MER Results: Facility Based
                    "HfhTPdnRWES", # MER Results: Community Based
                    "MGNVwVicMVm") # Host Country Results: DREAMS (USG)
    }
    if ("subnat_targets" %in% type) {
      datasets <- c(datasets,
                    "Va7TYyHraRn") #Host Country Targets: COP Prioritization SNU (USG) FY2022
    }
    if ("subnat_results" %in% type) {
      datasets <- c(datasets,
                    "IXiORiVFqIv") #Host Country Results: COP Prioritization SNU (USG)
    }
    if ("impatt" %in% type) {
      datasets <- c(datasets,
                    "Zn27xns9Fmx") #Planning Attributes: COP Prioritization SNU FY2022
    }
  } else if (fiscal_year == "2021") {
    if ("mer_targets" %in% type) {
      datasets <- c(datasets,
                    "Pmc0yYAIi1t", # MER Target Setting: PSNU (Facility and Community Combined) (TARGETS) FY2021
                    "s1sxJuqXsvV")  # MER Target Setting: PSNU (Facility and Community Combined) - DoD ONLY) FY2021
    }
    if ("mer_results" %in% type) {
      datasets <- c(datasets,
                    "zL8TlPVzEBZ", # MER Results: Facility Based FY2021Q4
                    "TBcmmtoaCBC", # MER Results: Community Based FY2021Q4
                    "qHyrHc4zwx4") # Host Country Results: DREAMS (USG) FY2021Q4
    }
    if ("subnat_targets" %in% type) {
      datasets <- c(datasets,
                    "j7jzezIhgPj") #Host Country Targets: COP Prioritization SNU (USG) FY2021
      }
    if ("subnat_results" %in% type) {
      datasets <- c(datasets,
                    "xiTCzZJ2GPP") #Host Country Results: COP Prioritization SNU (USG) FY2021Q4
      }
    if ("impatt" %in% type) {
      datasets <- c(datasets,
                    "jxnjnBAb1VD") # Planning Attributes: COP Prioritization SNU FY2021
      }
  } else if (fiscal_year == "2020") {
    if ("mer_targets" %in% type) {
      datasets <- c(datasets,
                    "sBv1dj90IX6", # MER Targets: Facility Based FY2020
                    "nIHNMxuPUOR", # MER Targets: Community Based FY2020
                    "C2G7IyPPrvD", # MER Targets: Community Based - DoD ONLY FY2020
                    "HiJieecLXxN") # MER Targets: Facility Based - DoD ONLY FY2020
    }
    if ("mer_results" %in% type) {
      datasets <- c(datasets,
                    "qzVASYuaIey", # MER Results: Community Based FY2020Q4
                    "BPEyzcDb8fT", # MER Results: Community Based - DoD ONLY FY2021Q4
                    "jKdHXpBfWop", # MER Results: Facility Based FY2020Q4
                    "em1U5x9hhXh", # MER Results: Facility Based - DoD ONLY FY2021Q4
                    "mbdbMiLZ4AA") # Host Country Results: DREAMS (USG) FY2020Q4
    }
    if ("subnat_targets" %in% type) {
      datasets <- c(datasets,
                    "N4X89PgW01w") # Host Country Targets: COP Prioritization SNU (USG) FY2020
    }
    if ("subnat_results" %in% type) {
      datasets <- c(datasets,
                    "ctKXzmv2CVu") # Host Country Results: COP Prioritization SNU (USG) FY2020Q4
    }
    if ("impatt" %in% type) {
      datasets <- c(datasets,
                    "pTuDWXzkAkJ") # Planning Attributes: COP Prioritization SNU FY2020
    }
  } else if (fiscal_year == "2019") {
    if ("mer_targets" %in% type) {
      datasets <- c(datasets,
                    "BWBS39fydnX", # MER Targets: Community Based - DoD ONLY FY2019
                    "l796jk9SW7q", # MER Targets: Community Based FY2019
                    "X8sn5HE5inC", # MER Targets: Facility Based - DoD ONLY FY2019
                    "eyI0UOWJnDk") # MER Targets: Facility Based FY2019)
    }
    if ("mer_results" %in% type) {
      datasets <- c(datasets,
                    "KWRj80vEfHU", # MER Results: Facility Based FY2019Q4
                    "fi9yMqWLWVy", # MER Results: Facility Based - DoD ONLY FY2019Q4
                    "zUoy5hk8r0q", # MER Results: Community Based FY2019Q4
                    "PyD4x9oFwxJ", # MER Results: Community Based - DoD ONLY FY2019Q4
                    "EbZrNIkuPtc") # Host Country Results: DREAMS (USG) FY2019Q4
    }
    if ("subnat_targets" %in% type) {
      datasets <- c(datasets,
                    "Ncq22MRC6gd") # Host Country Targets: COP Prioritization SNU (USG) FY2019
    }
    if ("subnat_results" %in% type) {
      # Host Country Results: COP Prioritization SNU (USG) FY2019Q4
      datasets <- c(datasets, "iJ4d5HdGiqG")
    }
    if ("impatt" %in% type) {
      # Planning Attributes: COP Prioritization SNU FY2020 - last used FY2020 also valid for FY2019
      datasets <- c(datasets, "pTuDWXzkAkJ")
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

#' @export
#' @title get_Map_DataPack_DATIM_DEs_COCs
#'
#' @param cop_year cop year to pull get map for
#'
#' @return {cop20, cop21}_map_DataPack_DATIM_DEs_COCs
#'
getMapDataPack_DATIM_DEs_COCs <- function(cop_year) {
  if (cop_year == 2020) {
      return(datapackr::cop20_map_DataPack_DATIM_DEs_COCs)
  } else if (cop_year == 2021 && identical(datapackr::cop21_map_DataPack_DATIM_DEs_COCs,
                                           datapackr::map_DataPack_DATIM_DEs_COCs)) {
    return(datapackr::cop21_map_DataPack_DATIM_DEs_COCs)
  } else if (cop_year == 2022) {
    return(datapackr::cop22_map_DataPack_DATIM_DEs_COCs)
  } else { # if map_DataPack_DATIM_DEs_COCs has drifted or COP year is invalid this notifies us
    stop("The COP year and configuration provided is not supported by get_Map_DataPack_DATIM_DEs_COCs")
  }
}


#' @export
#' @title Create a new Data Pack
#' @author Scott Jackson
#' @description Creates a brand new Data Pack with the supplied characteristics.
#'
#' @inheritParams datapackr_params
#'
#' @return Data Pack object
#'
createDataPack <- function(datapack_name = NULL,
                           country_uids,
                           template_path = NULL,
                           cop_year = NULL,
                           tool = NULL) {

  # Check & assign params
  params <- check_params(
    country_uids = country_uids,
    cop_year = cop_year,
    tool = tool,
    template_path = template_path,
    schema = NULL,
    datapack_name = datapack_name)

  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }

  wb <- openxlsx::loadWorkbook(template_path)

  options("openxlsx.numFmt" = "#,##0")

  # Write Home Sheet info
  wb <- writeHomeTab(wb = wb,
                    datapack_name = datapack_name,
                    country_uids = country_uids,
                    cop_year = cop_year,
                    tool = tool)

  # Create DP object
  d <- list(
    keychain = list(
      template_path = template_path),
    info = list(
      tool = tool,
      country_uids = country_uids,
      cop_year = cop_year,
      datapack_name = datapack_name,
      schema = schema),
    tool = list(
      wb = wb)
    )

  return(d)
}


# TODO: To deprecate. Duplicate of check_template_path inside check_params

#' @export
#' @title Compare Data Pack template against schema
#'
#' @param tool Either "Data Pack" or "OPU Data Pack"? Default is "Data Pack".
#' @inheritParams datapackr_params
#'
#' @return Message indicating comparison failure or success.
#'
compareTemplateToSchema <- function(template_path = NULL,
                                   cop_year = getCurrentCOPYear(),
                                   tool = "Data Pack",
                                   d2_session = dynGet("d2_default_session",
                                                       inherits = TRUE)) {

  interactive_print("Checking template against schema and DATIM...")

  if (is.null(template_path)) {
    template_path <- pick_template_path(cop_year, tool)
  }

  template_schema <-
    unPackSchema_datapack(
      filepath = template_path,
      skip = skip_tabs(tool = paste0(tool, " Template"), cop_year = cop_year),
      tool = paste0(tool, " Template"),
      cop_year = cop_year)

  package_schema <- pick_schema(cop_year, tool)

  if (!identical(package_schema, template_schema)) {
    stop("Template provided does not match published schema.")
  } else {
    print("Template provided matches published schema.")
  }

}


#' @export
#' @title Compile a list ending with different final collapse
#'
#' @param ... - one or more R objects, to be converted to character vectors
#' @param final - conjunction to use before serial list item (usually 'and' or 'or')
#' @param oxford - TRUE/FALSE indicating whether to use the Oxford (i.e., serial) comma
#'
#' @return A character vector of the concatenated values.
#'
paste_oxford <- function(..., final = "and", oxford = TRUE) {
  to_paste <- unlist(list(...))

  if (length(to_paste) == 1) {
    to_paste
  } else {
    first_bits <- to_paste[1:(length(to_paste) - 1)]
    last <- to_paste[length(to_paste)]

    start <- paste(first_bits, collapse = ", ")
    serial <- paste0(final, " ", last)

    start <- ifelse(oxford, paste0(start, ", "), paste0(start, " "))

    paste0(start, serial)
  }
}

#' Default value for `missing_arg`
#'
#' This infix function makes it easy to replace `missing_arg`s with a default
#' value. It's inspired by the way that rlang's `%||%` infix operator works.
#'
#' @param x,y If `x` is missing, will return `y`; otherwise returns `x`.
#' @export
#' @name op-missing-default
#' @examples
#' x <- rlang::missing_arg()
#' y <- x %missing% 2
`%missing%` <- function(x, y = NULL) {
  #if (rlang::is_missing(x)) y else x
  rlang::maybe_missing(x, y)
}


#' @title Paste a Dataframe in a string
#' @param x Dataframe to paste
#' @export
paste_dataframe <- function(x) {
  paste(capture.output(print(x)), collapse = "\n")
}


#' @export
#' @title Parse a value to numeric
#' @description If x is character, attempts to parse the first occurence of a
#' sub-string that looks like a number.
#'
#' @importFrom stringr str_extract
#' @importFrom rlang is_character
#'
#' @param x Value to test and coerce
#'
#' @return x parsed as numeric, if possible
parse_maybe_number <- function(x, default = NULL) {

  if (!is.numeric(x)) {

    # If supplied a character vector attempt to parse
    if (rlang::is_character(x)) {
      x_string <- stringr::str_extract(x, "\\d+")
      if (is.na(x_string)) {
        x <- default
        warning("Could not parse the provided character vector into something resembling a number.")
      } else {
        x <- as.numeric(x_string)
      }

    } else if (is.factor(x)) {
      x <- as.numeric(as.character(x))
    } else {
      x <- default
    }

  }

  x

}


#' @export
#' @title Is UID-ish
#' @md
#' @description Tests whether a character string matches the regex of a DHIS2
#' 11-digit UID. Vectorized over `string` and `pattern`.
#'
#' @param string Input vector. Either a character vector, or something coercible
#' to one.
#'
#' @importFrom stringr str_detect
#'
#' @return A logical vector.
is_uidish <- function(string) {
  stringr::str_detect(string, "^[[:alpha:]][[:alnum:]]{10}$")
}


commas <- function(...) paste0(..., collapse = ", ")

names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

has_names <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep_along(x, FALSE)
  } else {
    !(is.na(nms) | nms == "")
  }
}

ndots <- function(...) nargs()

bullet <- function(...) paste0(bold(silver(" * ")), sprintf(...))

# Re-exports ---------------------------------------------------

#' Default value for `NULL`
#' @importFrom rlang `%||%`
#' @keywords NULL
#' @export
#' @name null-default
rlang::`%||%`

#' Default value for `NA`
#' @importFrom rlang `%|%`
#' @keywords NA
#' @export
#' @name na-default
rlang::`%|%`

#' @importFrom magrittr `%>%`
#' @export
magrittr::`%>%`
