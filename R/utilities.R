#' @export
#'
#' @title merge two datapacks into one d object.
#'
#' @description
#' If two datapacks are supplied, they will be merged into one d object.
#'
#' @param d1 the first d object
#' @param d2 the second d object
#'
#' @return combined d object
#'
mergeDatapack <- function(d1 = d1, d2 = d2) {


  same_name <- identical(d1$info$datapack_name, d2$info$datapack_name)

  if (!same_name) {
    stop("We cannot merge those two tools.")
  } else {
    d <- d1
  }

  #Do not attempt to merge data from a PSNUxIM and a Datapack
  # bind data, datim and data
  if (identical(d1$info$tool, d2$info$tool)) {
    d$datim <- purrr::map2(d1$datim, d2$datim, dplyr::bind_rows)
    d$data <- purrr::map2(d1$data, d2$data, dplyr::bind_rows)
  }

  # ensure all test results are coded as data frames or tibbles
  d1$tests <- lapply(d1$tests, dplyr::tibble)
  d2$tests <- lapply(d2$tests, dplyr::tibble)

  #Overwrite the tool column. We need to have
  #definitive information about the tool at this point.
  d1$tests <- purrr::map(d1$tests, dplyr::mutate, tool = d1$info$tool)
  d2$tests <- purrr::map(d2$tests, dplyr::mutate, tool = d2$info$tool)

  d1_names <- names(d1$tests)
  d2_names <- names(d2$tests)
  d1_extras <- d1_names[!d1_names %in% d2_names]
  d2_extras <- d2_names[!d2_names %in% d1_names]

  # combine
  d$tests <- purrr::map2(d1$tests[!names(d1$tests) %in% d1_extras],
                         d2$tests[!names(d2$tests) %in% d2_extras],
                         dplyr::bind_rows)

  # add extras
  d$tests <-
    c(d$tests, d1$tests[d1_extras], d2$tests[d2_extras])

  #In case we have a DataPack and a PSNU
  if (setequal(c(d1$info$tool, d2$info$tool), c("Data Pack", "PSNUxIM"))) {
    d$sheets <- c(d1$sheets, d2$sheets)
  }

  # combine message information
  d$info <- d1$info


  #Modify the messages to include the tool name
  d1$info$messages$tool <- d1$info$tool
  d2$info$messages$tool <- d2$info$tool

  d$info$messages <- appendMessage(d1$info$messages,
                                   d2$info$messages$message,
                                   d2$info$messages$level,
                                   d2$info$messages$tool)

  d
}

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
#' @title Swap columns between two dataframes
#'
#' @description
#' Replaces columns in the dataframe \code{to} with those with identical names
#'  in the dataframe \code{from}.
#'
#' @param to Dataframe to pull columns into
#' @param from Dataframe to pull columns from
#'
#' @return A dataframe with the swapped columns
#'
swapColumns <- function(to, from) {
  # Grab column names from the `from` df
  cols <- colnames(from)

  # If the `from` df is a null dataframe, skip and return the `to` df
  if (length(cols) != 0) {

  # Loop through `from` columns and if there's a match in `to`, copy and paste
    #   it into `to`
    for (col in cols) {
      if (col %in% colnames(to)) {
        # base column swap
        to[, col] <- from[, col]
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
    warning(x, call. = FALSE)
  }
}


#' @export
#' @title Get Sane Name for Data Pack Tool
#'
#' @description Takes a Data Pack tool name and generates a
#' "Sane name" for the tool which has no spaces or punctuation.
#'
#' @param datapack_name A string from the \code{d$info$datapack_name} object.
#'
#' @return String with the sane name.

getSaneName <- function(datapack_name) {
  sane_name <- datapack_name %>%
    stringr::str_extract_all(
      string = .,
      pattern = "[A-Za-z0-9_]",
      simplify = TRUE) %>%
    paste0(., sep = "", collapse = "")
}


#' @export
#' @title Get Operating Unit from Country UIDs
#'
#' @description Takes in a set of Country UIDs and returns an Operating Unit name.
#'
#' @param country_uids List of country UIDs from the \code{d$info$country_uids} object.
#' @param cop_year The COP year to use. Defaults to the current COP Year if not specified.
#'
#' @return A data frame consisting of the name of the operating unit
#'
getOUFromCountryUIDs <- function(country_uids, cop_year = NA) {


  if (is.na(cop_year)) {
    warning("No COP Year specified so using the current COP year")
    cop_year <- getCurrentCOPYear()
  }

  if (length(cop_year) > 1) {
    stop("You must supply a single COP Year!")
  }

  cop_year %<>% check_cop_year(cop_year = cop_year)

  ou <- getValidOrgUnits(cop_year = cop_year) %>%
    dplyr::select(ou, ou_uid, country_name, country_uid) %>%
    dplyr::distinct() %>%
    dplyr::filter(country_uid %in% country_uids) %>%
    dplyr::select(ou, ou_uid) %>%
    dplyr::distinct()

  if (NROW(ou) != 1) {
    stop("Datapacks cannot belong to multiple operating units")
  }

  return(ou)
}


#' @export
#' @title Add list of columns as NULL columns to supplied dataframe.
#'
#' @description
#' Supplied a character vector of column names, \code{cnames}, \code{addcols}
#' will add one new, \code{NULL} column to \code{data} for each element of
#' \code{cnames} and name it after the corresponding element of \code{cnames}.
#'
#' @param data The dataframe to add the columns to.
#' @param cnames Character vector of one or more column names to be added to
#' \code{data}.
#' @param type \code{character}, \code{numeric}, \code{logical}
#'
#' @return Dataframe \code{data} with added columns listed in \code{cnames}.
#'
addcols <- function(data, cnames, type = "character") {
  add <- cnames[!cnames %in% names(data)] # Subsets column name list BY only
  # keeping names that are NOT in the supplied dataframes column names already.

  if (length(add) != 0) { #If their are columns that need to be filled in THEN
    #Impute the NA value based upon the type provided in the function.
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
#' @title getCOPDatasetUids
#'
#' @description returns character vector of dataset uids for a given FY:
#' {"2019", "2020", ... , "2023"}
#' and type {"mer_targets", "mer_results", "subnat_targets", "subnat_results",
#' "impatt"}
#' @param datastreams character vector - one or more of:
#' {"mer_targets", "mer_results", "subnat_targets", "subnat_results", "impatt"}
#' @inheritParams datapackr_params
#' @return returns a character vector of the related dataset uids
#'
getCOPDatasetUids <-  function(cop_year, datastreams) {

  #Datastream validation
  all_datastreams <- c("mer_targets", "mer_results",
                 "subnat_targets", "subnat_results",
                 "impatt")
  datastreams <- datastreams %missing% all_datastreams

  stopifnot("You must specify a vector of dataset types" = is.vector(datastreams))

  if (!(all(datastreams %in% all_datastreams))) {
    stop(paste("Could not find a data stream for", paste(datastreams, sep = "", collapse = ",")))
  }

  #List of COP Datasets by year
  #Found here https://www.datim.org/dhis-web-maintenance/index.html#/list/dataSetSection/dataSet
  #mer_targets NEEDS UPDATED BEFORE GO LIVE of generation
  cop_datasets <-
    list(
      "2025" = list(# NOT the COP25 versions NEED To update when released
        "mer_targets" =   c("HUEzkjkij1", # MER Target Setting: PSNU (Facility
                            # and Community Combined) (TARGETS) updated 6/28/24
                            "tNbhYbrKbnk"), # Host Country Targets: DREAMS (USG) updated 4/9/24
        "mer_results" = NA,
        "subnat_targets" = "bKSmkDP5YTc",
        "subnat_results" = "fZVvcMSA9mZ",
        "impatt" = "kWKJQYP1uT7"
      ),
      "2024" = list(
        "mer_targets" =   c("lHUEzkjkij1", # MER Target Setting: PSNU (Facility and Community Combined) (TARGETS)
                            "tNbhYbrKbnk"), # Host Country Targets: DREAMS (USG)
        "mer_results" = NA,
        "subnat_targets" = "bKSmkDP5YTc",
        "subnat_results" = "fZVvcMSA9mZ",
        "impatt" = "kWKJQYP1uT7"),
      "2023" = list(
        "mer_targets" =   c("dA9C5bL44NX", # MER Target Setting: PSNU (Facility and Community Combined) (TARGETS) FY2024
                            "A2GxohPT9Hw", # MER Target Setting:
                            #PSNU (Facility and Community Combined) - DoD ONLY (TARGETS) FY2024
                            "vpDd67HlZcT"), # Host Country Targets: DREAMS (USG) FY2024
        "mer_results" = NA,
        "subnat_targets" = "bKSmkDP5YTc",
        "subnat_results" = "fZVvcMSA9mZ",
        "impatt" = "kWKJQYP1uT7")
    )

  # If cop_year is NULL or missing, use default from package
  cop_year <- cop_year %missing% NULL
  cop_year <- cop_year %||% getCurrentCOPYear()

  if (length(cop_year) > 1) {
    stop("You must specify a single COP Year")
  }

  if (!(cop_year %in% names(cop_datasets))) {
    stop(paste("There are no COP datasets for ", cop_year))
  }


    datasets_filtered <- cop_datasets %>%
    purrr::pluck(as.character(cop_year)) %>%
    .[datastreams] %>%
    unlist(use.names = FALSE) %>%
    purrr::discard(~ is.na(.))

  if (is.null(datasets_filtered) || length(datasets_filtered) == 0) {
    stop(paste("No datasets could be found for cop_year",
               cop_year, "and type(s)",
               paste(datastreams, sep = "", collapse = ",")))
  }

  datasets_filtered
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
#' @title Extracts the desired columns for analysis via regular expression, then
#'  takes the maximum value row-wise. Ultimately resulting in a new column
#'  containing the max values.
#' @param df The dataframe to be analyzed.
#' @param cn The column name (character string) of the Max column that is
#'  created after execution of this function.
#' @param regex A regular expression used in identifying the columns of
#'  interest.
#'
#' @return df
#'
rowMax <- function(df, cn, regex) {
  df_filtered <- df %>% # Filters df based on regex
    dplyr::select(tidyselect::matches(match = regex))
# If the number of columns is 0, return the provided df without new columns.
  if (NCOL(df_filtered) == 0) {
    df[[cn]] <- NA_integer_
    return(df)
  }
# Create the new column in the dataframe, and ensure its column type is numeric.
  df[[cn]] <- df_filtered %>%
    purrr::pmap(pmax, na.rm = TRUE) %>% # Row-wise Calculations.
    as.numeric

  return(df)
}

#' @export
#' @title get_Map_DataPack_DATIM_DEs_COCs
#'
#' @param cop_year cop year to pull get map for
#' @param datasource Type of datasource (Data Pack, OPU Data Pack, DATIM)
#' @param year A vector of numeric values (either 1 or 2) which indicate which
#' Year should be returned. If year = 1, then the DataPack data elements
#' will be return. If Year = 2, then the Year 2 data elements will be returned
#' If Year = c(1,2) then both years will be returned.
#' @return {cop21, cop22, cop23}_map_DataPack_DATIM_DEs_COCs
#'
getMapDataPack_DATIM_DEs_COCs <- function(cop_year, datasource = NULL, year = 1) {

  if (!all(year %in% c(1, 2))) {
    stop("You must specify either year 1, 2 or both.")
  }

  if (is.null(datasource))  {
    datasource <- "Data Pack"
  }

  if (datasource %in%  c("Data Pack", "Data Pack Template")) {
    de_coc_map <- switch(as.character(cop_year),
           "2023" = cop23_map_DataPack_DATIM_DEs_COCs,
           "2024" = cop24_map_DataPack_DATIM_DEs_COCs,
           "2025" = cop25_map_DataPack_DATIM_DEs_COCs,
           stop("Invalid COP Year"))
    }

  if (datasource %in% c("OPU Data Pack", "OPU Data Pack Template", "DATIM", "PSNUxIM", "PSNUxIM Template")) {
    de_coc_map <- switch(as.character(cop_year),
                         "2023" = cop23_map_DataPack_DATIM_DEs_COCs,
                         "2024" = cop24_map_DataPack_DATIM_DEs_COCs,
                         "2025" = cop25_map_DataPack_DATIM_DEs_COCs,
                         stop("Invalid COP Year"))
  }

  if (cop_year >= 2023) {

     if (year == 1) {
       de_coc_map <- de_coc_map %>%
         dplyr::filter(!grepl("\\.T2", indicator_code))
     }

    if (year == 2) {
      de_coc_map <- de_coc_map %>%
        dplyr::filter(grepl("\\.T2", indicator_code))
    }

  }

  de_coc_map

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
    return(to_paste)
  } else {
    first_bits <- to_paste[1:(length(to_paste) - 1)]
    last <- to_paste[length(to_paste)]

    start <- paste(first_bits, collapse = ", ")
    serial <- paste0(final, " ", last)

    start <- ifelse(oxford & length(to_paste) > 2,
                    paste0(start, ", "),
                    paste0(start, " "))

    return(paste0(start, serial))
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
  paste(utils::capture.output(print(x)), collapse = "\n")
}


#' @export
#' @title Parse a value to numeric
#' @description If x is character, attempts to parse the first occurence of a
#' sub-string that looks like a number.
#'
#' @param x Value to test and coerce
#' @param default Default value to assign to x if not a character string,
#'  \code{NA_character_}, or a factor.
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


#' Title
#' @description Determine the number of cores to be used for parallel processing
#' operations using the environment variable MAX_CORES. If not specified
#' the total number of cores will be used.
#' @return An integer number of cores to use in parallel processing
#'
getMaxCores <- function() {

#Should never be called on Windows
 if (.Platform$OS.type == "windows") {
   return(1L)
 }

  n_cores <-
    ifelse(Sys.getenv("MAX_CORES") != "",
           as.numeric(Sys.getenv("MAX_CORES")),
           parallel::detectCores())

  stopifnot("MAX_CORES environment variable must be a whole integer" != is.integer(n_cores))

    if (n_cores > parallel::detectCores()) {
      n_cores <- parallel::detectCores()
      warning("MAX_CORES cannot be greater than available cores. Using available cores only.")
    }

  n_cores
}

#' Title
#' @note Lifted from https://stackoverflow.com/questions/16800803/
#' @description Format a vector of numbers into a string of ranges
#' @param vec A vector of numbers
#'
#' @return Formatted string of ranges
#' @export
#' @examples
#' formatSetStrings(c(1,2,3,5,6,7,8))
#' formatSetStrings(c(8,7,6,5,3,2,1))
#'
formatSetStrings <- function(vec) {

  if (!is.vector(vec)) return(NA_character_)

  if (is.list(vec)) {
    warning("Can only accept simple vectors")
    return(NA_character_)
    }

  vec <- vec[!is.na(vec)]

  if (length(vec) == 0) return(NA_character_)

  if (!all(is.numeric(vec))) {
     warning("Ensure that all values are numeric")
     return(NA_character_)
  }

  vec <- sort(vec)
  groups <- cumsum(c(0, diff(vec) > 1))
  sets <- split(vec, groups)
  set_strings <- sapply(sets, function(x) {
    ifelse(min(x) == max(x), x, paste0(min(x), ":", max(x))) })
  paste0(set_strings, collapse = ",")
}


#' @export
#' @title DHIS2 UID pattern
#' @md
#' @description Returns the DHIS2 UID pattern, expressed as a regular expression
#' in form of a character vector.
#'
#' @return DHIS2 UID pattern, expressed as a regular expression in form of a
#' character vector.
uid_pattern <- function() {
  "[[:alpha:]][[:alnum:]]{10}"
}


#' @export
#' @title Is UID-ish
#' @md
#' @description Tests whether a character string matches the regex of a DHIS2
#' 11-digit UID.
#'
#' @param string Input vector. Either a character vector, or something coercible
#' to one.
#' @param ish Logical. If TRUE, looks for the UID in all parts of string, rather
#' than requiring the string be only the UID.
#'
#' @return A logical vector.
is_uidish <- function(string, ish = FALSE) {
  if (!ish) {
    stringr::str_detect(string, paste0("^", uid_pattern(), "$"))
  } else {
    stringr::str_detect(string, uid_pattern())
  }
}

#' @export
#' @title Return the fresh portion of a cache file
#'
#' @param cache The cached file
#' @param max_age The maximum age allowed fora cache to be considered fresh
#'
#' @return A dataframe containing only the fresh portion of the cache file
fresh_cache_part <- function(cache, max_age) {
  cache[is_fresh(cache$cache_date, max_age)]
}


#' @export
#' @title Checks whether a cached file is stale
#'
#' @param cache Filepath to the cached file to check.
#' @param max_age The maximum age allowed for a cache to be considered fresh.
#' Follows syntax of \code{lubridate} package.
#'
#' @return A dataframe containing only the fresh portion of the cache file
#'
cache_is_fresh <- function(cache, max_age = NULL) {
  interactive_print(cache)

  if (!file.exists(cache)) {
    is_fresh <- FALSE
  } else if (file.access(cache, 4) != 0) { # Calc iff exists
    is_fresh <- FALSE
  } else { # Check age iff exists and can read

    max_age <- max_age %||% "1 day" %>%
      lubridate::duration()

    cache_age <-
      lubridate::as.duration(
        lubridate::interval(
          file.info(cache)$mtime,
          Sys.time()))

    is_fresh <- cache_age < max_age
  }

  is_fresh
}



#' Can Spawn
#' @description Determines whether processes can be run in parallel.
#' This is used in indicator and validation rule evaluation, but
#' should not be run on Windows currently
#' @return Boolean True or false
#' @export
#'
can_spawn <- function() {
  "parallel" %in% rownames(utils::installed.packages()) == TRUE &
    .Platform$OS.type != "windows"  #Never execute in parallel on Windows
}


#' Extract UID.
#'
#' @description Extracts a DHIS2 11-digit UID from provided string.
#'
#' @name extract_uid
#' @md
#'
#' @param string Input vector. Either a character vector, or something coercible
#' to one.
#' @param bracketed Boolean to indicate whether the UID is encased in square brackets.
#'
#' @return Character vector of DHIS2 11-digit UIDs found in string.
#'
NULL

#' @export
#' @rdname extract_uid
#'
extract_uid <- function(string, bracketed = TRUE) {

  pattern <- ifelse(bracketed,
                    paste0("(?<=\\[)", uid_pattern(), "(?=\\]$)"),
                    uid_pattern())

  stringr::str_extract(string, pattern)

}

#' @export
#' @rdname extract_uid
#'
extract_uid_all <- function(string) {
  unlist(stringr::str_extract_all(string, uid_pattern()))
}


#' @export
#' @title listWorkbookContents
#' @inheritParams datapackr_params
#' @return d
listWorkbookContents <- function(d) {
  d$info$workbook_contents <- utils::unzip(d$keychain$submission_path, list = TRUE) %>%
    dplyr::pull(`Name`)

  d
}

commas <- function(...) paste0(..., collapse = ", ")

names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

has_names <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rlang::rep_along(x, FALSE)
  } else {
    !(is.na(nms) | nms == "")
  }
}

ndots <- function(...) nargs()

bullet <- function(...) paste0(crayon::bold(crayon::silver(" * ")), sprintf(...))


is_empty <- function(x) {

  x <- stringr::str_trim(x)

  is.null(x) || is.na(x) || length(x) == 0 || x == ""
}


# Re-exports ---------------------------------------------------

#' Default value for `NULL`
#' @importFrom rlang `%||%`
rlang::`%||%`

#' Default value for `NA`
#' @importFrom rlang `%|%`
rlang::`%|%`

#' @importFrom magrittr `%>%`
magrittr::`%>%`

#' @importFrom magrittr `%<>%`
magrittr::`%<>%`
