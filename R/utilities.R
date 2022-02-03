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
#'
#' @return d
#'
getOUFromCountryUIDs <- function(country_uids) {
  ou <- datapackr::valid_PSNUs %>%
    dplyr::select(ou, ou_id, country_name, country_uid) %>%
    dplyr::distinct() %>%
    dplyr::filter(country_uid %in% country_uids) %>%
    dplyr::select(ou, ou_id) %>%
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
    # TODO: #Automate the character type or at least a list variable for type.
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
#' @param type character vector - one or more of:
#' {"mer_targets", "mer_results", "subnat_targets", "subnat_results", "impatt"}
#' @inheritParams datapackr_params
#' @return returns a character vector of the related dataset uids
#'
getDatasetUids <-  function(cop_year, type) {

  #Convert to cop_year everywhere

  cop_year <- check_cop_year(cop_year = cop_year)

  type <- type %missing% c("mer_targets", "mer_results",
                           "subnat_targets", "subnat_results",
                           "impatt")

  datasets_filtered <-
    list(
      "2022" = list(
        "mer_targets" =   c("iADcaCD5YXh", # MER Target Setting: PSNU (Facility and Community Combined)
        "o71WtN5JrUu", # MER Target Setting: PSNU (Facility and Community Combined) - DoD ONLY)
        "vzhO50taykm"), # Host Country Targets: DREAMS (USG)
        "mer_results" = NA,
        "subnat_targets" = "J4tdiDEi08O",
        "subnat_results" = NA,
        "impatt" = "CxMsvlKepvE"),
      "2021" = list(
        "mer_targets" =   c("YfZot37BbTm", # MER Target Setting: PSNU (Facility and Community Combined) FY2022
                            "cihuwjoY5xP", # MER Target Setting: PSNU (Facility and Community Combined) - DoD ONLY)
                            "wvnouBMuLuE"), # Host Country Targets: DREAMS (USG) FY2022),
        "mer_results" = c("BHlhyPmRTUY", # MER Results: Facility Based
                          "HfhTPdnRWES", # MER Results: Community Based
                          "MGNVwVicMVm"), # Host Country Results: DREAMS (USG),
        "subnat_targets" = "Va7TYyHraRn",
        "subnat_results" = "IXiORiVFqIv",
        "impatt" = "Zn27xns9Fmx"),
      "2020" = list(
        "mer_targets" =   c("Pmc0yYAIi1t", # MER Target Setting: PSNU (Facility and Community Combined) (TARGETS) FY2021
                             "s1sxJuqXsvV"),  # MER Target Setting: PSNU
                                              #(Facility and Community Combined) - DoD ONLY) FY2021,
                                              # Host Country Targets: DREAMS (USG) FY2022),
        "mer_results" =   c("zL8TlPVzEBZ", # MER Results: Facility Based FY2021Q4
                            "TBcmmtoaCBC", # MER Results: Community Based FY2021Q4
                            "qHyrHc4zwx4"), # Host Country Results: DREAMS (USG) FY2021Q4
        "subnat_targets" = "j7jzezIhgPj",
        "subnat_results" = "xiTCzZJ2GPP",
        "impatt" = "jxnjnBAb1VD"),
      "2019" = list(
        "mer_targets" = c("sBv1dj90IX6", # MER Targets: Facility Based FY2020
                        "nIHNMxuPUOR", # MER Targets: Community Based FY2020
                        "C2G7IyPPrvD", # MER Targets: Community Based - DoD ONLY FY2020
                        "HiJieecLXxN"), # MER Targets: Facility Based - DoD ONLY FY2020
          "mer_results" =   c("qzVASYuaIey", # MER Results: Community Based FY2020Q4
                              "BPEyzcDb8fT", # MER Results: Community Based - DoD ONLY FY2021Q4
                              "jKdHXpBfWop", # MER Results: Facility Based FY2020Q4
                               "em1U5x9hhXh", # MER Results: Facility Based - DoD ONLY FY2021Q4
                               "mbdbMiLZ4AA"), # Host Country Results: DREAMS (USG) FY2020Q4
          "subnat_targets" = "N4X89PgW01w",
          "subnat_results" = "ctKXzmv2CVu",
          "impatt" = "pTuDWXzkAkJ")) %>%
    purrr::pluck(as.character(cop_year)) %>%
    .[type] %>%
    unlist(use.names = FALSE) %>%
    purrr::discard(~ is.na(.))

  if (is.null(datasets_filtered) | length(datasets_filtered) == 0) {
    stop(paste("No datasets could be found for cop_year", cop_year, "and type(s)", type))
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
    purrr::pmap(pmax, na.rm = T) %>% # Row-wise Calculations.
    as.numeric

  return(df)
}

#' @export
#' @title get_Map_DataPack_DATIM_DEs_COCs
#'
#' @param cop_year cop year to pull get map for
#'
#' @return {cop21, cop22}_map_DataPack_DATIM_DEs_COCs
#'
getMapDataPack_DATIM_DEs_COCs <- function(cop_year) {

  switch(as.character(cop_year),
         "2021" = datapackr::cop21_map_DataPack_DATIM_DEs_COCs,
         "2022" = datapackr::cop22_map_DataPack_DATIM_DEs_COCs,
         stop("The COP year and configuration provided is not supported by get_Map_DataPack_DATIM_DEs_COCs"))

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
      template_path = template_path,
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
 if (.Platform$OS.type != "windows") {
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
#' @title Is UID-ish
#' @md
#' @description Tests whether a character string matches the regex of a DHIS2
#' 11-digit UID. Vectorized over `string` and `pattern`.
#'
#' @param string Input vector. Either a character vector, or something coercible
#' to one.
#'
#' @return A logical vector.
is_uidish <- function(string) {
  stringr::str_detect(string, "^[[:alpha:]][[:alnum:]]{10}$")
}





#' Title
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




extractWorkbook <- function(d) {
  #Create a temporary director to extract the XL object
  temp_dir <- file.path(tempdir(), "datapackR")
  #Save this in the keychain for later reuse
  d$keychain$extract_path <- temp_dir

  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir)
  file.copy(d$keychain$submission_path, temp_dir)

  new_file <- list.files(temp_dir, full.names = TRUE, pattern = basename(d$keychain$submission_path))
  utils::unzip(new_file, exdir = temp_dir)
  d$info$has_extract <- TRUE
  #Return the object
  d
}

listWorkbookContents <- function(d) {

  d$info$worbook_contents <- utils::unzip(d$keychain$submission_path, list = TRUE) %>%
    dplyr::pull(`Name`)

  d
}

#' @export
#' @title addPsnuid
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
addPsnuid <- function(string) {
  stringr::str_extract(string, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)")
}


#' @export
#' @title map_datim_join
#' @md
#' @description Tests whether a character string matches the regex of a DHIS2
#' 11-digit UID. Vectorized over `string` and `pattern`.
#'
#' @param data the dataset needed
#' @param second_data_set the datim_map used for the pack for datim function
#'
#' @importFrom stringr str_detect
#'
#' @return A dataframe, renamed and joined
map_datim_join <- function(data, second_data_set) {
  data %>% dplyr::left_join(., (second_data_set %>%
                                          dplyr::rename(Age = valid_ages.name,
                                                        Sex = valid_sexes.name,
                                                        KeyPop = valid_kps.name)),
                                    by = c("indicator_code", "Age", "Sex", "KeyPop", "support_type")) %>%
    tidyr::drop_na(dataelementuid, categoryoptioncombouid)
}

#' @export
#' @title set_datim_protocol
#' @md
#' @description Tests whether a character string matches the regex of a DHIS2
#' 11-digit UID. Vectorized over `string` and `pattern`.
#'
#' @param data the dataset needed
#'
#' @importFrom stringr str_detect
#'
#' @return a dataframe with the appropriate columns
set_datim_protocol <- function(data) {
  data %>% dplyr::select(
    dataElement = dataelementuid,
    period,
    orgUnit = psnuid,
    categoryOptionCombo = categoryoptioncombouid,
    attributeOptionCombo = mech_code,
    value
    )
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

#' @importFrom magrittr `%<>%`
#' @export
magrittr::`%<>%`
