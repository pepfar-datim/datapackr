#' Standardized Parameter Defaults
#'
#' @description Standardized package functions,in terms of parameter defaults
#' and checks.
#' These can be run individually (e.g., `check_country_uids`), or in bulk (e.g.,
#' `check_params(country_uids = "abcdefgh123", tool = "Data Pack")`).
#'
#' @name parameter-checks
#' @md
#'
#' @inheritParams datapackr_params
#'
#' @return
#' For lower-level functions, a valid function parameter value/object. For the
#' higher-level `check_params`, a list object containing one valid parameter
#' value/object for each non-missing parameter supplied.
#' * `check_country_uids`: List of `country_uids`.
#' * `check_PSNUs`: Dataframe of valid `PSNUs` (uid and names).
#' * `check_cop_year`: Valid `cop_year` as numeric value.
#' * `check_tool`: Valid `tool` type as string.
#' * `check_schema`: Valid `schema` as dataframe.
#' * `checkDataPackName`: Valid `datapack_name` as string.
#' * `checkTemplatePath`: Valid `template_path` as string.
#' * `checkWB`: Valid Data Pack shell for specified `cop_year` and `tool` type.
#' * `checkOutputFolder`: Valid `output_folder` as string.
#' * `checkResultsArchive`: Valid `results_archive` as `.rds` list object,
#' equivalent to the `d` object used throughout this package.
#' * `check_params`: List object containing one valid parameter value/object for
#' each non-missing parameter supplied.
#'
#' @section Defining Parameter Defaults:
#' As much as possible throughout this package, we have adhered to the principle
#' outlined in the Tidyverse Design Guide regarding parameter defaults for
#' functions: "Default values should be short and sweet. This makes the function
#' specification easier to scan."
#'
#' Where feasible, we have used the common approach of using `NULL` as the
#' parameter default. In many cases, we set this in the function parameter
#' definition upfront, and then calculate the default only when this value is
#' unsupplied or otherwise left `NULL`. The virtue of this approach is that not
#' supplying this value at all still supplies `NULL` to the  function's internal
#' logic without a `missing` error. In many cases, we use the following custom
#' function to make this simpler and cleaner:
#'
#' ```
#' `%||%` <- function(x, y) if (is.null(x)) y else x
#' ```
#'
#' For example:
#'
#' ```
#' example_function <- function(arg1 = NULL) {
#'   arg2 <- arg1 %||% 10
#'   arg2 + 10
#' }
#'
#' example_function()
#' 20
#' ```
#'
#' However, in some cases within this package, `NULL` is an equally valid value
#' that could be passed to a parameter from a higher-level function. To
#' distinguish these from truly missing values, we have in some places left the
#' default undefined in the function parameter and employed the following custom
#' function:
#'
#' ```
#' `%missing%` <- function(x, y = NULL) rlang::maybe_missing(x, y)
#' ```
#'
#' For example:
#'
#' ```
#' example_function2 <- function(arg1) {
#'   arg1 <- arg1 %missing% NULL
#'   arg2 <- arg1 %||% 10
#'   arg2 + 10
#' }
#' ```
#' This function allows the following to return equivalent values without
#' `missing` errors:
#'
#' ```
#' test_arg <- rlang::missing_arg()
#' test_arg2 <- NULL
#' example_function2()
#' 20
#'
#' example_function2(test_arg)
#' 20
#'
#' example_function2(test_arg2)
#' 20
#' ```
#'
#' Similarly, when nested within a higher-level function, this approach also
#' accommodates scenarios where `missing` or `NULL` values may be meaningful:
#'
#' ```
#' example_function3 <- function(arg1, arg2) {
#'   example_function2(arg1)
#' }
#'
#' example_function3(arg1 = test_arg, arg2 = 3)
#' 20
#'
#' example_function3(arg1 = test_arg2, arg2 = 3)
#' 20
#' ```
#'
#' So, within this package, we alternatively use `%||%` and `%missing%` to
#' determine default parameters based on the situation and package usage.
#'
#' Because of the sometimes complicated manner in determining default parameters,
#' which can often change from year to year, we have attempted to centralize and
#' standardize how all parameters are validated and how default parameters are
#' determined here within this function.
#'
#' @family parameter-helpers
NULL



#' @export
#' @param force logical. Should country_uids be required?
#' @rdname parameter-checks
check_country_uids <- function(country_uids, cop_year, force = TRUE) {

  country_uids <- country_uids %missing% NULL
  cop_year <- cop_year %missing% NULL
  cop_year %<>% check_cop_year(cop_year = cop_year)

  valid_orgunits_local <- getValidOrgUnits(cop_year)

  # If any country_uids are invalid, warn but remove and still move on.
  if (!all(country_uids %in% valid_orgunits_local$country_uid)) {
    # subset submitted list base on it values NOT being in valid_OrgUnits
    invalid_country_uids <- country_uids[!country_uids %in% valid_orgunits_local$country_uid]

    interactive_message(
      paste0("The following supplied country_uids appear to be invalid and will be removed: ",
             paste_oxford(invalid_country_uids, final = "&"))
    )
    # subset submitted list base on it values being in valid_OrgUnits
    country_uids <- country_uids[country_uids %in% valid_orgunits_local$country_uid]

    if (length(country_uids) == 0) {
      interactive_message(
        "All supplied country_uids appear to have been invalid."
      )

      country_uids <- NULL
    }
  }

  if (is.null(country_uids)) {
    # Usually, country_uids is a required parameter.
    if (force) {
      stop("Must supply valid country_uids.")
    } else {
      # Less often, we can move on by using all country_uids
      interactive_message(
        paste0("Given no valid country_uids were supplied, and you have ",
               "selected force = FALSE, all country_uids have been returned.")
      )

      country_uids <- unique(valid_orgunits_local$country_uid)
    }
  }

  country_uids

}

#' @export
#' @rdname parameter-checks
check_PSNUs <- function(PSNUs = NULL, country_uids = NULL, cop_year = NULL) {

  cop_year <- cop_year %missing% NULL
  cop_year %<>% check_cop_year(cop_year = cop_year)

  valid_orgunits_local <- getValidOrgUnits(cop_year)
  # If no country_uids provided, return PSNUs across all country_uids.
  country_uids <- country_uids %missing% NULL
  country_uids %<>% check_country_uids(cop_year = cop_year, force = FALSE)

  # If PSNUs not provided, fill with all PSNUs
  if (is.null(PSNUs)) {
    PSNUs <- valid_orgunits_local %>%
      dplyr::filter(., country_uid %in% country_uids) %>%
      add_dp_psnu(.) %>%
      dplyr::arrange(dp_label) %>%
      dplyr::select(PSNU = dp_label, psnu_uid = uid)
  } else {
    # If PSNUs is provided, check to make sure these are all valid.
    # Warn and remove invalid PSNu's as needed.
    if (!all(PSNUs$psnu_uid %in% valid_orgunits_local$uid)) {
      invalid_PSNUs <- PSNUs %>%
        dplyr::filter(!psnu_uid %in% valid_orgunits_local$uid) %>%
        add_dp_label(orgunits = ., cop_year = cop_year) %>%
        dplyr::arrange(dp_label) %>%
        dplyr::select(PSNU = dp_label, psnu_uid)

      interactive_message(
        paste0("The following PSNUs were supplied as a parameter, but appear to ",
               "be invalid and will be removed: \n\n",
               paste_dataframe(invalid_PSNUs)))

      PSNUs <- PSNUs %>%
        dplyr::filter(psnu_uid %in% valid_orgunits_local$uid)
    }
  }

  PSNUs

}


#' @export
#' @rdname parameter-checks
check_cop_year <- function(cop_year, tool) {

  # If cop_year is NULL or missing, use default from package
  cop_year <- cop_year %missing% NULL
  cop_year <- cop_year %||% getCurrentCOPYear()

  # Check type & parse if character and resembles a numeric
  cop_year %<>% parse_maybe_number() # Found in utilities.R

  # Check that provided COP Years are supported ####
  if (!cop_year %in% supportedCOPYears()) {
    stop(paste0("Sorry, datapackr only supports tools from ",
                paste_oxford(paste0("COP", supportedCOPYears() - 2000),
                             final = "&",
                             oxford = FALSE),
                "."))
  }

  cop_year
}


#' @export
#' @rdname parameter-checks
check_tool <- function(tool, cop_year) {
  # If tool not provided — even if cop_year is — return default.
  # If only tool provided, validate it's a valid choice.
  # If tool & cop_year provided, validate against each other.

  # Collect parameters.
  tool <- tool %missing% NULL
  tool_provided <- !is.null(tool)

  cop_year <- cop_year %missing% NULL
  cop_year_provided <- !is.null(cop_year)

  # Validate clue parameters
  if (cop_year_provided) {
    cop_year %<>% check_cop_year()
  }

  # If tool not provided, return default.
  default_cop_tool <- "Data Pack"
  default_opu_tool <- "PSNUxIM"

  #Default to a datapack if the tool is not specified.
  if (!tool_provided) {
    return(default_cop_tool)
  }

  # Rule out invalid tools.
  if (tool_provided) {
    if (!tool %in% supportedTools()) {
      stop("Unknown tool parameter provided. We only support ",
           paste_oxford(paste0(supportedTools(), "s"), final = "&"))
    }
  }


  if (tool_provided && cop_year_provided) {
    if (!cop_year %in% supportedCOPYears(tool = tool) ||
        !tool %in% supportedTools(cop_year = cop_year)) {
      stop("In check_tool, provided tool & provided cop_year don't match.")
    }
  }

  tool
}

#' @export
#' @rdname parameter-checks
check_schema <- function(schema, cop_year, tool) {

  # Collect parameters
  schema <- schema %missing% NULL
  schema_provided <- !is.null(schema)

  cop_year <- cop_year %missing% NULL
  cop_year_provided <- !is.null(cop_year)

  tool <- tool %missing% NULL
  tool_provided <- !is.null(tool)

  # Validate parameters
  cop_year %<>% check_cop_year()
  tool %<>% check_tool(tool = ., cop_year = cop_year)

  # For NULL schemas, attempt to deduce from other parameters, if provided.
  # Default here is the COP schema for the most recent/current COP Year
  expected_schema <- suppressMessages(pick_schema(tool = tool, cop_year = cop_year))

  schema <- schema %||% expected_schema

  if (!schema_provided && !tool_provided && !cop_year_provided) {
    interactive_message(
      paste0(
        "Because of ommitted parameters, we assumed you meant the schema for ",
        "the COP", cop_year - 2000, " ", tool, "."))
  }

  if (!identical(schema, expected_schema)) {
    interactive_message(
      paste0("Schema provided either does not match archived schema, or is ",
             "mismatched with the provided parameters. Are you using a custom schema on purpose?")
    )
  }

  schema
}


#' @export
#' @rdname parameter-checks
checkDataPackName <- function(datapack_name, country_uids, cop_year) {

  # If cop_year is NULL or missing, use default from package
  cop_year <- cop_year %missing% NULL
  cop_year <- cop_year %||% getCurrentCOPYear()

  valid_orgunits_local <- getValidOrgUnits(cop_year)
  valid_dp_names <- c(unique(valid_orgunits_local$country_name), "Caribbean Region", "Central America and Brazil")

  # Collect parameters
  datapack_name <- datapack_name %missing% NULL
  datapack_name_provided <- !is.null(datapack_name)

  country_uids <- country_uids %missing% NULL
  country_uids_provided <- !is.null(country_uids)

  if (country_uids_provided) {
    country_uids %<>% check_country_uids(cop_year = cop_year)

    caribbean <- c("RKoVudgb05Y", "PeOHqAwdtez", "WuxG6jzaypt",
                   "zhJINyURZ5Y", "WSl5y9jxCpC")
    central_america <- c("joGQFpKiHl9", "QKD4CzBG2GM", "N7QAPGSaODP",
                         "EXVC4bNtv84", "w5NMe34EjPN", "aUTsSmqqu9O",
                         "oK0gC85xx2f")

    # Thu Nov 30 13:59:22 2023 added the 2024 to address country's being broken out.
    if (all(country_uids %in% caribbean) && cop_year != 2024) {
      expected_dpname <- "Caribbean Region"
    } else if (all(country_uids %in% central_america) && cop_year != 2024) {
      expected_dpname <- "Central America and Brazil"
    } else {
      expected_dpname <- valid_orgunits_local %>%
        dplyr::filter(country_uid %in% country_uids) %>%
        dplyr::pull(country_name) %>%
        unique() %>%
        sort() %>%
        paste(collapse = ", ")
    }
  }

  if (!datapack_name_provided) {
    if (!country_uids_provided) {
      interactive_message("Could not deduce datapack_name, so applied 'Global'.")
      datapack_name <- "Global"
    } else {
      interactive_message("Deduced datapack_name based on country_uids.")
      datapack_name <- expected_dpname
    }
  }

  # If country_uids provided, use it to validate datapack_name
  if (country_uids_provided) {
    if (datapack_name != expected_dpname) {
      if (datapack_name %in% valid_dp_names) {
        stop(
          "The datapack_name does not match the provided country_uids."
        )
      } else {
        interactive_message("Did you mean to use a custom Data Pack name?")
      }
    }
  }

  datapack_name
}


#' @export
#' @rdname parameter-checks
#' @importFrom utils capture.output
checkTemplatePath <- function(template_path,
                              cop_year,
                              tool) {

  # Collect parameters
  template_path <- template_path %missing% NULL
  template_path_provided <- !is.null(template_path)

  cop_year <- cop_year %missing% NULL
  cop_year_provided <- !is.null(cop_year)

  tool <- tool %missing% NULL
  tool_provided <- !is.null(tool)

  # Validate parameters
  cop_year %<>% check_cop_year()
  tool %<>% check_tool(tool = ., cop_year = cop_year)

  # For NULL template_paths, attempt to deduce from other parameters, if
  # provided. Default here is the template_path for the most recent/current COP
  # Year for the Data Pack.
  invisible(
    utils::capture.output(
      # pick_template_path found in packageSetup.R
      expected_template_path <- pick_template_path(cop_year = cop_year, tool = tool)))

  template_path <- template_path %||% expected_template_path
  template_path %<>%
    handshakeFile(path = ., tool = tool)

  if (!template_path_provided) {
    interactive_message(
      paste0(
        "Because of ommitted parameters, we assumed you meant the template_path for ",
        "the COP", cop_year - 2000, " ", tool, "."))
  }

  if (template_path != expected_template_path) {
    interactive_message("That template_path is either custom, or doesn't match other parameters.")
  }

  interactive_message("Checking template against schema and DATIM...")
  expected_schema <- pick_schema(cop_year, tool)

  # Only compare submitted template to template on record if `tool` is not a template
  # Trying to compare template files using this method results in an endless loop with `unPackSchema`
  if (!stringr::str_detect(tool, "Template$")) {
    input_tool <- paste0(tool, " Template")
    template_schema <-
      unPackSchema(
        template_path = template_path,
        skip = skip_tabs(tool = input_tool, cop_year = cop_year),
        tool = input_tool,
        cop_year = cop_year)

    if (!identical(expected_schema, template_schema)) {
      interactive_message("Template at that destination does not match our archived schema.")
    }
  }

  template_path
}


#' @export
#' @rdname parameter-checks
checkWB <- function(wb = NULL,
                    country_uids = NULL,
                    cop_year = NULL,
                    tool = NULL,
                    datapack_name = NULL,
                    template_path = NULL) {

  if (is.null(wb)) {
    country_uids <- check_country_uids(country_uids = country_uids, cop_year = cop_year)
    cop_year <- check_cop_year(cop_year = cop_year)
    tool <- check_tool(tool = tool, cop_year = cop_year)
    datapack_name <- checkDataPackName(datapack_name = datapack_name, country_uids = country_uids, cop_year = cop_year)
    template_path <- checkTemplatePath(template_path = template_path, cop_year = cop_year, tool = tool)

    d <- createDataPack(datapack_name = datapack_name,
                        country_uids = country_uids,
                        template_path = template_path,
                        cop_year = cop_year,
                        tool = tool)

    wb <- d$tool$wb
  }

  wb
}


#' @export
#' @rdname parameter-checks
checkOutputFolder <- function(output_folder = NULL) {
  # If output_folder parameter is not set or not a valid filepath, throw error message.
  if (is.null(output_folder) || file.access(output_folder, 2) != 0) {
    stop("output_folder must be a valid filepath")
  }

  output_folder
}


#' @export
#' @rdname parameter-checks
checkResultsArchive <- function(results_archive = FALSE) {
  # IF results_archive parameter is not set throw error message.
  if (!isTRUE(results_archive) && !isFALSE(results_archive)) {
    stop("results_archive must be either TRUE or FALSE.")
  }

  results_archive
}


#' @export
#' @param all_sheets Logical. Return/check against all sheets (as opposed to only
#'   those with targets)?
#' @param psnuxim Logical. Return/check against PSNUxIM tab as well?
#' @param operation String. Options = "unpack", "pack", "schema", or "other".
#' @rdname parameter-checks
checkSheets <- function(sheets,
                        cop_year,
                        tool,
                        all_sheets = FALSE,
                        operation = "schema",
                        psnuxim = FALSE) {

  # Collect parameters
  sheets <- sheets %missing% NULL
  sheets_provided <- !is.null(sheets)

  tool <- tool %missing% NULL
  tool_provided <- !is.null(tool)

  cop_year <- cop_year %missing% NULL
  cop_year_provided <- !is.null(cop_year)

  # Validate parameters
  cop_year %<>% check_cop_year()
  tool %<>% check_tool(tool = ., cop_year = cop_year)
  schema <- check_schema(schema = NULL, cop_year = cop_year, tool = tool)

  all_sheets <- all_sheets %||% FALSE
  psnuxim <- psnuxim %||% FALSE

  if (all_sheets) {
    skips <- ""
  } else {
    skips <- skip_tabs(tool = tool, cop_year = cop_year)

    skips <-
      switch(operation,
             pack = skips$pack,
             unpack = skips$unpack,
             schema = skips$schema)
  }

  sheets_schema <- schema %>%
    dplyr::filter(
      !sheet_name %in% skips) %>%
    dplyr::pull(sheet_name) %>%
    unique()

  if (!psnuxim) {
    sheets_schema <- sheets_schema[!sheets_schema %in% c("PSNUxIM")]
  }

  if (!sheets_provided) {
    sheets <- sheets_schema
  } else {
    invalid_sheets_param <- sheets[!sheets %in% sheets_schema]

    sheets <- sheets[sheets %in% sheets_schema]

    if (length(sheets) == 0) {
      stop("All provided sheets were either invalid or not present.\n")
    } else if (length(invalid_sheets_param) > 0) {
      interactive_warning(
        paste0(
          "The following sheets are either invalid or not present. Only those ",
          "remaining valid sheets will be returned. -> \n\t* ",
          paste(invalid_sheets_param, collapse = "\n\t* "),
          "\n"))
    }

  }

  sheets

}


#' @export
#' @rdname parameter-checks
check_params <- function(country_uids,
                         PSNUs,
                         cop_year,
                         tool,
                         schema,
                         datapack_name,
                         template_path,
                         wb,
                         model_data,
                         snuxim_model_data,
                         output_folder,
                         results_archive,
                         sheets,
                         ...) {

  params <- list()

  dots <- list(...)


  # Check cop_year ####
  if (!missing(cop_year)) {
    params$cop_year <- check_cop_year(cop_year)
  }

  # Check Country UIDs ####
  if (!missing(country_uids)) {
    params$country_uids <- check_country_uids(country_uids, cop_year, ...)
  }

  # Check PSNUs ####
  if (!missing(PSNUs)) {
    params$PSNUs <- check_PSNUs(PSNUs, country_uids, cop_year)
  }

  # Check tool ####
  if (!missing(tool)) {
    params$tool <- check_tool(tool, cop_year)
  }

  # Check schema ####
  if (!missing(schema)) {
    params$schema <- check_schema(schema = schema,
                                  cop_year = cop_year,
                                  tool = tool)
  }

  # Check datapack_name ####
  if (!missing(datapack_name)) {
    params$datapack_name <- checkDataPackName(datapack_name = datapack_name,
    country_uids = country_uids, cop_year = cop_year)
  }

  # Check template path ####
  if (!missing(template_path)) {
    params$template_path <- checkTemplatePath(template_path = template_path,
                                              cop_year = cop_year,
                                              tool = tool)
  }

  # Check wb ####
  if (!missing(wb)) {
    params$wb <- checkWB(wb = wb,
                         country_uids = country_uids,
                         cop_year = cop_year,
                         tool = tool,
                         datapack_name = datapack_name,
                         template_path = template_path)
  }

  # Check model_data ####


  # Check snuxim_model_data ####
  # check_snuxim_model_data <- function(snuxim_model_data = NULL,
  #                                     cop_year = NULL,
  #                                     country_uids = NULL,
  #                                     d2_session) {
  #
  #   cop_year = check_cop_year(cop_year)
  #   country_uids = check_country_uids(country_uids)
  #
  #   if (is.null(snuxim_model_data)) {
  #
  #   }
  #   return(snuxim_model_data)
  # }
  #
  # if (!missing(snuxim_model_data)) {
  #   params$snuxim_model_data <- check_snuxim_model_data(snuxim_model_data = snuxim_model_data,
  #                                                       cop_year = cop_year,
  #                                                       country_uids = country_uids,
  #                                                       d2_session = d2_session)
  # }

  # Check output_folder ####
  if (!missing(output_folder)) {
    params$output_folder <- checkOutputFolder(output_folder)
  }

  # Check results_archive ####
  if (!missing(results_archive)) {
    params$results_archive <- checkResultsArchive(results_archive)
  }

  # Check sheets ----
  if (!missing(sheets)) {
    params$sheets <- checkSheets(sheets = sheets,
                                 cop_year = params$cop_year,
                                 tool = params$tool,
                                 all_sheets = dots$all_sheets,
                                 psnuxim = dots$psnuxim)
  }


  return(params)
}
