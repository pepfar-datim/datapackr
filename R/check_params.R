#' Standardized Parameter Defaults
#'
#' @description Standardized package function parameter defaults and checks.
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
#' * `check_season`: Valid `season` as string.
#' * `check_schema`: Valid `schema` as dataframe.
#' * `checkDataPackName`: Valid `datapack_name` as string.
#' * `checkTemplatePath`: Valid `template_path` as string.
#' * `checkWB`: Valid Data Pack shell for specified `cop_year` and `tool` type.
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
#' determine default parameters based on this situation and package usage.
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
check_country_uids <- function(country_uids, force = TRUE) {

  country_uids <- country_uids %missing% NULL

  # If any country_uids are invalid, warn but remove and still move on.
  if (any(!country_uids %in% valid_PSNUs$country_uid)) {

    invalid_country_uids <- country_uids[!country_uids %in% valid_PSNUs$country_uid]

    interactive_message(
      paste0("The following supplied country_uids appear to be invalid and will be removed: ",
             paste_oxford(invalid_country_uids, final = "&"))
    )

    country_uids <- country_uids[country_uids %in% valid_PSNUs$country_uid]

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

      country_uids <- unique(valid_PSNUs$country_uid)
    }
  }

  country_uids

}

#' @export
#' @rdname parameter-checks
check_PSNUs <- function(PSNUs = NULL, country_uids = NULL) {
  # TODO: Update how we use PSNUs everywhere to use a character vector of uids
  #   instead of dataframe of all metadata

  # If no country_uids provided, return PSNUs across all country_uids.
  country_uids <- country_uids %missing% NULL
  country_uids %<>% check_country_uids(force = FALSE)

  # If PSNUs not provided, fill with all PSNUs
  if (is.null(PSNUs)) {
    PSNUs <- datapackr::valid_PSNUs %>%
      dplyr::filter(., country_uid %in% country_uids) %>%
      add_dp_psnu(.) %>%
      dplyr::arrange(dp_psnu) %>%
      dplyr::select(PSNU = dp_psnu, psnu_uid)
  } else {
    # If PSNUs is provided, check to make sure these are all valid. Warn and remove
    if (any(!PSNUs$psnu_uid %in% valid_PSNUs$psnu_uid)) {
      invalid_PSNUs <- PSNUs %>%
        dplyr::filter(!psnu_uid %in% valid_PSNUs$psnu_uid) %>%
        add_dp_psnu(.) %>%
        dplyr::arrange(dp_psnu) %>%
        dplyr::select(PSNU = dp_psnu, psnu_uid)

      interactive_message(
        paste0("The following PSNUs were supplied as a parameter, but appear to ",
               "be invalid and will be removed: \n\n",
               paste_dataframe(invalid_PSNUs)))

      PSNUs <- PSNUs %>%
        dplyr::filter(psnu_uid %in% valid_PSNUs$psnu_uid)
    }
  }

  PSNUs

}


#' @export
#' @rdname parameter-checks
check_cop_year <- function(cop_year) {
  supported_cop_years <- c(2020, 2021, 2022)

  # If cop_year is NULL or missing, use default from package
  cop_year <- cop_year %missing% NULL
  cop_year <- cop_year %||% getCurrentCOPYear()

  # Check type & parse if character and resembling numeric
  cop_year %<>% parse_maybe_number()

  if (!cop_year %in% supported_cop_years) {
    stop(paste0("Sorry, datapackr only supports tools from ",
                paste_oxford(paste0("COP",supported_cop_years - 2000))))
  }

  cop_year

}


#' @export
#' @rdname parameter-checks
check_tool <- function(tool, season, cop_year) {

  supported_tools <- c("Data Pack", "OPU Data Pack")
  default_tool <- "Data Pack"

  # Collect parameters
  tool <- tool %missing% NULL
  tool_provided <- !is.null(tool)

  season <- season %missing% NULL
  season_provided <- !is.null(season)

  cop_year <- cop_year %missing% NULL
  cop_year_provided <- !is.null(cop_year)

  # Rule out any bogus tools so only NULL or valid tools remain.
  if (tool_provided) {
    if (!tool %in% supported_tools) {
      stop("Unknown tool parameter provided.")
    }
  }

  # Validate cop_year and season, if provided.
  if (cop_year_provided) cop_year %<>% check_cop_year()
  if (season_provided) {
    season %<>% check_season(season = ., tool = tool)
    deduced_tool <- switch(season, "OPU" = "OPU Data Pack", "COP" = "Data Pack")
  }

  # For NULL tools, attempt to deduce from season.
  if (!tool_provided) {
    if (season_provided) {
      tool <- deduced_tool
      interactive_message("Deduced tool based on season.")
    } else {
      # If tool and season are both NULL, default tool is "Data Pack".
      tool <- default_tool
      interactive_message("Since neither tool nor season was provided, we assumed you meant 'Data Pack'.")
    }
  }

  # No matter what, we now have a tool. If we also have season, use it to
  # validate tool type.
  if (!tool %in% supported_tools) {
    stop("Unknown tool parameter provided.")
  }

  if (season_provided) {
    if (tool != deduced_tool) {
      interactive_message("That tool is not valid for that season.")
    }
  }

  # If we have cop_year, check whether the tool is still compatible with
  # datapackr for that year.
  if (cop_year_provided) {
    valid_cop_years <-
      switch(
        tool,
        "Data Pack" = 2021:2022,
        "OPU Data Pack" = 2020:2021
      )

    if (!cop_year %in% valid_cop_years) {
      interactive_message(paste0("Sorry, we no longer fully support ", tool, "s for that cop_year."))
    }

  }

  tool
}


#' @export
#' @rdname parameter-checks
check_season <- function(season, tool) {
  # If season & tool are both provided, validate season against tool. If season
  # alone is provided, . If only tool is provided, deduce season from tool. If
  # neither is provided, default to "COP".

  supported_seasons <- c("COP", "OPU")
  default_season <- "COP"

  # If tool provided, validate it.
  tool <- tool %missing% NULL
  tool_provided <- !is.null(tool)
  if (tool_provided) {
    tool %<>% check_tool()
    deduced_season <- switch(tool, "OPU Data Pack" = "OPU", "Data Pack" = "COP")
  }

  # Determine if season is provided
  season <- season %missing% NULL
  season_provided <- !is.null(season)

  if (!season_provided) {
    # If season not provided, attempt to deduce based on tool
    if (tool_provided) {
      season <- deduced_season
      interactive_message("Deduced season based on tool.")
    } else {
      # Default season is "COP", because default tool type is "Data Pack"
      season <- default_season
      interactive_message("Since neither season nor tool was provided, we assumed you meant 'COP'.")
    }
  }

  # No matter what, we now have a season. If we also have a tool, use it to validate season.
  if (!season %in% supported_seasons) {
    stop("Cannot support any seasons other than `COP` or `OPU`.")
  }

  if (tool_provided) {
    if (season != deduced_season) {
      interactive_message("That season is not valid for that tool.")
    }
  }

  season

}


#' @export
#' @rdname parameter-checks
check_schema <- function(schema, cop_year, tool, season) {

  # Collect parameters
  schema <- schema %missing% NULL
  schema_provided <- !is.null(schema)

  cop_year <- cop_year %missing% NULL
  cop_year_provided <- !is.null(cop_year)

  tool <- tool %missing% NULL
  tool_provided <- !is.null(tool)

  season <- season %missing% NULL
  season_provided <- !is.null(season)

  # Validate parameters
  cop_year %<>% check_cop_year()
  season %<>% check_season(season = ., tool = tool)
  tool %<>% check_tool(tool = ., season = season, cop_year = cop_year)

  # For NULL schemas, attempt to deduce from other parameters, if provided.
  # Default here is the COP schema for the most recent/current COP Year
  invisible(
    capture.output(
      expected_schema <- pick_schema(tool = tool, cop_year = cop_year)))

  schema <- schema %||% expected_schema

  if (!schema_provided) {
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
checkDataPackName <- function(datapack_name, country_uids) {

  valid_dp_names <- c(unique(valid_PSNUs$country_name), "Caribbean Region", "Central America and Brazil")

  # Collect parameters
  datapack_name <- datapack_name %missing% NULL
  datapack_name_provided <- !is.null(datapack_name)

  country_uids <- country_uids %missing% NULL
  country_uids_provided <- !is.null(country_uids)

  if (country_uids_provided) {
    country_uids %<>% check_country_uids()

    caribbean <- c("RKoVudgb05Y", "PeOHqAwdtez", "WuxG6jzaypt",
                   "zhJINyURZ5Y", "WSl5y9jxCpC")
    central_america <- c("joGQFpKiHl9", "QKD4CzBG2GM", "N7QAPGSaODP",
                         "EXVC4bNtv84", "w5NMe34EjPN", "aUTsSmqqu9O",
                         "oK0gC85xx2f")

    if (all(country_uids %in% caribbean)) {
      expected_dpname <- "Caribbean Region"
    } else if (all(country_uids %in% central_america)) {
      expected_dpname <- "Central America and Brazil"
    } else {
      expected_dpname <- valid_PSNUs %>%
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
checkTemplatePath <- function(template_path,
                              cop_year,
                              tool,
                              season) {

  # Collect parameters
  template_path <- template_path %missing% NULL
  template_path_provided <- !is.null(template_path)

  cop_year <- cop_year %missing% NULL
  cop_year_provided <- !is.null(cop_year)

  tool <- tool %missing% NULL
  tool_provided <- !is.null(tool)

  season <- season %missing% NULL
  season_provided <- !is.null(season)

  # Validate parameters
  cop_year %<>% check_cop_year()
  season %<>% check_season(season = ., tool = tool)
  tool %<>% check_tool(tool = ., season = season, cop_year = cop_year)

  # For NULL template_paths, attempt to deduce from other parameters, if
  # provided. Default here is the template_path for the most recent/current COP
  # Year for the Data Pack.
  invisible(
    capture.output(
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

  input_tool <- paste0(tool, " Template")
  template_schema <-
    unPackSchema_datapack(
      filepath = template_path,
      skip = skip_tabs(tool = input_tool, cop_year = cop_year),
      tool = input_tool,
      cop_year = cop_year)

  if (!identical(expected_schema, template_schema)) {
    interactive_message("Template at that destination does not match our archived schema.")
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
    country_uids <- check_country_uids(country_uids)
    cop_year <- check_cop_year(cop_year)
    tool <- check_tool(tool)
    datapack_name <- checkDatapackName(datapack_name, country_uids)
    template_path <- checkTemplatePath(template_path, cop_year, tool)

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
checkResultsArchive <- function(results_archive = FALSE) {

  if (!isTRUE(results_archive) & !isFALSE(results_archive)) {
    stop("results_archive must be either TRUE or FALSE.")
  }

  results_archive
}



#' @export
#' @rdname parameter-checks
check_params <- function(country_uids,
                         PSNUs,
                         cop_year,
                         tool,
                         season,
                         schema,
                         datapack_name,
                         template_path,
                         wb,
                         model_data,
                         snuxim_model_data,
                         output_folder,
                         results_archive,
                         ...) {

  params <- list()

  # Check Country UIDs ####
  if (!missing(country_uids)) {
    params$country_uids <- check_country_uids(country_uids, ...)
  }

  # Check PSNUs ####
  if (!missing(PSNUs)) {
    params$PSNUs <- check_PSNUs(PSNUs, country_uids)
  }

  # Check cop_year ####
  if (!missing(cop_year)) {
    params$cop_year <- check_cop_year(cop_year)
  }

  # Check tool ####
  if (!missing(tool)) {
    params$tool <- check_tool(tool, season, cop_year)
  }

  # Check season ####
  if (!missing(season)) {
    params$season <- check_season(season, tool = tool)
  }

  # Check schema ####
  if (!missing(schema)) {
    params$schema <- check_schema(schema = schema,
                                  cop_year = cop_year,
                                  tool = tool,
                                  season = season)
  }

  # Check datapack_name ####
  if (!missing(datapack_name)) {
    params$datapack_name <- checkDataPackName(datapack_name, country_uids)
  }

  # Check template path ####
  if (!missing(template_path)) {
    params$template_path <- checkTemplatePath(template_path = template_path,
                                              cop_year = cop_year,
                                              tool = tool,
                                              season = season)
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


  # Check results_archive ####
  if (!missing(results_archive)) {
    params$results_archive <- checkResultsArchive(results_archive)
  }


  return(params)
}