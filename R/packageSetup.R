#:TODO Move these next three methods of this into the schema and make a function of the COP Year.
#This should NOT be hard coded here as it may and will change.
#' @export
#' @title Returns current COP Year
#'
#' @return Current COP Year. (e.g., for COP19, returns 2019)
#'
getCurrentCOPYear <- function() {
  2024 #Should be updated with the release of Beta-Packs every year
}


#' @export
#' @title Supported Tools & COP Years
#' @return Tibble of supported tools mapped to their supported COP Years
#'
datapackrSupports <- function() {
  tibble::tribble(
    ~tools, ~yrs, ~seasons,
    "Data Pack", c(2023, 2024), c("COP", "OPU"),
    "OPU Data Pack", c(2023), c("OPU", "COP"),
    "Data Pack Template", c(2023, 2024), c("COP", "OPU"),
    "OPU Data Pack Template", c(2023), c("OPU", "COP"),
    "PSNUxIM", c(2023, 2024), c("COP", "OPU"),
    "PSNUxIM Template", c(2024), c("COP", "OPU"))
}


#' @export
#' @title Returns COP Years currently supported by the package for a given tool.
#'
#' @description If no tool is provided, will supply COP Years supported across
#' all tools.
#'
#' @inheritParams datapackr_params
#' @return Vector of COP Years currently supported by the package for the given tool.
supportedCOPYears <- function(tool = NULL) {

  tool <- tool %missing% NULL
  tool_provided <- !is.null(tool)
  tool %<>% suppressWarnings(check_tool())

  if (tool_provided) {
    supported_cop_years <- datapackrSupports()$yrs[datapackrSupports()$tools == tool] %>%
      unlist()
  } else {
    supported_cop_years <- datapackrSupports()$yrs %>%
      unlist %>%
      unique() %>%
      sort()
  }

  supported_cop_years
}

#' @export
#' @title Supported Tools
#' @inheritParams datapackr_params
#' @return Character vector of tools supported by the package for a given cop_year.
#' If cop_year is not provided, will provide list of all tools supported for any
#' cop_year.
supportedTools <- function(cop_year = NULL) {

  cop_year <- cop_year %missing% NULL
  cop_year_provided <- !is.null(cop_year)
  cop_year %<>% suppressWarnings(check_cop_year())

  supported_tools <- datapackrSupports() %>%
    tidyr::unnest(yrs) %>%
    tidyr::unnest(seasons)

  if (cop_year_provided) {
    supported_tools %<>%
      dplyr::filter(yrs == cop_year)
  }

  unique(supported_tools$tools)
}



#' @export
#' @title Location of name of the tool on Home tab.
#'
#' @return Cell reference where the name of the tool is located.
#'
toolName_homeCell <- function() {
  "B10"
}

#' @export
#' @title Location of Country UIDs on Home tab.
#'
#' @return Cell reference where the name of the DataPack is located.
#'
countryUIDs_homeCell <- function() {
  "B25"
}


#' @export
#' @title Location of Name of the DataPack on the Home tab
#'
#' @return Cell reference where the name of the datapack can be found.
#'
dataPackName_homeCell <- function() {
  "B20"
}


#' @export
#' @title List of tabs to skip for given tool.
#'
#' @param tool "Data Pack", "Data Pack Template".
#' @param cop_year COP year for dating as well as selection of
#' templates.
#'
#' @return List of tab names to skip.
#'
skip_tabs <- function(tool = "Data Pack", cop_year) {

  # Check/Fill in parameters ####
  params <- check_params(cop_year = cop_year,
                         tool = tool)

  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }

  rm(params, p)

  skip <- list("pack" = c(NA_character_),
               "unpack" = c(NA_character_),
               "schema" = c(NA_character_))

  if (tool %in% c("Data Pack", "Data Pack Template")) {
    skip$pack <-
      switch(as.character(cop_year),
             "2023" = c("Home", "Spectrum", "Year 2"),
             "2024" = c("Home", "Spectrum", "Year 2"),
             NA_character_)

    skip$unpack <-
      switch(as.character(cop_year),
             "2023" = c("Home", "Spectrum", "KP Validation"),
             "2024" = c("Home", "Spectrum", "KP Validation"),
             NA_character_)

    skip$schema <- skip$pack[skip$pack %in% skip$unpack]

  } else if (tool %in% c("OPU Data Pack Template", "OPU Data Pack", "PSNUxIM", "PSNUxIM Template") &&
             cop_year %in% c(2023, 2024)) {
    skip$pack <- c("Home")
    skip$unpack <- c("Home")
    skip$schema <- c("Home")
  }

  return(skip)
}

#' @export
#' @title Tool to assist with formatting the header rows
#'
#' @param tool "Data Pack", "Data Pack Template",""OPU Data Pack".
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#'
#' @return Header row
#'
headerRow <- function(tool, cop_year) {

  # Check/Fill in parameters ####
  params <- check_params(cop_year = cop_year,
                         tool = tool)

  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }

  rm(params, p)

  #Currently all tools use row 14 as the header.
  if (cop_year %in% c(2023, 2024)) {
    if (tool %in% datapackrSupports()$tools) {
      return(14)
    } else {
      stop("That tool type is not supported for that cop_year.")
      }
  } else {
    stop("That cop_year is not currently supported.")
  }
}

#' @export
#' @title Pick correct schema
#'
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#' @param tool Either Data Pack or OPU Data Pack
#'
#' @return Schema file for given cop_year and tool type
#'
pick_schema <- function(cop_year, tool) {

  # Collect parameters
  tool <- tool %missing% NULL
  cop_year <- cop_year %missing% NULL

  tool_provided <- !is.null(tool)
  cop_year_provided <- !is.null(cop_year)

  if (!tool_provided || !cop_year_provided) {
    interactive_print("Attempted to deduce schema.")
  }

  cop_year %<>% check_cop_year()
  invisible(utils::capture.output(tool %<>% check_tool(tool = ., cop_year = cop_year)))

  if (tool %in% c("OPU Data Pack", "OPU Data Pack Template")) {
    schema <- switch(as.character(cop_year),
                     "2023" = cop23_psnuxim_schema,
                     stop("OPU Data Pack schema not available for the COP year provided."))

  } else if (tool %in% c("Data Pack", "Data Pack Template")) {
    schema <- switch(as.character(cop_year),
                     "2023" =  cop23_data_pack_schema,
                     "2024" =  cop24_data_pack_schema,
                     stop("Data Pack schema not available for the COP year provided."))

  } else if (tool %in% c("PSNUxIM", "PSNUxIM Template")) {
    schema <- switch(as.character(cop_year),
                     "2023" =  cop23_psnuxim_schema,
                     "2024" =  cop24_psnuxim_schema,
                     stop("PSNUxIM schema not available for the COP year provided."))
  } else {
    stop("No schema could be found for the combination of tool and COP year provided.")
  }

  schema
}

#' @export
#' @title Pick correct template filepath
#'
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#' @param tool Either Data Pack or OPU Data Pack
#'
#' @return Template filepath for given cop_year and tool.
#'
pick_template_path <- function(cop_year, tool) {

  cop_year <- cop_year %missing% NULL
  tool <- tool %missing% NULL

  params <- check_params(cop_year = cop_year,
                         tool = tool)

  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }

  template_filename <- NULL

  if (tool %in% c("OPU Data Pack", "OPU Data Pack Template")) {

    template_filename <- switch(as.character(cop_year),
      "2023" = "COP23_PSNUxIM_Template.xlsx",
      NULL)
  }

  if (tool %in% c("Data Pack", "Data Pack Template")) {
    template_filename <- switch(as.character(cop_year),
                                "2023" = "COP23_Data_Pack_Template.xlsx",
                                "2024" = "COP24_Data_Pack_Template.xlsx",
                                NULL)

  }

  if (tool %in% c("PSNUxIM", "PSNUxIM Template")) {
    template_filename <- switch(as.character(cop_year),
                                "2023" = "COP23_PSNUxIM_Template.xlsx",
                                "2024" = "COP24_PSNUxIM_Template.xlsx",
                                NULL)

  }

  if (is.null(template_filename)) {
    stop("Could not find any template for the provided parameters")
  }

  template_path <- system.file("extdata",
                               template_filename,
                               package = "datapackr",
                               mustWork = TRUE)

  template_path <- handshakeFile(path = template_path,
                                 tool = tool)

  template_path
}




#' @title Standardized package function parameter definitions
#'
#' @param model_data Data from DATIM needed to pack into a COP Data Pack.
#' @param model_data_path Local filepath to a Data Pack model data file.
#' @param snuxim_model_data Export from DATIM needed to allocate data across
#' mechanisms in the PSNUxIM tab.
#' @param snuxim_model_data_path Local filepath to an SNUxIM Model Data file.
#' @param undistributed_mer_data Data from the \code{d$datim$UndistributedMER}
#' dataset that can be provided while generating an OPU tool such that the
#' targets to be distributed will be sourced from this file.
#' @param SNUxIM SNUxIM dataset extract from unPackSNUxIM
#' @param MER MER dataset extract from unPackSheets
#' @param PSNUxIM_combos Dataset extract from unPackSNUxIM that shows data
#' missing from the PSNUxIM tab.
#' @param datapack_name Name you would like associated with this Data Pack.
#' (Example: "Western Hemisphere", or "Caribbean Region", or "Kenya".)
#' @param country_uids Unique IDs for countries to include in the Data Pack.
#' For full list of these IDs, see \code{datapackr::valid_OrgUnits}.
#' @param template_path Local filepath to Data Pack template Excel (XLSX) file.
#' This file MUST NOT have any data validation formats present. If left
#' \code{NULL}, will select the default based on \code{cop_year} and \code{tool}.
#' @param submission_path Local path to the file to import.
#' @param cached_mechs_path Local file path to an RDS file containing
#' a cached copy of the mechanisms SQL view.
#' @param cop_year COP Year to use for tailoring functions. Remember,
#' FY22 targets = COP21.
#' @param output_folder Local folder where you would like your Data Pack to be
#' saved upon export.
#' @param results_archive If TRUE, will export compiled results of all tests and
#' processes to output_folder.
#' @param d2_session DHIS2 Session id. R6 datimutils object which handles
#' authentication with DATIM.
#' @param d Datapackr sidecar object
#' @param datastreams Data stream or streams. One or more of \code{mer_targets},
#' \code{mer_results}, \code{subnat_targets}, \code{subnat_results}, or
#' \code{impatt}. If not specified, then all data streams
#' are returned.
#' @param schema Which datapackr schema to use in guiding this function. If left
#' \code{NULL} will select the default based on \code{cop_year} and \code{tool}.
#' @param skip Character vector of Sheet Names to label for skipping in schema.
#' @param wb Openxlsx workbook object.
#' @param PSNUs Dataframe of PSNUs to use in this function, containing at least
#' \code{psnu_uid}.
#' @param psnus Dataframe of PSNUs to use in this function, containing at least
#' \code{psnu_uid}.
#' @param tool Type of tool this function will create or interact with. Either
#' \code{OPU Data Pack} or \code{Data Pack}
#' @param season Either \code{COP} or \code{OPU}.
#' @param draft_memo Boolean indicating whether the memo being written is a
#' draft or final memo.
#' @param memo_type memo_type One of the following:
#' datapack: Create the memo based on the data in the datapack or OPU datapack
#' datim: Create the memo based on data currently in DATIM
#' comparison: Create a comparison memo with data from both DATIM and datapack
#' @param memo_doc \code{Officer} document object containing
#' the target memo tables.
#' @param memo_structure Structure of the memo d$memo$structure
#' @param memoStructure Structure of the memo d$memo$structure
#' @param source_type Indicates whether the data for a COP Approval Memo table
#' should come from the Data Pack or from DATIM. Values can be either
#' \code{datapack} or \code{datim}.
#' @param prios Data frame of prioritization levels.
#' @param include_no_prio If TRUE, include \code{"No Prioritiation"}
#' as a column in the output.
#' @param remove_empty_columns Should empty columns be removed from memos?
#' @param spectrum_data Spectrum output, as a dataframe.
#' @param sheet String. Name of sheet/tab within tool.
#' @param sheets Character vector. Names of sheets/tabs within tool.
#' @param expand_formulas Write all formulas on right side of PSNUxIM tab, not
#' just the first row.
#' @param pzns A object containing prioiritization information by organisation unit
#'  to be added to the DataPack object.
#' @param mer_data If prior MER data (for instance from DATIM), is passed to this
#' parameter, it will be used. Otherwise, data from the existing set of tools
#' will be used.
#' @param ... Additional arguments to pass.
#'
#' @family parameter-helpers
#'
#' @return list of all paramaters of this constructor function
datapackr_params <- function(model_data,
                             model_data_path,
                             snuxim_model_data,
                             snuxim_model_data_path,
                             undistributed_mer_data,
                             SNUxIM,
                             MER,
                             PSNUxIM_combos,
                             datapack_name,
                             country_uids,
                             template_path,
                             submission_path,
                             cached_mechs_path,
                             cop_year,
                             output_folder,
                             results_archive,
                             d2_session,
                             d,
                             datastreams,
                             schema,
                             skip,
                             wb,
                             PSNUs,
                             psnus,
                             tool,
                             season,
                             draft_memo,
                             memo_type,
                             memo_doc,
                             memo_structure,
                             memoStructure,
                             source_type,
                             prios,
                             include_no_prio,
                             remove_empty_columns,
                             spectrum_data,
                             sheet,
                             sheets,
                             expand_formulas,
                             pzns,
                             mer_data,
                             ...) {

  # This function should return something
  #Return its own argument names
  #rlang::fn_fmls_names(fn = datapackr_params)
  #or explicitly return
  #NULL
}
