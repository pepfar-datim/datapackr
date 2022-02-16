#:TODO Move these next three methods of this into the schema and make a function of the COP Year.
#This should NOT be hard coded here as it may and will change.
#' @export
#' @title Returns current COP Year
#'
#' @return Current COP Year. (e.g., for COP19, returns 2019)
#'
getCurrentCOPYear <- function() {
  2021
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
#' @return Character vector of tab names to skip.
#'
skip_tabs <- function(tool = "Data Pack", cop_year = getCurrentCOPYear()) {
  if (tool %in% c("Data Pack", "Data Pack Template")) {
    if (cop_year == 2020) {
      skip <- c("Home", "Instructions", "Summary", "Spectrum", "Spectrum IDs")
    } else if (cop_year %in% c(2021)) {
      skip <- c("Home", "Summary", "Spectrum")
    } else if (cop_year %in% c(2022)) {
      skip <- c("Home", "Spectrum")
    }
  }
  else if (tool == "OPU Data Pack Template" &
           cop_year %in% c(2020, 2021)) {
    skip <- c("Home")
  } else {
    skip <- c(NA_character_)
  }

  return(skip)
}

#' @export
#' @title Tool header rows
#'
#' @param tool "Data Pack", "Data Pack Template".
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#'
#' @return Header row
#'
headerRow <- function(tool, cop_year = getCurrentCOPYear()) {

  if (cop_year %in% c(2020, 2021, 2022)) {
    if (tool %in% c("Data Pack", "Data Pack Template", "OPU Data Pack Template", "OPU Data Pack")) {
      header_row <- 14
    } else stop("That tool type is not supported for that cop_year.")
  } else stop("That cop_year is not currently supported.")

  return(header_row)

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

  if (!tool_provided | !cop_year_provided) {
    interactive_print("Attempted to deduce schema.")
  }

  cop_year %<>% check_cop_year()
  invisible(capture.output(tool %<>% check_tool(tool = ., cop_year = cop_year)))

  if (tool == "OPU Data Pack") {
    if (cop_year == 2020) {
      schema <- datapackr::cop20OPU_data_pack_schema
    } else if (cop_year == 2021) {
      schema <- datapackr::cop21OPU_data_pack_schema
    } else {
      stop("OPU Data Pack schema not available for the COP year provided.")
    }
  } else if (tool == "Data Pack") {
    if (cop_year == 2020) {
      schema <- datapackr::cop20_data_pack_schema
    } else if (cop_year == 2021) {
      schema <- datapackr::cop21_data_pack_schema
    } else if (cop_year == 2022) {
      schema <- datapackr::cop22_data_pack_schema
    } else {
      stop("Data Pack schema not available for the COP year provided.")
    }
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

  if (tool == "OPU Data Pack") {
    if (cop_year == 2020) {
      template_filename <- "COP20_OPU_Data_Pack_Template.xlsx"
    } else if (cop_year == 2021) {
      template_filename <- "COP21_OPU_Data_Pack_Template.xlsx"
    }
  }

  if (tool == "Data Pack") {
    if (cop_year == 2020) {
      template_filename <- "COP20_Data_Pack_Template_vFINAL.xlsx"
    } else if (cop_year == 2021) {
      template_filename <- "COP21_Data_Pack_Template.xlsx"
    } else if (cop_year == 2022) {
      template_filename <- "COP22_Data_Pack_Template.xlsx"
    }
  }

  if (is.null(template_filename)) {
    stop("Could not find any template for the provided paramaters")
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
#' @param snuxim_model_data Export from DATIM needed to allocate data across
#' mechanisms in the PSNUxIM tab
#' @param SNUxIM SNUxIM dataset extract from unPackSNUxIM
#' @param MER MER dataset extract from unPackSheets
#' @param PSNUxIM_combos Dataset extract from unPackSNUxIM that shows data
#' missing from the PSNUxIM tab.
#' @param datapack_name Name you would like associated with this Data Pack.
#' (Example: "Western Hemisphere", or "Caribbean Region", or "Kenya".)
#' @param country_uids Unique IDs for countries to include in the Data Pack.
#' For full list of these IDs, see \code{datapackr::valid_PSNUs}.
#' @param template_path Local filepath to Data Pack template Excel (XLSX) file.
#' This file MUST NOT have any data validation formats present. If left
#' \code{NULL}, will select the default based on \code{cop_year} and \code{tool}.
#' @param submission_path Local path to the file to import.
#' @param cached_mechs_path Local file path to an RDS file containing
#' a cached copy of the mechanisms SQL view.
#' @param cop_year COP Year to use for tailoring functions. Remember,
#' FY22 targets = COP21.
#' @param output_folder Local folder where you would like your Data Pack to be
#' saved upon export. If left as \code{NULL}, will output to
#' \code{Working Directory}.
#' @param results_archive If TRUE, will export compiled results of all tests and
#' processes to output_folder.
#' @param d2_session DHIS2 Session id
#' @param d Datapackr sidecar object
#' @param schema Which datapackr schema to use in guiding this function. If left
#' \code{NULL} will select the default based on \code{cop_year} and \code{tool}.
#' @param wb Openxlsx workbook object.
#' @param PSNUs Dataframe of PSNUs to use in this function, containing at least
#' \code{psnu_uid}.
#' @param tool Type of tool this function will create or interact with. Either
#' \code{OPU Data Pack} or \code{Data Pack}
#' @param season Either \code{COP} or \code{OPU}.
#' @param ... Additional arguments to pass.
#'
#' @family parameter-helpers
#'
#' @return list of all paramaters of this constructor function
datapackr_params <- function(model_data,
                             snuxim_model_data,
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
                             schema,
                             wb,
                             PSNUs,
                             tool,
                             season,
                             ...) {

  # This function should return something
  #Return its own argument names
  #rlang::fn_fmls_names(fn = datapackr_params)
  #or explicitly return
  #NULL
}
