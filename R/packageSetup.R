#:TODO Move these next three methods of this into the schema and make a function of the COP Year.
#This should NOT be hard coded here as it may and will change.
#' @export
#' @title Returns current COP Year
#'
#' @return Current COP Year. (e.g., for COP19, returns 2019)
#'
getCurrentCOPYear <- function() { 2021 }


#' @export
#' @title Location of name of the tool on Home tab.
#'
#' @return Cell reference where the name of the tool is located.
#'
toolName_homeCell<-function() {"B10"}

#' @export
#' @title Location of Country UIDs on Home tab.
#'
#' @return Cell reference where the name of the DataPack is located.
#'
countryUIDs_homeCell <- function() { "B25" }


#' @export
#' @title Location of Name of the DataPack on the Home tab
#'
#' @return Cell reference where the name of the datapack can be found.
#'
dataPackName_homeCell <- function() { "B20" }


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
      skip = c("Home", "Instructions", "Summary", "Spectrum", "Spectrum IDs")
    } else if (cop_year == 2021) {
      skip = c("Home", "Summary", "Spectrum")
    }
  }
  else if (tool == "OPU Data Pack Template" & cop_year %in% c(2020,2021)) {
    skip = c("Home")
  } else {skip = c(NA_character_)}

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

  if (cop_year %in% c(2020,2021)) {
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
pick_schema <- function(cop_year = getCurrentCOPYear(), tool = "Data Pack") {
  if (is.null(cop_year)) {cop_year = getCurrentCOPYear()}
  if (is.null(tool)) {tool = "Data Pack"}
  
  if (tool == "OPU Data Pack") {
    if (cop_year == 2020) {
      schema <- datapackr::cop20OPU_data_pack_schema
    } else if (cop_year == 2021) {
      schema <- datapackr::cop21OPU_data_pack_schema}  
  }
  
  if (tool == "Data Pack") {
    if (cop_year == 2020) {
      schema <- datapackr::cop20_data_pack_schema
    } else if (cop_year == 2021) {
      schema <- datapackr::cop21_data_pack_schema
    } else {schema <- datapackr::data_pack_schema}
  }
  
  return(schema)

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
pick_template_path <- function(cop_year = getCurrentCOPYear(), tool = "Data Pack") {
  if (is.null(cop_year)) {cop_year = getCurrentCOPYear()}
  if (is.null(tool)) {tool = "Data Pack"}
  
  if (tool == "OPU Data Pack") {
    if (cop_year == 2020) {
      template_filename <- "COP20_OPU_Data_Pack_Template.xlsx"
    } else if (cop_year == 2021) {
      template_filename <- "COP21_OPU_Data_Pack_Template.xlsx"}  
  }
  
  if (tool == "Data Pack") {
    if (cop_year == 2020) {
      template_filename <- "COP20_Data_Pack_Template_vFINAL.xlsx"
    } else if (cop_year == 2021) {
      template_filename <- "COP21_Data_Pack_Template.xlsx"}
  }
  
  template_path <- system.file("extdata",
                               template_filename,
                               package = "datapackr",
                               mustWork = TRUE)
  
  template_path <- handshakeFile(path = template_path,
                                 tool = tool)
  
  return(template_path)
  
}



#' @export
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
#'
datapackr_params <- function(model_data,
                             snuxim_model_data,
                             SNUxIM,
                             MER,
                             PSNUxIM_combos,
                             datapack_name,
                             country_uids,
                             template_path,
                             cop_year,
                             output_folder,
                             results_archive,
                             d2_session,
                             d,
                             schema,
                             wb,
                             PSNUs,
                             tool,
                             season) {
  
}

#' @export
#' @title Standardized package function parameter definitions
#' 
#' @inheritParams datapackr_params
#'
#' @return params List of valid parameters to use in functions.
#'
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
                         d2_session = dynGet("d2_default_session",
                                             inherits = TRUE)) {
  
  params <- list()
  
  # Check Country UIDs ####
  check_country_uids <- function(country_uids = NULL, force = TRUE) {
    if (is.null(country_uids)) {
      if (force) {
        stop("Must supply country_uids.")
      } else {
        country_uids <- valid_PSNUs$country_uid %>%
          unique()        
      }
    } else {
      if (any(!country_uids %in% valid_PSNUs$country_uid)) {
        invalid_country_uids <- country_uids[!country_uids %in% valid_PSNUs$country_uid]
        interactive_print(
          paste0("The following supplied country_uids appear to be invalid and will be removed: ",
                 paste(invalid_country_uids, collapse = ", "))
        )
        country_uids <- country_uids[country_uids %in% valid_PSNUs$country_uid]
      }
    }
    return(country_uids)
  }
  
  if (!missing(country_uids)) {
    params$country_uids <- check_country_uids(country_uids)
  }
  
  # Check PSNUs ####
  check_PSNUs <- function(PSNUs = NULL, country_uids = NULL) {
    # If no country_uids provided, return PSNUs across all country_uids.
    country_uids <- check_country_uids(country_uids, force = FALSE)
    
    # FILL
    if(is.null(PSNUs)) { # PSNUs is NULL
      PSNUs <- datapackr::valid_PSNUs %>%
        dplyr::filter(., country_uid %in% country_uids) %>%
        add_dp_psnu(.) %>%
        dplyr::arrange(dp_psnu) %>%
        dplyr::select(PSNU = dp_psnu, psnu_uid)
    } else { # PSNUs is not NULL
      # CLEAN
      if (any(!PSNUs$psnu_uid %in% valid_PSNUs$psnu_uid)) {
        # NOTE: May consider instead including both the name and uid for ease of resolution by user
        invalid_PSNUs <- PSNUs$psnu_uid[!PSNUs$psnu_uid %in% valid_PSNUs$psnu_uid]
        interactive_print(
          paste0("The following supplied PSNUs appear to be invalid and will be removed: ",
                 paste(invalid_PSNUs, collapse = ", "))
        )
        PSNUs <- PSNUs %>%
          dplyr::filter(psnu_uid %in% valid_PSNUs$psnu_uid)
      }
    }
    return(PSNUs)
  }
  
  if (!missing(PSNUs)) {
    params$PSNUs <- check_PSNUs(PSNUs, country_uids)
  }

  # Check cop_year ####
  check_cop_year <- function(cop_year = getCurrentCOPYear()) {
    if (is.null(cop_year)) {cop_year = getCurrentCOPYear()}
    if (!cop_year %in% c(2020, 2021)) {
      stop("Sorry, datapackr only supports COP20 and COP21 Data Packs.")
    }
    return(cop_year)
  }
  
  if (!missing(cop_year)) {
    params$cop_year <- check_cop_year(cop_year)
  }

  # Check Tool ####
  check_tool <- function(tool = NULL) {
    if (is.null(tool)) {
      tool = "Data Pack"
    } else {
      if (!tool %in% c("Data Pack", "OPU Data Pack")) {
        stop("Cannot support any tools other than `Data Pack` or `OPU Data Pack`")
      }
    }
    return(tool)
  }
  
  if (!missing(tool)) {
    params$tool <- check_tool(tool)
  }
  
  # Check season ####
  check_season <- function(season = NULL, tool = NULL) {
    tool <- check_tool(tool)
    if (is.null(season)) {
      if (tool == "OPU Data Pack") {
        season = "OPU"
      } else {season = "COP"}
    } else {
      if (!season %in% c("COP", "OPU")) {
        stop("Cannot support any tools other than `COP` or `OPU`")
      }
    }
    return(season)
  }
  
  if (!missing(season)) {
    if (missing(tool)) {tool <- check_tool()}
    params$season <- check_season(season, tool = tool)
  }
  
  # Check schema ####
  check_schema <- function(schema = NULL, cop_year = NULL, tool = NULL) {
    cop_year <- check_cop_year(cop_year)
    tool <- check_tool(tool)
    
    if (is.null(schema)) {
      schema <- pick_schema(cop_year, tool)
    } else {
      true_schema <- pick_schema(cop_year, tool)
      if (!identical(schema, true_schema)) {
        interactive_print("Schema provided does not match archived schema. Are you using a custom schema on purpose?")
      }
    }
    return(schema)
  }
  
  if (!missing(schema)) {
    params$schema <- check_schema(schema, cop_year, tool)
  }
  
  # Check datapack_name ####
  check_datapack_name <- function(datapack_name = NULL, country_uids = NULL) {
    if (is.null(datapack_name)) {
      if (is.null(country_uids)) {
        datapack_name <- "Global"
      } else {
        country_uids <- check_country_uids(country_uids)
        datapack_name <- valid_PSNUs %>%
          dplyr::filter(country_uid %in% country_uids) %>%
          dplyr::pull(country_name) %>%
          unique() %>%
          sort() %>%
          paste(collapse = ", ")
      }
    }
    return(datapack_name)
  }
  
  if (!missing(datapack_name)) {
    params$datapack_name <- check_datapack_name(datapack_name, country_uids)
  }

  # Check template path ####
  check_template_path <- function(template_path = NULL,
                                  cop_year = NULL,
                                  tool = NULL) {
    cop_year = check_cop_year(cop_year)
    tool = check_tool(tool)
    
    if (is.null(template_path)) {
      template_path <- pick_template_path(cop_year, tool)
    } else {
      template_path %<>%
        handshakeFile(tool)
    }
    
    interactive_print("Checking template against schema and DATIM...")
    schema <- pick_schema(cop_year, tool)
    
    input_tool <- paste0(tool, " Template")
    schema_check <-
      unPackSchema_datapack(
        filepath = template_path,
        skip = skip_tabs(tool = input_tool, cop_year = cop_year),
        tool = input_tool,
        cop_year = cop_year,
        d2_session = d2_session)
    
    if (!identical(schema, schema_check)) {
      stop("Template provided does not match archived schema.")
    }
    return(template_path)
  }
  
  if (!missing(template_path)) {
    params$template_path <- check_template_path(template_path, cop_year, tool)
  }

  
  # Check wb ####
  check_wb <- function(wb = NULL,
                       country_uids = NULL,
                       cop_year = NULL,
                       tool = NULL,
                       datapack_name = NULL,
                       template_path = NULL,
                       d2_session) {
    country_uids <- check_country_uids(country_uids)
    cop_year = check_cop_year(cop_year)
    tool = check_tool(tool)
    datapack_name = check_datapack_name(datapack_name, country_uids)
    template_path = check_template_path(template_path, cop_year, tool)
    
      if (is.null(wb)) {
        wb <- createWorkbook(datapack_name = datapack_name,
                             country_uids = country_uids,
                             template_path = template_path,
                             cop_year = cop_year,
                             tool = tool,
                             d2_session = d2_session)
      }
    return(wb)
  }
  
  if (!missing(wb)) {
    params$wb <- check_wb(wb = wb,
                          country_uids = country_uids,
                          cop_year = cop_year,
                          tool = tool,
                          datapack_name = datapack_name,
                          template_path = template_path,
                          d2_session = d2_session)
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
  check_results_archive <- function(results_archive = FALSE) {
    
    if (!isTRUE(results_archive) & !isFALSE(results_archive)) {
      stop("results_archive must be either TRUE or FALSE.")
    }
    return(results_archive)
  }
  
  if (!missing(results_archive)) {
    params$results_archive <- check_results_archive(results_archive)
  }
  
  
  return(params)
}
