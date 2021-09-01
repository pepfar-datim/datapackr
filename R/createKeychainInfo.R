#' @export
#' @title Create Keychain Info for use in datapackr sidecar.
#'
#' @description
#' Creates Keychain info needed for use across most datapackr unPack functions.
#'
#' @param submission_path Local path to the file to import.
#' @param tool What type of tool is the submission file? Default is "Data Pack".
#' @param country_uids List of 11 digit alphanumeric DATIM codes representing
#' countries. If not provided, will check file for these codes. If not in file,
#' will flag error.
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#'
createKeychainInfo <- function(submission_path = NULL,
                               tool = NULL,
                               country_uids = NULL,
                               cop_year = NULL) {
  
  # Create data sidecar for use across remainder of program
  d <- list(
    keychain = list(
      submission_path = submission_path),
    info = list(
      tool = tool,
      country_uids = country_uids,
      cop_year = cop_year)
  )
  
  # Start running log of all warning and information messages
  d$info$warning_msg <- Messages$new()
  d$info$has_error <- FALSE
  
  # If path is NULL or has issues, prompt user to select file from window. ####
  if (!canReadFile(d$keychain$submission_path)) {
    
    if (interactive()) {
      interactive_print("Please choose a file.")
      d$keychain$submission_path <- file.choose()
    }
    
    if (!canReadFile(d$keychain$submission_path)) {stop("File could not be read!")}
  }
  
  # Bootstrap tool type, if not provided ####
  tool_name_type <- 
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = "Home",
      range = datapackr::toolName_homeCell(),
      col_names = c("home_cell"),
      col_types = "text",
      trim_ws = TRUE) %>%
    dplyr::mutate(
      cop_year = stringr::str_extract(home_cell, "COP\\d{2}"),
      cop_year = as.numeric(stringr::str_replace(cop_year, "COP", "20")),
      type = stringr::str_extract(home_cell, "OPU Data Pack|Data Pack")
    )
  
  if (!tool_name_type$cop_year[1] %in% c(2018:2030)
        | !tool_name_type$type[1] %in% c("Data Pack","OPU Data Pack")) {
    stop("Please correct cell B10 on the Home tab. This should read 'COP21 Data Pack', or similar")
  }
  
  # Determine if template, and if so, label type as template ####
  is_template <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = dplyr::if_else(tool_name_type$type == "OPU Data Pack", "PSNUxIM", "Prioritization"),
      range = readxl::cell_limits(
        c(headerRow(tool = tool_name_type$type, cop_year = tool_name_type$cop_year), 1),
        c(NA, 10)),
      col_types = "text",
      .name_repair = "minimal") %>%
    dplyr::select(PSNU)
  
  if (NROW(is_template$PSNU) == 0) {
    is_template = TRUE
  } else if (all(is.na(is_template$PSNU))) {
    is_template = TRUE
  } else {is_template = FALSE}
  
  if (is_template) {
    tool_name_type %<>% dplyr::mutate(type = paste0(type, " Template"))
  }
  
  # Assign COP Year based on Home tab ####
  if (is.null(d$info$cop_year)) {
    d$info$cop_year <- tool_name_type$cop_year
    
    if (is.null(d$info$cop_year)) {
      stop("The file submitted seems to no longer indicate applicable COP Year on the Home tab.")
    }
  }
  
  # Assign tool type based on Home tab ####
  if (is.null(d$info$tool)) {
    d$info$tool <- tool_name_type$type
  } else if (d$info$tool != tool_name_type$type) {
    stop("The file submitted does not seem to match the type you've specified.")
  }
  
  # Assign schema based on tool type ####
  if (d$info$tool %in% c("Data Pack", "Data Pack Template")) {
    if (d$info$cop_year == 2021) {
      d$info$schema <- datapackr::cop21_data_pack_schema
    } else if (d$info$cop_year == 2020) {
      d$info$schema <- datapackr::cop20_data_pack_schema
    } else if (d$info$cop_year == 2019) {
      d$info$schema <- datapackr::data_pack_schema
    } else {stop(paste0("Unable to process Data Packs from COP ", d$info$cop_year))}
  } else if (d$info$tool %in% c("OPU Data Pack", "OPU Data Pack Template")) {
    if (d$info$cop_year == 2020) {
      d$info$schema <- datapackr::cop20OPU_data_pack_schema
    } else if (d$info$cop_year == 2021) {
      d$info$schema <- cop21OPU_data_pack_schema
    } else {stop(paste0("Unable to process OPU Data Packs from COP ", d$info$cop_year))}
  } else {stop("Unable to process that type of Data Pack.")}
  
  # TEST to make sure tool type matches what we see in the submitted file's structure ####
  # TODO: Improve to use checkColStructure
  # tab_names_expected <- unique(d$info$schema$sheet_name)
  # tab_names_received <- readxl::excel_sheets(d$keychain$submission_path)
  # 
  # if (any(tab_names_expected != tab_names_received)) {
  #   warning("The sheets included in your submitted file don't seem to match the file type specified.")
  # }
  
  # Grab datapack_name from Home Page
  d$info$datapack_name <-
    datapackr::unPackDataPackName(
      submission_path = d$keychain$submission_path,
      tool = d$info$tool)
  
  if (!d$info$datapack_name %in% unique(c(datapackr::valid_PSNUs$country_name,
                                          "Latin America Region",
                                          "Caribbean Region",
                                          datapackr::valid_PSNUs$ou))) {
    #TODO: This seems to do nothing. Should it?
  }
  
  # Determine country uids ####
  if (is.null(d$info$country_uids)) {
    d$info$country_uids <- 
      unPackCountryUIDs(submission_path = d$keychain$submission_path,
                        tool = d$info$tool)
  }
  
  # Check the submission file exists and prompt for user input if not
  d$keychain$submission_path <- handshakeFile(path = d$keychain$submission_path,
                                              tool = d$info$tool)
  
  # Add placeholders for info messages ####
  if (d$info$tool %in% c("Data Pack", "Data Pack Template", "OPU Data Pack", "OPU Data Pack Template")
      & d$info$cop_year %in% c("2020", "2021")) {
    d$info$needs_psnuxim <- FALSE
    d$info$newSNUxIM <- FALSE
    d$info$has_psnuxim <- FALSE
    d$info$missing_psnuxim_combos <- FALSE
    d$info$missing_DSNUs <- FALSE
  }
  
  return(d)
  
}
