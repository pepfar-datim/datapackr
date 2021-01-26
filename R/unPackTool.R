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

  # If path is NULL or has issues, prompt user to select file from window.
  if (!canReadFile(submission_path)) {
    
    if (interactive()) {
      interactive_print("Please choose a file.")
      submission_path <- file.choose()
    }
    
    if (!canReadFile(submission_path)) {stop("File could not be read!")}
  }
  
  # Attempt to bootstrap the tool type and COP year if it is not explicitly provided
  tool_name_type <- 
    readxl::read_excel(
      path = submission_path,
      sheet = "Home",
      range = datapackr::toolName_homeCell(),
      col_names = c("type"),
      col_types = "text",
      trim_ws = TRUE) %>%
    tidyr::separate(
      col = type,
      into = c("cop_year", "type"),
      sep = " ",
      remove = TRUE,
      convert = TRUE,
      extra = "merge",
      fill = "warn") %>%
    dplyr::mutate(
      cop_year = stringr::str_replace(cop_year, "COP", "20"),
      cop_year = as.numeric(cop_year))
  
  is_template <-
    readxl::read_excel(
      path = submission_path,
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
  
  if (is.null(cop_year)) {
    cop_year <- tool_name_type$cop_year
  }
  
  # TEST to make sure tool type matches what we see in the submitted file's structure
  if (is.null(tool)) {
    tool <- tool_name_type$type
  } else if (tool != tool_name_type$type) {
    stop("The file submitted does not seem to match the type you've specified.")
  }
  
  if (tool %in% c("Data Pack", "Data Pack Template")) {
    if (cop_year == 2021) {
      schema <- datapackr::cop21_data_pack_schema
    } else if (cop_year == 2020) {
      schema <- datapackr::cop20_data_pack_schema
    } else if (cop_year == 2019) {
      schema <- datapackr::data_pack_schema
    } else {stop(paste0("Unable to process Data Packs from COP ", cop_year))}
  } else if (tool %in% c("OPU Data Pack", "OPU Data Pack Template")) {
    if (cop_year == 2020) {
      schema <- datapackr::cop20OPU_data_pack_schema
    } else {stop(paste0("Unable to process OPU Data Packs from COP ", cop_year))}
  } else {stop("Unable to process that type of Data Pack.")}
  
  tab_names_expected <- unique(schema$sheet_name)
  tab_names_received <- readxl::excel_sheets(submission_path)
  
  if (any(tab_names_expected != tab_names_received)) {
    warning("The sheets included in your submitted file don't seem to match the file type specified.")
  }
  
  # Grab datapack_name from Home Page
  datapack_name <- unPackDataPackName(
    submission_path = submission_path,
    tool = tool)  
  
  # Determine country uids ####
  if (is.null(country_uids)) {
    country_uids <- 
      unPackCountryUIDs(submission_path = submission_path,
                        tool = tool)
  }
  
  # Check the submission file exists and prompt for user input if not
  submission_path <- handshakeFile(path = submission_path,
                                   tool = tool)
  
  # Create data sidecar for use across remainder of program
  d <- list(
    keychain = list(
      submission_path = submission_path
    ),
    info = list(
      datapack_name = datapack_name,
      tool = tool,
      country_uids = country_uids,
      cop_year = cop_year,
      schema = schema)
    )

  # Start running log of all warning and information messages
  d$info$warning_msg <- NULL
  d$info$has_error <- FALSE
  if (d$info$tool %in% c("Data Pack", "Data Pack Template") & d$info$cop_year %in% c("2020", "2021")) {
    d$info$newSNUxIM <- FALSE
    d$info$has_psnuxim <- FALSE
    d$info$missing_psnuxim_combos <- FALSE
    d$info$missing_DSNUs <- FALSE
  }

  return(d)

}

#' @export
#' @title Unpack a submitted tool
#'
#' @description
#' Processes a submitted Data Pack by identifying integrity issues, checking
#' data against DATIM validations, and extracting data.
#'
#' @param submission_path Local path to the file to import.
#' @param tool What type of tool is the submission file? Default is "Data Pack".
#' @param country_uids List of 11 digit alphanumeric DATIM codes representing
#' countries. If not provided, will check file for these codes. If not in file,
#' will flag error.
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#' @param d2_session DHIS2 Session id
#'
#' @details
#' Executes the following operations in relation to a submitted tool
#' \enumerate{
#'     \item Performs integrity checks on file structure;
#' }
#'
unPackTool <- function(submission_path = NULL,
                       tool = NULL,
                       country_uids = NULL,
                       cop_year = NULL,
                       d2_session = dynGet("d2_default_session",
                                           inherits = TRUE)) {
  d <- createKeychainInfo(submission_path,
                     tool,
                     country_uids,
                     cop_year)

  # unPack file based on type
  if (d$info$tool == "Data Pack") {
    d <- unPackDataPack(d,
                        d2_session = d2_session)
  } else if (d$info$tool == "OPU Data Pack") {
    d <- unPackOPUDataPack(d,
                           d2_session = d2_session)
  } else {stop("Selected tool not currently supported.")}

  # If warnings, show all grouped by sheet and issue
  if (!is.null(d$info$warning_msg) & interactive()) {
    options(warning.length = 8170)

    messages <-
      paste(
        paste(
          seq_along(d$info$warning_msg),
          ": " , d$info$warning_msg
          #stringr::str_squish(gsub("\n", "", d$info$warning_msg))
        ),
        sep = "",
        collapse = "\r\n")

    key = paste0(
      "*********************\r\n",
      "KEY:\r\n",
      "- WARNING!: Problematic, but doesn't stop us from processing your tool. May waive with approval from PPM and DUIT.\r\n",
      "- ERROR!: You MUST address these issues and resubmit your tool.\r\n",
      "*********************\r\n\r\n")

    cat(crayon::red(crayon::bold("VALIDATION ISSUES: \r\n\r\n")))
    cat(crayon::red(key))
    cat(crayon::red(messages))
  }

  return(d)
}
