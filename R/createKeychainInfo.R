
unPackHomeTabMetadata <- function(submission_path)  {
  readxl::read_excel(
    path = submission_path,
    sheet = "Home",
    range = datapackr::toolName_homeCell(),
    col_names = c("home_cell"),
    col_types = "text",
    trim_ws = TRUE) %>%
    dplyr::mutate(
      cop_year = as.numeric(stringr::str_extract(home_cell, "(?<=(FY|COP))\\d{2}")),
      cop_year = dplyr::case_when(
        stringr::str_detect(home_cell, "^FY\\d{2}") ~ cop_year + 2000 - 1,
        TRUE ~ cop_year + 2000),
      tool = stringr::str_extract(home_cell, "OPU Data Pack|Data Pack|Target Setting Tool|PSNUxIM"),
      tool = stringr::str_replace(tool, "Target Setting Tool", "Data Pack"))
}


#' @export
#' @title Create Keychain Info for use in DataPack object.
#'
#' @description
#' Creates Keychain info needed for use across many `datapackr` functions.
#'
#' @md
#'
#' @inheritParams datapackr_params
#'
#' @return DataPack object
#'
createKeychainInfo <- function(submission_path = NULL,
                               tool = NULL,
                               country_uids = NULL,
                               cop_year = NULL,
                               d2_session = NULL) {

  # d ----
  d <- list(
    keychain = list(
      submission_path = submission_path),
    info = list(
      tool = tool,
      country_uids = country_uids,
      cop_year = cop_year)
  )

  # Pulls username if `d2_session` object provided
  d$info$source_user <- switch(!is.null(d2_session),
                               d2_session$me$userCredentials$username,
                               NULL)

  # Messages ----
  d$info$messages <- MessageQueue()
  d$info$has_error <- FALSE

  # If path is NULL or has issues, prompt user to select file from window. ####
  if (!canReadFile(d$keychain$submission_path)) {

    if (interactive()) {
      interactive_print("Please choose a file.")
      d$keychain$submission_path <- file.choose()
    }

    if (!canReadFile(d$keychain$submission_path)) {
      stop("File could not be read!")
      }
  }

  # Home Tab metadata ----
  tool_metadata <- unPackHomeTabMetadata(d$keychain$submission_path)

  # Accommodate COP23 Name Change ----
  tool_metadata$tool <- stringr::str_replace(tool_metadata$tool,
                                             "Target Setting Tool",
                                             "Data Pack")

  #OPU Datapacks are really just PSNUxIM Tools
  tool_metadata$tool <- stringr::str_replace(tool_metadata$tool,
                                             "OPU Data Pack",
                                             "PSNUxIM")

  # Is this even a DataPack tool? ----
  if (!tool_metadata$cop_year[1] %in% supportedCOPYears()
        || !tool_metadata$tool[1] %in% supportedTools()) {
    stop(paste0("Based on a quick scan, the file submitted does not appear to",
                " be a Data Pack."))
  }

  # cop_year ----
  d$info$cop_year <- d$info$cop_year %||% tool_metadata$cop_year

  if (d$info$cop_year != tool_metadata$cop_year) {
    stop("The file submitted does not seem to match the cop_year you've specified.")
  }

  d$info$cop_year %<>% check_cop_year()

  # tool ####
  d$info$tool <- d$info$tool %||% tool_metadata$tool

  if (d$info$tool != tool_metadata$tool) {
    #Warn and try and proceed
    warning("The file submitted does not seem to match the tool type you've specified.")
  }

  d$info$tool %<>% check_tool()

  # schema ####
  d$info$schema <- check_schema(cop_year = d$info$cop_year, tool = d$info$tool)

  # Template? ####
  header_row <- headerRow(tool = d$info$tool,
                          cop_year = d$info$cop_year)

  check_if_template <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet =
        dplyr::if_else(
          d$info$tool %in% c("OPU Data Pack", "PSNUxIM"), "PSNUxIM", "Prioritization"),
      range = readxl::cell_limits(c(header_row, 1), c(NA, 10)),
      col_types = "text",
      .name_repair = "minimal") %>%
    dplyr::select(PSNU)

  is_template <- NROW(check_if_template$PSNU) == 0 | all(is.na(check_if_template$PSNU))

  if (is_template) {
    d$info$tool <- paste0(d$info$tool, " Template")
  }

  # country_uids ----
  submitted_country_uids <-
    unPackCountryUIDs(submission_path = d$keychain$submission_path,
                      tool = d$info$tool,
                      cop_year = d$info$cop_year)

  d$info$country_uids <- d$info$country_uids %||% submitted_country_uids

  if (!setequal(d$info$country_uids, submitted_country_uids)) {
    stop("The file submitted does not seem to match the country_uids you've specified.")
  }

  d$info$country_uids %<>% check_country_uids(cop_year = d$info$cop_year)

  # datapack_name ----
  d$info$datapack_name <-
    unPackDataPackName(
      submission_path = d$keychain$submission_path,
      tool = d$info$tool)

  d$info$datapack_name %<>% checkDataPackName(country_uids = d$info$country_uids,
                                              cop_year = d$info$cop_year)

  # Generate sane_name for tool
  d$info$sane_name <- getSaneName(d$info$datapack_name)

  ## Determine additional Organisation Unit information and save
  ## under `d$info$operating_unit` for use in validating mechanisms
  d$info$operating_unit <- getOUFromCountryUIDs(d$info$country_uids, d$info$cop_year)

  # submission_path ----
  d$keychain$submission_path <- handshakeFile(path = d$keychain$submission_path,
                                              tool = d$info$tool)

  # Placeholders ####
  d$info$needs_psnuxim <- FALSE
  d$info$newSNUxIM <- FALSE
  d$info$has_psnuxim <- FALSE
  d$info$missing_psnuxim_combos <- FALSE
  d$info$missing_DSNUs <- FALSE
  d$info$unallocatedIMs <- FALSE

  return(d)

}
