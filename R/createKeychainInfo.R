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
  tool_metadata <-
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
      tool = stringr::str_extract(home_cell, "OPU Data Pack|Data Pack"))

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
    stop("The file submitted does not seem to match the tool type you've specified.")
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
          d$info$tool == "OPU Data Pack", "PSNUxIM", "Prioritization"),
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
                      tool = d$info$tool)

  d$info$country_uids <- d$info$country_uids %||% submitted_country_uids

  if (d$info$country_uids != submitted_country_uids) {
    stop("The file submitted does not seem to match the country_uids you've specified.")
  }

  d$info$country_uids %<>% check_country_uids()

  # datapack_name ----
  d$info$datapack_name <-
    datapackr::unPackDataPackName(
      submission_path = d$keychain$submission_path,
      tool = d$info$tool)

  d$info$datapack_name %<>% checkDataPackName(country_uids = d$info$country_uids)

  # TEST to make sure tool type matches what we see in the submitted file's structure ####
  # TODO: Improve to use checkColStructure
  # tab_names_expected <- unique(d$info$schema$sheet_name)
  # tab_names_received <- readxl::excel_sheets(d$keychain$submission_path)
  #
  # if (any(tab_names_expected != tab_names_received)) {
  #   warning("The sheets included in your submitted file don't seem to match the file type specified.")
  # }

  # Generate sane_name for tool
  d$info$sane_name <- getSaneName(d$info$datapack_name)

  ## Determine additional Organisation Unit information and save
  ## under `d$info$operating_unit` for use in validating mechanisms
  d$info$operating_unit <- getOUFromCountryUIDs(d$info$country_uids)

  # submission_path ----
  d$keychain$submission_path <- handshakeFile(path = d$keychain$submission_path,
                                              tool = d$info$tool)

  # Placeholders ####
  if (d$info$tool %in% c("Data Pack", "Data Pack Template", "OPU Data Pack", "OPU Data Pack Template")
      && d$info$cop_year %in% c("2021", "2022")) {
    d$info$needs_psnuxim <- FALSE
    d$info$newSNUxIM <- FALSE
    d$info$has_psnuxim <- FALSE
    d$info$missing_psnuxim_combos <- FALSE
    d$info$missing_DSNUs <- FALSE
    d$info$unallocatedIMs <- FALSE
  }

  return(d)

}
