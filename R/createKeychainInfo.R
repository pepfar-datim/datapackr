#' @export
#' @title Create Keychain Info for use in datapackr sidecar.
#'
#' @description
#' Creates Keychain info needed for use across most datapackr unPack functions.
#'
#' @inheritParams datapackr_params
#' 
#' @return d datapackr sidecar object
#'
createKeychainInfo <- function(submission_path = NULL,
                               tool = NULL,
                               country_uids = NULL,
                               cop_year = NULL,
                               schema = NULL,
                               datapack_name = NULL) {
  
  # Create data sidecar for use across remainder of program ####
  d <- list(
    keychain = list(
      submission_path = submission_path),
    info = list(
      tool = tool,
      country_uids = country_uids,
      cop_year = cop_year,
      schema = NULL,
      datapack_name = datapack_name)
  )
  
  # Start running log of all warning and information messages ####
  d$info$warning_msg <- NULL
  d$info$has_error <- FALSE
  
  # submission_path: If NULL or has issues, prompt user. ####
  if (!canReadFile(d$keychain$submission_path)) {
    
    if (interactive()) {
      interactive_print("Please choose a file.")
      d$keychain$submission_path <- file.choose()
    }
    
    if (!canReadFile(d$keychain$submission_path)) {stop("File could not be read!")}
  }
  
  # Check the submission file exists and prompt for user input if not
  d$keychain$submission_path <- handshakeFile(path = d$keychain$submission_path,
                                              tool = d$info$tool)
  
  # tool: Bootstrap, if not provided ####
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
      tool = stringr::str_extract(home_cell, "OPU Data Pack|Data Pack|PSNUxIM Tool"),
      season = dplyr::case_when(
        tool == "OPU Data Pack" ~ "OPU",
        tool %in% c("Data Pack", "PSNUxIM Tool") ~ "COP",
        TRUE ~ NA_character_
      )
    ) %>%
    purrr::flatten()
  
  if (!tool_name_type$cop_year %in% c(2018:(getCurrentCOPYear()+1))
        | !tool_name_type$tool %in% c("Data Pack","OPU Data Pack", "PSNUxIM Tool")) {
    stop("Please correct cell B10 on the Home tab. This should read 'COP## Data Pack', 'COP## OPU Data Pack' or similar")
  }
  
  # Determine if template, and if so, label tool type as template
  is_template <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = dplyr::if_else(tool_name_type$tool == "Data Pack", "Prioritization", "PSNUxIM"),
      range = readxl::cell_limits(
        c(headerRow(
            tool = tool_name_type$tool,
            cop_year = tool_name_type$cop_year),
          1),
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
    tool_name_type$tool %<>% paste0(., " Template")
  }
  
  if (is.null(d$info$tool)) {
    d$info$tool <- tool_name_type$tool
  } else if (d$info$tool != tool_name_type$type) {
    stop("The file submitted does not seem to match the type of tool you've specified.")
  }
  
  d$info$tool %<>% check_params(tool = .) %>%
    magrittr::use_series(tool)
  
  # cop_year: Assign based on Home tab ####
  if (is.null(d$info$cop_year)) {
    d$info$cop_year <- tool_name_type$cop_year
    
    if (is.null(d$info$cop_year)) {
      stop("The file submitted seems to no longer indicate applicable COP Year on the Home tab.")
    }
    
  } else if (d$info$cop_year != tool_name_type$cop_year) {
    stop("The COP Year specified does not seem to match the Home tab of this tool.")}
    
  d$info$cop_year %>% check_params(cop_year = .) %>%
    magrittr::use_series(cop_year)
  
  # schema: Assign based on tool type ####
  d$info$schema %<>%
    check_params(schema = ., cop_year = d$info$cop_year, tool = d$info$tool) %>%
    magrittr::use_series(schema)
  
  # TEST to make sure tool type matches what we see in the submitted file's structure
  # TODO: Improve to use checkColStructure
  # tab_names_expected <- unique(d$info$schema$sheet_name)
  # tab_names_received <- readxl::excel_sheets(d$keychain$submission_path)
  # 
  # if (any(tab_names_expected != tab_names_received)) {
  #   warning("The sheets included in your submitted file don't seem to match the file type specified.")
  # }
  
  # country_uids: Pull from Home tab ####
  if (is.null(d$info$country_uids)) {
    d$info$country_uids <- 
      unPackCountryUIDs(submission_path = d$keychain$submission_path,
                        tool = d$info$tool)
  }
  
  d$info$country_uids %<>%
    check_params(country_uids = ., force_country_uids = TRUE) %>%
    magrittr::use_series(country_uids)
  
  # datapack_name: Grab from Home Page ####
  d$info$datapack_name <-
    datapackr::unPackDataPackName(
      submission_path = d$keychain$submission_path,
      tool = d$info$tool)
  
  d$info$datapack_name %<>%
    check_params(datapack_name = ., country_uids = d$info$country_uids) %>%
    magrittr::use_series(datapack_name)
  
  # Add placeholders for info messages ####
  if (d$info$tool %in% c("Data Pack", "Data Pack Template", "OPU Data Pack", "OPU Data Pack Template")
      & d$info$cop_year %in% c("2020", "2021")) {
    d$info$needs_psnuxim <- FALSE
    d$info$newSNUxIM <- FALSE
    d$info$has_psnuxim <- FALSE
    d$info$missing_psnuxim_combos <- FALSE
    d$info$missing_DSNUs <- FALSE
  }
  
  d
  
}
