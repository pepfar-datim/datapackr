#' @export
#' @title Extract country uids from submitted file
#'
#' @description
#' When supplied a submission path, will return the list of country_uids
#' pertaining to the file, as read from the Home tab.
#'
#' @param submission_path Local path to the file to import.
#' @param tool What type of tool is the submission file? Default is "Data Pack".
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#'
#' @return Character vector of country_uids.
#'
unPackCountryUIDs <- function(submission_path,
                              tool = "Data Pack",
                              cop_year) {

  if (!tool %in% supportedTools()) {
    stop("Cannot unpack Country UIDs for that type of tool.")
  }

  submission_path <- handshakeFile(path = submission_path,
                                   tool = tool)

  country_uids <-
    readxl::read_excel(
      path = submission_path,
      sheet = "Home",
      range = countryUIDs_homeCell()) %>%
    names() %>%
    stringr::str_remove_all("\\s") %>%
    stringr::str_split(",") %>% #nolint
    purrr::pluck(1)


  #If this has not been specified, we need to try and get it

  cop_year <- cop_year %missing% NULL

  if (is.null(cop_year)) {
    home_tab_metadata <- unPackHomeTabMetadata(submission_path)
    cop_year <- check_cop_year(cop_year = home_tab_metadata$cop_year)
  }

  # Check that country_uids in correct cell
  if (length(country_uids) == 0) {
    warning(
      paste0(
        "Unable to deduce Country UIDs from cell B25 on Home tab. ",
        "Attempting to deduce from org units listed on ",
          ifelse(tool %in% c("Data Pack", "Data Pack Template"), "Prioritization", "SNUxIM"),
        " tab instead."))

    PSNUs <- parsePSNUs(submission_path, tool, cop_year)

    if (NROW(PSNUs) == 0) {
      blank_psnus <- TRUE
    } else if (all(is.na(PSNUs$PSNU))) {
      blank_psnus <- TRUE
    } else {
      blank_psnus <- FALSE
    }

    if (!blank_psnus) {
      country_uids <- unique(PSNUs$country_uid)
    } else {
       warning(
         paste0("Unable to deduce Country UIDs from ",
                ifelse(tool %in% c("Data Pack", "Data Pack Template"), "Prioritization", "SNUxIM"),
                " tab. Attempting to deduce from Data Pack name on Home tab.")
       )

      datapack_name <- unPackDataPackName(
          submission_path = submission_path,
          tool = tool)

      valid_orgunits_local <- getValidOrgUnits(cop_year)

      if (datapack_name == "Latin America Region") {

        country_uids <- c("joGQFpKiHl9", "QKD4CzBG2GM",
        "N7QAPGSaODP", "EXVC4bNtv84", "w5NMe34EjPN",
        "aUTsSmqqu9O", "oK0gC85xx2f")

      } else if (datapack_name == "Caribbean Region") {
        country_uids <- c("RKoVudgb05Y", "PeOHqAwdtez", "WuxG6jzaypt", "zhJINyURZ5Y", "WSl5y9jxCpC")
      } else if (datapack_name %in% unique(valid_orgunits_local$country_name)) {
        country_uids <- unique(valid_orgunits_local$country_uid[valid_orgunits_local$country_name == datapack_name])
      } else if (datapack_name %in% unique(valid_orgunits_local$ou)) {
        country_uids <- unique(valid_orgunits_local$country_uid[valid_orgunits_local$ou == datapack_name])
      } else {
        stop("Impossible to deduce Country UIDs from submission.")
      }

    }

  }

  # If Regional UID provided, prompt for list of individual Country UIDs instead
  invalid_uids <-
    stringr::str_extract_all(
      country_uids,
      paste0("Asia_Regional_Data_Pack|iD2i0aynOGm|t25400wXrNB",
             "|Caribbean_Data_Pack|nBo9Y4yZubB",
             "|Central_America_Data_Pack|vSu0nPMbq7b|IJOkeEIdw9i",
             "|Western_Africa_Data_Pack"
      )
    ) %>%
    unlist()

  if (length(invalid_uids) > 0) {
    msg <-
      paste0(
        "Cell ", countryUIDs_homeCell(), " in the Home tab of your ",
        tool,
        " contains the following Regional OU uids: \n\n  * ",
        paste(invalid_uids, collapse = "\n  * "),
        "\n\nThis approach is no longer supported.",
        " Please return to your ",
        tool,
        " and enter a list of DATIM country-level uids separated by commas in cell",
        countryUIDs_homeCell(),
        " of the Home tab.")

    stop(msg)
  }

  PSNUs <- parsePSNUs(submission_path, tool, cop_year)

  if (NROW(PSNUs) > 0) {
    # TEST: Check country_uids and PSNUs in Data Pack match
    if (!all(purrr::map_lgl(unique(PSNUs$country_uid),
                            ~ .x %in% country_uids))) {
      warning("Deduced or provided Country UIDs do no match Country UIDs observed in submission.")
    }
  }

  country_uids

}

#' @export
#' @title Extract PSNUs from submitted file
#'
#' @description
#' When supplied a submission path, tool type and COP year,
#' will return a data frame of PSNU, psnu_uid,country_name, and country_uid
#' If there are malformed PSNU UIDs in the file, an error will be thrown.
#'
#' @param submission_path Local path to the file to import.
#' @param tool What type of tool is the submission file? Default is "Data Pack".
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#'
#' @return Data frame of parsed PSNUs.
#'
parsePSNUs <- function(submission_path, tool, cop_year) {

  valid_orgunits_local <- getValidOrgUnits(cop_year)

  PSNUs <-
    readxl::read_excel(
      path = submission_path,
      sheet = ifelse(tool %in% c("Data Pack", "Data Pack Template"), "Prioritization", "PSNUxIM"),
      range = readxl::cell_limits(
        c(headerRow(tool = tool, cop_year = cop_year), 1),
        c(NA, NA)),
      col_types = "text",
      .name_repair = "minimal") %>%
    dplyr::select(PSNU) %>%
    # Add PSNU uid ####
  dplyr::mutate(
    psnu_uid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)")) %>%
    dplyr::left_join(valid_orgunits_local %>%
                       dplyr::select(psnu_uid = uid, country_name, country_uid),
                     by = "psnu_uid") %>%
    dplyr::filter_all(dplyr::any_vars(!is.na(.))) %>%
    dplyr::distinct()

  #Test for valid PSNU UIDs
  malformed_psnu_uids <- PSNUs %>%
    dplyr::filter(is.na(psnu_uid)) %>%
    dplyr::distinct()

  if (NROW(malformed_psnu_uids) > 0) {
    msg <- paste("ERROR: The PSNUxIM tab contains malformed PSNU identifiers. The following
      rows were affected: ", paste(malformed_psnu_uids$PSNU, sep = "", collapse = ";"), ". This error
      must be fixed in order to proceed.")
    stop(msg)
  }

  PSNUs
}
