#' @export
#' @title Pull list of Org Units approved for use in Data Packs from DATIM based
#' on provided country UIDs.
#'
#' @description
#' Queries DATIM to extract tibble of Org Units approved for use in Data Pack
#' target setting.
#'
#' @inheritParams datapackr_params
#' @param include_mil Logical. If \code{TRUE}, will also include _Military nodes
#' related to \code{country_uids}. Default is \code{TRUE}.
#' @param include_DREAMS Logical. If \code{TRUE}, will also include DREAMS
#' organisation units.
#' @param additional_fields Character string of any fields to return from DATIM
#' API other than those returned by default.
#' @param use_cache If \code{TRUE}, will first check to see if a cached export
#' of Data Pack Org Units already exists and is not stale. If so, will use this
#' cached version to save time.
#' @param cache_path Local file path to an RDS file containing a cached copy of
#' Data Pack Org Units from DATIM.
#'
#' @return Tibble of Data Pack Org Units.
#'
getDataPackOrgUnits <- function(country_uids = NULL,
                                include_mil = TRUE,
                                include_DREAMS = TRUE,
                                additional_fields = NULL,
                                use_cache = TRUE,

                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {

  api_filters <-
    c(paste0("organisationUnitGroups.id:in:[AVy8gJXym2D", # Filter for COP Prioritization SNU
             ifelse(include_mil, ",nwQbMeALRjL", ""), # Add military SNUs if requested
             ifelse(include_DREAMS, ",mRRlkbZolDR", ""), # Add DREAMS SNUs if requested
             "]"))

  # Check parameters ####
  if (!is.null(country_uids)) {
    country_uids %<>% check_country_uids(force = FALSE)
    api_filters %<>% append(paste0("ancestors.id:in:[", paste(country_uids, collapse = ","), "]"))
  }

  fields <-
    paste0(
      "id,name,lastUpdated,ancestors[id,name,organisationUnitGroups[id,name]],organisationUnitGroups[id,name]",
      ifelse(!is.null(additional_fields), paste0(",", additional_fields), ""))

  #Calculate a cache hash
  cache_hash <- digest::sha1(list(api_filters, fields))
  cache_path <- paste0(Sys.getenv("support_files_directory"), cache_hash, ".rds")


  # Pull Org Units ####
  is_fresh <- cache_is_fresh(cache_path)

  if (use_cache && is_fresh) {
    interactive_print("Loading cached Data Pack Org Units")
    orgunits <- readRDS(cache_path)
  } else {

    orgunits <-
      datimutils::getMetadata(
        "organisationUnits",
        api_filters,
        fields = fields,
        d2_session = d2_session)

    # orgunits <- datimutils::getSqlView(sql_view_uid = ""), #Replace once DP-786 resolved

    # Extract metadata ####
    orgunits %<>%
      tibble::as_tibble(.) %>%
      dplyr::rename(uid = id) %>%
        dplyr::mutate(
          org_type =
            dplyr::case_when(
              stringr::str_detect(as.character(organisationUnitGroups), "nwQbMeALRjL") ~ "Military",
              stringr::str_detect(as.character(organisationUnitGroups), "cNzfcPWEGSH") ~ "Country",
              stringr::str_detect(as.character(organisationUnitGroups), "AVy8gJXym2D|mRRlkbZolDR") ~ "SNU"),
          DREAMS =
            dplyr::case_when(
              stringr::str_detect(as.character(organisationUnitGroups), "mRRlkbZolDR") ~ "Y"
            ),
          level_4_type = purrr::map(ancestors, list("organisationUnitGroups", 4, "id"), .default = NA),
          country_name = dplyr::case_when(
            org_type == "Country" ~ name,
            stringr::str_detect(as.character(level_4_type), "cNzfcPWEGSH") ~
              purrr::map_chr(ancestors, list("name", 4), .default = NA),
            TRUE ~ purrr::map_chr(ancestors, list("name", 3), .default = NA)),
          country_uid = dplyr::case_when(
            org_type == "Country" ~ uid,
            stringr::str_detect(as.character(level_4_type), "cNzfcPWEGSH") ~
              purrr::map_chr(ancestors, list("id", 4), .default = NA),
            TRUE ~ purrr::map_chr(ancestors, list("id", 3), .default = NA)),
          # What to do about DSNUs != PSNU. Leave these NA?
          ou_uid = purrr::map_chr(ancestors, list("id", 3), .default = NA),
          ou = purrr::map_chr(ancestors, list("name", 3), .default = NA),
          snu1_uid = purrr::map_chr(ancestors, list("id", 4), .default = NA)
              %|% uid,
          snu1 = purrr::map_chr(ancestors, list("name", 4), .default = NA)
              %|% name) %>%
        dplyr::select(ou, ou_uid, country_name, country_uid, snu1, snu1_uid,
                      name, uid, org_type,
                      tidyselect::everything(), -level_4_type)
    }

  can_write_file <- file.access(dirname(cache_path), 2) == 0

  # If cache location can be written and fresh data was pulled, write fresh cache to location
  if (can_write_file && !is_fresh) {
    interactive_print(paste0("Updating Data Pack org units cache at ", cache_path))
    saveRDS(orgunits, file = cache_path)
  }

  orgunits
}


#' @export
#' @title Modify Data Pack Org Unit list to add datapackr IDs.
#'
#' @description
#' Adds Org Unit label used in Data Packs.
#'
#' @param orgunits Data frame of Data Pack org units produced by \code{\link{getDataPackOrgUnits}}.
#'
#' @return Data frame of Data Pack Org units with added Data Pack label, \code{dp_psnu}.
#'
add_dp_label <- function(orgunits) {

  country_count <- unique(orgunits$country_uid) %>% length()

  orgunits %<>%
    dplyr::mutate(
      dp_label = paste0(
        dplyr::if_else(
          country_count > 1 & country_uid != uid,
          paste0(country_name, " > "),
          ""),
        name,
        dplyr::if_else(!is.na(org_type), paste0(" [#", org_type, "]"), ""),
        dplyr::if_else(!is.na(DREAMS), " [#DREAMS]", ""),
        " [", uid, "]")
    )

  orgunits
}
