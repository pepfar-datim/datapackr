#' @export
#' @title Pull list of PSNUs from DATIM based on provided country UIDs.
#'
#' @description
#' Queries DATIM to extract list of PSNUs and adds additional PSNUs not
#' currently in DATIM as needed.
#'
#' @inheritParams datapackr_params
#'
#' @return Data frame of PSNUs
#'
getPSNUs <- function(country_uids = NULL,
                     include_mil = TRUE,
                     include_DREAMS = TRUE,
                     additional_fields = NULL,
                     cached_psnus_path = paste0(Sys.getenv("support_files_directory"), "psnus.rds"),
                     d2_session = dynGet("d2_default_session",
                                         inherits = TRUE)) {
  
  # If Cached PSNUs list is available and fresh, use this to save processing time
  print(cached_psnus_path)
  can_read_file <- file.access(cached_psnus_path, 4) == 0
  can_write_file <- file.access(dirname(cached_psnus_path), 2) == 0
  
  # Check whether Cached PSNUs List is stale
  if (is.null(d2_session$max_cache_age)) {
    max_cache_age <- "1 day"
  } else {
    max_cache_age <- d2_session$max_cache_age
  }
  
  if (file.exists(cached_psnus_path) & can_read_file) {
    is_lt <- function(x, y)  x < y
    cache_age_dur <- lubridate::as.duration(lubridate::interval(file.info(cached_psnus_path)$mtime, Sys.time()))
    max_cache_age_dur <- lubridate::duration(max_cache_age)
    is_fresh <- is_lt(cache_age_dur, max_cache_age_dur)
  } else{
    is_fresh <- FALSE
  }
  
  if (is_fresh & can_read_file) {
    interactive_print("Loading cached PSNUs file")
    psnus <- readRDS(cached_psnus_path)
  }

  # If no fresh cache, pull PSNUs from DATIM ####
  if (!is_fresh) {
    interactive_print("Fetching new PSNUs file from DATIM")
    api_filters <-
      # Filter by appropriate organisation unit groups
      c(paste0("organisationUnitGroups.id:in:[AVy8gJXym2D", # Filter for COP Prioritization SNU
               ifelse(include_mil, ",nwQbMeALRjL", ""), # Add military SNUs if requested
               ifelse(include_DREAMS, ",mRRlkbZolDR", ""), "]")) %>% # Add DREAMS SNUs if requested
      # If country UIDs are provided, add filter for country UIDs
      {if (!is.null(country_uids)) {
        append(., paste0("ancestors.id:in:[", paste(country_uids, collapse = ","), "]"))
      } else .
      }

    PSNUs <-
      datimutils::getMetadata(
        "organisationUnits",
        api_filters,
        fields = paste0("id,name,ancestors[id,name,organisationUnitGroups[id,name]],organisationUnitGroups[id,name]",
                        ifelse(!is.null(additional_fields), paste0(",", additional_fields), "")), # Pastes additional fields
        d2_session = d2_session)
  
    if (can_write_file) {
      interactive_print(paste0("Overwriting stale mechanisms view to ", cached_psnus_path))
      saveRDS(dplyr::select(psnu, "name", "id", "ancestors", "organisationUnitGroups"), # Filter to desired columns
              file = cached_psnus_path)
    }
  }

  # Extract metadata ####
  PSNUs %<>%
    dplyr::rename(psnu = name, psnu_uid = id) %>%
    dplyr::mutate(
      psnu_type =
        dplyr::case_when(
          stringr::str_detect(as.character(organisationUnitGroups), "nwQbMeALRjL") ~ "Military",
          stringr::str_detect(as.character(organisationUnitGroups), "cNzfcPWEGSH") ~ "Country",
          stringr::str_detect(as.character(organisationUnitGroups), "AVy8gJXym2D") ~ "SNU"),
      DREAMS =
        dplyr::case_when(
          stringr::str_detect(as.character(organisationUnitGroups), "mRRlkbZolDR") ~ "Y"
        ),
      level_4_type = purrr::map(ancestors, list("organisationUnitGroups", 4), .default = NA),
      country_name = dplyr::case_when(
        psnu_type == "Country" ~ psnu,
        stringr::str_detect(as.character(level_4_type), "cNzfcPWEGSH") ~
          purrr::map_chr(ancestors, list("name", 4), .default = NA),
        TRUE ~ purrr::map_chr(ancestors, list("name", 3), .default = NA)
      ),
      country_uid = dplyr::case_when(
        psnu_type == "Country" ~ psnu_uid,
        stringr::str_detect(as.character(level_4_type), "cNzfcPWEGSH") ~
          purrr::map_chr(ancestors, list("id", 4), .default = NA),
        TRUE ~ purrr::map_chr(ancestors, list("id", 3), .default = NA)
      ),
      ou_id = purrr::map_chr(ancestors, list("id", 3), .default = NA),
      ou = purrr::map_chr(ancestors, list("name", 3), .default = NA),
      snu1_id = dplyr::if_else(
        condition = is.na(purrr::map_chr(ancestors, list("id", 4), .default = NA)),
        true = psnu_uid,
        false = purrr::map_chr(ancestors, list("id", 4), .default = NA)),
      snu1 = dplyr::if_else(
        condition = is.na(purrr::map_chr(ancestors, list("name", 4), .default = NA)),
        true = psnu,
        false = purrr::map_chr(ancestors, list("name", 4), .default = NA))
    ) %>%
    dplyr::select(ou, ou_id, country_name, country_uid, snu1, snu1_id,
                  psnu, psnu_uid, psnu_type,
                  tidyselect::everything(), -level_4_type)

  return(PSNUs)
}


#' @export
#' @title Modify PSNU list to add datapackr IDs.
#'
#' @description
#' Adds PSNU label used in Data Packs.
#'
#' @param PSNUs Data frame of PSNUs produced by \code{\link{getPSNUs}}.
#'
#' @return Data frame of PSNUs with added Data Pack label, \code{dp_psnu}.
#'
add_dp_psnu <- function(PSNUs) {

  country_count <- unique(PSNUs$country_uid) %>% length()

  PSNUs %<>%
    dplyr::mutate(
      dp_psnu = paste0(
        dplyr::if_else(
          country_count > 1 & country_uid != psnu_uid,
          paste0(country_name, " > "),
          ""),
        psnu,
        dplyr::if_else(!is.na(psnu_type), paste0(" [#", psnu_type, "]"), ""),
        dplyr::if_else(!is.na(DREAMS), " [#DREAMS]", ""),
        " [", psnu_uid, "]")
    )

  return(PSNUs)
}
