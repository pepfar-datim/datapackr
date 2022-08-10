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

  # If any country_uids are not actually UIDs, warn, but remove and still move on.
  if (!any(is_uidish(country_uids))) {
    country_uids <- NULL
    interactive_message(paste("WARNING: All supplied country_uids appear to have been invalid.",
                              "PSNUs for all countries will be returned instead."))
  } else if (!all(is_uidish(country_uids))) {
    invalid_uids <- country_uids[is_uidish(country_uids)]
    country_uids <- country_uids[!is_uidish(country_uids)]
    interactive_message(paste0("WARNING: The following country_uids do not appear to be UIDs and will be removed: ",
                               paste_oxford(invalid_uids), final = "&"))
  }

  # If cached PSNUs list is available and fresh, use this to save processing time
  interactive_print(cached_psnus_path)
  can_read_file <- file.access(cached_psnus_path, 4) == 0
  can_write_file <- file.access(dirname(cached_psnus_path), 2) == 0

  #Check whether cached PSNUs list is stale
  if (is.null(d2_session$max_cache_age)) {
    max_cache_age <- "1 day"
  } else {
    max_cache_age <- d2_session$max_cache_age
  }

  cache_file_is_fresh <- is_fresh(file.info(cached_psnus_path)$mtime, max_cache_age)

  if (can_read_file && cache_file_is_fresh) {
    interactive_print("Loading cached PSNUs file")
    cached_psnus <- readRDS(cached_psnus_path)
    fresh_part <- fresh_cache_part(cached_psnus)
  } else {
    cached_psnus <- NULL
    fresh_part <- NULL
  }

  if (length(country_uids) > 0) {
    stale_countries <- country_uids[!country_uids %in% fresh_part$country_uid]
  } else {
    stale_countries <- NULL
  }

  fetch_fresh_psnus <- function(country_uids, include_mil, include_DREAMS, additional_fields, d2_session) {
    ## Assemble API filters ####
    api_filters <-
      # Filter by appropriate organisation unit groups
      c(paste0("organisationUnitGroups.id:in:[AVy8gJXym2D", # Filter for COP Prioritization SNU
               ifelse(include_mil, ",nwQbMeALRjL", ""), # Add military SNUs if requested
               ifelse(include_DREAMS, ",mRRlkbZolDR", ""), "]")) # Add DREAMS SNUs if requested

    # If country UIDs are provided, add filter for country UIDs
    if (!is.null(country_uids) && length(missing_country_uids) > 0) {
      api_filters %<>% append(paste0("ancestors.id:in:[", paste(country_uids, collapse = ","), "]"))
    }

    ## Make API call to DATIM ####
    fresh_psnus <-
      datimutils::getMetadata(
        "organisationUnits",
        api_filters,
        fields = paste0("id,name,ancestors[id,name,organisationUnitGroups[id,name]],organisationUnitGroups[id,name]",
                        ifelse(!is.null(additional_fields),
                               paste0(",", additional_fields), "")), # Pastes additional fields
        d2_session = d2_session)

    if (NROW(fresh_psnus) == 0) return(NULL)

    # Extract metadata ####
    fresh_psnus %<>%
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

    return(fresh_psnus)
  }

  # If no fresh cache or missing country_uids in fresh cache, pull PSNUs from DATIM ####
  interactive_print("Fetching new PSNUs file from DATIM")
  refreshed_part <- fetch_fresh_psnus(stale_countries, include_mil, include_DREAMS, additional_fields, d2_session)

  # Combine existing fresh part with new refreshed part ####
  updated_cache <- rbind(fresh_part, refreshed_part)

  # If any countries are missing from the cache, attempt to append them from cached data ####
  if (!all(country_uids %in% updated_cache$country_uid) && !is.null(cached_psnus)) {
    missing_psnus <- country_uids[!country_uids %in% updated_cache$country_uid]
    updated_cache <- rbind(updated_cache,
                           cached_psnus[cached_psnus$country_uid %in% missing_psnus])
    stopifnot("ERROR: Fresh data cannot be pulled and some countries are missing from cache!" =
                all(country_uids %in% updated_cache$country_uid))
  } else if (!all(country_uids %in% updated_cache$country_uid) && is.null(cached_psnus)) {
    stop("ERROR: Fresh data cannot be pulled and no cache could be retrieved!")
  }

  # If cache location can be written and fresh data was pulled, write fresh cache to location
  if (can_write_file && !is.null(refreshed_part)) {
    interactive_print(paste0("Overwriting stale mechanisms view to ", cached_psnus_path))
    saveRDS(PSNUs, file = cached_psnus_path)
  }

  if (!is.null(country_uids)) {
    updated_cache <- updated_cache[updated_cache$country_uid %in% country_uids]
  }

  updated_cache
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
