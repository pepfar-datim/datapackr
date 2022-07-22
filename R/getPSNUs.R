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
  interactive_print(cached_psnus_path)
  can_read_file <- file.access(cached_psnus_path, 4) == 0
  can_write_file <- file.access(dirname(cached_psnus_path), 2) == 0

  # Check whether Cached PSNUs List is stale
  if (is.null(d2_session$max_cache_age)) {
    max_cache_age <- "1 day"
  } else {
    max_cache_age <- d2_session$max_cache_age
  }

  is_fresh <- function(time, max_age) {
    lubridate::as.duration(lubridate::interval(time, Sys.time())) < lubridate::duration(max_age)
  }

  cache_file_is_fresh <- is_fresh(file.info(cached_psnus_path)$mtime, max_cache_age)

  # Reads in cache file if exists, can be read, and is fresh ####
  if (can_read_file && cache_file_is_fresh) {
    interactive_print("Loading cached PSNUs file")
    # Loads cache
    cached_psnus <- readRDS(cached_psnus_path)
  }

  if (!can_read_file || !cache_file_is_fresh) { # If no cache available, mark cache as stale
    cached_psnus <- NULL
    missing_country_uids <- country_uids
    cache_status <- "stale"
  } else if (is.null(country_uids) && # If no countries are specified
      all(is_fresh(cached_psnus$cache_date, max_cache_age)) && # And all data is fresh
      NROW(unique(cached_psnus$country_name)) == 73) { # And all countries are represented, then return cached data
    return(cached_psnus)
  } else if (is.null(country_uids)) { # Otherwise if no countries specified, but not all fresh, mark entire cache stale
    cache_status <- "stale"
    missing_country_uids <- NULL
  } else if (all(country_uids %in% cached_psnus$country_name) && # If countries specified and none are missing or stale
      !any(country_uids %in% unique(cached_psnus$country_name[!is_fresh(cached_psnus$cache_date, max_cache_age)]))) {
    return(cached_psnus) # return cache
  } else { # Otherwise if countries specified and missing or stale, mark cache partially stale and return missing UIDs
    # Finds if any countries do not have fresh data or are not in cache
    missing_country_uids <- c(
      # Countries with stale data in cache
      country_uids[country_uids %in% cached_psnus$country_uid[!is_fresh(cached_psnus$cache_date, max_cache_age)]],
      # Countries missing from cache
      country_uids[!country_uids %in% cached_psnus$country_uid]
    )
    cache_status <- "partially_stale"
  }

  # If no fresh cache or missing country_uids in fresh cache, pull PSNUs from DATIM ####
  interactive_print("Fetching new PSNUs file from DATIM")
  
  ## Assemble API filters ####
  api_filters <-
    # Filter by appropriate organisation unit groups
    c(paste0("organisationUnitGroups.id:in:[AVy8gJXym2D", # Filter for COP Prioritization SNU
             ifelse(include_mil, ",nwQbMeALRjL", ""), # Add military SNUs if requested
             ifelse(include_DREAMS, ",mRRlkbZolDR", ""), "]")) # Add DREAMS SNUs if requested
    
  # If country UIDs are provided, add filter for country UIDs
  if (!is.null(missing_country_uids)) {
    api_filters %<>% append(paste0("ancestors.id:in:[", paste(missing_country_uids, collapse = ","), "]"))
  }
  
  ## Make API call to DATIM ####
  fresh_psnus <-
    datimutils::getMetadata(
      "organisationUnits",
      api_filters,
      fields = paste0("id,name,ancestors[id,name,organisationUnitGroups[id,name]],organisationUnitGroups[id,name]",
                      ifelse(!is.null(additional_fields), paste0(",", additional_fields), "")), # Pastes additional fields
      d2_session = d2_session)

  # Extract metadata if fresh data was pulled ####
  if (!is.null(fresh_psnus)) {
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
                    tidyselect::everything(), -level_4_type) %>%
      dplyr::mutate(cache_date = Sys.time())
  }

  if (cache_status == "stale" && !is.null(fresh_psnus)) {
    # If no fresh cache was pulled and data was retrieved from DATIM, sets this as `PSNUs` dataset
    PSNUs <- fresh_psnus
  } else if (cache_status == "partially_stale" && !is.null(fresh_psnus)) {
    # If some data was missing or stale, appends to fresh cache data
    PSNUs <- rbind(dplyr::filter(cached_psnus, !is_fresh(cache_date, max_cache_age)),
                   fresh_psnus)
  } else if (is.null(fresh_psnus) && can_read_file) {
    # If no fresh cache was pulled but no data was retrieved from DATIM, attempts to find stale cache
    # Loads cache
    PSNUs <- readRDS(cached_psnus_path)
    stopifnot("ERROR: Fresh data cannot be pulled and some countries missing from cache!" =
                !is.null(country_uids[!country_uids %in% PSNUs$country_name]))
  } else {
    # If no cache was found and no data was retrieved from DATIM, stops with error
    stop("ERROR: Could not retrieve cache and could not fetch data from DATIM!")
  }

  # If cache location can be written and fresh data was pulled, write fresh cache to location
  if (can_write_file && !is.null(fresh_psnus)) {
    interactive_print(paste0("Overwriting stale mechanisms view to ", cached_psnus_path))
    saveRDS(PSNUs, file = cached_psnus_path)
  }

  PSNUs
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
