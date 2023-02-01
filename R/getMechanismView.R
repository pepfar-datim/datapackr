
.fetchMechanismViewFromDATIM <- function(d2_session) {
  interactive_print("Fetching new mechs file from DATIM")

  #We are going to make this large request once by partition it by COP year
  #This query usually times out if it is made without filters.
  cop_years <- supportedCOPYears()
  mechs_list <- vector(mode='list', length=length(cop_years))

  for (i in seq_len(length(cop_years))) {
    url_filter <-
      c(
        paste0("startdate:lt:", as.numeric(cop_years[i]) + 1, "-10-01"),
        paste0("enddate:gt:", cop_years[i], "-09-30")
      )

    mechs_list[[i]] <- datimutils::getSqlView(
      url_filter,
      sql_view_uid = "fgUtV6e9YIX",
      d2_session = d2_session,
      timeout = 600
    )

  }

  mechs <- purrr::map_df(mechs_list, rbind.data.frame) %>%
    dplyr::distinct()

  special_mechs  <- datimutils::getSqlView(
    "code:^like:00",
    sql_view_uid = "fgUtV6e9YIX",
    d2_session = d2_session,
    timeout = 600
  )

   dplyr::bind_rows(special_mechs, mechs) %>%
     dplyr::rename(
       mechanism_desc = mechanism,
       attributeOptionCombo = uid,
       mechanism_code = code,
       partner_desc = partner,
       partner_id = primeid)


}
#' @export
#' @title getMechanismView
#'
#' @description Retrieves a view of mechanisms with partners and agencies
#' The function will attempt to read from a cached file, if defined in
#' the support_files_directory option has been set, and the mechs.rds file
#' is available to be read. Otherwise, if the user is logged in, the view
#' will be obtained from DATIM. Otherwise, an empty dataframe is returned.
#'
#' Note that only country UIDs are accepted. If Regional Operating Unit uids
#' are supplied, returns entire mechanism list, trimmed to user's DATIM permissions.
#'
#' @param include_dedupe Logical. If TRUE will include deduplication mechanisms.
#' Default is FALSE.
#' @param include_MOH Logical. If TRUE will include MOH mechanisms. Default is
#' FALSE.
#' @param update_stale_cache If the cached_mechs_path file is outdated or unreadable,
#' should a new cache be saved?
#' @param include_default Should the default mechanism also be included?
#' @inheritParams datapackr_params
#'
#' @return Mechs
#'
getMechanismView <- function(country_uids = NULL,
                             cop_year = NULL,
                             include_dedupe = FALSE,
                             include_MOH = FALSE,
                             d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE),
                             cached_mechs_path = paste0(Sys.getenv("support_files_directory"), "mechs.rds"),
                             update_stale_cache = FALSE,
                             include_default = TRUE) {

  empty_mechs_view <- tibble::tibble(
    "mechanism_desc" = character(),
    "mechanism_code" = character(),
    "attributeOptionCombo" = character(),
    "partner_desc" = character(),
    "partner_id" = character(),
    "agency" = character(),
    "ou" = character(),
    "startdate" = character(),
    "enddate" = character()
    )

  # If Cached Mech list is available and fresh, use this to save processing time

  can_read_file <- file.access(cached_mechs_path, 4) == 0
  can_write_file <- file.access(dirname(cached_mechs_path), 2) == 0

  # Check whether Cached Mech List is stale
  if (is.null(d2_session$max_cache_age)) {
    max_cache_age <- "1 day"
  } else {
    max_cache_age <- d2_session$max_cache_age
  }

  if (file.exists(cached_mechs_path) && can_read_file) {
    is_lt <- function(x, y)  x < y
    cache_age_dur <- lubridate::as.duration(lubridate::interval(file.info(cached_mechs_path)$mtime, Sys.time()))
    max_cache_age_dur <- lubridate::duration(max_cache_age)
    is_fresh <- is_lt(cache_age_dur, max_cache_age_dur)
  } else {
    is_fresh <- FALSE
  }

  if (is_fresh && can_read_file) {
    interactive_print("Loading cached mechs file")
    mechs <- readRDS(cached_mechs_path)
  }

  if (!is_fresh) {
    mechs <- .fetchMechanismViewFromDATIM(d2_session = d2_session)
    if (can_write_file) {
      interactive_print(paste0("Overwriting stale mechanisms view to ", cached_mechs_path))
      saveRDS(mechs, file = cached_mechs_path)
    }
}


# Filter by COP Year ####
if (!is.null(cop_year)) {

  cop_year %<>% check_cop_year(cop_year = cop_year)

  mechs %<>%
    dplyr::filter(
      (startdate < paste0(max(as.numeric(cop_year)) + 1, "-10-01") &
         enddate > paste0(min(as.numeric(cop_year)), "-09-30")))
}



  # Filter by OU from a vector of country UIDs
  if (!is.null(country_uids)) {

    cop_year <- cop_year %missing% NULL
    cop_year %<>% check_cop_year(cop_year = cop_year)

    ous <- getValidOrgUnits(cop_year) %>%
      dplyr::select(ou, ou_uid, country_uid) %>%
      dplyr::distinct() %>%
      dplyr::filter(country_uid %in% country_uids) %>%
      dplyr::pull(ou) %>%
      unique(.)

    mechs %<>%
      dplyr::filter(ou %in% ous | is.na(ou))
  }


  # Include Dedupe or MOH ####
  if (!include_MOH) {
    mechs <- mechs %>%
      dplyr::filter(!(mechanism_code %in% c("00100", "00200")))
  }

  if (!include_dedupe) {
    mechs <-  mechs %>%
      dplyr::filter(!(mechanism_code %in% c("00000", "00001")))
  }

  if (include_default) {

    default_mech <- list(
      mechanism_desc = "default",
      mechanism_code = "default",
      attributeOptionCombo = datapackr::default_catOptCombo(),
      partner_desc = "None",
      partner_id = "None",
      agency = "None",
      ou = "",
      startdate = "",
      enddate = ""
    )

    mechs <- dplyr::bind_rows(mechs, default_mech)
  }

  structure_ok <- dplyr::setequal(names(empty_mechs_view), names(mechs))

  if (any(duplicated(mechs$mechanism_code))) {

    stop("Duplicated mechanisms codes detected: ",
         paste(mechs$mechanism_code[duplicated(mechs$mechanism_code)], sep="", collapse=","))
  }

  if (!structure_ok) warning("Mechanism view names are not correct!")

  if (NROW(mechs) == 0) {
    warning("No mechanisms for the combination of paramaters were found")
  }

  return(mechs)

}
