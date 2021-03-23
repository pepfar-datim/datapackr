#' @export
#' @title fetchMechsViewFromAPI
#'
#' @inheritParams unPackTool
#' @inheritParams getMechanismViewFromDATIM
#'
#' @return Mechanism List
#'
fetchMechsViewFromAPI <- function(operating_units = NULL,
                                  cop_year = NULL,
                                  uids = NULL,
                                  d2_session = dynGet("d2_default_session",
                                                      inherits = TRUE)) {
  paste0(d2_session$base_url, "api/",datapackr::api_version(),
         "/sqlViews/fgUtV6e9YIX/data.csv?paging=false") %>%
    {if (!is.null(operating_units))
      paste0(., "&filter=ou:in:[",paste(operating_units, collapse = "."),"]")
      else . } %>%
    {if (!is.null(cop_year))
      paste0(., "&filter=startdate:lt:", cop_year+1, "-10-01",
             "&filter=enddate:gt:", cop_year, "-09-30")
      else . } %>%
    {if (!is.null(uids))
      paste0(., "&filter=uid:in:[",paste(uids, collapse = ","),"]")
      else . } %>%
    utils::URLencode() %>%
    httr::GET(httr::timeout(180), handle = d2_session$handle) %>%
    httr::content(., "text") %>%
    readr::read_csv(col_types = readr::cols(.default = "c")) %>%
    dplyr::rename(
      mechanism_desc = mechanism,
      attributeOptionCombo = uid,
      mechanism_code = code,
      partner_desc = partner,
      partner_id = primeid)
}

#' @export
#' @title getMechanismViewFromDATIM
#'
#' @param operating_units OUs to return data for. If not supplied, returns for all.
#' @param uids Character vector of uids to filter list by.
#' @inheritParams unPackTool
#' @inheritParams getMechanismView
#'
#' @return Mechanism List
#'
getMechanismViewFromDATIM <- function(operating_units = NULL,
                                      cop_year = NULL,
                                      uids = NULL,
                                      include_dedupe = FALSE,
                                      include_MOH = FALSE,
                                      d2_session = dynGet("d2_default_session",
                                                          inherits = TRUE)) {
 
  
  mechs <- fetchMechsViewFromAPI(operating_units = operating_units,
                   cop_year = cop_year,
                   d2_session = d2_session)
  
  if (include_dedupe | include_MOH) {
    dedupes <- c("X8hrDf6bLDC","YGT1o7UxfFu")
    MOH <- c("QCJpv5aDCJU","TRX0yuTsJA9")
    
    uids <- NULL
    if (include_dedupe) {uids <- c(uids, dedupes)}
    if (include_MOH) {uids <- c(uids, MOH)}
    
    dedupe_MOH <- fetchMechsViewFromAPI(uids = uids,
                               d2_session = d2_session)
    
    mechs %<>%
      dplyr::bind_rows(dedupe_MOH)
  }
  
  return(mechs)

}



#' @export
#' @importFrom magrittr %>% %<>%
#' @title getMechanismView
#' 
#' @description Retrieves a view of mechanisms with partners and agencies
#' The function will attempt to read from a cached file, if defined in
#' the support_files_directory option has been set, and the mechs.rds file
#' is available to be read. Otherwise, if the user is logged in, the view
#' will be obtained from DATIM. Otherwise, an empty dataframe is returned.
#' 
#' @param country_uids Character vector of DATIM country IDs. This can only
#' include countries. Regional Operating Unit uids will not be accepted. If not
#' supplied, returns entire mechanism list, trimmed to user's DATIM permissions.
#' @param cop_year Numeric value of COP Fiscal Year to filter mechanism list by.
#' Ex: For mechanisms active in FY 2020, pertaining to COP 2019, enter
#' \code{2019}. If a FY is not supplied, returns entire mechanism list.
#' @param include_dedupe Logical. If TRUE will include deduplication mechanisms.
#' Default is FALSE.
#' @param include_MOH Logical. If TRUE will include MOH mechanisms. Default is
#' FALSE.
#' @param cached_mechs_path Local file path to the cached mechanisms file. 
#' @param cached_mechs_path Filepath to an RDS file containing a cached copy of the 
#' SQL view used defined via a envionment variable.
#' @param update_stale_cache If the cached_mechs_path file is outdated or unreadable,
#' should a new cache be saved?
#' @inheritParams unPackTool
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
                             update_stale_cache = FALSE) {
  
  empty_mechs_view <- tibble::tibble(
    "mechanism_desc" = character() ,
    "mechanism_code"= character(),
    "attributeOptionCombo" = character(),
    "partner_desc" = character(),
    "partner_id" = character(),
    "agency" = character(),
    "ou" = character(),
    "startdate" = character(),
    "enddate" = character()
    )

  # If Cached Mech list is available and fresh, use this to save processing time
  need_new_cache <- FALSE
  can_read_file <- file.access(cached_mechs_path, 4) == 0
  if (!can_read_file) {
    need_new_cache <- TRUE
  } else {
    
    # Check whether Cached Mech List is stale
    if (is.null(d2_session$max_cache_age)) {
      max_cache_age <- "1 day"
    } else {
      max_cache_age <- d2_session$max_cache_age
    } 
    
    is_fresh <-
      lubridate::as.duration(lubridate::interval(Sys.time(), file.info(cached_mechs_path)$mtime)) < lubridate::duration(max_cache_age)
    
    # If it is fresh, use this to produce the mech list
    if (!is_fresh) {
      need_new_cache <- TRUE
    } else {
      cached_mech_list <- readRDS(cached_mechs_path)
    
    # Filter by country
      mechs <- cached_mech_list %>%
        {if (!is.null(country_uids)) dplyr::filter(., ou %in% country_uids)
          else .} %>%
    # Filter by FY
        {if (!is.null(cop_year))
          dplyr::filter(.,
                        startdate < paste0(cop_year+1,"-10-01"),
                        enddate > paste0(cop_year,"-09-30"))
          else .}
    
    # Include Dedupe or MOH
      if (include_dedupe | include_MOH) {
        dedupes <- c("X8hrDf6bLDC","YGT1o7UxfFu")
        MOH <- c("QCJpv5aDCJU","TRX0yuTsJA9")
        
        uids <- NULL
        if (include_dedupe) {uids <- c(uids, dedupes)}
        if (include_MOH) {uids <- c(uids, MOH)}
        
        dedupe_MOH <- cached_mech_list %>%
          dplyr::filter(
            attributeOptionCombo %in% uids
          )
        
        mechs %<>%
          dplyr::bind_rows(dedupe_MOH)
      }
    }
  }
  
  if (need_new_cache) {
  # If cached file not accessible or stale, pull and save fresh
    if (!isLoggedIn(d2_session)) {
      warning("You are not logged in but have requested a mechanism view.")
      return(empty_mechs_view)
    }
    
    # If indicated, pull Mechs for all OUs and save to cache for other users
    if (update_stale_cache & cached_mechs_path != "") {
      # TODO: Need to check and be sure we can write here. 
      mechs <-
        getMechanismViewFromDATIM(operating_units = NULL,
                                  cop_year = cop_year,
                                  include_dedupe = include_dedupe,
                                  include_MOH = include_MOH,
                                  d2_session = d2_session)
      print(paste0("Overwriting stale mechanisms view to ", cached_mechs_path))
      saveRDS(mechs, file = cached_mechs_path)
    } else {
    # Otherwise, pull just for the user's OU
      # Convert country_uids to OU names for filtering
      if (!is.null(country_uids)) {
        operating_units <-
          datimutils::getMetadata(
            end_point = "organisationUnits",
            paste0("id:in:[",paste(country_uids, collapse = ","),"]"),
            fields = "id,name,ancestors[id,name,organisationUnitGroups[id,name]],organisationUnitGroups[id,name]",
            d2_session = d2_session) %>%
          dplyr::mutate(
            ou = purrr::map_chr(ancestors, list("name", 3), .default = NA),
            ou = dplyr::if_else(is.na(ou), name, ou)) %>%
          dplyr::pull(ou) %>%
          unique()
      } else {operating_units = NULL}
        
    # Pull Mechs just for this OU
      mechs <-
        getMechanismViewFromDATIM(operating_units = operating_units,
                                  cop_year = cop_year,
                                  include_dedupe = include_dedupe,
                                  include_MOH = include_MOH,
                                  d2_session = d2_session)
    }
  }
  
  structure_ok <- dplyr::setequal(names(empty_mechs_view), names(mechs))

  if (!structure_ok) y

  return(mechs)

}


#' @export
#' @importFrom magrittr %>% %<>%
#' @title adornMechanisms(data)
#'
#' @description Join analytical dimensions with d$data$analtyics related
#' to partner, agency and mechanism information.
#'
#' @param data Dataset to adorn, typically d$data$analytics
#' @inheritParams unPackTool
#'
#' @return Modified data object
#'
adornMechanisms <- function(data,
                            d2_session = dynGet("d2_default_session",
                                                inherits = TRUE)) {

  mechs <-
    getMechanismView(
      country_uids = NULL,
      cop_year = NULL,
      include_dedupe = FALSE,
      include_MOH = FALSE,
      d2_session = d2_session) %>%
    dplyr::select(-ou, -startdate, -enddate)

  data %<>%
    dplyr::left_join(mechs, by = "mechanism_code") %>%
    dplyr::mutate(
      mechanism_desc = dplyr::case_when(mechanism_code == "99999" ~ 'Dedupe approximation',
                                        TRUE ~ mechanism_desc),
      partner_desc = dplyr::case_when(mechanism_code == "99999" ~ 'Dedupe approximation',
                                      TRUE ~ partner_desc),
      partner_id = dplyr::case_when(mechanism_code == "99999" ~ '99999',
                                    TRUE ~ partner_id),
      agency = dplyr::case_when(mechanism_code == "99999" ~ 'Dedupe approximation',
                                TRUE ~ agency))

  data
}
