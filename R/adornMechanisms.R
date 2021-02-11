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
#' @param operating_units
#' @param cop_year Numeric value of COP Fiscal Year to filter mechanism list by.
#' Ex: For mechanisms active in FY 2020, pertaining to COP 2019, enter
#' \code{2019}. If a FY is not supplied, returns entire mechanism list.
#' @param uids Character vector of uids to filter list by.
#' @param include_dedupe Logical. If TRUE will include deduplication mechanisms.
#' Default is FALSE.
#' @param include_MOH Logical. If TRUE will include MOH mechanisms. Default is
#' FALSE.
#' @param d2_session 
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
#' @title getMechanismView(d2_session, support_files_path)
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
#' @param d2_session datimutils d2Session object
#' @param cached_mechs_path Local file path to the cached mechanisms file. 
#'
#' @description Retrieves a view of mechanisms with partners and agencies
#' The function will attempt to read from a cached file, if defined and accessible.
#'Otherwise, if the user is logged in, the view
#' will be obtained from DATIM. Otherwise, an empty dataframe is returned.
#'
#' @return Mechs
#'
getMechanismView <- function(country_uids = NULL,
                             cop_year = NULL,
                             include_dedupe = FALSE,
                             include_MOH = FALSE,
                             d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE),
                             cached_mechs_path = paste0(Sys.getenv("support_files_directory"), "mechs.rds")) {
  
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

  if (file.access(cached_mechs_path, 4) == 0) {
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
      
    
    
  } else {
    if (!isLoggedIn(d2_session)) {
      warning("You are not logged in but have requested a mechanism view.")
      return(empty_mechs_view)
    }
      
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
      
  # Pull Mechs
    mechs <-
      getMechanismViewFromDATIM(operating_units = operating_units,
                                cop_year = cop_year,
                                include_dedupe = include_dedupe,
                                include_MOH = include_MOH,
                                d2_session = d2_session)
  }

  structure_ok <- dplyr::setequal(names(empty_mechs_view), names(mechs))

  if (!structure_ok) {warning("Mechanism structure is not correct.")}

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
