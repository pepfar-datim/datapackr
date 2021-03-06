#' @export
#' @title getMechanismViewFromDATIM
#'
#' @inheritParams unPackTool
#' @inheritParams getMechanismViewFromDATIM
#'
#' @return Mechanism List
#'
getMechanismViewFromDATIM <- function( cop_year = NULL,
                                  d2_session = dynGet("d2_default_session",
                                                      inherits = TRUE)) {
  paste0(d2_session$base_url, "api/",datapackr::api_version(),
         "/sqlViews/fgUtV6e9YIX/data.csv?paging=false") %>%
    {if (!is.null(cop_year))
      paste0(., "&filter=startdate:lt:", cop_year+1, "-10-01",
             "&filter=enddate:gt:", cop_year, "-09-30")
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
  print(cached_mechs_path)
  can_read_file <- file.access(cached_mechs_path, 4) == 0
  can_write_file <-file.access(dirname(cached_mechs_path), 2) == 0
  
    # Check whether Cached Mech List is stale
    if (is.null(d2_session$max_cache_age)) {
      max_cache_age <- "1 day"
    } else {
      max_cache_age <- d2_session$max_cache_age
    } 
    
  if (file.exists(cached_mechs_path) & can_read_file) {
    is_fresh <-
      lubridate::as.duration(lubridate::interval(file.info(cached_mechs_path)$mtime,Sys.time())) < lubridate::duration(max_cache_age)
  } else{
    is_fresh<-FALSE
  }

  
  if (is_fresh & can_read_file) {
    interactive_print("Loading cached mechs file")
    mechs <- readRDS(cached_mechs_path)
  }
  
  if (!is_fresh) {
    interactive_print("Fetching new mechs file from DATIM")
    mechs <-
      getMechanismViewFromDATIM(
                                d2_session = d2_session)
    if (can_write_file) {
      interactive_print(paste0("Overwriting stale mechanisms view to ", cached_mechs_path))
      saveRDS(mechs, file = cached_mechs_path)
    }
  }
  
  
  # Filter by OU from a vector of country UIDs
  if (!is.null(country_uids)) {
    
    ous<-datapackr::valid_PSNUs %>% 
      dplyr::select(ou,ou_id,country_uid) %>% 
      dplyr::distinct() %>% 
      dplyr::filter(country_uid %in% country_uids) %>% 
      dplyr::pull(ou) %>% 
      unique(.)
  
    mechs %<>% dplyr::filter(ou %in% ous) 
    }

  if (!is.null(cop_year)) {
    mechs %<>%
        dplyr::filter(
                      startdate < paste0(cop_year+1,"-10-01"),
                      enddate > paste0(cop_year,"-09-30"))
  }

    # Include Dedupe or MOH
      if (!include_dedupe ) {
        dedupe <- c("X8hrDf6bLDC","YGT1o7UxfFu")
        mechs %<>%  dplyr::filter( (attributeOptionCombo %in% dedupe ) == FALSE )
      }
          
      # Include Dedupe or MOH
      if (!include_MOH ) {
        MOH <- c("QCJpv5aDCJU","TRX0yuTsJA9")
        mechs %<>%  dplyr::filter( (attributeOptionCombo %in% MOH ) == FALSE )
      }
          
  structure_ok <- dplyr::setequal(names(empty_mechs_view), names(mechs))

  if (!structure_ok) warning("Mechanism view names are not correct!")

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
