#' Title
#'
#' @param d2_session 
#'
#' @return
#' @export
#'
#' @examples
getMechanismViewFromDATIM <- function(d2_session = dynGet("d2_default_session",
                                                          inherits = TRUE)) {
  
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
  
  if (!isLoggedIn(d2_session)) {
    warning("You are not logged in but have requested a mechanism view.")
    return(empty_mechs_view)
  } else {
    paste0(d2_session$base_url,
           "api/sqlViews/fgUtV6e9YIX/data.csv") %>%
      httr::GET(httr::timeout(180),
                handle = d2_session$handle) %>%
      httr::content(., "text") %>%
      readr::read_csv(col_names = TRUE) %>%
      dplyr::rename(
        mechanism_desc = mechanism,
        attributeOptionCombo = uid,
        mechanism_code = code,
        partner_desc = partner,
        partner_id = primeid
      )
  }
}



#' @export
#' @importFrom magrittr %>% %<>%
#' @title getMechanismView(d2_session, cached_mechs_path)
#' @description Retrieves a view of mechanisms with partners and agencies
#' The function will attempt to read from a cached file, if defined and accessible.
#'Otherwise, if the user is logged in, the view
#' will be obtained from DATIM. Otherwise, an empty dataframe is returned.
#'
#' @param d2_session datimutils d2Session object
#' SQL view used defined via a environment variable.
#' @return Mechs
#'
getMechanismView <- function(d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE)) {
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
  
  cached_mechs_file = paste0(Sys.getenv("support_files_directory"), "mechs.rds")

  #Test for existence of support file
  can_read_file <- file.access(cached_mechs_file, 4) == 0
  
  if (can_read_file) {
    
    #Set a reasonable default here
    if (is.null(d2_session$max_cache_age)) {
      max_cache_age <- "1 day"
    } else {
      max_cache_age <- d2_session$max_cache_age
    }
    
    is_fresh <-
      lubridate::as.duration(lubridate::interval(Sys.time(), file.info(cached_mechs_file)$mtime)) < lubridate::duration(max_cache_age)
    if (is_fresh) {
      print(paste0("Using cached mechanism support file at ", cached_mechs_file))
      mechs <- readRDS(cached_mechs_file)
    }
  }
  
  if (!exists("mechs")) {
    mechs <- getMechanismViewFromDATIM(d2_session = d2_session)
    #TODO: Need to check and be sure we can write here.
    print(paste0("Overwriting stale mechanisms view to ", cached_mechs_file))
    saveRDS(mechs, file = cached_mechs_file)
  }
    
  structure_ok <- dplyr::setequal(names(empty_mechs_view), names(mechs))

  if (!structure_ok) {warning("Mechanism structure is not correct.")}
  
  mechs

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


  
  mechs <- getMechanismView(d2_session = d2_session) %>%
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
