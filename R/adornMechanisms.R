#' @export
#' @importFrom magrittr %>% %<>%
#' @title getMechanismView()
#'
#' @description Retreives a view of mechanisms with partners and agencies
#' The function will attempt to read from a cached file, if defined in 
#' the support_files_directory option has been set, and the mechs.rds file
#' is available to be read. Otherwise, if the user is logged in, the view 
#' will be obtained from DATIM. Otherwise, an empty dataframe is returned.
#' 
#' @return Mechs
#' 
getMechanismView <- function() {
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
  
  getMechanismViewFromDATIM <- function() {
    if (!isLoggedIn()) {
      warning("You are not logged in but have requested a mechanism view.")
      return(empty_mechs_view)
    } else {
      paste0(getOption("baseurl"),
             "api/sqlViews/fgUtV6e9YIX/data.csv") %>%
        httr::GET() %>%
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
  
  support_files_directory <- getOption("support_files_directory")
  
  if (is.null(support_files_directory)) {
    mechs <- getMechanismViewFromDATIM() 
  } else {
      
    cached_mechs_path <- paste0(support_files_directory,"mechs.rds")
    
    if (file.access(cached_mechs_path, 4) == 0) {
      mechs <- readRDS(cached_mechs_path)
    } else {mechs <- getMechanismViewFromDATIM()}
    
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
#' @param data
#' 
#' @return Modified data object
#' 
adornMechanisms <- function(data) {
  
  mechs <- getMechanismView() %>% 
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
