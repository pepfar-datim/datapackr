#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom utils URLencode
#' @title Compile Mechanism List for Site Tool
#'
#' @description
#' Extracts full list of valid mechanisms from DATIM for Fiscal Year & countries
#' specified and compiles these into the list as needed for use in the PEPFAR
#' Site Tool.
#'
#' @param country_uids Character vector of DATIM country IDs. This can only
#' include countries. Regional Operating Unit uids will not be accepted. If not
#' supplied, returns entire mechanism list, trimmed to user's DATIM permissions.
#' @param include_dedupe Logical. If TRUE will include deduplication mechanisms.
#' Default is FALSE.
#' @param include_MOH Logical. If TRUE will include MOH mechanisms. Default is
#' FALSE
#' @param cop_year Numeric value of COP Fiscal Year to filter mechanism list by.
#' Ex: For mechanisms active in FY 2020, pertaining to COP 2019, enter
#' \code{2019}. If a FY is not supplied, returns entire mechanism list.
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return A dataframe of mechanisms, including start and end dates, mechanism
#' code, partner name, funding agency, and related OU.
#'
getMechList <- function(country_uids = NULL,
                        include_dedupe = FALSE,
                        include_MOH = FALSE,
                        cop_year = NULL,
                        d2_session = dynGet("d2_default_session",
                                            inherits = TRUE)) {

  getMechsView <- function(filter = FALSE, field = NULL, operation = NULL, match = NULL,
                           d2_session = dynGet("d2_default_session",
                                               inherits = TRUE)) {


    paste0( d2_session$base_url, "api/", datapackr::api_version(),
           "/sqlViews/fgUtV6e9YIX/data.csv") %>%
      { if (filter)
        paste0(., "?filter=", field, ":", operation, ":",
               ifelse(operation == "in", paste0("[", paste0(match, collapse=", "), "]"), match))
        else .
        } %>%
      utils::URLencode() %>%
      httr::GET(httr::timeout(180), handle = d2_session$handle) %>%
      httr::content(., "text") %>%
      readr::read_csv(col_types = readr::cols(.default = "c")) %>%
      dplyr::rename(
        mechanism_desc = mechanism,
        attributeOptionCombo = uid,
        mechanism_code = code,
        partner_desc = partner,
        partner_id = primeid
      )
  }

  empty_mechs_view <- tibble::tibble(
    "mechanism_desc" = character(),
    "mechanism_code"= character(),
    "attributeOptionCombo" = character(),
    "partner_desc" = character(),
    "partner_id" = character(),
    "agency" = character(),
    "ou" = character(),
    "startdate" = character(),
    "enddate" = character()
  )

  if (!isLoggedIn(d2_session = d2_session)) {
    warning("You are not logged in but have requested a mechanism view.")
    return(empty_mechs_view)
  }

  # Convert country_uids to OU names for filtering
  if (!is.null(country_uids)) {
    ous <- api_call("organisationUnits",
                    d2_session = d2_session) %>%
      api_filter("id", "in", match = paste(country_uids, collapse = ", ")) %>%
      datapackr::api_fields("id,name,ancestors[id,name,organisationUnitGroups[id,name]],organisationUnitGroups[id,name]") %>% # nolint
      datapackr::api_get(d2_session = d2_session) %>%
      dplyr::mutate(
        ou = purrr::map_chr(ancestors, list("name", 3), .default = NA),
        ou = dplyr::if_else(is.na(ou), name, ou)
    )

  # Pull Mechs ####
    mechs <-
      getMechsView(filter = TRUE, field = "ou", operation = "in", match = ous$ou,
                   d2_session = d2_session)
  } else {
    mechs <-
      getMechsView(d2_session = d2_session)
  }

  # Filter by Fiscal Year ####
    if (!is.null(cop_year)) {
      mechs %<>%
        dplyr::filter((startdate < paste0(cop_year+1, "-10-01") &
                         enddate > paste0(cop_year, "-09-30"))
                      | mechanism_code %in% c("00000", "00001", "00100", "00200"))
    }

  # Handle Dedupes ####
    if (!include_dedupe) {
      mechs %<>%
        dplyr::filter(!mechanism_code %in% c("00000", "00001"))
    }

    if (include_dedupe & !is.null(country_uids)) {
      dedupes <-
        getMechsView(filter = TRUE,
                     field = "uid",
                     operation = "in",
                     match = c("X8hrDf6bLDC", "YGT1o7UxfFu"),
                     d2_session = d2_session)

      mechs %<>%
        dplyr::bind_rows(dedupes)
    }

  # Handle MOH mechs ####
    if (!include_MOH) {
      mechs %<>%
        dplyr::filter(!mechanism_code %in% c("00100", "00200"))
    }

    if (include_MOH & !is.null(country_uids)) {
      MOH <-
        getMechsView(filter = TRUE,
                     field = "uid",
                     operation = "in",
                     match = c("QCJpv5aDCJU", "TRX0yuTsJA9"),
                     d2_session = d2_session)

      mechs %<>%
        dplyr::bind_rows(MOH)
    }

  return(mechs)

}
