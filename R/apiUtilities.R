#' @export
#' @title Returns a version of the DHIS2 API for the current version of DATIM
#'
#' @return API version.
#'
api_version <- function() {
  "33"
}

#' @export
#' @title Start DATIM API query and specify table
#'
#' @description
#' Constructs URL for DATIM API query against specified table without paging.
#'
#' @param endpoint Character. DATIM API endpoint to query.
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return Web-encoded URL for DATIM API query.
#'
api_call <- function(endpoint,
                     d2_session = dynGet("d2_default_session",
                                         inherits = TRUE)) {

  URL <- paste0(
    d2_session$base_url, "api/", datapackr::api_version(),
    "/",
    endpoint,
    ".json?paging=false") %>%
    utils::URLencode()

  return(URL)
}

#' @export
#' @title Filter a DATIM API query.
#'
#' @description
#' Adds filter to DATIM API query and encodes for web.
#'
#' @param api_call Base DATIM API query, specifying API table and setting paging
#' as false.
#' @param field Endpoint field aginst which to filter.
#' @param operation Operation to apply as filter. See
#' \href{https://docs.dhis2.org/2.22/en/developer/html/ch01s08.html}{DHIS2 Web API documentation}
#' for valid operators.
#' @param match Text to match using \code{operator}.
#'
#' @return Web-encoded URL for DATIM API query.
#'
api_filter <- function(api_call, field, operation, match) {

  URL <- paste0(
    api_call,
    "&filter=",
    field,
    ":",
    operation,
    ":",
    ifelse(operation == "in", paste0("[", match, "]"), match)) %>%
    utils::URLencode()

  return(URL)
}

#' @export
#' @title Select fields to return from a DATIM API query.
#'
#' @description
#' Specifies fields to return from DATIM API query and encodes for web.
#'
#' @param api_call Base DATIM API query, specifying API table and setting paging
#' as false.
#' @param fields Fields to return. No need to include \code{&fields=}.
#'
#' @return Web-encoded URL for DATIM API query.
#'
api_fields <- function(api_call, fields) {
  URL <- paste0(
    api_call,
    "&fields=",
    fields) %>%
    utils::URLencode()

  return(URL)
}

#' @export
#' @title Execute and return a DATIM API query.
#'
#' @description
#' Gets and flattens DATIM API query as dataframe.
#'
#' @param api_call Base DATIM API query, specifying API table and setting paging
#' as false.
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return Result of DATIM API query returned as dataframe.
#'
api_get <- function(api_call,
                    d2_session = dynGet("d2_default_session",
                                        inherits = TRUE)) {
  r <- api_call %>%
    httr::GET(httr::timeout(180),
              handle = d2_session$handle) %>%
    httr::content(., "text") %>%
    jsonlite::fromJSON(., flatten = TRUE) %>%
    do.call(rbind.data.frame, .)

  return(r)
}
