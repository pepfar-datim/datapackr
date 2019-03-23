#' @export
#' @title Start DATIM API query and specify table
#' 
#' @description
#' Constructs URL for DATIM API query against specified table without paging.
#' 
#' @param table Character. DATIM API table to query.
#' 
#' @return Web-encoded URL for DATIM API query.
#' 
api_call <- function(table) {
  
  URL <- paste0(
    getOption("baseurl"),"api/",datapackr::api_version(),
    "/",
    table,
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
#' @param filter Filter parameters. No need to include \code{&filter=}.
#' 
#' @return Web-encoded URL for DATIM API query.
#' 
api_filter <- function(api_call, filter) {
  
  URL <- paste0(
    api_call,
    "&filter=",
    filter) %>%
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
#' 
#' @return Result of DATIM API query returned as dataframe.
#' 
api_get <- function(api_call) {
  r <- api_call %>%
    httr::GET() %>%
    httr::content(., "text") %>%
    jsonlite::fromJSON(., flatten = TRUE) %>%
    do.call(rbind.data.frame, .)
    
  return(r)
}