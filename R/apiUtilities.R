#' @export
#' @title Start DATIM API query and specify table
#' 
#' @description
#' Constructs URL for DATIM API query against specified table without paging.
#' 
#' @param endpoint Character. DATIM API endpoint to query.
#' 
#' @return Web-encoded URL for DATIM API query.
#' 
api_call <- function(endpoint) {
  
  URL <- paste0(
    getOption("baseurl"),"api/",datapackr::api_version(),
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
    ifelse(operation == "in", paste0("[",match,"]") , match)) %>% #TODO: Accommodate match coming in as character vector instead of string
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
    httr::GET(httr::timeout(180)) %>%
    httr::content(., "text") %>%
    jsonlite::fromJSON(., flatten = TRUE) %>%
    do.call(rbind.data.frame, .)
    
  return(r)
}


#' @export
#' @title Query DATIM SQL View.
#' 
#' @description
#' Queries a DATIM SQL View and returns data object.
#' 
#' @param sqlView uid of sqlView table to query.
#' @param var Variable to substitute into SQL query. Only supply if SQL view is
#' of type query.
#' 
#' @return Web-encoded URL for DATIM API query.
#' 
api_sql_call <- function(sqlView, var = NULL) {
  
  URL <-   
    paste0(
      getOption("baseurl"),"api/",datapackr::api_version(),
      "/sqlViews/",
      sqlView,
      "/data.csv?",
      ifelse(!is.null(var),paste0("var=dataSets:",var,"&"),""),
      "paging=false") %>%
    utils::URLencode()
    
  r <- 
    URL %>%
    httr::GET(httr::timeout(180)) %>%
    httr::content(., "text") %>%
    readr::read_csv()
    
  return(r)
  
}
