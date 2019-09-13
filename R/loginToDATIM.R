#' @title GetCredentialsFromConsole()
#' @description Obtains the credentials information from the console. 
#' @return A list of baseurl, username and password
#'
GetCredentialsFromConsole <- function() {
  
  s <- list(dhis=list())
  s$dhis$username <- readline("Username: ")
  s$dhis$password <- getPass::getPass()
  s$dhis$baseurl <- readline("Server URL (ends with /): ")
  return(s)
}

#' @title LoadConfig(config_path)
#'
#' @description Loads a JSON configuration file to access a DHIS2 instance
#' @param config_path Path to the DHIS2 credentials file
#' @return A list of baseurl, username and password
#'
LoadConfigFile <- function(config_path = NA) {
  #Load from a file
  if (!is.na(config_path)) {
    if (file.access(config_path, mode = 4) == -1) {
      stop(paste("Cannot read configuration located at",config_path))
    }
    dhis_config <- jsonlite::fromJSON(config_path)
    #Mangle the config to be sure it always ends with a single forward slash. 
    #All other URIs should thus NOT begin with a /
    dhis_config$dhis$baseurl <-
      stringi::stri_reverse(gsub("^/+", "/", paste0("/",stringi::stri_reverse(dhis_config$dhis$baseurl))))
    return(dhis_config)
  } else {
    stop("You must specify a credentials file!") }
}

#' @export
#' @title Returns version of the API
#'
#' @return Version of the API
#' 
api_version <- function() { "29" }


#' @title Check login credentials
#' 
#' @description
#' Validates login credentials to make sure they've been provided correctly.
#' 
#' @param dhis_config List of DATIM login credentials, including username, 
#' password, and login URL.
#' 
ValidateConfig<-function(dhis_config) {
  
  is.baseurl <-function(x) { grepl("^http(?:[s])?://.+datim.org/$", x)}
  is.missing <- function(x) { is.na(x) || missing(x) || x == "" }
  
  if (is.missing(dhis_config$dhis$username)) {stop("Username cannot by blank.")}
  if (is.missing(dhis_config$dhis$password)) {stop("Username cannot by blank.")}
  if (!is.baseurl(dhis_config$dhis$baseurl)) {stop("The base url does not appear to be valid. It should end in /")}
}


#' @title DHISLogin(config_path)
#'
#' @param dhis_config List of DHIS2 credentials
#'
#' @return TRUE if you are able to login to the server. 
#' 
DHISLogin<-function(dhis_config) {
  
  url <- URLencode(URL = paste0(getOption("baseurl"), "api/",api_version(),"/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url ,
                 httr::authenticate(dhis_config$dhis$username, dhis_config$dhis$password),
                 httr::timeout(60))
  if(r$status != 200L){
    stop("Could not authenticate you with the server!")
  } else {
    me <- jsonlite::fromJSON(httr::content(r,as = "text"))
    options("organisationUnit" = me$organisationUnits$id)
    return("Successfully logged into DATIM")
  }
}


#' @export
#' @importFrom utils URLencode
#' @title Log into DATIM using DATIM credentials
#'
#' @description
#' Using provided DATIM credentials, logs into DATIM to allow other functions to
#' retrieve data from DATIM as needed. Can also be used to log into
#' non-production instances of DATIM. See Details for explanation. Where DATIM
#' credentials are not provided, uses Console and getPass to request these.
#' 
#' @param secrets A local path directing to a file containing DATIM login
#' credentials. See Details for more explanation.
#' @return Returns a boolean value indicating that the secrets file is valid by
#' accessing /api/me
#' 
#' @details
#' To securely connect with DATIM, create a JSON file structured as follows:
#'
#' \preformatted{
#' {
#'   "dhis": {
#'       "baseurl": "https://www.datim.org/",
#'       "username": "example",
#'       "password": "3x@mpl3!"
#'    }
#'  }
#' }
#' 
#' Replace the username and password with yours, and save this file in a secure
#' location on your computer. For more details about how to setup a hidden
#' folder or file on your operating system, see:
#' https://www.howtogeek.com/194671/how-to-hide-files-and-folders-on-every-operating-system/
#' 
#' You can also save multiple versions of this login file to allow login to
#' multiple instances of DATIM. For example, a document saved as devDATIM.json:
#' \preformatted{
#' {
#'   "dhis": {
#'       "baseurl": "https://dev.datim.org/",
#'       "username": "example",
#'       "password": "3x@mpl3!"
#'    }
#'  }
#' }
loginToDATIM <- function(secrets = NA) {
  #Load from a file
  if (is.null(secrets)) {
    s <- GetCredentialsFromConsole()
  } else {
    s <- LoadConfigFile(secrets)
  }
  
  ValidateConfig(s)
  options("baseurl" = s$dhis$baseurl)
  options("secrets" = secrets)
  options("maxCacheAge" = "7 days")
  DHISLogin(s)
}