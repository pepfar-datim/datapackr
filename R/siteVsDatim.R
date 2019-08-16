# Retry API is part of data pack commons but I don't currently export it.
#' @title RetryAPI(api_url, content_type, max_attempts)
#' 
#' @description Submits specified api request up to specified maximum times
#' stopping when expected content type is returned with 200 response
#' @param api_url string - full url for web request
#' @param content_type string - expected type of content in reposne e.d 'application/json'
#' @param max_attempts integer - maximum number of retries for succesful request
#' @return  full api response
#'
RetryAPI <- function(api_url, content_type, max_attempts = 10){
  for(i in 1:max_attempts){
    try({
      response <- httr::GET(api_url, httr::timeout(180))
      if (response$status_code == 200L && 
          response$url == api_url && 
          httr::http_type(response) == content_type){
        return(response)
      }
    })
    Sys.sleep(i/2 + 1)
  }
  # if i am here all my attempts failed
  stop(paste("Failed to obtain valid response in RetryAPI for:", api_url))
}

SiteVsDatim <- function(d, org_unit_uid = NULL, period, base_url = getOption("baseurl")){
  
  if (is.null(org_unit_uid)) {
    org_unit_uid <- d$info$datapack_uid
  }
  
  if(period == "2019Oct"){
    print("put data sets here")
  }
  
  site_data <- d$datim$site_data %>% 
    dplyr::group_by(dataElement,period,orgUnit,categoryOptionCombo,attributeOptionCombo) %>% 
    dplyr::summarise(value=round(sum(as.numeric(value)))) %>% 
    dplyr::rename( "tool_value" = "value")

  api_call <- "https://triage.datim.org/api/30/dataValueSets.csv?dataSet=nIHNMxuPUORX&dataSet=sBv1dj90IX6&dataSet=C2G7IyPPrvD&dataSet=HiJieecLXxNX&period=2019Oct&orgUnit=XtxUYCsDWrR&children=true&categoryOptionComboIdScheme=code&includeDeleted=false"
  
  
  
# note the deleted column comes back empty (missing a comma in fact) which results in a warning
# we can disregard that warning
#  row col   expected     actual         file
#  1  -- 11 columns 10 columns literal data
  
  datim_data <-  RetryAPI(api_call,"application/csv") 
    datim_data <- datim_data %>%   httr::content(., "text") %>% 
    readr::read_csv(col_names = TRUE, col_types = readr::cols(.default = "c", 
                                                              value = "d", 
                                                              lastupdated = "T")) %>%
      dplyr::rename(dataElement = dataelement, 
                    orgUnit = orgunit,
                    categoryOptionCombo = categoryoptioncombo,
                    attributeOptionCombo = attributeoptioncombo,
                    datim_value = value)
    
    data=dplyr::full_join(site_data,datim_data)
    data_dedups <- dplyr::filter(data, attributeOptionCombo == "00000" | attributeOptionCombo == "00001")
    data <- dplyr::filter(data, attributeOptionCombo != "00000" & attributeOptionCombo != "00001")
    data_different_value <- dplyr::filter(data, tool_value != datim_value)
    data_datim_only <- dplyr::filter(data, is.na(tool_value))
    data_site_tool_only <- dplyr::filter(data, is.na(datim_value))
    

}
