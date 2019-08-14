compareSiteVsDatim <- function(site_tool_location, org_unit_uid = NULL, period, base_url = getOption("baseurl")){
  
  d <- datapackr::unPackSiteToolData(site_tool_location)
  if (is.null(org_unit_uid)) org_unit_uid <- d$info$datapack_uid
  if(period == "2019Oct"){
    print("put data sets here")
  }
  
  site_data <- d$datim$site_data %>% dplyr::rename( "tool_value" = "value")
  # site_data_dedups <- dplyr::filter(site_data, attributeOptionCombo == "00000" || attributeOptionCombo == "00001")
  # site_data <- dplyr::filter(site_data, attributeOptionCombo != "00000" && attributeOptionCombo != "00001")
  # 
  
  api_call <- "https://triage.datim.org/api/30/dataValueSets.csv?dataSet=nIHNMxuPUORX&dataSet=sBv1dj90IX6&dataSet=C2G7IyPPrvD&dataSet=HiJieecLXxNX&period=2019Oct&orgUnit=Qh4XMQJhbk8&children=true&categoryOptionComboIdScheme=code&includeDeleted=false"
  
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
    
temp=dplyr::full_join(site_data,datim_data)
      
}
