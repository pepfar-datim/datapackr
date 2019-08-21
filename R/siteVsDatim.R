#' @export
#' @title CompareData_SiteVsDatim
#' 
#' @description Compares the import file from a parsed site tool with target date in DATIM.
#' @param site_data data frame - d$datim$site_data object as of COP 19
#' @param org_unit_uid string - the org unit uid of the site tool (d$info$datapack_uid for COP 19)
#' @param period string - ISO format for fiscal year of the site tool e.g. 2019Oct for COP 19 
#' @param base_url string - base address of instance (text before api/ in URL)
#' @return  list object of differences $data_different_value, $data_datim_only and $data_site_tool_only

CompareData_SiteVsDatim <- function(site_data, 
                                    org_unit_uid, 
                                    period, 
                                    base_url = getOption("baseurl")){

# this is a very specific and self contained function, so the details of each year's 
# site tool are are hard coded here
  if(period == "2019Oct"){
    parameters <- tibble::tribble(~key, ~value,
                                  "dataSet", "nIHNMxuPUORX",
                                  "dataSet", "sBv1dj90IX6",
                                  "dataSet", "C2G7IyPPrvD",
                                  "dataSet", "HiJieecLXxNX",
                                  "period", "2019Oct",
                                  "orgUnit", org_unit_uid,
                                  "children", "true",
                                  "categoryOptionComboIdScheme", "code",
                                  "includeDeleted", "false")
  } else if{
    stop("You are trying to compare a site tool for an unsupported period.")
  }
  
# rename columns to fit standards
# aggregate duplicate rows from site tool data
  site_data <- site_data %>%
    dplyr::rename(tool_value = value, data_element_uid = dataElement, 
                  org_unit_uid = orgUnit,
                  category_option_combo_uid = categoryOptionCombo,
                  attribute_option_combo_code = attributeOptionCombo) %>% 
    dplyr::group_by(data_element_uid, period, org_unit_uid,
                    category_option_combo_uid, attribute_option_combo_code) %>%
    dplyr::summarise(tool_value=round(sum(as.numeric(tool_value))))
    
# get data from datim
# rename to standard names
  datim_data <- GetDataValueSet(parameters$key, parameters$value) %>% 
    dplyr::rename(datim_value = value, data_element_uid = data_element, 
                  org_unit_uid = org_unit,
                  category_option_combo_uid = category_option_combo,
                  attribute_option_combo_code = attribute_option_combo)

  data <- dplyr::full_join(site_data, datim_data)
    
# pull dedups into their own object - we do not currently return this 
  data_dedups <- dplyr::filter(data, 
                                 attribute_option_combo_code == "00000" | 
                                   attribute_option_combo_code == "00001")
  data <- dplyr::filter(data, 
                          attribute_option_combo_code != "00000" & 
                            attribute_option_combo_code != "00001")
    
# Find the cases with different values 
  data_different_value <- dplyr::filter(data, tool_value != datim_value)
  data_datim_only <- dplyr::filter(data, is.na(tool_value))
  data_site_tool_only <- dplyr::filter(data, is.na(datim_value))
    
  list(data_different_value = data_different_value,
       data_datim_only = data_datim_only,
       data_site_tool_only = data_site_tool_only
       )
  }