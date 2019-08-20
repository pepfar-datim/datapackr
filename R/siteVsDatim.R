SiteVsDatim <- function(site_data, 
                        org_unit_uid, 
                        period, 
                        base_url = getOption("baseurl")){
  
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
    
# pull dedups into their own object
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
    
  list(data_dedups = data_dedups,
       data_different_value = data_different_value,
       data_datim_only = data_datim_only,
       data_site_tool_only = data_site_tool_only
       )
  }