#' @export
#' @title compareData_SiteVsDatim
#'
#' @description Compares the import file from a parsed site tool with target date in DATIM.
#' @param site_data data frame - d$datim$site_data object as of COP 19
#' @param org_unit_uids character vector - the org unit uids of the site tool
#' - d$info$datapack_uid for COP 19 standard countries
#' - if regional and not all contries from the region are in the site tool data,
#'   list the countries individually
#' @param iso_fy string - ISO format for fiscal year of the site tool e.g. 2019Oct for COP 19
#' @param base_url string - base address of instance (text before api/ in URL)
#' @return  list object of differences $data_different_value, $data_datim_only and $data_site_tool_only

compareData_DatapackVsDatim <- function(d, base_url = getOption("baseurl")) {
  
  datapack_data <- d$datim$MER
  org_unit_uids <- d$info$country_uids
  cop_year <- d$info$cop_year
  
  # ensure datapack_data has the expected columns
  if (!identical(
    names(datapack_data),
    c(
      "dataElement",
      "period",
      "orgUnit",
      "categoryOptionCombo",
      "attributeOptionCombo",
      "value"
    )
  )) {
    stop("The column names of your data aren't as expected by compareData_DatapackVsDatim.")
  }

  dataset_uids <- cop_year %>% 
    stringr::str_sub(3,4) %>% #get last two digits of cop year
    datapackcommons::getDatasetUids("targets")
  
    parameters <- tibble::tibble(key = "dataSet", value = dataset_uids) %>% 
      dplyr::bind_rows(c(key = "period",  value = paste0(cop_year, "Oct")))
      
    parameters <- tibble::tribble(
    ~ key, ~ value,
    "children", "true", 
    "categoryOptionComboIdScheme", "code",
    "includeDeleted", "false"
  ) %>%
    dplyr::bind_rows(tibble::tibble(key = "orgUnit", value = org_unit_uids)) %>%
    dplyr::bind_rows(parameters)
  
    
  # rename site_data columns to fit standards
  # aggregate duplicate rows from site tool data as would be done before import
  
  datapack_data <- datapack_data %>%
    dplyr::rename(
      datapack_value = value,
      data_element_uid = dataElement,
      org_unit_uid = orgUnit,
      category_option_combo_uid = categoryOptionCombo,
      attribute_option_combo_code = attributeOptionCombo
    )
  
  # get data from datim
  # rename to standard names
  datim_data <-
    getDataValueSets(parameters$key, 
                     parameters$value, 
                     base_url = base_url) %>%
    dplyr::rename(
      datim_value = value,
      data_element_uid = data_element,
      org_unit_uid = org_unit,
      category_option_combo_uid = category_option_combo,
      attribute_option_combo_code = attribute_option_combo
    ) %>% 
    dplyr::filter(datim_value != 0) %>% 
    dplyr::filter(datim_value != "") 
  
  data <- dplyr::full_join(datapack_data, datim_data)

  # Find the cases with different values. These should be  imported into DATIM
  data_different_value <-
    dplyr::filter(data, tool_value != datim_value | is.na(datim_value)) %>%
  dplyr::select(data_element_uid,
                period,
                org_unit_uid,
                category_option_combo_uid,
                attribute_option_combo_code,
                tool_value)
  
  
  data_datim_only <- dplyr::filter(data, is.na(tool_value)) %>% 
    select(data_element_uid,period,org_unit_uid,category_option_combo_uid,attribute_option_combo_code,datim_value)

  #Make the data prettier
  data$data_element <-datimvalidation::remapDEs(data$data_element_uid,mode_in="id",mode_out = "shortName")
  data$disagg <- datimvalidation::remapCategoryOptionCombos(data$category_option_combo_uid,mode_in = "id",mode_out = "name")
  

  
  data_pretty <-
    data %>%  dplyr::select(data_element,
                            org_unit_uid,
                            period,
                            disagg,
                            mechanism = attribute_option_combo_code,
                            tool_value,
                            datim_value) #%>%
    #tidyr::gather(source,value,tool_value:datim_value) %>% 
    #mutate(source = plyr::mapvalues(source,
                #                    c("tool_value","datim_value"),
                 #                   c("Site Tool","DATIM")))
  
    #Adorn the sites
  site_list <- getSiteList(org_unit_uids) %>%
    dplyr::select(psnu, site = name, 
                  org_unit_uid=id)
  
  data_pretty %<>% 
    dplyr::left_join(site_list, by="org_unit_uid") %>%
    dplyr::select(psnu, site,
                  data_element,
                  disagg,
                  mechanism,
                  tool_value,
                  datim_value) %>% 
    dplyr::mutate(difference = tool_value - datim_value) %>% 
    dplyr::mutate(effect = dplyr::case_when(is.na(difference) & is.na(tool_value) ~ "Delete",
                                            is.na(difference) & is.na(datim_value) ~ "Create",
                                            !is.na(difference) & difference != 0 ~ "Update",
                                            difference == 0 ~ "No Change"))# %>%
    # dplyr::group_by(data_element,period,disagg,mechanism,source,psnu) %>% 
    # dplyr::summarise(value=sum(as.numeric(value),na.rm = TRUE)) %>% 
    # dplyr::ungroup()
    # 
 
   list(
    data_pretty = data_pretty,
    updates = data_different_value,
    deletes = data_datim_only
  )
}
