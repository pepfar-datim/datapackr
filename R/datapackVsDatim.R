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
  beautify <- function(data){
    
  }
  
# start off with dedups included
  d <- datapackr::exportDistributedDataToDATIM(d, keep_dedup = TRUE)
  
  d$datim$MER$value <- as.numeric(d$datim$MER$value)
  d$datim$subnat_impatt$value <- as.numeric(d$datim$subnat_impatt$value)
  datapack_data <- dplyr::bind_rows(d$datim$MER,d$datim$subnat_impatt)

  # ensure datapack_data has the expected columns
  if (!identical(names(datapack_data),
                 c("dataElement",
                   "period",
                   "orgUnit",
                   "categoryOptionCombo",
                   "attributeOptionCombo",
                   "value"))) {
    stop("The column names of your data aren't as expected by compareData_DatapackVsDatim.")
  }
  # rename columns to fit standards

  datapack_data <- datapack_data %>%
    dplyr::rename(
      datapack_value = value,
      data_element_uid = dataElement,
      org_unit_uid = orgUnit,
      category_option_combo_uid = categoryOptionCombo,
      attribute_option_combo_code = attributeOptionCombo)  
  
  datapack_data_psnu_w_dedup <- dplyr::group_by(datapack_data,
                                                data_element_uid,
                                                org_unit_uid,
                                                category_option_combo_uid) %>% 
    dplyr::summarise(datapack_value = sum(datapack_value)) %>% 
    dplyr::ungroup()

  datapack_data_psnu_x_im_wo_dedup <- datapack_data %>% 
    dplyr::filter(attribute_option_combo_code != "99999")
  
  # Get data from datim using data value sets
  
  org_unit_uids <- d$info$country_uids
  cop_yyyy <- d$info$cop_year %>% as.character()
  fiscal_yy <- (d$info$cop_year + 1) %>% 
    stringr::str_sub(3,4)
  
  dataset_uids <- datapackcommons::getDatasetUids(fiscal_yy, "targets") %>% 
    c(datapackcommons::getDatasetUids(fiscal_yy, "subnat_impatt"))
  
    parameters <- tibble::tibble(key = "dataSet", value = dataset_uids) %>% 
      dplyr::bind_rows(c(key = "period",  value = paste0(cop_yyyy, "Oct")))
      
    parameters <- tibble::tribble(
    ~ key, ~ value,
    "children", "true", 
    "categoryOptionComboIdScheme", "code",
    "includeDeleted", "false"
  ) %>%
    dplyr::bind_rows(tibble::tibble(key = "orgUnit", value = org_unit_uids)) %>%
    dplyr::bind_rows(parameters)

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
    dplyr::filter(datim_value != 0) %>% # we don't import 0s up front so we should ignore any here
    dplyr::filter(datim_value != "")
  
  datim_data$attribute_option_combo_code[datim_data$attribute_option_combo_code == "00000" |
                                           datim_data$attribute_option_combo_code == "00001"] <- "99999"
  
  
  datim_data_psnu_w_dedup <- dplyr::group_by(datim_data,
                                             data_element_uid,
                                             org_unit_uid,
                                             category_option_combo_uid) %>% 
    dplyr::summarise(datim_value = sum(datim_value)) %>% 
    dplyr::ungroup()
  
  datim_data_psnu_x_im_wo_dedup <- datim_data %>% 
    dplyr::filter(attribute_option_combo_code != "99999")
  
  data_psnu_w_dedup <- dplyr::full_join(datim_data_psnu_w_dedup, 
                                        datapack_data_psnu_w_dedup)
  
  data_psnu_im_wo_dedup <- dplyr::full_join(datim_data_psnu_x_im_wo_dedup, 
                                             datapack_data_psnu_x_im_wo_dedup)

  # Find the cases with different values. These should be  imported into DATIM
  data_different_value <-
    dplyr::filter(data_psnu_im_wo_dedup, 
                  datapack_value != datim_value | is.na(datim_value)) %>%
  dplyr::select(data_element_uid,
                period,
                org_unit_uid,
                category_option_combo_uid,
                attribute_option_combo_code,
                datapack_value)
  
  
  data_datim_only <- dplyr::filter(data_psnu_im_wo_dedup, 
                                   is.na(datapack_value)) %>% 
    select(data_element_uid,
           period,
           org_unit_uid,
           category_option_combo_uid,
           attribute_option_combo_code,
           datim_value)

  #Make the data prettier
  data$data_element <-datimvalidation::remapDEs(data$data_element_uid,mode_in="id",mode_out = "shortName")
  data$disagg <- datimvalidation::remapCategoryOptionCombos(data$category_option_combo_uid,mode_in = "id",mode_out = "name")
  
  data_pretty <-
    data %>%  dplyr::select(data_element,
                            org_unit_uid,
                            period,
                            disagg,
                            mechanism = attribute_option_combo_code,
                            datapack_value,
                            datim_value) 
  
    #Adorn the sites
  
  psnus <- datapackr::valid_PSNUs %>% dplyr::select(psnu, psnu_uid)
  
  data_pretty %<>% 
    dplyr::left_join(psnus, by = c("org_unit_uid" = "psnu_uid")) %>%
    dplyr::select(psnu,
                  data_element,
                  disagg,
                  mechanism,
                  datapack_value,
                  datim_value) %>% 
    dplyr::mutate(difference = datapack_value - datim_value) %>% 
    dplyr::mutate(effect = dplyr::case_when(is.na(difference) & is.na(datapack_value) ~ "Delete",
                                            is.na(difference) & is.na(datim_value) ~ "Create",
                                            !is.na(difference) & difference != 0 ~ "Update",
                                            difference == 0 ~ "No Change"))
    
   list(
    data_pretty = data_pretty,
    updates = data_different_value,
    deletes = data_datim_only
  )
}
