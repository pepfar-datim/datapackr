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

compareData_SiteVsDatim <- function(site_data,
                                    org_unit_uids,
                                    iso_fy,
                                    base_url = getOption("baseurl")) {
  
#inconsistent capitalization in column names across cop years 18 and 19 so convert to LC
  names(site_data) <- stringr::str_to_lower(names(site_data))
  
  # ensure site_data has the expected columns
  if (!identical(
    names(site_data),
    c(
      "dataelement",
      "period",
      "orgunit",
      "categoryoptioncombo",
      "attributeoptioncombo",
      "value"
    )
  )) {
    stop("The column names of your site data aren't as expected.")
  }

  if (iso_fy == "2019Oct") {
    parameters <- tibble::tribble(
      ~ key, ~ value,
      "dataSet", "nIHNMxuPUOR",
      "dataSet", "sBv1dj90IX6",
      "dataSet", "C2G7IyPPrvD",
      "dataSet", "HiJieecLXxN",
      "period",  "2019Oct"
    )
  } else if (iso_fy == "2018Oct") {
    parameters <- tibble::tribble(
      ~ key, ~ value,
      "dataSet", "BWBS39fydnX", #MER Targets: Community Based - DoD ONLY FY2019
      "dataSet", "l796jk9SW7q", #MER Targets: Community Based FY2019
      "dataSet", "X8sn5HE5inC", #MER Targets: Facility Based - DoD ONLY FY2019
      "dataSet", "eyI0UOWJnDk", #MER Targets: Facility Based FY2019
      "period", "2018Oct"
    )

# go from mech id to code for COP 18 data    
    site_data <-
      purrr::map(org_unit_uids, datimvalidation::getMechanismsMap) %>%
      dplyr::bind_rows() %>% 
      dplyr::select(id, code) %>% 
      dplyr::distinct() %>% 
      dplyr::right_join(site_data, by = c("id" = "attributeoptioncombo")) %>% 
      dplyr::mutate(attributeoptioncombo = code) %>% 
      dplyr::select(-id, -code) %>% 
      dplyr::select(dataelement,
                     period,
                     orgunit,
                     categoryoptioncombo,
                     attributeoptioncombo,
                     value
                     )
    } else {
      stop("You are trying to compare a site tool for an unsupported period.")
      }
  
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
  
  site_data <- site_data %>%
    dplyr::rename(
      tool_value = value,
      data_element_uid = dataelement,
      org_unit_uid = orgunit,
      category_option_combo_uid = categoryoptioncombo,
      attribute_option_combo_code = attributeoptioncombo
    ) %>%
    dplyr::group_by(
      data_element_uid,
      period,
      org_unit_uid,
      category_option_combo_uid,
      attribute_option_combo_code
    ) %>%
    dplyr::summarise(tool_value = round(sum(as.numeric(tool_value))))
  
  # This is a specific and self contained function, so the data sets of each year's
  # site tool are are hard coded here
  
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
    dplyr::filter(datim_value != 0)
  
  data <- dplyr::full_join(site_data, datim_data)
  
  # pull dedups into their own object - we do not currently return this
  data_dedups <- dplyr::filter(
    data,
    attribute_option_combo_code == "00000" |
      attribute_option_combo_code == "00001"
  )
  data <- dplyr::filter(
    data,
    attribute_option_combo_code != "00000" &
      attribute_option_combo_code != "00001"
  )
  
  # Find the cases with different values
  data_different_value <-
    dplyr::filter(data, tool_value != datim_value)
  data_matched_value <-
    dplyr::filter(data, tool_value == datim_value)
  data_datim_only <- dplyr::filter(data, is.na(tool_value))
  data_site_tool_only <- dplyr::filter(data, is.na(datim_value))
  
  list(
    dedup = data_dedups, 
    matched = data_matched_value,
    different = data_different_value,
    datim_only = data_datim_only,
    site_tool_only = data_site_tool_only
  )
}