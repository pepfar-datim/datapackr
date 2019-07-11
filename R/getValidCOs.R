#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom utils URLencode
#' @title Compile valid categoryOptions for a given dataset
#' 
#' @description
#' Queries DATIM API against the dataElementGroups endpoint and returns list of
#' all dataElements associated with a provided dataSet, as well as all valid 
#' categoryOptions associated with that dataElement.
#'
#' @param data_element_group.id DATIM dataElementGroup id to filter against.
#' 
#' @return validCategoryOptions
#'
getValidCOs <- function(data_element_group.id) {
  
  # Query dataElementGroups end point ####
  dataElementGroup <- 
    datapackr::api_call("dataElementGroups") %>%
  # Filter to include only the dataElementGroup specified
    datapackr::api_filter(field = "id", operation = "eq", match = "XUA8pDYjPsw") %>%
      # TODO generate match automatically based on current FY...
    datapackr::api_fields("dataElements[id,name,categoryCombo[categories[id,name,categoryOptions[id,name]]]]") %>%
    datapackr::api_get() %>%
    tidyr::unnest() %>%
    tidyr::unnest() %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      grp = dplyr::case_when(stringr::str_detect(name1, "Age") ~ "Age",
                             stringr::str_detect(name1, "Sex") ~ "Sex",
                             stringr::str_detect(name1, "Key Pop") ~ "KP")
      ) %>%
  # Keep only Age, Sex, KP categoryOptions
    tidyr::drop_na(grp) %>%
    dplyr::select(
      data_element.name = name,
      data_element.id = id,
      category_combo.name = name1,
      category_combo.id = id1,
      grp,
      category_option.name = name2,
      category_combo.id = id2)
  
   
}