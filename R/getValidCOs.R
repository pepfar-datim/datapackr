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
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return validCategoryOptions
#'
getValidCOs <- function(data_element_group.id = NULL,
                        d2_session = dynGet("d2_default_session",
                                            inherits = TRUE)) {


    #TODO: Replace this with equivalent
    de_groups<-datapackr::api_call("dataElementGroups", d2_session = d2_session) %>%
    # Filter to include only the dataElementGroup specified
    datapackr::api_filter(field = "id",
                          operation = "eq",
                          match = "XUA8pDYjPsw") %>%
    # TODO generate match automatically based on current FY...
    datapackr::api_fields(
      "dataElements[id,name,categoryCombo[categories[id,name,categoryOptions[id,name]]]]" # nolint
    ) %>%
    datapackr::api_get(d2_session = d2_session) %>%
    tidyr::unnest(cols = "dataElements") %>% #TODO: unnest requires specific columns to unnest now
    dplyr::rename(data_element.name = "name",
                  data_element.id = "id") %>%
    tidyr::unnest(cols = "categoryCombo.categories") %>%
    dplyr::rename(category_combo.name = "name",
                  category_combo.id  = "id") %>%
  tidyr::unnest(cols = "categoryOptions") %>%
    dplyr::rename(category_option.name = "name",
                  category_option.id  = "id") %>%
    dplyr::mutate(
      grp = dplyr::case_when(
        stringr::str_detect(category_combo.name, "Age") ~ "Age",
        stringr::str_detect(category_combo.name, "Sex") ~ "Sex",
        stringr::str_detect(category_combo.name, "Key Pop") ~ "KP"
      )
      ) %>%
  # Keep only Age, Sex, KP categoryOptions
    tidyr::drop_na(grp) %>%
    dplyr::select(
      data_element.name,
      data_element.id,
      category_combo.name,
      category_combo.id,
      grp,
      category_option.name,
      category_option.id)

  if (!is.null(data_element_group.id)) {
    return(dplyr::filter(de_groups, data_element_group.id %in% data_element_group.id))
  } else {
    return(de_groups)
  }

}
