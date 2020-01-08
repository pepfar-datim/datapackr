#' @importFrom magrittr %>% %<>%
#' @importFrom utils URLencode
#' @title Compile valid categoryOptions for a given data element group
#' 
#' @description
#' Queries DATIM API against the dataElementGroups endpoint and returns list of
#' all dataElements associated with a provided data element group, as well as all valid 
#' categoryOptions associated with that dataElement. Should be noted that this does
#' not guarentee that the category option will actually be in use in the data entry form. 
#' 
#'
#' @param deg_id DATIM dataElementGroup id to filter against.
#' 
#' @return validCategoryOptions
#'
getValidCOs <- function(deg_id = "XUA8pDYjPsw") {
  

  # Query dataElementGroups end point ####

    datapackr::api_call("dataElementGroups") %>%
    # Filter to include only the dataElementGroup specified
    datapackr::api_filter(field = "id", operation = "eq", match = deg_id ) %>%
    # TODO generate match automatically based on current FY...
    datapackr::api_fields("dataElements[id~rename(de_id),name~rename(de_name),categoryCombo[categories[id~rename(categoryid),name~rename(categoryname),categoryOptions[id~rename(coid),name~rename(coname)]]]]") %>%
    datapackr::api_get() %>%
    tidyr::unnest(cols = dataElements)  %>% 
    tidyr::unnest(cols = categoryCombo.categories) %>% 
    tidyr::unnest(cols = categoryOptions) %>%
    dplyr::mutate(
      grp = dplyr::case_when(stringr::str_detect(categoryname, "Age") ~ "Age",
                             stringr::str_detect(categoryname, "Sex") ~ "Sex",
                             stringr::str_detect(categoryname, "Key Pop") ~ "KP")
      ) %>%
  # Keep only Age, Sex, KP categoryOptions
    tidyr::drop_na(grp) 
  
}