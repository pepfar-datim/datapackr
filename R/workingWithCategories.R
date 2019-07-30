#' @export
#' @title Map categoryoptioncombos to underlying categoryoptions.
#' 
#' @description
#' maps categoryoptioncombos to underlying categoryoptions.
#' 
#' @return dataframe of categoryoptioncombos mapped to underlying category
#' options
#'
map_COCs_to_COs <- function() {
  
  COCs_to_COs <- api_call("categoryOptionCombos") %>%
    api_filter("categoryCombo.name", "!like", "Funding Mechanism") %>%
    api_fields("id,name,categoryOptions[id,name]") %>%
    api_get()
  
  return(COCs_to_COs)
}




#' @export
#' @title Map categoryoptioncombos to underlying categoryoptions.
#' 
#' @description
#' maps categoryoptioncombos to underlying categoryoptions.
#' 
#' @return dataframe of categoryoptioncombos mapped to underlying category
#' options
#'
map_Cs_to_COs <- function() {
  
  str <-
    paste0(
      "Age|Sex|Key Population|HIV Status|Test Indication",
      "|Violence Service Type|Program Status|Pregnancy Status",
      "|New Existing (ART|Art)|Adverse Event|HIV Commodity|Key Cadres",
      "|Observed Commodity|Outcome Type|Receiving ART|Service Delivery Point",
      "|TB Therapy Type")
  
  Cs_to_COs <- api_call("categories") %>%
    api_fields("id,name,categoryOptions[id,name]") %>%
    api_get() %>%
    dplyr::mutate(
      categoryoptiongroup = stringr::str_extract(name, str),
      categoryoptiongroup = ifelse(is.na(categoryoptiongroup),name,categoryoptiongroup)) %>%
    dplyr::select(categoryoptiongroup, categoryOptions) %>%
    dplyr::filter(!is.na(categoryoptiongroup)) %>%
    tidyr::unnest(categoryOptions) %>%
    dplyr::distinct() %>%
    dplyr::rename(categoryoption = name, categoryoptionuid = id) %>%
    dplyr::arrange(categoryoptiongroup, categoryoption) #%>%
  # dplyr::group_by(categoryoptiongroup) %>%
  # tidyr::nest() %>%
  # tibble::deframe()
  
  return(Cs_to_COs)
}



#' @export
#' @title Map code list to COs
#' 
#' @description
#' Combine DEs and COCs from code list to COs, broken out by group.
#' 
#' @return fullCodeList
#'
DEs_COCs_COs_Cs <- function() {
  
  # Pull from code list instead of dataElementGroups to make sure constrains
    # list of categoryOptionCombos to only those actually allowable in forms.
  codeList <- pullFullCodeList(FY = cop_year() + 1)
  
  fullCodeList <- codeList %>%
  
    ## Map COCs from code list to COs
    dplyr::left_join(
      map_COCs_to_COs() %>% dplyr::select(-name),
      by = c("categoryoptioncombouid" = "id")) %>%
    
    ## Pull COs apart
    tidyr::unnest(categoryOptions) %>%
    dplyr::rename(categoryoption = name, categoryoptionuid = id) %>%
    
    ## Map COs to Cs
    dplyr::left_join(
      map_Cs_to_COs() %>% dplyr::select(-categoryoption),
      by = c("categoryoptionuid" = "categoryoptionuid")) %>%
    dplyr::select(-categoryoption) %>% # TODO: Find a way to pivot both the codes and the names...
    
    ## Group COs by type and convert to columns to make DE-COCs unique to each row.
    dplyr::group_by(dataelement, dataelementuid, categoryoptioncombo, categoryoptioncombouid) %>%
    tidyr::spread(key = categoryoptiongroup, value = categoryoptionuid) %>%
    dplyr::ungroup()
  
  return(fullCodeList)
}
