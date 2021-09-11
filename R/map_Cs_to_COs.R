#' @export
#' @title Map categoryoptioncombos to underlying categoryoptions.
#'
#' @description
#' maps categoryoptioncombos to underlying categoryoptions.
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return dataframe of categoryoptioncombos mapped to underlying category
#' options
#'
map_Cs_to_COs <- function(d2_session = dynGet("d2_default_session",
                                              inherits = TRUE)) {

  str <-
    paste0(
      "Age|Sex|Key Population|HIV Status|Test Indication",
      "|Violence Service Type|Program Status|Pregnancy Status",
      "|New Existing (ART|Art)|Adverse Event|HIV Commodity|Key Cadres",
      "|Observed Commodity|Outcome Type|Receiving ART|Service Delivery Point",
      "|TB Therapy Type")

  Cs_to_COs <- api_call("categories",
                        d2_session = d2_session) %>%
    api_fields("id,name,categoryOptions[id,name]") %>%
    api_get(d2_session = d2_session) %>%
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
