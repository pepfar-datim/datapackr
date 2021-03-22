#' @export
#' @title Map categoryoptioncombos to underlying categoryoptions.
#' 
#' @description
#' maps categoryoptioncombos to underlying categoryoptions.
#' 
#' @return dataframe of categoryoptioncombos mapped to underlying category
#' options
#'
map_COCs_to_COs <- function(d2_session = dynGet("d2_default_session",
                                                inherits = TRUE)) {
  
  COCs_to_COs <- api_call("categoryOptionCombos",
                          d2_session = d2_session) %>%
    api_filter("categoryCombo.name", "!like", "Funding Mechanism") %>%
    api_fields("id,name,categoryOptions[id,name],categoryCombos[id,name]") %>%
    api_get(d2_session = d2_session)
  
  return(COCs_to_COs)
}
