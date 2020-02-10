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
