#' @export
#' @title Map categoryoptioncombos to underlying categoryoptions.
#'
#' @description
#' maps categoryoptioncombos to underlying categoryoptions.
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return dataframe of categoryoptioncombos mapped to underlying category
#' options
#'
map_COCs_to_COs <- function(d2_session = dynGet("d2_default_session",
                                                inherits = TRUE)) {

  datimutils::getMetadata("categoryOptionCombos",
                          "categoryCombo.id:!eq:wUpfppgjEza",
  fields="id,name,categoryOptions[id,name],categoryCombos[id,name]",
  d2_session = d2_session)


}
